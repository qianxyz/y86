use crate::syntax::*;

use regex::Regex;

/// A line in assembly code, with its components parsed.
#[derive(Debug, PartialEq, Eq)]
struct Line {
    /// The optional label leading the line, e.g. `loop: `.
    label: Option<String>,

    /// The optional statement (instruction or directive).
    statement: Option<Statement>,

    /// The source code line.
    src: String,
}

fn parse_line(src: &str) -> Result<Line, SyntaxError> {
    // strip comment after the first `#`
    let s = src.split('#').next().unwrap(); // iterator guaranteed not empty

    // split optional label and the rest of the line
    let (label, rest) = if let Some((l, s)) = s.split_once(':') {
        // strip whitespaces and validate label
        (Some(validate_label(l.trim())?.to_owned()), s)
    } else {
        (None, s)
    };

    // handle the rest as an optional statement
    let rest = rest.trim();
    let statement = if rest.is_empty() {
        None
    } else {
        Some(parse_statement(rest)?)
    };

    Ok(Line {
        label,
        statement,
        src: src.to_string(),
    })
}

fn validate_label(label: &str) -> Result<&str, SyntaxError> {
    let re = Regex::new(r"^[A-Za-z][0-9A-Za-z_]*$").unwrap();

    if re.is_match(label) {
        Ok(label)
    } else {
        Err(SyntaxError)
    }
}

fn parse_statement(src: &str) -> Result<Statement, SyntaxError> {
    let (command, args) = if let Some((s, t)) = src.split_once(char::is_whitespace) {
        (s, t.split(',').map(|s| s.trim()).collect())
    } else {
        (src, vec![])
    };

    macro_rules! parse_args {
        ($f:ident) => {{
            let [a] = args[..] else { return Err(SyntaxError); };
            $f(a)?
        }};
        ($f:ident, $g:ident) => {{
            let [a, b] = args[..] else { return Err(SyntaxError); };
            ($f(a)?, $g(b)?)
        }};
    }

    macro_rules! noargs {
        ($variant:ident) => {{
            if !args.is_empty() {
                return Err(SyntaxError);
            }
            Statement::$variant
        }};
    }

    macro_rules! directive {
        ($variant:ident) => {{
            let n = parse_args!(number);
            Statement::$variant(n.try_into().map_err(|_| SyntaxError)?)
        }};
    }

    let statement = match command {
        ".byte" => directive!(Dbyte),
        ".word" => directive!(Dword),
        ".long" => directive!(Dlong),
        ".quad" => directive!(Dquad),
        ".pos" => directive!(Dpos),
        ".align" => directive!(Dalign),

        "halt" => noargs!(Ihalt),
        "nop" => noargs!(Inop),

        "rrmovq" => {
            let (src, dest) = parse_args!(register, register);
            Statement::Irrmovq { src, dest }
        }
        "irmovq" => {
            let (value, dest) = parse_args!(constant, register);
            Statement::Iirmovq { dest, value }
        }
        "rmmovq" => {
            let (src, mem) = parse_args!(register, memory);
            Statement::Irmmovq { src, mem }
        }
        "mrmovq" => {
            let (mem, dest) = parse_args!(memory, register);
            Statement::Imrmovq { dest, mem }
        }

        "addq" | "subq" | "andq" | "xorq" => {
            let op = match command {
                "addq" => Op::Add,
                "subq" => Op::Sub,
                "andq" => Op::And,
                "xorq" => Op::Xor,
                _ => unreachable!(),
            };
            let (src, dest) = parse_args!(register, register);
            Statement::Iopq { op, src, dest }
        }

        "jmp" | "jle" | "jl" | "je" | "jne" | "jge" | "jg" => {
            let cond = match command {
                "jmp" => Cond::Always,
                "jle" => Cond::Le,
                "jl" => Cond::L,
                "je" => Cond::E,
                "jne" => Cond::Ne,
                "jge" => Cond::Ge,
                "jg" => Cond::G,
                _ => unreachable!(),
            };
            let target = parse_args!(constant);
            Statement::Ij { cond, target }
        }

        "cmovle" | "cmovl" | "cmove" | "cmovne" | "cmovge" | "cmovg" => {
            let cond = match command {
                "cmovle" => Cond::Le,
                "cmovl" => Cond::L,
                "cmove" => Cond::E,
                "cmovne" => Cond::Ne,
                "cmovge" => Cond::Ge,
                "cmovg" => Cond::G,
                _ => unreachable!(),
            };
            let (src, dest) = parse_args!(register, register);
            Statement::Icmov { cond, src, dest }
        }

        "call" => {
            let c = parse_args!(constant);
            Statement::Icall(c)
        }
        "ret" => noargs!(Iret),

        "pushq" => {
            let r = parse_args!(register);
            Statement::Ipushq(r)
        }
        "popq" => {
            let r = parse_args!(register);
            Statement::Ipopq(r)
        }

        _ => return Err(SyntaxError),
    };

    Ok(statement)
}

fn number(src: &str) -> Result<u64, SyntaxError> {
    // NOTE: In X86, `$` is used to prefix an immediate value.
    // `$8` means the literal, while `8` is a memory access at that offset.
    // This is unambiguous in Y86, so `$` is optional.
    let s = src.strip_prefix('$').unwrap_or(src);

    if let Some(t) = s.strip_prefix("0x") {
        u64::from_str_radix(t, 16)
    } else {
        s.parse()
    }
    .map_err(|_| SyntaxError)
}

fn constant(src: &str) -> Result<Constant, SyntaxError> {
    if src.starts_with(|c: char| c == '$' || c.is_ascii_digit()) {
        number(src).map(|n| Constant::Literal(n))
    } else {
        Ok(Constant::Label(validate_label(src)?.to_owned()))
    }
}

fn register(src: &str) -> Result<Register, SyntaxError> {
    use Register::*;

    let reg = match src {
        "%rax" => Rax,
        "%rcx" => Rcx,
        "%rdx" => Rdx,
        "%rbx" => Rbx,
        "%rsp" => Rsp,
        "%rbp" => Rbp,
        "%rsi" => Rsi,
        "%rdi" => Rdi,
        "%r8" => R8,
        "%r9" => R9,
        "%r10" => R10,
        "%r11" => R11,
        "%r12" => R12,
        "%r13" => R13,
        "%r14" => R14,
        _ => return Err(SyntaxError),
    };

    Ok(reg)
}

fn memory(src: &str) -> Result<Memory, SyntaxError> {
    let re = Regex::new(r"^(.*)\((.*)\)$").unwrap();
    let mem = if let Some(caps) = re.captures(src) {
        let offset = caps[1].trim();
        let reg = caps[2].trim();

        let offset = if offset.is_empty() {
            Constant::Literal(0)
        } else {
            constant(offset)?
        };
        let reg = Some(register(reg)?);

        Memory { reg, offset }
    } else {
        Memory {
            reg: None,
            offset: constant(src)?,
        }
    };

    Ok(mem)
}

#[cfg(test)]
mod tests {
    use super::*;
}
