use crate::syntax::*;

use regex::Regex;

/// A line in assembly code, with its components parsed.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(crate) struct ParsedLine<'a> {
    /// The optional label leading the line, e.g. `loop: `.
    pub label: Option<&'a str>,

    /// The optional statement (instruction or directive).
    pub statement: Option<Statement<'a>>,

    /// The source code line.
    pub src: &'a str,
}

fn parse_line(src: &str) -> Result<ParsedLine, SyntaxError> {
    // strip comment after the first `#`
    let s = src.split('#').next().unwrap(); // iterator guaranteed not empty

    // split optional label and the rest of the line
    let (label, rest) = if let Some((l, s)) = s.split_once(':') {
        // strip whitespaces and validate label
        (Some(validate_label(l.trim())?), s)
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

    Ok(ParsedLine {
        label,
        statement,
        src,
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

    macro_rules! op {
        ($op:ident) => {{
            let (src, dest) = parse_args!(register, register);
            Statement::Iopq {
                op: Op::$op,
                src,
                dest,
            }
        }};
    }

    macro_rules! j {
        ($cond:ident) => {{
            let target = parse_args!(constant);
            Statement::Ij {
                cond: Cond::$cond,
                target,
            }
        }};
    }

    macro_rules! cmov {
        ($cond:ident) => {{
            let (src, dest) = parse_args!(register, register);
            Statement::Icmov {
                cond: Cond::$cond,
                src,
                dest,
            }
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

        "addq" => op!(Add),
        "subq" => op!(Sub),
        "andq" => op!(And),
        "xorq" => op!(Xor),

        "jmp" => j!(Always),
        "jle" => j!(Le),
        "jl" => j!(L),
        "je" => j!(E),
        "jne" => j!(Ne),
        "jge" => j!(Ge),
        "jg" => j!(G),

        "cmovle" => cmov!(Le),
        "cmovl" => cmov!(L),
        "cmove" => cmov!(E),
        "cmovne" => cmov!(Ne),
        "cmovge" => cmov!(Ge),
        "cmovg" => cmov!(G),

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
        number(src).map(Constant::Literal)
    } else {
        Ok(Constant::Label(validate_label(src)?))
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
        // The difference between `caps.get(1).unwrap().as_str()` and
        // `&caps[1]` is that the lifetime of the former is associated with
        // the haystack `src` while that of the latter with `caps` due to how
        // the `Index` trait is defined.
        let offset = caps.get(1).unwrap().as_str().trim();
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

    use Cond::*;
    use Constant::*;
    use Op::*;
    use Register::*;
    use Statement::*;

    #[test]
    fn line_components() {
        let labels = [
            ("", None),
            ("main:", Some("main")),
            ("    main:", Some("main")),
            ("    main    :", Some("main")),
        ];
        let statements = [
            ("", None),
            ("nop", Some(Statement::Inop)),
            ("    nop", Some(Statement::Inop)),
            ("    nop    ", Some(Statement::Inop)),
        ];
        let comments = [
            "# comment",
            "    # comment",
            "    # comment:",
            "    ## comment",
        ];

        for (ls, label) in labels {
            for (ss, statement) in statements {
                for comment in comments.iter() {
                    let src = &format!("{ls}{ss}{comment}");
                    assert_eq!(
                        parse_line(&src),
                        Ok(ParsedLine {
                            label,
                            statement,
                            src,
                        })
                    );
                }
            }
        }
    }

    fn test_statement(src: &str, statement: Statement) {
        assert_eq!(
            parse_line(src),
            Ok(ParsedLine {
                label: None,
                statement: Some(statement),
                src,
            })
        );
    }

    #[test]
    fn directives() {
        let cases = [
            (".byte 0x4", Dbyte(0x4)),
            (".word 0x4", Dword(0x4)),
            (".long 0x4", Dlong(0x4)),
            (".quad 0x4", Dquad(0x4)),
            (".pos 0x4", Dpos(0x4)),
            (".align 0x4", Dalign(0x4)),
        ];

        for (src, statement) in cases {
            test_statement(src, statement);
        }
    }

    #[test]
    fn simple_instructions() {
        #[rustfmt::skip]
        let cases = [
            ("halt", Ihalt),
            ("nop", Inop),
            ("rrmovq %rax, %rax", Irrmovq { src: Rax, dest: Rax }),
            ("irmovq $1, %rax", Iirmovq { dest: Rax, value: Literal(1) }),
            ("irmovq array, %rax", Iirmovq { dest: Rax, value: Label("array") }),
            ("call main", Icall(Label("main"))),
            ("call 0x8", Icall(Literal(8))),
            ("ret", Iret),
            ("pushq %rax", Ipushq(Rax)),
            ("popq %rax", Ipopq(Rax)),
        ];

        for (src, statement) in cases {
            test_statement(src, statement);
        }
    }

    #[test]
    fn rmmovq_mrmovq() {
        #[rustfmt::skip]
        let memories = [
            ("(%rax)", Memory { reg: Some(Rax), offset: Literal(0) }),
            ("0x4(%rax)", Memory { reg: Some(Rax), offset: Literal(4) }),
            ("array(%rax)", Memory { reg: Some(Rax), offset: Label("array") }),
            ("0x4", Memory { reg: None, offset: Literal(4) }),
            ("array", Memory { reg: None, offset: Label("array") }),
        ];

        for (ms, mem) in memories {
            test_statement(&format!("rmmovq %rax, {ms}"), Irmmovq { src: Rax, mem });
            test_statement(&format!("mrmovq {ms}, %rax"), Imrmovq { dest: Rax, mem });
        }
    }

    #[test]
    fn opq() {
        let cases = [
            ("addq %rax, %rax", Add),
            ("subq %rax, %rax", Sub),
            ("andq %rax, %rax", And),
            ("xorq %rax, %rax", Xor),
        ];

        for (src, op) in cases {
            test_statement(
                src,
                Iopq {
                    op,
                    src: Rax,
                    dest: Rax,
                },
            )
        }
    }

    #[test]
    fn jxx() {
        let conds = [
            ("jmp", Always),
            ("jle", Le),
            ("jl", L),
            ("je", E),
            ("jne", Ne),
            ("jge", Ge),
            ("jg", G),
        ];
        let dest = [("0x1234abcd", Literal(0x1234abcd)), ("main", Label("main"))];

        for (cs, cond) in conds {
            for (ds, target) in dest {
                test_statement(&format!("{cs} {ds}"), Ij { cond, target });
            }
        }
    }

    #[test]
    fn cmovxx() {
        let cases = [
            ("cmovle %rax, %rax", Le),
            ("cmovl %rax, %rax", L),
            ("cmove %rax, %rax", E),
            ("cmovne %rax, %rax", Ne),
            ("cmovge %rax, %rax", Ge),
            ("cmovg %rax, %rax", G),
        ];

        for (src, cond) in cases {
            test_statement(
                src,
                Icmov {
                    cond,
                    src: Rax,
                    dest: Rax,
                },
            )
        }
    }
}
