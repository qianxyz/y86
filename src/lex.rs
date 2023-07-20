use std::{iter::Peekable, str::Chars};

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Register {
    Rax,
    Rcx,
    Rdx,
    Rbx,
    Rsp,
    Rbp,
    Rsi,
    Rdi,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Op {
    Add,
    Sub,
    And,
    Xor,
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Cond {
    Always,
    Le,
    L,
    E,
    Ne,
    Ge,
    G,
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Token {
    // Instruction mnemonics
    Ihalt,
    Inop,
    Irrmovq,
    Iirmovq,
    Irmmovq,
    Imrmovq,
    Iopq(Op),
    Ij(Cond),
    Icmov(Cond),
    Icall,
    Iret,
    Ipushq,
    Ipopq,

    Reg(Register),

    Number(u64),

    Label(String),

    // Directives
    Dbyte,
    Dword,
    Dlong,
    Dquad,
    Dpos,
    Dalign,

    // Punctuations
    Colon,
    Lparen,
    Rparen,
    Comma,
    Dollar,
}

struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
}

#[derive(Debug)]
struct SyntaxError; // TODO: Context

impl<'a> Lexer<'a> {
    fn new(src: &'a str) -> Self {
        Self {
            chars: src.chars().peekable(),
        }
    }

    fn skip_comment(&mut self) {
        for c in self.chars.by_ref() {
            if c == '\n' {
                break;
            }
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.chars.peek() {
            if c.is_ascii_whitespace() {
                self.chars.next();
            } else {
                break;
            }
        }
    }

    fn collect_while<P>(&mut self, mut pred: P) -> String
    where
        P: FnMut(&char) -> bool,
    {
        let mut s = String::new();

        while let Some(c) = self.chars.peek() {
            if pred(c) {
                s.push(*c);
                self.chars.next();
            } else {
                break;
            }
        }

        s
    }

    fn read_number(&mut self) -> Result<Token, SyntaxError> {
        let s = self.collect_while(|c| c.is_ascii_hexdigit() || *c == 'x');

        let result = if let Some(t) = s.strip_prefix("0x") {
            u64::from_str_radix(t, 16)
        } else {
            s.parse::<u64>()
        };

        match result {
            Ok(n) => Ok(Token::Number(n)),
            _ => Err(SyntaxError),
        }
    }

    fn read_directive(&mut self) -> Result<Token, SyntaxError> {
        self.chars.next(); // Consume `.`

        let s = self.collect_while(|c| !c.is_ascii_whitespace());
        let tok = match s.as_str() {
            "byte" => Token::Dbyte,
            "word" => Token::Dword,
            "long" => Token::Dlong,
            "quad" => Token::Dquad,
            "pos" => Token::Dpos,
            "align" => Token::Dalign,
            _ => return Err(SyntaxError),
        };

        Ok(tok)
    }

    fn read_register(&mut self) -> Result<Token, SyntaxError> {
        self.chars.next(); // Consume `%`

        let s = self.collect_while(|c| c.is_ascii_alphanumeric());
        let reg = match s.as_str() {
            "rax" => Register::Rax,
            "rcx" => Register::Rcx,
            "rdx" => Register::Rdx,
            "rbx" => Register::Rbx,
            "rsi" => Register::Rsi,
            "rdi" => Register::Rdi,
            "rsp" => Register::Rsp,
            "rbp" => Register::Rbp,
            "r8" => Register::R8,
            "r9" => Register::R9,
            "r10" => Register::R10,
            "r11" => Register::R11,
            "r12" => Register::R12,
            "r13" => Register::R13,
            "r14" => Register::R14,
            _ => return Err(SyntaxError),
        };

        Ok(Token::Reg(reg))
    }

    fn read_symbol(&mut self) -> Result<Token, SyntaxError> {
        let s = self.collect_while(|c| c.is_ascii_alphanumeric() || *c == '_');

        let tok = match s.as_str() {
            "halt" => Token::Ihalt,
            "nop" => Token::Inop,
            "rrmovq" => Token::Irrmovq,
            "irmovq" => Token::Iirmovq,
            "rmmovq" => Token::Irmmovq,
            "mrmovq" => Token::Imrmovq,
            "addq" => Token::Iopq(Op::Add),
            "subq" => Token::Iopq(Op::Sub),
            "andq" => Token::Iopq(Op::And),
            "xorq" => Token::Iopq(Op::Xor),
            "jmp" => Token::Ij(Cond::Always),
            "jle" => Token::Ij(Cond::Le),
            "jl" => Token::Ij(Cond::L),
            "je" => Token::Ij(Cond::E),
            "jne" => Token::Ij(Cond::Ne),
            "jge" => Token::Ij(Cond::Ge),
            "jg" => Token::Ij(Cond::G),
            "cmovle" => Token::Icmov(Cond::Le),
            "cmovl" => Token::Icmov(Cond::L),
            "cmove" => Token::Icmov(Cond::E),
            "cmovne" => Token::Icmov(Cond::Ne),
            "cmovge" => Token::Icmov(Cond::Ge),
            "cmovg" => Token::Icmov(Cond::G),
            "call" => Token::Icall,
            "ret" => Token::Iret,
            "pushq" => Token::Ipushq,
            "popq" => Token::Ipopq,

            _ => Token::Label(s),
        };

        Ok(tok)
    }
}

impl Iterator for Lexer<'_> {
    type Item = Result<Token, SyntaxError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();

        let Some(&c) = self.chars.peek() else { return None; };

        let result = match c {
            '#' => {
                self.skip_comment();
                return self.next();
            }

            ':' | '(' | ')' | ',' | '$' => {
                self.chars.next();
                let tok = match c {
                    ':' => Token::Colon,
                    '(' => Token::Lparen,
                    ')' => Token::Rparen,
                    ',' => Token::Comma,
                    '$' => Token::Dollar,
                    _ => unreachable!(),
                };

                Ok(tok)
            }

            '.' => self.read_directive(),
            '%' => self.read_register(),
            c if c.is_ascii_digit() => self.read_number(),
            c if c.is_ascii_alphabetic() => self.read_symbol(),

            _ => Err(SyntaxError),
        };

        Some(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use Cond::*;
    use Op::*;
    use Register::*;
    use Token::*;

    #[test]
    fn sum() {
        let src = "\
# Execution begins at address 0
        .pos 0
        irmovq stack, %rsp  	# Set up stack pointer
        call main		# Execute main program
        halt			# Terminate program

# Array of 4 elements
        .align 8
array: 
        .quad 0x000d000d000d
        .quad 0x00c000c000c0
        .quad 0x0b000b000b00
        .quad 0xa000a000a000

main:
        irmovq array,%rdi
        irmovq $4,%rsi
        call sum		# sum(array, 4)
        ret

# long sum(long *start, long count)
# start in %rdi, count in %rsi
sum:
        irmovq $8,%r8        # Constant 8
        irmovq $1,%r9	     # Constant 1
        xorq %rax,%rax	     # sum = 0
        andq %rsi,%rsi	     # Set CC
        jmp     test         # Goto test
loop:
        mrmovq (%rdi),%r10   # Get *start
        addq %r10,%rax       # Add to sum
        addq %r8,%rdi        # start++
        subq %r9,%rsi        # count--.  Set CC
test:
        jne    loop          # Stop when 0
        ret                  # Return

# Stack starts here and grows to lower addresses
        .pos 0x200
stack:
";

        #[rustfmt::skip]
        let expected = vec![
            Dpos, Number(0),
            Iirmovq, Label("stack".to_string()), Comma, Reg(Rsp),
            Icall, Label("main".to_string()),
            Ihalt,
            Dalign, Number(8),
            Label("array".to_string()), Colon,
            Dquad, Number(0x000d000d000d),
            Dquad, Number(0x00c000c000c0),
            Dquad, Number(0x0b000b000b00),
            Dquad, Number(0xa000a000a000),
            Label("main".to_string()), Colon,
            Iirmovq, Label("array".to_string()), Comma, Reg(Rdi),
            Iirmovq, Dollar, Number(4), Comma, Reg(Rsi),
            Icall, Label("sum".to_string()),
            Iret,
            Label("sum".to_string()), Colon,
            Iirmovq, Dollar, Number(8), Comma, Reg(R8),
            Iirmovq, Dollar, Number(1), Comma, Reg(R9),
            Iopq(Xor), Reg(Rax), Comma, Reg(Rax),
            Iopq(And), Reg(Rsi), Comma, Reg(Rsi),
            Ij(Always), Label("test".to_string()),
            Label("loop".to_string()), Colon,
            Imrmovq, Lparen, Reg(Rdi), Rparen, Comma, Reg(R10),
            Iopq(Add), Reg(R10), Comma, Reg(Rax),
            Iopq(Add), Reg(R8), Comma, Reg(Rdi),
            Iopq(Sub), Reg(R9), Comma, Reg(Rsi),
            Label("test".to_string()), Colon,
            Ij(Ne), Label("loop".to_string()),
            Iret,
            Dpos, Number(0x200),
            Label("stack".to_string()), Colon,
        ];

        let lexer = Lexer::new(src);
        let tokens = lexer.collect::<Result<Vec<_>, _>>().unwrap();
        assert_eq!(tokens, expected);
    }
}
