use crate::lex::Token;

#[derive(Debug, PartialEq, Eq)]
enum Register {
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

impl From<Token> for Register {
    fn from(value: Token) -> Self {
        match value {
            Token::Rax => Self::Rax,
            Token::Rcx => Self::Rcx,
            Token::Rdx => Self::Rdx,
            Token::Rbx => Self::Rbx,
            Token::Rsp => Self::Rsp,
            Token::Rbp => Self::Rbp,
            Token::Rsi => Self::Rsi,
            Token::Rdi => Self::Rdi,
            Token::R8 => Self::R8,
            Token::R9 => Self::R9,
            Token::R10 => Self::R10,
            Token::R11 => Self::R11,
            Token::R12 => Self::R12,
            Token::R13 => Self::R13,
            Token::R14 => Self::R14,
            _ => todo!(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Constant {
    Literal(u64),
    Label(String),
}

impl From<Token> for Constant {
    fn from(value: Token) -> Self {
        match value {
            Token::Number(n) => Self::Literal(n),
            Token::Label(s) => Self::Label(s),
            _ => todo!(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Statement {
    // Label definitions
    LabelDef(String),

    // Directives
    Dbyte(u8),
    Dword(u16),
    Dlong(u32),
    Dquad(u64),
    Dpos(u64),
    Dalign(u64),

    // Instructions
    Ihalt,
    Inop,
    Irrmovq {
        ra: Register,
        rb: Register,
    },
    Iirmovq {
        v: Constant,
        rb: Register,
    },
    Irmmovq {
        ra: Register,
        d: Constant,
        rb: Register,
    },
    Imrmovq {
        d: Constant,
        rb: Register,
        ra: Register,
    },
    Iaddq(Register, Register),
    Isubq(Register, Register),
    Iandq(Register, Register),
    Ixorq(Register, Register),
    Ijmp(Constant),
    Ijle(Constant),
    Ijl(Constant),
    Ije(Constant),
    Ijne(Constant),
    Ijge(Constant),
    Ijg(Constant),
    Icmovle(Register, Register),
    Icmovl(Register, Register),
    Icmove(Register, Register),
    Icmovne(Register, Register),
    Icmovge(Register, Register),
    Icmovg(Register, Register),
    Icall(Constant),
    Iret,
    Ipushq(Register),
    Ipopq(Register),
}

fn parse(tokens: Vec<Token>) -> Vec<Statement> {
    let mut statements = Vec::new();

    let mut iter = tokens.into_iter();

    while let Some(token) = iter.next() {
        let statement = match token {
            Token::Ihalt => Statement::Ihalt,
            Token::Inop => Statement::Inop,

            Token::Irrmovq => {
                let ra: Register = iter.next().unwrap().into();
                assert_eq!(iter.next(), Some(Token::Comma));
                let rb: Register = iter.next().unwrap().into();
                Statement::Irrmovq { ra, rb }
            }
            Token::Iirmovq => {
                let v: Constant = iter.next().unwrap().into();
                assert_eq!(iter.next(), Some(Token::Comma));
                let rb: Register = iter.next().unwrap().into();
                Statement::Iirmovq { v, rb }
            }
            Token::Irmmovq => {
                let ra: Register = iter.next().unwrap().into();
                assert_eq!(iter.next(), Some(Token::Comma));
                let d: Constant = match iter.next().unwrap() {
                    Token::Lparen => Constant::Literal(0),
                    t => {
                        assert_eq!(iter.next(), Some(Token::Lparen));
                        t.into()
                    }
                };
                let rb: Register = iter.next().unwrap().into();
                assert_eq!(iter.next(), Some(Token::Rparen));
                Statement::Irmmovq { ra, d, rb }
            }
            Token::Imrmovq => {
                let d: Constant = match iter.next().unwrap() {
                    Token::Lparen => Constant::Literal(0),
                    t => {
                        assert_eq!(iter.next(), Some(Token::Lparen));
                        t.into()
                    }
                };
                let rb: Register = iter.next().unwrap().into();
                assert_eq!(iter.next(), Some(Token::Rparen));
                assert_eq!(iter.next(), Some(Token::Comma));
                let ra: Register = iter.next().unwrap().into();
                Statement::Imrmovq { ra, d, rb }
            }

            Token::Iaddq => {
                let ra: Register = iter.next().unwrap().into();
                assert_eq!(iter.next(), Some(Token::Comma));
                let rb: Register = iter.next().unwrap().into();
                Statement::Iaddq(ra, rb)
            }
            Token::Isubq => {
                let ra: Register = iter.next().unwrap().into();
                assert_eq!(iter.next(), Some(Token::Comma));
                let rb: Register = iter.next().unwrap().into();
                Statement::Isubq(ra, rb)
            }
            Token::Iandq => {
                let ra: Register = iter.next().unwrap().into();
                assert_eq!(iter.next(), Some(Token::Comma));
                let rb: Register = iter.next().unwrap().into();
                Statement::Iandq(ra, rb)
            }
            Token::Ixorq => {
                let ra: Register = iter.next().unwrap().into();
                assert_eq!(iter.next(), Some(Token::Comma));
                let rb: Register = iter.next().unwrap().into();
                Statement::Ixorq(ra, rb)
            }

            Token::Ijmp => {
                let dest: Constant = iter.next().unwrap().into();
                Statement::Ijmp(dest)
            }
            Token::Ijle => {
                let dest: Constant = iter.next().unwrap().into();
                Statement::Ijle(dest)
            }
            Token::Ijl => {
                let dest: Constant = iter.next().unwrap().into();
                Statement::Ijl(dest)
            }
            Token::Ije => {
                let dest: Constant = iter.next().unwrap().into();
                Statement::Ije(dest)
            }
            Token::Ijne => {
                let dest: Constant = iter.next().unwrap().into();
                Statement::Ijne(dest)
            }
            Token::Ijge => {
                let dest: Constant = iter.next().unwrap().into();
                Statement::Ijge(dest)
            }
            Token::Ijg => {
                let dest: Constant = iter.next().unwrap().into();
                Statement::Ijg(dest)
            }

            Token::Icmovle => {
                let ra: Register = iter.next().unwrap().into();
                assert_eq!(iter.next(), Some(Token::Comma));
                let rb: Register = iter.next().unwrap().into();
                Statement::Icmovle(ra, rb)
            }
            Token::Icmovl => {
                let ra: Register = iter.next().unwrap().into();
                assert_eq!(iter.next(), Some(Token::Comma));
                let rb: Register = iter.next().unwrap().into();
                Statement::Icmovl(ra, rb)
            }
            Token::Icmove => {
                let ra: Register = iter.next().unwrap().into();
                assert_eq!(iter.next(), Some(Token::Comma));
                let rb: Register = iter.next().unwrap().into();
                Statement::Icmove(ra, rb)
            }
            Token::Icmovne => {
                let ra: Register = iter.next().unwrap().into();
                assert_eq!(iter.next(), Some(Token::Comma));
                let rb: Register = iter.next().unwrap().into();
                Statement::Icmovne(ra, rb)
            }
            Token::Icmovge => {
                let ra: Register = iter.next().unwrap().into();
                assert_eq!(iter.next(), Some(Token::Comma));
                let rb: Register = iter.next().unwrap().into();
                Statement::Icmovge(ra, rb)
            }
            Token::Icmovg => {
                let ra: Register = iter.next().unwrap().into();
                assert_eq!(iter.next(), Some(Token::Comma));
                let rb: Register = iter.next().unwrap().into();
                Statement::Icmovg(ra, rb)
            }

            Token::Icall => {
                let dest: Constant = iter.next().unwrap().into();
                Statement::Icall(dest)
            }
            Token::Iret => Statement::Iret,

            Token::Ipushq => {
                let ra: Register = iter.next().unwrap().into();
                Statement::Ipushq(ra)
            }
            Token::Ipopq => {
                let ra: Register = iter.next().unwrap().into();
                Statement::Ipopq(ra)
            }

            Token::Label(s) => {
                assert_eq!(iter.next(), Some(Token::Colon));
                Statement::LabelDef(s)
            }

            Token::Dbyte => {
                let Some(Token::Number(n)) = iter.next() else {
                    todo!()
                };
                Statement::Dbyte(n as u8)
            }
            Token::Dword => {
                let Some(Token::Number(n)) = iter.next() else {
                    todo!()
                };
                Statement::Dword(n as u16)
            }
            Token::Dlong => {
                let Some(Token::Number(n)) = iter.next() else {
                    todo!()
                };
                Statement::Dlong(n as u32)
            }
            Token::Dquad => {
                let Some(Token::Number(n)) = iter.next() else {
                    todo!()
                };
                Statement::Dquad(n as u64)
            }
            Token::Dpos => {
                let Some(Token::Number(n)) = iter.next() else {
                    todo!()
                };
                Statement::Dpos(n)
            }
            Token::Dalign => {
                let Some(Token::Number(n)) = iter.next() else {
                    todo!()
                };
                Statement::Dalign(n)
            }

            _ => todo!(),
        };

        statements.push(statement);
    }

    statements
}

#[cfg(test)]
mod tests {
    use super::*;
    use Token::*;

    #[test]
    fn sum() {
        #[rustfmt::skip]
        let tokens = vec![
            Dpos, Number(0),
            Iirmovq, Label("stack".to_string()), Comma, Rsp,
            Icall, Label("main".to_string()),
            Ihalt,
            Dalign, Number(8),
            Label("array".to_string()), Colon,
            Dquad, Number(0x000d000d000d),
            Dquad, Number(0x00c000c000c0),
            Dquad, Number(0x0b000b000b00),
            Dquad, Number(0xa000a000a000),
            Label("main".to_string()), Colon,
            Iirmovq, Label("array".to_string()), Comma, Rdi,
            Iirmovq, Number(4), Comma, Rsi,
            Icall, Label("sum".to_string()),
            Iret,
            Label("sum".to_string()), Colon,
            Iirmovq, Number(8), Comma, R8,
            Iirmovq, Number(1), Comma, R9,
            Ixorq, Rax, Comma, Rax,
            Iandq, Rsi, Comma, Rsi,
            Ijmp, Label("test".to_string()),
            Label("loop".to_string()), Colon,
            Imrmovq, Lparen, Rdi, Rparen, Comma, R10,
            Iaddq, R10, Comma, Rax,
            Iaddq, R8, Comma, Rdi,
            Isubq, R9, Comma, Rsi,
            Label("test".to_string()), Colon,
            Ijne, Label("loop".to_string()),
            Iret,
            Dpos, Number(0x200),
            Label("stack".to_string()), Colon,
        ];

        let expected = vec![
            Statement::Dpos(0),
            Statement::Iirmovq {
                v: Constant::Label("stack".to_string()),
                rb: Register::Rsp,
            },
            Statement::Icall(Constant::Label("main".to_string())),
            Statement::Ihalt,
            Statement::Dalign(8),
            Statement::LabelDef("array".to_string()),
            Statement::Dquad(0x000d000d000d),
            Statement::Dquad(0x00c000c000c0),
            Statement::Dquad(0x0b000b000b00),
            Statement::Dquad(0xa000a000a000),
            Statement::LabelDef("main".to_string()),
            Statement::Iirmovq {
                v: Constant::Label("array".to_string()),
                rb: Register::Rdi,
            },
            Statement::Iirmovq {
                v: Constant::Literal(4),
                rb: Register::Rsi,
            },
            Statement::Icall(Constant::Label("sum".to_string())),
            Statement::Iret,
            Statement::LabelDef("sum".to_string()),
            Statement::Iirmovq {
                v: Constant::Literal(8),
                rb: Register::R8,
            },
            Statement::Iirmovq {
                v: Constant::Literal(1),
                rb: Register::R9,
            },
            Statement::Ixorq(Register::Rax, Register::Rax),
            Statement::Iandq(Register::Rsi, Register::Rsi),
            Statement::Ijmp(Constant::Label("test".to_string())),
            Statement::LabelDef("loop".to_string()),
            Statement::Imrmovq {
                d: Constant::Literal(0),
                rb: Register::Rdi,
                ra: Register::R10,
            },
            Statement::Iaddq(Register::R10, Register::Rax),
            Statement::Iaddq(Register::R8, Register::Rdi),
            Statement::Isubq(Register::R9, Register::Rsi),
            Statement::LabelDef("test".to_string()),
            Statement::Ijne(Constant::Label("loop".to_string())),
            Statement::Iret,
            Statement::Dpos(0x200),
            Statement::LabelDef("stack".to_string()),
        ];

        assert_eq!(parse(tokens), expected);
    }
}
