use crate::{lex::*, syntax::*};

struct Parser<'a> {
    tokens: Lexer<'a>,
}

impl<'a> Parser<'a> {
    fn new(src: &'a str) -> Self {
        Self {
            tokens: Lexer::new(src),
        }
    }

    /// Require a token, err if no more is available.
    fn require_token(&mut self) -> Result<Token, SyntaxError> {
        match self.tokens.next() {
            Some(r) => r,
            None => Err(SyntaxError),
        }
    }

    /// Assert the next token is as expected.
    fn assert_token(&mut self, expected: Token) -> Result<(), SyntaxError> {
        if self.require_token()? == expected {
            Ok(())
        } else {
            Err(SyntaxError)
        }
    }

    /// Assert the next token is a register.
    fn assert_register(&mut self) -> Result<Register, SyntaxError> {
        self.require_token()?.try_into()
    }

    /// Assert the next token is an immediate number.
    fn assert_number(&mut self) -> Result<u64, SyntaxError> {
        match self.require_token()? {
            Token::Number(n) => Ok(n),
            _ => Err(SyntaxError),
        }
    }

    /// Assert the next token is a constant (immediate value or label).
    fn assert_constant(&mut self) -> Result<Constant, SyntaxError> {
        self.require_token()?.try_into()
    }

    /// Assert the following tokens represent a memory reference,
    /// i.e. `(<reg>)` or `<offset>(<reg>)`.
    fn assert_memory(&mut self) -> Result<(Constant, Register), SyntaxError> {
        let offset = match self.require_token()? {
            Token::Lparen => Constant::Literal(0),
            t => {
                self.assert_token(Token::Lparen)?;
                t.try_into()?
            }
        };
        let reg = self.assert_register()?;
        self.assert_token(Token::Rparen)?;

        Ok((offset, reg))
    }
}

impl Iterator for Parser<'_> {
    type Item = Result<Statement, SyntaxError>;

    fn next(&mut self) -> Option<Self::Item> {
        let r = self.tokens.next()?;

        let result = (|| {
            let stmt = match r? {
                Token::Ihalt => Statement::Ihalt,
                Token::Inop => Statement::Inop,

                Token::Irrmovq => {
                    let src = self.assert_register()?;
                    self.assert_token(Token::Comma)?;
                    let dest = self.assert_register()?;
                    Statement::Irrmovq { src, dest }
                }

                Token::Iirmovq => {
                    let value = match self.require_token()? {
                        Token::Dollar => self.assert_constant()?,
                        t => t.try_into()?,
                    };
                    self.assert_token(Token::Comma)?;
                    let dest = self.assert_register()?;
                    Statement::Iirmovq { dest, value }
                }

                Token::Irmmovq => {
                    let src = self.assert_register()?;
                    self.assert_token(Token::Comma)?;
                    let (offset, dest) = self.assert_memory()?;
                    Statement::Irmmovq { src, dest, offset }
                }

                Token::Imrmovq => {
                    let (offset, src) = self.assert_memory()?;
                    self.assert_token(Token::Comma)?;
                    let dest = self.assert_register()?;
                    Statement::Imrmovq { src, dest, offset }
                }

                Token::Iopq(op) => {
                    let src = self.assert_register()?;
                    self.assert_token(Token::Comma)?;
                    let dest = self.assert_register()?;
                    Statement::Iopq { op, src, dest }
                }

                Token::Ij(cond) => {
                    let target = self.assert_constant()?;
                    Statement::Ij { cond, target }
                }

                Token::Icmov(cond) => {
                    let src = self.assert_register()?;
                    self.assert_token(Token::Comma)?;
                    let dest = self.assert_register()?;
                    Statement::Icmov { cond, src, dest }
                }

                Token::Icall => {
                    let target = self.assert_constant()?;
                    Statement::Icall(target)
                }
                Token::Iret => Statement::Iret,

                Token::Ipushq => {
                    let reg = self.assert_register()?;
                    Statement::Ipushq(reg)
                }
                Token::Ipopq => {
                    let reg = self.assert_register()?;
                    Statement::Ipopq(reg)
                }

                Token::Label(s) => {
                    self.assert_token(Token::Colon)?;
                    Statement::LabelDef(s)
                }

                Token::Dbyte => {
                    let n = self.assert_number()?;
                    Statement::Dbyte(n as u8)
                }
                Token::Dword => {
                    let n = self.assert_number()?;
                    Statement::Dword(n as u16)
                }
                Token::Dlong => {
                    let n = self.assert_number()?;
                    Statement::Dlong(n as u32)
                }
                Token::Dquad => {
                    let n = self.assert_number()?;
                    Statement::Dquad(n)
                }
                Token::Dpos => {
                    let n = self.assert_number()?;
                    Statement::Dpos(n)
                }
                Token::Dalign => {
                    let n = self.assert_number()?;
                    Statement::Dalign(n)
                }

                _ => return Err(SyntaxError),
            };

            Ok(stmt)
        })();

        Some(result)
    }
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

        let expected = vec![
            Dpos(0),
            Iirmovq {
                dest: Rsp,
                value: Label("stack".to_string()),
            },
            Icall(Label("main".to_string())),
            Ihalt,
            Dalign(8),
            LabelDef("array".to_string()),
            Dquad(0x000d000d000d),
            Dquad(0x00c000c000c0),
            Dquad(0x0b000b000b00),
            Dquad(0xa000a000a000),
            LabelDef("main".to_string()),
            Iirmovq {
                dest: Rdi,
                value: Label("array".to_string()),
            },
            Iirmovq {
                dest: Rsi,
                value: Literal(4),
            },
            Icall(Label("sum".to_string())),
            Iret,
            LabelDef("sum".to_string()),
            Iirmovq {
                dest: R8,
                value: Literal(8),
            },
            Iirmovq {
                dest: R9,
                value: Literal(1),
            },
            Iopq {
                op: Xor,
                src: Rax,
                dest: Rax,
            },
            Iopq {
                op: And,
                src: Rsi,
                dest: Rsi,
            },
            Ij {
                cond: Always,
                target: Label("test".to_string()),
            },
            LabelDef("loop".to_string()),
            Imrmovq {
                src: Rdi,
                dest: R10,
                offset: Literal(0),
            },
            Iopq {
                op: Add,
                src: R10,
                dest: Rax,
            },
            Iopq {
                op: Add,
                src: R8,
                dest: Rdi,
            },
            Iopq {
                op: Sub,
                src: R9,
                dest: Rsi,
            },
            LabelDef("test".to_string()),
            Ij {
                cond: Ne,
                target: Label("loop".to_string()),
            },
            Iret,
            Dpos(0x200),
            LabelDef("stack".to_string()),
        ];

        let parser = Parser::new(src);
        for (stmt, exp) in parser.zip(expected) {
            assert_eq!(stmt, Ok(exp));
        }
    }
}
