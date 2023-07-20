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
        match self.require_token()? {
            Token::Reg(r) => Ok(r),
            _ => Err(SyntaxError),
        }
    }

    /// Assert the next token is a number.
    /// Note that a number is different from a constant literal,
    /// which should always be preceded by `$`.
    fn assert_number(&mut self) -> Result<u64, SyntaxError> {
        match self.require_token()? {
            Token::Number(n) => Ok(n),
            _ => Err(SyntaxError),
        }
    }

    /// Assert the following tokens represent a constant,
    /// i.e. `$<number>` or `<label>`.
    fn assert_constant(&mut self) -> Result<Constant, SyntaxError> {
        match self.require_token()? {
            Token::Dollar => Ok(Constant::Literal(self.assert_number()?)),
            Token::Label(s) => Ok(Constant::Label(s)),
            _ => Err(SyntaxError),
        }
    }

    /// Assert the following tokens represent a memory reference,
    /// i.e. `(<reg>)` or `<offset>(<reg>)`.
    fn assert_memory(&mut self) -> Result<(Constant, Register), SyntaxError> {
        let offset = match self.require_token()? {
            Token::Lparen => Constant::Literal(0),
            Token::Number(n) => {
                self.assert_token(Token::Lparen)?;
                Constant::Literal(n)
            }
            // TODO: Should we support label as an offset?
            _ => return Err(SyntaxError),
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
            macro_rules! directive {
                ($ident:ident $(, $type:ty)?) => {{
                    let n = self.assert_number()?;
                    Statement::$ident(n $(as $type)?)
                }};
            }

            macro_rules! binop {
                ($ident:ident $(, $extra:ident)?) => {{
                    let src = self.assert_register()?;
                    self.assert_token(Token::Comma)?;
                    let dest = self.assert_register()?;
                    Statement::$ident { $($extra,)? src, dest }
                }};
            }

            let stmt = match r? {
                Token::Ihalt => Statement::Ihalt,
                Token::Inop => Statement::Inop,

                Token::Irrmovq => binop!(Irrmovq),

                Token::Iirmovq => {
                    let value = self.assert_constant()?;
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

                Token::Iopq(op) => binop!(Iopq, op),

                Token::Ij(cond) => {
                    let target = self.assert_constant()?;
                    Statement::Ij { cond, target }
                }

                Token::Icmov(cond) => binop!(Icmov, cond),

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

                Token::Dbyte => directive!(Dbyte, u8),
                Token::Dword => directive!(Dword, u16),
                Token::Dlong => directive!(Dlong, u32),
                Token::Dquad => directive!(Dquad),
                Token::Dpos => directive!(Dpos),
                Token::Dalign => directive!(Dalign),

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

        macro_rules! irmovq {
            ($r:ident, $s:expr) => {
                Iirmovq {
                    dest: $r,
                    value: Label($s.to_string()),
                }
            };
            ($r:ident, $n:expr, u64) => {
                Iirmovq {
                    dest: $r,
                    value: Literal($n),
                }
            };
        }

        let def = |s: &str| LabelDef(s.to_string());
        let op = |op, src, dest| Iopq { op, src, dest };
        let j = |cond, s: &str| Ij {
            cond,
            target: Label(s.to_string()),
        };

        let expected = vec![
            Dpos(0),
            irmovq!(Rsp, "stack"),
            Icall(Label("main".to_string())),
            Ihalt,
            //
            Dalign(8),
            def("array"),
            Dquad(0x000d000d000d),
            Dquad(0x00c000c000c0),
            Dquad(0x0b000b000b00),
            Dquad(0xa000a000a000),
            //
            def("main"),
            irmovq!(Rdi, "array"),
            irmovq!(Rsi, 4, u64),
            Icall(Label("sum".to_string())),
            Iret,
            //
            def("sum"),
            irmovq!(R8, 8, u64),
            irmovq!(R9, 1, u64),
            op(Xor, Rax, Rax),
            op(And, Rsi, Rsi),
            j(Always, "test"),
            def("loop"),
            Imrmovq {
                src: Rdi,
                dest: R10,
                offset: Literal(0),
            },
            op(Add, R10, Rax),
            op(Add, R8, Rdi),
            op(Sub, R9, Rsi),
            def("test"),
            j(Ne, "loop"),
            Iret,
            //
            Dpos(0x200),
            def("stack"),
        ];

        let parser = Parser::new(src);
        for (stmt, exp) in parser.zip(expected) {
            assert_eq!(stmt, Ok(exp));
        }
    }
}
