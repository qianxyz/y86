#[derive(Debug, PartialEq, Eq)]
#[rustfmt::skip]
enum Token {
    /// Instruction mnemonics
    Ihalt, Inop,
    Irrmovq, Iirmovq, Irmmovq, Imrmovq,
    Iaddq, Isubq, Iandq, Ixorq,
    Ijmp, Ijle, Ijl, Ije, Ijne, Ijge, Ijg,
    Icmovle, Icmovl, Icmove, Icmovne, Icmovge, Icmovg,
    Icall, Iret,
    Ipushq, Ipopq,

    /// Registers
    Rax, Rcx, Rdx, Rbx, Rsp, Rbp, Rsi, Rdi,
    R8, R9, R10, R11, R12, R13, R14,

    Number(u64),

    Label(String),

    /// Directives
    Dbyte, Dword, Dlong, Dquad, Dpos, Dalign,

    /// Punctuations
    Colon, Lparen, Rparen, Comma,
}

fn lex(src: String) -> Vec<Token> {
    let mut tokens = Vec::new();

    let mut chars = src.chars().peekable();

    while let Some(c) = chars.peek() {
        match c {
            // Comment
            '#' => {
                while let Some(c) = chars.next() {
                    if c == '\n' {
                        break;
                    }
                }
            }

            // Punctuations
            ':' => {
                tokens.push(Token::Colon);
                chars.next();
            }
            '(' => {
                tokens.push(Token::Lparen);
                chars.next();
            }
            ')' => {
                tokens.push(Token::Rparen);
                chars.next();
            }
            ',' => {
                tokens.push(Token::Comma);
                chars.next();
            }
            // TODO: Enforce `$` before literals
            '$' => {
                chars.next();
            }

            // Directives
            '.' => {
                let s: String = chars
                    .by_ref()
                    .take_while(|c| !c.is_ascii_whitespace())
                    .collect();
                tokens.push(match s.as_str() {
                    ".byte" => Token::Dbyte,
                    ".word" => Token::Dword,
                    ".long" => Token::Dlong,
                    ".quad" => Token::Dquad,
                    ".pos" => Token::Dpos,
                    ".align" => Token::Dalign,
                    _ => todo!(),
                })
            }

            // Registers
            '%' => {
                let mut s = chars.next().unwrap().to_string();
                while let Some(c) = chars.peek() {
                    if !c.is_ascii_alphanumeric() {
                        break;
                    }
                    s.push(chars.next().unwrap())
                }
                tokens.push(match s.as_str() {
                    "%rax" => Token::Rax,
                    "%rcx" => Token::Rcx,
                    "%rdx" => Token::Rdx,
                    "%rbx" => Token::Rbx,
                    "%rsi" => Token::Rsi,
                    "%rdi" => Token::Rdi,
                    "%rsp" => Token::Rsp,
                    "%rbp" => Token::Rbp,
                    "%r8" => Token::R8,
                    "%r9" => Token::R9,
                    "%r10" => Token::R10,
                    "%r11" => Token::R11,
                    "%r12" => Token::R12,
                    "%r13" => Token::R13,
                    "%r14" => Token::R14,
                    _ => todo!(),
                })
            }

            // Skip whitespaces
            c if c.is_ascii_whitespace() => {
                chars.next();
            }

            // Numbers
            c if c.is_ascii_digit() => {
                let mut s = chars.next().unwrap().to_string();
                while let Some(c) = chars.peek() {
                    if !(c.is_ascii_hexdigit() || *c == 'x') {
                        break;
                    }
                    s.push(chars.next().unwrap())
                }
                let n: u64 = if s.starts_with("0x") {
                    u64::from_str_radix(&s[2..], 16).unwrap()
                } else {
                    s.parse().unwrap()
                };
                tokens.push(Token::Number(n));
            }

            // Instructions or labels
            c if c.is_ascii_alphabetic() => {
                let mut s = chars.next().unwrap().to_string();
                while let Some(c) = chars.peek() {
                    if !(c.is_ascii_alphanumeric() || *c == '_') {
                        break;
                    }
                    s.push(chars.next().unwrap())
                }
                tokens.push(match s.as_str() {
                    "halt" => Token::Ihalt,
                    "nop" => Token::Inop,
                    "rrmovq" => Token::Irrmovq,
                    "irmovq" => Token::Iirmovq,
                    "rmmovq" => Token::Irmmovq,
                    "mrmovq" => Token::Imrmovq,
                    "addq" => Token::Iaddq,
                    "subq" => Token::Isubq,
                    "andq" => Token::Iandq,
                    "xorq" => Token::Ixorq,
                    "jmp" => Token::Ijmp,
                    "jle" => Token::Ijle,
                    "jl" => Token::Ijl,
                    "je" => Token::Ije,
                    "jne" => Token::Ijne,
                    "jge" => Token::Ijge,
                    "jg" => Token::Ijg,
                    "cmovle" => Token::Icmovle,
                    "cmovl" => Token::Icmovl,
                    "cmove" => Token::Icmove,
                    "cmovne" => Token::Icmovne,
                    "cmovge" => Token::Icmovge,
                    "cmovg" => Token::Icmovg,
                    "call" => Token::Icall,
                    "ret" => Token::Iret,
                    "pushq" => Token::Ipushq,
                    "popq" => Token::Ipopq,
                    _ => Token::Label(s),
                })
            }

            _ => todo!(),
        }
    }

    tokens
}

#[cfg(test)]
mod tests {
    use super::*;
    use Token::*;

    #[test]
    fn sum() {
        let src = "\
# Execution begins at address 0 
        .pos 0
        irmovq stack, %rsp  	# Set up stack pointer
        call main		        # Execute main program
        halt			        # Terminate program 

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
        call sum		        # sum(array, 4)
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

        let tokens = lex(src.to_string());
        assert_eq!(
            tokens,
            vec![
                Dpos,
                Number(0),
                Iirmovq,
                Label("stack".to_string()),
                Comma,
                Rsp,
                Icall,
                Label("main".to_string()),
                Ihalt,
                Dalign,
                Number(8),
                Label("array".to_string()),
                Colon,
                Dquad,
                Number(0x000d000d000d),
                Dquad,
                Number(0x00c000c000c0),
                Dquad,
                Number(0x0b000b000b00),
                Dquad,
                Number(0xa000a000a000),
                Label("main".to_string()),
                Colon,
                Iirmovq,
                Label("array".to_string()),
                Comma,
                Rdi,
                Iirmovq,
                Number(4),
                Comma,
                Rsi,
                Icall,
                Label("sum".to_string()),
                Iret,
                Label("sum".to_string()),
                Colon,
                Iirmovq,
                Number(8),
                Comma,
                R8,
                Iirmovq,
                Number(1),
                Comma,
                R9,
                Ixorq,
                Rax,
                Comma,
                Rax,
                Iandq,
                Rsi,
                Comma,
                Rsi,
                Ijmp,
                Label("test".to_string()),
                Label("loop".to_string()),
                Colon,
                Imrmovq,
                Lparen,
                Rdi,
                Rparen,
                Comma,
                R10,
                Iaddq,
                R10,
                Comma,
                Rax,
                Iaddq,
                R8,
                Comma,
                Rdi,
                Isubq,
                R9,
                Comma,
                Rsi,
                Label("test".to_string()),
                Colon,
                Ijne,
                Label("loop".to_string()),
                Iret,
                Dpos,
                Number(0x200),
                Label("stack".to_string()),
                Colon
            ]
        )
    }
}
