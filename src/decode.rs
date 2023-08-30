use crate::{YisErrorContext, MEM_SIZE};

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum DecodeError<'a> {
    NoSep,
    Addr(&'a str),
    Bytes(&'a str),
}

impl std::fmt::Display for DecodeError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NoSep => write!(f, "Missing Separator `|`"),
            Self::Addr(a) => write!(f, "Invalid address: {a}"),
            Self::Bytes(b) => write!(f, "Invalid bytecode: {b}"),
        }
    }
}

pub(crate) fn decode<'a>(
    obj: impl Iterator<Item = &'a str>,
) -> Result<[u8; MEM_SIZE], Vec<YisErrorContext<'a>>> {
    let mut memory = [0; MEM_SIZE];
    let mut errors = Vec::new();

    for (line, lineno) in obj.zip(1..) {
        let Some((line, _)) = line.split_once('|') else {
            errors.push(YisErrorContext {
                error: DecodeError::NoSep,
                lineno,
                src: line,
            });
            continue;
        };

        let Some((addr, bytes)) = line.split_once(':') else {
            continue;
        };

        let addr = addr.trim();
        let Ok(addr) = usize::from_str_radix(addr.trim_start_matches("0x"), 16) else {
            errors.push(YisErrorContext {
                error: DecodeError::Addr(addr),
                lineno,
                src: line,
            });
            continue;
        };

        let bytes = bytes.trim();
        let Ok(bytes) = hex::decode(bytes.trim()) else {
            errors.push(YisErrorContext {
                error: DecodeError::Bytes(bytes),
                lineno,
                src: line,
            });
            continue;
        };

        // TODO: Memory bound check
        memory[addr..addr + bytes.len()].copy_from_slice(&bytes);
    }

    if errors.is_empty() {
        Ok(memory)
    } else {
        Err(errors)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn components() {
        let obj = "\
                            | # comment
0x000:                      | label:
0x000: 00                   |   halt
";
        let expected = [0; MEM_SIZE];

        assert_eq!(decode(obj.lines()), Ok(expected))
    }

    #[test]
    fn sequential() {
        let obj = "\
0x000: 30f20a00000000000000 |   irmovq $10,%rdx
0x00a: 30f00300000000000000 |   irmovq  $3,%rax
0x014: 6020                 |   addq %rdx,%rax
0x016: 00                   |   halt
";

        let expected = "\
30f20a00000000000000\
30f00300000000000000\
6020\
00\
";

        let mut expected = hex::decode(expected).unwrap();
        expected.resize(MEM_SIZE, 0);
        assert_eq!(decode(obj.lines()).unwrap().as_slice(), &expected);
    }

    #[test]
    fn padding() {
        let obj = "\
                            | # Demonstration of return
                            | # prog6
0x000: 30f43000000000000000 |    irmovq stack,%rsp  #   Initialize stack pointer
0x00a: 802000000000000000   |    call proc          #   Procedure call
0x013: 30f20a00000000000000 |    irmovq $10,%rdx    #   Return point
0x01d: 00                   |    halt
0x020:                      | .pos 0x20
0x020:                      | proc:                 # proc:
0x020: 90                   |    ret                #   Return immediately
0x021: 2023                 |    rrmovq %rdx,%rbx   #   Not executed
0x030:                      | .pos 0x30
0x030:                      | stack:                # stack: Stack pointer
";
        let expected = "\
30f43000000000000000\
802000000000000000\
30f20a00000000000000\
00\
0000\
90\
2023\
00000000000000000000000000\
";

        let mut expected = hex::decode(expected).unwrap();
        expected.resize(MEM_SIZE, 0);
        assert_eq!(decode(obj.lines()).unwrap().as_slice(), &expected);
    }
}
