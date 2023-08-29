#[derive(Debug, PartialEq, Eq)]
struct DecodeError;

fn decode<'a>(obj: impl Iterator<Item = &'a str>) -> Result<Vec<u8>, DecodeError> {
    let mut memory = Vec::new();

    for line in obj {
        let Some((line, _)) = line.split_once('|') else {
            return Err(DecodeError);
        };

        let Some((addr, bytes)) = line.split_once(':') else {
            continue;
        };

        let addr = usize::from_str_radix(addr.trim().trim_start_matches("0x"), 16)
            .map_err(|_| DecodeError)?;
        let bytes = hex::decode(bytes.trim()).map_err(|_| DecodeError)?;

        if memory.len() < addr + bytes.len() {
            memory.resize(addr + bytes.len(), 0);
        }
        memory[addr..addr + bytes.len()].copy_from_slice(&bytes);
    }

    Ok(memory)
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
        let expected = vec![0];

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

        assert_eq!(decode(obj.lines()).unwrap(), hex::decode(expected).unwrap())
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

        assert_eq!(decode(obj.lines()).unwrap(), hex::decode(expected).unwrap())
    }
}
