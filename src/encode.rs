use std::collections::HashMap;

use crate::parse::ParsedLine;
use crate::syntax::*;

/// The encoded record of a line.
#[derive(Debug, PartialEq, Eq)]
struct EncodedLine {
    /// The address of the line.
    address: u64,

    /// The byte encoding of the line.
    bytecode: Vec<u8>,
}

#[derive(Debug, PartialEq, Eq)]
struct EncodeError; // TODO: Context

fn encode(lines: &[ParsedLine]) -> Result<Vec<EncodedLine>, EncodeError> {
    // The first pass: Calculate the addresses for the lines
    let (addrs, labels) = calculate_address(&lines)?;

    // The second pass: Encode the instructions line by line
    lines
        .into_iter()
        .zip(addrs)
        .map(|(line, addr)| encode_line(line, addr, &labels))
        .collect()
}

/// A hashmap from label strings to addresses.
type LabelAddrs<'a> = HashMap<&'a str, u64>;

/// Calculate the addresses from an array of parsed lines.
/// Return an array of addresses of the same length as the input,
/// and a hashmap for the addresses of labels.
fn calculate_address<'a>(
    lines: &[ParsedLine<'a>],
) -> Result<(Vec<u64>, LabelAddrs<'a>), EncodeError> {
    let mut addrs = Vec::new();
    let mut labels = LabelAddrs::new();

    let mut pc = 0;

    for line in lines {
        // add the address for the line
        addrs.push(pc);

        // register the label
        if let Some(label) = line.label {
            // errs if the label is already defined
            if labels.contains_key(label) {
                return Err(EncodeError);
            }
            labels.insert(label, pc);
        }

        if let Some(statement) = line.statement {
            use Statement::*;

            // advance program counter
            match statement {
                Dbyte(_) => pc += 1,
                Dword(_) => pc += 2,
                Dlong(_) => pc += 4,
                Dquad(_) => pc += 8,
                Dpos(n) => {
                    if pc > n {
                        return Err(EncodeError);
                    } else {
                        pc = n;
                    }
                }
                Dalign(width) => {
                    let rem = pc % width;
                    if rem != 0 {
                        pc += width - rem;
                    }
                }

                Ihalt | Inop | Iret => pc += 1,
                Irrmovq { .. } | Iopq { .. } | Icmov { .. } | Ipushq(_) | Ipopq(_) => pc += 2,
                Ij { .. } | Icall(_) => pc += 9,
                Iirmovq { .. } | Irmmovq { .. } | Imrmovq { .. } => pc += 10,
            }
        }
    }

    Ok((addrs, labels))
}

fn encode_line(
    line: &ParsedLine,
    address: u64,
    labels: &LabelAddrs,
) -> Result<EncodedLine, EncodeError> {
    let bytecode = match line.statement {
        Some(s) => encode_statement(s, labels)?,
        None => Vec::new(),
    };

    Ok(EncodedLine { address, bytecode })
}

fn encode_statement(statement: Statement, labels: &LabelAddrs) -> Result<Vec<u8>, EncodeError> {
    macro_rules! bytecode {
        () => { Vec::new() };
        ( $( $x:expr ),* ) => {{
            let mut temp_vec = Vec::new();
            $( temp_vec.extend($x.to_le_bytes()); )*
            temp_vec
        }};
    }

    use Statement::*;

    let bytecode = match statement {
        Dbyte(n) => bytecode!(n),
        Dword(n) => bytecode!(n),
        Dlong(n) => bytecode!(n),
        Dquad(n) => bytecode!(n),
        Dpos(_) | Dalign(_) => bytecode!(),

        Ihalt => bytecode!(0x00_u8),
        Inop => bytecode!(0x10_u8),

        Irrmovq { src, dest } => bytecode!(0x20_u8, (src as u8) << 4 | dest as u8),
        Iirmovq { dest, value } => bytecode!(
            0x30_u8,
            0xf << 4 | dest as u8,
            resolve_constant(value, labels)?
        ),
        Irmmovq { src, mem } => {
            let (reg, offset) = resolve_memory(mem, labels)?;
            bytecode!(0x40_u8, (src as u8) << 4 | reg, offset)
        }
        Imrmovq { dest, mem } => {
            let (reg, offset) = resolve_memory(mem, labels)?;
            bytecode!(0x50_u8, (dest as u8) << 4 | reg, offset)
        }

        Iopq { op, src, dest } => bytecode!(0x60 | op as u8, (src as u8) << 4 | dest as u8),

        Ij { cond, target } => bytecode!(0x70 | cond as u8, resolve_constant(target, labels)?),

        Icmov { cond, src, dest } => {
            bytecode!(0x20 | cond as u8, (src as u8) << 4 | dest as u8)
        }

        Icall(target) => bytecode!(0x80_u8, resolve_constant(target, labels)?),
        Iret => bytecode!(0x90_u8),

        Ipushq(reg) => bytecode!(0xa0_u8, (reg as u8) << 4 | 0xf),
        Ipopq(reg) => bytecode!(0xb0_u8, (reg as u8) << 4 | 0xf),
    };

    Ok(bytecode)
}

fn resolve_constant(c: Constant, labels: &LabelAddrs) -> Result<u64, EncodeError> {
    match c {
        Constant::Literal(n) => Ok(n),
        Constant::Label(s) => labels.get(s).copied().ok_or(EncodeError),
    }
}

fn resolve_memory(mem: Memory, labels: &LabelAddrs) -> Result<(u8, u64), EncodeError> {
    let reg = if let Some(r) = mem.reg { r as u8 } else { 0xf };
    let offset = resolve_constant(mem.offset, labels)?;

    Ok((reg, offset))
}

#[cfg(test)]
mod tests {
    use super::*;

    use Cond::*;
    use Constant::*;
    use Op::*;
    use Register::*;
    use Statement::*;

    fn test_single_statement(statement: Statement, bytecode: Vec<u8>) {
        let parsed = vec![ParsedLine {
            label: None,
            statement: Some(statement),
        }];
        assert_eq!(
            encode(&parsed),
            Ok(vec![EncodedLine {
                address: 0,
                bytecode
            }]),
            "{statement:?}"
        )
    }

    #[test]
    fn simple_single() {
        let cases = [
            (Dbyte(0x8), vec![0x08]),
            (Dword(0x8), vec![0x08, 0]),
            (Dlong(0x8), vec![0x08, 0, 0, 0]),
            (Dquad(0x8), vec![0x08, 0, 0, 0, 0, 0, 0, 0]),
            (Ihalt, vec![0x00]),
            (Inop, vec![0x10]),
            (
                Irrmovq {
                    src: Rax,
                    dest: Rcx,
                },
                vec![0x20, 0x01],
            ),
            (
                Iirmovq {
                    dest: Rax,
                    value: Literal(0x8),
                },
                vec![0x30, 0xf0, 0x08, 0, 0, 0, 0, 0, 0, 0],
            ),
            (Icall(Literal(0x8)), vec![0x80, 0x08, 0, 0, 0, 0, 0, 0, 0]),
            (Iret, vec![0x90]),
            (Ipushq(Rax), vec![0xa0, 0x0f]),
            (Ipopq(Rax), vec![0xb0, 0x0f]),
        ];

        for (statement, bytecode) in cases {
            test_single_statement(statement, bytecode);
        }
    }

    #[test]
    fn rmmovq_mrmovq() {
        let cases = [(Some(Rax), 0x10), (None, 0x1f)];
        for (reg, byte) in cases {
            let mem = Memory {
                reg,
                offset: Literal(0x8),
            };

            test_single_statement(
                Irmmovq { src: Rcx, mem },
                vec![0x40, byte, 0x08, 0, 0, 0, 0, 0, 0, 0],
            );
            test_single_statement(
                Imrmovq { dest: Rcx, mem },
                vec![0x50, byte, 0x08, 0, 0, 0, 0, 0, 0, 0],
            );
        }
    }

    #[test]
    fn opq() {
        let cases = [(Add, 0x60), (Sub, 0x61), (And, 0x62), (Xor, 0x63)];

        for (op, byte) in cases {
            test_single_statement(
                Iopq {
                    op,
                    src: Rax,
                    dest: Rcx,
                },
                vec![byte, 0x01],
            )
        }
    }

    #[test]
    fn jxx() {
        let cases = [
            (Always, 0x70),
            (Le, 0x71),
            (L, 0x72),
            (E, 0x73),
            (Ne, 0x74),
            (Ge, 0x75),
            (G, 0x76),
        ];

        for (cond, byte) in cases {
            test_single_statement(
                Ij {
                    cond,
                    target: Literal(0x8),
                },
                vec![byte, 0x08, 0, 0, 0, 0, 0, 0, 0],
            )
        }
    }

    #[test]
    fn cmovxx() {
        let cases = [
            (Always, 0x20),
            (Le, 0x21),
            (L, 0x22),
            (E, 0x23),
            (Ne, 0x24),
            (Ge, 0x25),
            (G, 0x26),
        ];

        for (cond, byte) in cases {
            test_single_statement(
                Icmov {
                    cond,
                    src: Rax,
                    dest: Rcx,
                },
                vec![byte, 0x01],
            )
        }
    }

    #[test]
    fn address() {
        todo!()
    }
}
