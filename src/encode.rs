use std::collections::HashMap;

use crate::parse::ParsedLine;
use crate::syntax::*;

/// A line encoded into bytecode.
struct EncodedLine<'a> {
    /// The address of the line, `None` when it has no label or statement.
    address: Option<u64>,

    /// The byte encoding of the line.
    bytecode: Vec<u8>,

    /// The assembly source code.
    src: &'a str,
}

struct EncodeError; // TODO: Context

fn encode(lines: Vec<ParsedLine>) -> Result<Vec<EncodedLine>, EncodeError> {
    // The first pass: Calculate the addresses for the lines
    let (addrs, labels) = calculate_address(lines)?;

    // The second pass: Encode the instructions line by line
    addrs
        .into_iter()
        .map(|(addr, line)| encode_line(line, addr, &labels))
        .collect()
}

/// A parsed line with its address.
type AddrLine<'a> = (u64, ParsedLine<'a>);

/// A hashmap from label strings to addresses.
type LabelAddrs<'a> = HashMap<&'a str, u64>;

/// Calculate the addresses from a vector of parsed lines.
/// Return an associative array of addresses and lines,
/// and a hashmap for the addresses of labels.
fn calculate_address(lines: Vec<ParsedLine>) -> Result<(Vec<AddrLine>, LabelAddrs), EncodeError> {
    let mut addrs = Vec::new();
    let mut labels = LabelAddrs::new();

    let mut pc = 0;

    for line in lines {
        // add the address for the line
        addrs.push((pc, line));

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

fn encode_line<'a>(
    line: ParsedLine<'a>,
    addr: u64,
    labels: &LabelAddrs,
) -> Result<EncodedLine<'a>, EncodeError> {
    let address = if line.label.is_some() || line.statement.is_some() {
        Some(addr)
    } else {
        None
    };

    macro_rules! bytecode {
        () => { Vec::new() };
        ( $( $x:expr ),* ) => {{
            let mut temp_vec = Vec::new();
            $( temp_vec.extend($x.to_le_bytes()); )*
            temp_vec
        }};
    }

    let bytecode = if let Some(statement) = line.statement {
        use Statement::*;

        match statement {
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
        }
    } else {
        bytecode!()
    };

    Ok(EncodedLine {
        address,
        bytecode,
        src: line.src,
    })
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
