use std::collections::HashMap;

use crate::syntax::{Constant, Statement};

struct Encoder {
    label_def: HashMap<String, usize>,
    label_ref: HashMap<String, Vec<usize>>,
    bytecode: Vec<u8>,
}

impl Encoder {
    fn new() -> Self {
        Self {
            label_def: HashMap::new(),
            label_ref: HashMap::new(),
            bytecode: Vec::new(),
        }
    }

    fn encode(&mut self, statements: Vec<Statement>) {
        macro_rules! extend {
            ( $( $x:expr ),* ) => {
                {
                    $(
                        let n = $x;
                        self.bytecode.extend(n.to_le_bytes());
                    )*
                }
            };
        }

        for stmt in statements {
            match stmt {
                Statement::LabelDef(s) => self.def_label(s),

                Statement::Dbyte(n) => extend!(n),
                Statement::Dword(n) => extend!(n),
                Statement::Dlong(n) => extend!(n),
                Statement::Dquad(n) => extend!(n),

                // TODO: Should `.pos` work backwards?
                Statement::Dpos(n) => self.bytecode.resize(n as usize, 0),

                Statement::Dalign(n) => self.align(n),

                Statement::Ihalt => extend!(0x00_u8),
                Statement::Inop => extend!(0x10_u8),

                Statement::Irrmovq { src, dest } => {
                    extend!(0x20_u8, (src as u8) << 4 | dest as u8)
                }

                Statement::Iirmovq { dest, value } => {
                    extend!(
                        0x30_u8,
                        0xFu8 << 4 | dest as u8,
                        self.resolve_constant(value)
                    )
                }

                Statement::Irmmovq { src, dest, offset } => {
                    extend!(
                        0x40_u8,
                        (src as u8) << 4 | dest as u8,
                        self.resolve_constant(offset)
                    )
                }

                Statement::Imrmovq { src, dest, offset } => {
                    extend!(
                        0x50_u8,
                        (dest as u8) << 4 | src as u8,
                        self.resolve_constant(offset)
                    )
                }

                Statement::Iopq { op, src, dest } => {
                    extend!(0x6 << 4 | op as u8, (src as u8) << 4 | dest as u8)
                }

                Statement::Ij { cond, target } => {
                    extend!(0x7 << 4 | cond as u8, self.resolve_constant(target))
                }

                Statement::Icmov { cond, src, dest } => {
                    extend!(0x2 << 4 | cond as u8, (src as u8) << 4 | dest as u8)
                }

                Statement::Icall(c) => extend!(0x80_u8, self.resolve_constant(c)),
                Statement::Iret => extend!(0x90_u8),

                Statement::Ipushq(r) => extend!(0xA0_u8, (r as u8) << 4 | 0xF),
                Statement::Ipopq(r) => extend!(0xB0_u8, (r as u8) << 4 | 0xF),
            }
        }
        // TODO: Handle when constant left unresolved
    }

    fn def_label(&mut self, label: String) {
        // TODO: Handle when label has been defined
        let pc = self.bytecode.len();
        if let Some(refs) = self.label_ref.remove(&label) {
            for r in refs {
                self.bytecode[r..r + 8].copy_from_slice(&pc.to_le_bytes())
            }
        }
        self.label_def.insert(label, pc);
    }

    fn align(&mut self, mul: u64) {
        let l = self.bytecode.len();
        let rem = l % mul as usize;
        if rem != 0 {
            let pad = mul as usize - rem;
            self.bytecode.resize(l + pad, 0);
        }
    }

    fn resolve_constant(&mut self, c: Constant) -> u64 {
        match c {
            Constant::Literal(n) => n,
            Constant::Label(s) => {
                if let Some(&pc) = self.label_def.get(&s) {
                    pc as u64
                } else {
                    self.label_ref
                        .entry(s)
                        .or_insert_with(Vec::new)
                        .push(self.bytecode.len());
                    0
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::Cond::*;
    use crate::syntax::Op::*;
    use crate::syntax::Register::*;
    use Constant::*;
    use Statement::*;

    #[test]
    fn sum() {
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

        let statements = vec![
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

        let mut encoder = Encoder::new();
        encoder.encode(statements);
        let bytecode = encoder.bytecode;
        let hex: String = bytecode.iter().map(|b| format!("{:02x}", b)).collect();

        let expected = "\
30f40002000000000000\
803800000000000000\
00\
00000000\
0d000d000d000000\
c000c000c0000000\
000b000b000b0000\
00a000a000a00000\
30f71800000000000000\
30f60400000000000000\
805600000000000000\
90\
30f80800000000000000\
30f90100000000000000\
6300\
6266\
708700000000000000\
50a70000000000000000\
60a0\
6087\
6196\
747700000000000000\
90\
";

        assert!(hex.starts_with(expected));
    }
}
