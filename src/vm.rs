#[derive(Debug, Default, PartialEq, Eq)]
struct VM {
    registers: [u64; 16],

    zf: bool,
    sf: bool,
    of: bool,

    stat: Stat,

    pc: u64,
    mem: Vec<u8>,
}

#[derive(Debug, Default, PartialEq, Eq)]
enum Stat {
    /// Normal operation
    #[default]
    Aok,

    /// `halt` instruction encountered
    Hlt,

    /// invalid address encountered
    Adr,

    /// invalid instruction encountered
    Ins,
}

impl VM {
    fn from_memory(mem: Vec<u8>) -> Self {
        Self {
            mem,
            ..Default::default()
        }
    }

    fn branch(&self, lo: usize) -> bool {
        match lo {
            0x0 => true,                            // always
            0x1 => (self.sf ^ self.of) | self.zf,   // le
            0x2 => self.sf ^ self.of,               // l
            0x3 => self.zf,                         // e
            0x4 => !self.zf,                        // ne
            0x5 => !(self.sf ^ self.of),            // ge
            0x6 => !(self.sf ^ self.of) & !self.zf, // g
            _ => unreachable!(),
        }
    }

    fn step(&mut self) {
        macro_rules! oob {
            () => {{
                self.stat = Stat::Adr;
                return;
            }};
        }

        macro_rules! advance {
            () => {
                if let Some(b) = self.mem.get(self.pc as usize) {
                    self.pc += 1;
                    ((b >> 4) as usize, (b & 0xF) as usize)
                } else {
                    oob!()
                }
            };
        }

        macro_rules! read8 {
            ($addr:expr) => {
                if let Some(m) = self.mem.get($addr as usize..$addr as usize + 8) {
                    u64::from_le_bytes(m.try_into().unwrap())
                } else {
                    oob!()
                }
            };
        }

        macro_rules! write8 {
            ($addr:expr, $v:expr) => {
                if let Some(m) = self.mem.get_mut($addr as usize..$addr as usize + 8) {
                    m.copy_from_slice(&$v.to_le_bytes());
                } else {
                    oob!()
                }
            };
        }

        macro_rules! advance8 {
            () => {{
                let v = read8!(self.pc);
                self.pc += 8;

                v
            }};
        }

        macro_rules! push {
            ($v:expr) => {{
                let rsp = &mut self.registers[4];
                write8!(*rsp - 8, $v);
                *rsp -= 8;
            }};
        }

        macro_rules! pop {
            () => {{
                let rsp = &mut self.registers[4];
                *rsp += 8;
                read8!(*rsp - 8)
            }};
        }

        let (hi, lo) = advance!();

        match (hi, lo) {
            // halt
            (0x0, 0x0) => self.stat = Stat::Hlt,

            // nop
            (0x1, 0x0) => (),

            // rrmovq, cmovXX
            (0x2, 0x0..=0x6) => {
                let (ra, rb) = advance!();
                if self.branch(lo) {
                    self.registers[rb] = self.registers[ra];
                }
            }

            // irmovq
            (0x3, 0x0) => {
                let (_, rb) = advance!();
                let v = advance8!();
                self.registers[rb] = v;
            }

            // rmmovq
            (0x4, 0x0) => {
                let (ra, rb) = advance!();
                let d = advance8!();
                let addr = self.registers[rb] + d;
                write8!(addr, self.registers[ra]);
            }

            // mrmovq
            (0x5, 0x0) => {
                let (ra, rb) = advance!();
                let d = advance8!();
                let addr = self.registers[rb] + d;
                self.registers[ra] = read8!(addr);
            }

            // OPq
            // HACK: Integers are internally represented as unsigned, but are casted to
            // signed in arithmetic for the condition codes to be set properly.
            (0x6, 0x0..=0x3) => {
                let (ra, rb) = advance!();
                let a = self.registers[ra];
                let b = self.registers[rb];
                let (result, overflow) = match lo {
                    0x0 => (a as i64).overflowing_add(b as i64), // addq
                    0x1 => (a as i64).overflowing_sub(b as i64), // subq
                    0x2 => ((a & b) as i64, false),              // andq
                    0x3 => ((a ^ b) as i64, false),              // xorq
                    _ => unreachable!(),
                };
                self.of = overflow;
                self.sf = result < 0;
                self.zf = result == 0;
                self.registers[ra] = result as u64;
            }

            // jXX
            (0x7, 0x0..=0x6) => {
                let dest = advance8!();
                if self.branch(lo) {
                    self.pc = dest;
                }
            }

            // call
            (0x8, 0x0) => {
                let dest = advance8!();
                push!(self.pc);
                self.pc = dest;
            }

            // ret
            (0x9, 0x0) => self.pc = pop!(),

            // pushq
            (0xA, 0x0) => {
                let (ra, _) = advance!();
                let v = self.registers[ra];
                push!(v);
            }

            // popq
            (0xB, 0x0) => {
                let (ra, _) = advance!();
                self.registers[ra] = pop!();
            }

            _ => self.stat = Stat::Ins,
        }
    }

    fn run(&mut self) {
        while self.stat == Stat::Aok {
            self.step();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn mov() {
        let obj = [
            "30f02200000000000000", //   irmovq  src,%rax
            "50700000000000000000", //   mrmovq  (%rax),%rdi
            "2076",                 //   rrmovq  %rdi,%rsi
            "406f2a00000000000000", //   rmmovq  %rsi,dest
            "10",                   //   nop
            "00",                   //   halt
            "",                     //
            "cdab000000000000",     // src:      .quad   0xabcd
            "0000000000000000",     // dest:     .quad   0x0
        ];

        let mut mem = hex::decode(obj.join("")).unwrap();
        let mut vm = VM::from_memory(mem.clone());

        vm.run();

        assert_eq!(
            vm,
            VM {
                registers: [0x22, 0, 0, 0, 0, 0, 0xabcd, 0xabcd, 0, 0, 0, 0, 0, 0, 0, 0],
                stat: Stat::Hlt,
                pc: 0x22,
                mem: {
                    let l = mem.len();
                    mem[l - 8..l].copy_from_slice(&0xabcdu64.to_le_bytes());

                    mem
                },
                ..Default::default()
            }
        )
    }

    #[test]
    fn branch() {
        unimplemented!();
    }

    #[test]
    fn stack() {
        unimplemented!();
    }
}
