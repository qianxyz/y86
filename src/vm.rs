#[derive(Default)]
struct VM {
    registers: [u64; 16],

    zf: bool,
    sf: bool,
    of: bool,

    stat: Stat,

    pc: usize,
    mem: Vec<u8>,
}

#[derive(Default)]
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
            0x0 => true,                            // rrmovq
            0x1 => (self.sf ^ self.of) | self.zf,   // cmovle
            0x2 => self.sf ^ self.of,               // cmovl
            0x3 => self.zf,                         // cmove
            0x4 => !self.zf,                        // cmovne
            0x5 => !(self.sf ^ self.of),            // cmovge
            0x6 => !(self.sf ^ self.of) & !self.zf, // cmovg
            _ => unreachable!(),
        }
    }

    fn step(&mut self) {
        macro_rules! read {
            () => {
                if let Some(b) = self.mem.get(self.pc) {
                    self.pc += 1;
                    ((b >> 4) as usize, (b & 0xF) as usize)
                } else {
                    self.stat = Stat::Adr;
                    return;
                }
            };
        }

        macro_rules! read8 {
            () => {
                if let Some(bs) = self.mem.get(self.pc..self.pc + 8) {
                    self.pc += 8;
                    u64::from_le_bytes(bs.try_into().unwrap())
                } else {
                    self.stat = Stat::Adr;
                    return;
                }
            };
        }

        let (hi, lo) = read!();

        match (hi, lo) {
            // halt
            (0x0, 0x0) => self.stat = Stat::Hlt,

            // nop
            (0x1, 0x0) => (),

            // rrmovq, cmovXX
            (0x2, 0x0..=0x6) => {
                let (ra, rb) = read!();
                if self.branch(lo) {
                    self.registers[rb] = self.registers[ra];
                }
            }

            // irmovq
            (0x3, 0x0) => {
                let (_, rb) = read!();
                let v = read8!();
                self.registers[rb] = v;
            }

            // rmmovq
            (0x4, 0x0) => {
                let (ra, rb) = read!();
                let d = read8!();
                let addr = (self.registers[rb] + d) as usize;
                if let Some(m) = self.mem.get_mut(addr..addr + 8) {
                    m.copy_from_slice(&self.registers[ra].to_le_bytes());
                } else {
                    self.stat = Stat::Adr;
                    return;
                }
            }

            // mrmovq
            (0x5, 0x0) => {
                let (ra, rb) = read!();
                let d = read8!();
                let addr = (self.registers[rb] + d) as usize;
                if let Some(m) = self.mem.get(addr..addr + 8) {
                    self.registers[ra] = u64::from_le_bytes(m.try_into().unwrap());
                } else {
                    self.stat = Stat::Adr;
                    return;
                }
            }

            // OPq
            (0x6, 0x0..=0x3) => {
                let (ra, rb) = read!();
                let a = self.registers[ra];
                let b = self.registers[rb];
                let (result, overflow) = match lo {
                    0x0 => a.overflowing_add(b), // addq
                    0x1 => a.overflowing_sub(b), // subq
                    0x2 => (a & b, false),       // andq
                    0x3 => (a ^ b, false),       // xorq
                    _ => unreachable!(),
                };
                self.of = overflow;
                // FIXME: SF is set as if it is signed arithmetic
                self.sf = (result as i64) < 0;
                self.zf = result == 0;
                self.registers[ra] = result;
            }

            // jXX
            (0x7, 0x0..=0x6) => {
                let dest = read8!();
                if self.branch(lo) {
                    self.pc = dest as usize;
                }
            }

            // call
            (0x8, 0x0) => {
                let dest = read8!();
                let rsp = self.registers[4] as usize;
                if let Some(m) = self.mem.get_mut(rsp - 8..rsp) {
                    m.copy_from_slice(&self.pc.to_le_bytes());
                    self.registers[4] -= 8;
                    self.pc = dest as usize;
                } else {
                    self.stat = Stat::Adr;
                    return;
                }
            }

            // ret
            (0x9, 0x0) => {
                self.registers[4] += 8;
                let rsp = self.registers[4] as usize;
                if let Some(m) = self.mem.get(rsp - 8..rsp) {
                    self.pc = usize::from_le_bytes(m.try_into().unwrap());
                } else {
                    self.stat = Stat::Adr;
                    return;
                }
            }

            // pushq
            (0xA, 0x0) => {
                let (ra, _) = read!();
                let rsp = self.registers[4] as usize;
                if let Some(m) = self.mem.get_mut(rsp - 8..rsp) {
                    m.copy_from_slice(&self.registers[ra].to_le_bytes());
                    self.registers[4] -= 8;
                } else {
                    self.stat = Stat::Adr;
                    return;
                }
            }

            // popq
            (0xB, 0x0) => {
                let (ra, _) = read!();
                self.registers[4] += 8;
                let rsp = self.registers[4] as usize;
                if let Some(m) = self.mem.get(rsp - 8..rsp) {
                    self.registers[ra] = u64::from_le_bytes(m.try_into().unwrap());
                } else {
                    self.stat = Stat::Adr;
                    return;
                }
            }

            _ => self.stat = Stat::Ins,
        }
    }
}
