#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Register {
    Rax,
    Rcx,
    Rdx,
    Rbx,
    Rsp,
    Rbp,
    Rsi,
    Rdi,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
}

/// Operator variants for `OPq` instructions.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Op {
    Add,
    Sub,
    And,
    Xor,
}

/// Condition variants for `jXX` and `cmovXX` instructions.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Cond {
    Always,
    Le,
    L,
    E,
    Ne,
    Ge,
    G,
}

/// A constant value, which can be a literal or a label.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Constant<'a> {
    Literal(u64),
    Label(&'a str),
}

/// A memory address for reference. Can be of form:
///
/// - Num(Reg)
/// - (Reg)
/// - Num
/// - Ident
/// - Ident(Reg)
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Memory<'a> {
    pub reg: Option<Register>,
    pub offset: Constant<'a>,
}

/// A statement, which can be a directive or an instruction.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Statement<'a> {
    // Directives
    Dbyte(u8),
    Dword(u16),
    Dlong(u32),
    Dquad(u64),
    Dpos(u64),
    Dalign(u64),

    // Instructions
    Ihalt,
    Inop,
    Irrmovq {
        src: Register,
        dest: Register,
    },
    Iirmovq {
        dest: Register,
        value: Constant<'a>,
    },
    Irmmovq {
        src: Register,
        mem: Memory<'a>,
    },
    Imrmovq {
        dest: Register,
        mem: Memory<'a>,
    },
    Iopq {
        op: Op,
        src: Register,
        dest: Register,
    },
    Ij {
        cond: Cond,
        target: Constant<'a>,
    },
    Icmov {
        cond: Cond,
        src: Register,
        dest: Register,
    },
    Icall(Constant<'a>),
    Iret,
    Ipushq(Register),
    Ipopq(Register),
}
