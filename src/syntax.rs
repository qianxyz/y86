#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Register {
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
#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Op {
    Add,
    Sub,
    And,
    Xor,
}

/// Condition variants for `jXX` and `cmovXX` instructions.
#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Cond {
    Always,
    Le,
    L,
    E,
    Ne,
    Ge,
    G,
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Token {
    // Instruction mnemonics
    Ihalt,
    Inop,
    Irrmovq,
    Iirmovq,
    Irmmovq,
    Imrmovq,
    Iopq(Op),
    Ij(Cond),
    Icmov(Cond),
    Icall,
    Iret,
    Ipushq,
    Ipopq,

    Reg(Register),

    Number(u64),

    Label(String),

    // Directives
    Dbyte,
    Dword,
    Dlong,
    Dquad,
    Dpos,
    Dalign,

    // Punctuations
    Colon,
    Lparen,
    Rparen,
    Comma,
    Dollar,
}

/// A constant value, which can be a literal (preceded by `$`) or a label.
#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Constant {
    Literal(u64),
    Label(String),
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Statement {
    // Label definitions
    LabelDef(String),

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
        value: Constant,
    },
    Irmmovq {
        src: Register,
        dest: Register,
        offset: Constant,
    },
    Imrmovq {
        src: Register,
        dest: Register,
        offset: Constant,
    },
    Iopq {
        op: Op,
        src: Register,
        dest: Register,
    },
    Ij {
        cond: Cond,
        target: Constant,
    },
    Icmov {
        cond: Cond,
        src: Register,
        dest: Register,
    },
    Icall(Constant),
    Iret,
    Ipushq(Register),
    Ipopq(Register),
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct SyntaxError; // TODO: Context
