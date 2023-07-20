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

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Op {
    Add,
    Sub,
    And,
    Xor,
}

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

#[derive(Debug)]
pub(crate) struct SyntaxError; // TODO: Context
