mod encode;
mod parse;
mod syntax;

mod decode;
mod vm;

#[derive(Debug, PartialEq, Eq)]
enum YasError<'a> {
    Syntax(parse::SyntaxError<'a>),
    Encode(encode::EncodeError<'a>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct YasErrorContext<'a> {
    error: YasError<'a>,
    lineno: usize,
    src: &'a str,
}

impl std::fmt::Display for YasErrorContext<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "Error: {}",
            match &self.error {
                YasError::Syntax(e) => e.to_string(),
                YasError::Encode(e) => e.to_string(),
            }
        )?;
        write!(f, "\t{} | {}", self.lineno, self.src)?;

        Ok(())
    }
}

impl std::error::Error for YasErrorContext<'_> {}

pub fn assemble<'a>(
    src: impl Iterator<Item = &'a str>,
) -> Result<Vec<String>, Vec<YasErrorContext<'a>>> {
    let parsed = parse::parse(src)?;
    let encoded = encode::encode(&parsed)?;

    let print = encoded
        .into_iter()
        .zip(parsed)
        .map(|(enc, par)| {
            let src = par.src;
            let addr = enc.address;
            let addr = format!("0x{addr:03x}:");
            // do not print the address if the line is semantically blank
            let addr = if par.label.is_some() || par.statement.is_some() {
                addr
            } else {
                " ".repeat(addr.len())
            };

            let bytes = enc
                .bytecode
                .iter()
                .map(|b| format!("{b:02x}"))
                .collect::<Vec<_>>()
                .join("");

            format!("{addr} {bytes:<20} | {src}")
        })
        .collect();

    Ok(print)
}

pub use vm::{Stat, VM};

#[derive(Debug, PartialEq, Eq)]
pub struct YisErrorContext<'a> {
    error: decode::DecodeError<'a>,
    lineno: usize,
    src: &'a str,
}

impl std::fmt::Display for YisErrorContext<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Error: {}", self.error)?;
        write!(f, "\t{} | {}", self.lineno, self.src)?;

        Ok(())
    }
}

impl std::error::Error for YisErrorContext<'_> {}

pub fn emulate<'a>(obj: impl Iterator<Item = &'a str>) -> Result<VM, Vec<YisErrorContext<'a>>> {
    let mem = decode::decode(obj)?;
    let mut vm = vm::VM::from_memory(mem);

    vm.run();

    Ok(vm)
}
