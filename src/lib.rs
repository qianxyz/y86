mod encode;
mod parse;
mod syntax;

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
