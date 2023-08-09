use std::io::{self, Read};

use y86::{encode, parse_line};

fn main() {
    // read from stdin
    let mut src = String::new();
    io::stdin().read_to_string(&mut src).unwrap();

    // parse
    let parsed: Vec<_> = src.lines().map(|s| parse_line(s).unwrap()).collect();

    // encode
    let encoded = encode(&parsed).unwrap();

    // print to stdout
    for ((enc, src), par) in encoded.into_iter().zip(src.lines()).zip(parsed) {
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

        println!("{addr} {bytes:<20} | {src}");
    }
}
