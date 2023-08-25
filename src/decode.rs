use hex;

struct DecodeError;

fn decode<'a>(obj: impl Iterator<Item = &'a str>) -> Result<Vec<u8>, DecodeError> {
    let mut memory = Vec::new();

    for line in obj {
        let Some((line, _)) = line.split_once('|') else {
            return Err(DecodeError);
        };

        let Some((addr, bytes)) = line.split_once(':') else {
            continue;
        };

        let addr = usize::from_str_radix(addr.trim().trim_start_matches("0x"), 16)
            .map_err(|_| DecodeError)?;
        let bytes = hex::decode(bytes.trim()).map_err(|_| DecodeError)?;

        if memory.len() < addr + bytes.len() {
            memory.resize(addr + bytes.len(), 0);
        }
        memory[addr..addr + bytes.len()].copy_from_slice(&bytes);
    }

    Ok(memory)
}
