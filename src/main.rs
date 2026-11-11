use std::fs;

// Basic classes
#[derive(Debug, PartialEq)]
enum ArchiveError {
    Truncated,
    UnknownMode(u8),
    Corrupted,
    ChecksumMismatch
}
enum Node {
    Leaf {byte: u8, freq:usize},
    Internal {freq: usize, left: Box<Node>, right: Box<Node>}
}
impl Node {
    fn freq(&self) -> usize {
        match self {
            Node::Leaf { freq, .. } => *freq,
            Node::Internal { freq, .. } => *freq,
        }
    }
}

struct BitWriter {
    bytes: Vec<u8>,
    current: u8,
    filled: u8
}
impl BitWriter {
    fn new() -> Self {
        BitWriter { bytes: Vec::new(), current: 0, filled: 0 }
    }
    fn write_bit(&mut self, bit: bool) {
        self.current <<= 1;
        self.current |= bit as u8;
        self.filled += 1;
        if self.filled == 8 {
            self.bytes.push(self.current);
            self.current = 0;
            self.filled = 0;
        }
    }
    fn finish(mut self) -> Vec<u8> {
        if self.filled > 0 {
            self.current <<= 8-self.filled;
            self.bytes.push(self.current);
        }
        self.bytes
    }
}
struct BitReader {
    bytes: Vec<u8>,
    pos: usize
}
impl BitReader {
    fn read_bit(&mut self) -> Option<bool> {
        if self.pos / 8 >= self.bytes.len() {
            return None;
        }
        let byte = self.bytes[self.pos/8];
        let bit = (byte >> (7 - self.pos % 8)) & 1 == 1;
        self.pos+=1;
        Some(bit)
    }
}


// Utilities
fn read_u16(data: &[u8], start: usize) -> Result<u16, ArchiveError> {
    let bytes = data.get(start..start+2).ok_or(ArchiveError::Truncated)?;
    Ok(u16::from_le_bytes(bytes.try_into().unwrap()))
}
fn read_u32(data: &[u8], start: usize) -> Result<u32, ArchiveError> {
    let bytes = data.get(start..start+4).ok_or(ArchiveError::Truncated)?;
    Ok(u32::from_le_bytes(bytes.try_into().unwrap()))
}
fn crc32(data: &[u8]) -> u32 {
    let mut crc: u32 = 0xFFFF_FFFF;
    for &byte in data {
        crc ^= byte as u32;
        for _ in 0..8 {
            if (crc & 1) == 1 {
                crc >>= 1;
                crc ^= 0xEDB8_8320;
            } else {
                crc >>= 1;
            }
        }
    }
    !crc
}
fn pop_min(nodes: &mut Vec<Node>) -> Node {
    let (idx, _) = nodes.iter().enumerate().min_by_key(|(_, n)| n.freq()).unwrap();
    nodes.swap_remove(idx)
}
fn build_codes(node: &Node, prefix: String, codes: &mut Vec<String>) {
    match node {
        Node::Leaf { byte, .. } => {
            codes[*byte as usize] = prefix;
        },
        Node::Internal {left, right, .. } => {
            build_codes(left, format!("{}0", prefix), codes);
            build_codes(right, format!("{}1", prefix), codes);
        }
    }
}
fn build_tree(freq: &[usize; 256]) -> Node {
    let mut counts: Vec<(u8, usize)> = Vec::new();
    for i in 0..256 {
        if freq[i] > 0 {
            counts.push((i as u8, freq[i]));
        }
    }
    let mut nodes: Vec<Node> = counts.iter().map(|&(byte, freq)| Node::Leaf { byte, freq }).collect();
    while nodes.len() > 1 {
        let left: Node = pop_min(&mut nodes);
        let right: Node = pop_min(&mut nodes);
        nodes.push(Node::Internal { freq: left.freq()+right.freq(), left: Box::new(left), right: Box::new(right) })
    }
    pop_min(&mut nodes)
}


// (De)Compressing functions
fn huffman_compress(data: &[u8]) -> Vec<u8>{
    if data.len() == 0 {
        return vec![0; 6];
    }
    let mut out: Vec<u8> = Vec::new();
    let mut freq: [usize; 256] = [0usize; 256];
    for &b in data {
        freq[b as usize] += 1;
    }
    let root: Node = build_tree(&freq);
    let mut codes: Vec<String> = vec![String::new(); 256];
    build_codes(&root, String::new(), &mut codes);

    let mut writer = BitWriter::new();
    for &b in data {
        for c in codes[b as usize].chars() {
            writer.write_bit(c == '1');
        }
    }
    let mut k: u16 = 0;
    for i in 0..256 {
        if freq[i] > 0 {
            k += 1;
        }
    }
    out.extend_from_slice(&(data.len() as u32).to_le_bytes());
    out.extend_from_slice(&k.to_le_bytes());
    for i in 0..256 {
        if freq[i] > 0 {
            out.extend_from_slice(&(i as u8).to_le_bytes());
            out.extend_from_slice(&(freq[i] as u32).to_le_bytes());
        }
    }
    out.extend_from_slice(&writer.finish());
    out
}
fn huffman_decompress(data: &[u8]) -> Result<Vec<u8>, ArchiveError> {
    let len = read_u32(data, 0)? as usize;
    if len == 0 {
        return Ok(Vec::new());
    }
    let k: usize =  read_u16(data, 4)? as usize;
    let mut freq: [usize; 256] = [0usize; 256];
    for i in 0..k {
        let start = 6 + i * 5;
        let byte = *data.get(start).ok_or(ArchiveError::Truncated)?;
        freq[byte as usize] = read_u32(data, 6 + i * 5 + 1)? as usize;
    }
    let mut total = 0;
    for i in 0..256 {
        total += freq[i];
    }
    if total != len {
        return Err(ArchiveError::Corrupted);
    }
    let root: Node = build_tree(&freq);
    let mut node: &Node = &root;
    let mut out: Vec<u8> = Vec::new();
    let bytes = data.get(6 + k * 5..).ok_or(ArchiveError::Truncated)?;
    let mut reader = BitReader {bytes: bytes.to_vec(), pos: 0};
    while out.len() < len {
        match node {
            Node::Leaf { byte, .. } => {
                out.push(*byte);
                node = &root;
            },
            Node::Internal { left, right, .. } => {
                let bit = reader.read_bit().ok_or(ArchiveError::Corrupted)?;
                node = if bit { right } else { left };
            }
        }
    } 
    Ok(out)
}


// Main functions
fn compress(data: &[u8]) -> Vec<u8> {
    let packed = huffman_compress(data);
    let mut out: Vec<u8> = Vec::new();
    if packed.len() < data.len() {
        out.push(1);
        out.extend_from_slice(&(crc32(data) as u32).to_le_bytes());
        out.extend_from_slice(&packed);
    } else {
        out.push(0);
        out.extend_from_slice(&data);
    }
    out
}
fn decompress(data: &[u8]) -> Result<Vec<u8>, ArchiveError> {
    let mode = *data.first().ok_or(ArchiveError::Truncated)?;
    match mode {
        0 => Ok(data[1..].to_vec()),
        1 => {
            let crc = read_u32(data, 1).unwrap();
            let restored = huffman_decompress(&data[5..]);
            match &restored {
                Ok(bytes) => {
                    if crc32(&bytes) != crc {
                        return Err(ArchiveError::ChecksumMismatch);
                    }
                }
                Err(_) => return restored
            }
            
            return restored;
        },
        other => Err(ArchiveError::UnknownMode(other))
    }
}
fn main() -> std::io::Result<()> {
    let args: Vec<String> = std::env::args().collect();

    if args.len() == 4 {
        match args[1].as_str() {
            "compress" => {
                let data = fs::read(&args[2])?;
                let packed: Vec<u8> = compress(&data);
                fs::write(&args[3], &packed)?;
                println!("{} -> {}", data.len(), packed.len());
            }
            "decompress" => {
                let data = fs::read(&args[2])?;
                match decompress(&data) {
                    Ok(restored) => {
                        fs::write(&args[3], &restored)?;
                        println!("{} -> {}", data.len(), restored.len());
                    },
                    Err(e) => {
                        eprintln!("Error: The archive won't open ({:?})", e);
                        std::process::exit(1);
                    }
                }
            }
            _ => eprintln!("Usage: main.exe [(de)compress] [input_path] [output_path]")
        }
    } else {
        eprintln!("Usage: main.exe [(de)compress] [input_path] [output_path]")
    }
    Ok(())
}


// Tests
#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn empty_data() {
        assert_eq!(decompress(&[]).err(), Some(ArchiveError::Truncated));
    }
    #[test]
    fn unknown_mode() {
        assert_eq!(decompress(&[7, 1, 2]).err(), Some(ArchiveError::UnknownMode(7)));
    }
    #[test]
    fn not_truncated() {
        let data = b"The Bayeux Tapestry (also known in France as a Tapestry of Queen Matilda)".repeat(100);
        let packed = compress(&data);
        assert_eq!(decompress(&packed[..10]).err(), Some(ArchiveError::Truncated));
        assert_eq!(decompress(&packed[..packed.len()/2]).err(), Some(ArchiveError::Corrupted));
    }
    #[test]
    fn corrupted() {
        let data = b"The Bayeux Tapestry (also known in France as a Tapestry of Queen Matilda)".repeat(100);
        let mut packed = compress(&data);
        packed[8] ^= 1;
        assert_eq!(decompress(&packed[..packed.len()]).err(), Some(ArchiveError::Corrupted));
    }
    #[test]
    fn aaaa_text() {
        let data = &b"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa".repeat(100);
        let packed = compress(data);
        assert!(packed.len() < 20);
        assert!(decompress(&packed).unwrap() == *data);
    }
    #[test]
    fn normal_text() {
        let data = "The Bayeux Tapestry (also known in France as a Tapestry of Queen Matilda)".repeat(100);
        let packed = compress(data.as_bytes());
        assert_eq!(decompress(&packed).unwrap(), data.as_bytes());
    }
    #[test]
    fn with_256_bytes() {
        let mut data: Vec<u8> = Vec::new();
        for b in 0..=255u8 {
            data.push(b);
        }
        let packed = huffman_compress(&data);
        assert_eq!(huffman_decompress(&packed).unwrap(), data);
    }
    #[test]
    fn check_sum() {
        let data = "The Bayeux Tapestry (also known in France as a Tapestry of Queen Matilda)".repeat(100);
        let mut packed = compress(data.as_bytes());
        packed[11] ^= 1;
        assert_eq!(decompress(&packed).err(), Some(ArchiveError::ChecksumMismatch));
    }
    #[test]
    fn bit_corrupting() {
        let data = "The Bayeux Tapestry (also known in France as a Tapestry of Queen Matilda)".repeat(100);
        let mut packed = compress(data.as_bytes());
        let l = packed.len();
        packed[l/2] ^= 1;
        assert!(decompress(&packed).is_err());
    }
}