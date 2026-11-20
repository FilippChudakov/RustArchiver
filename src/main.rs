use std::{collections::HashMap, fs, path::{Component, Path, PathBuf}};

// Basic classes
#[derive(Debug, PartialEq)]
enum ArchiveError {
    Truncated,
    UnknownMode(u8),
    Corrupted,
    ChecksumMismatch,
    InvalidName,
    Io(String)
}
impl From<std::io::Error> for ArchiveError {
    fn from(value: std::io::Error) -> Self {
        ArchiveError::Io(value.to_string())
    }
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
fn is_safe_name(name: &str) -> bool {
    !name.is_empty() && Path::new(name).components().all(|c| matches!(c, Component::Normal(_)))
}
fn pop_min(nodes: &mut Vec<Node>) -> Node {
    let (idx, _) = nodes.iter().enumerate().min_by_key(|(_, n)| n.freq()).unwrap();
    nodes.swap_remove(idx)
}
fn fill_lengths(node: &Node, depth: u8, lengths: &mut [u8; 256]) {
    match node {
        Node::Leaf { byte, ..} => {
            lengths[*byte as usize] = depth;
        }
        Node::Internal { left, right, .. } => {
            fill_lengths(left, depth+1, lengths);
            fill_lengths(right, depth+1, lengths);
        }
    }
}
fn canonical_codes(lengths: &[u8; 256]) -> Result<[u64; 256], ArchiveError> {
    let mut pairs: Vec<(u8, u8)> = Vec::new();
    for i in 0..256 {
        if lengths[i] > 0 {
            pairs.push((lengths[i], i as u8));
        }
    }
    pairs.sort();
    let mut codes = [0u64; 256];
    let mut code: u64 = 0;
    let mut prev_len: u8 = 0;
    for (len, byte) in pairs {
        if len > 63 {
            return Err(ArchiveError::Corrupted);
        } if len > prev_len {
            code <<= len - prev_len;
        } if code >= (1u64 << len) {
            return Err(ArchiveError::Corrupted);
        }
        codes[byte as usize] = code;
        code += 1; prev_len = len;
    }
    Ok(codes)
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
    let mut lengths= [0u8; 256];
    fill_lengths(&root, 0, &mut lengths);
    if let Node::Leaf { byte, ..} = &root {
        lengths[*byte as usize] = 1;
    }
    let codes = canonical_codes(&lengths).expect("Lengths from tree is always accessbile");
    let mut k: u16 = 0;
    for i in 0..256 {
        if lengths[i] > 0 {
            k += 1;
        }
    }

    let mut writer = BitWriter::new();
    if k > 1 {
        for &b in data {
            let len = lengths[b as usize];
            let code = codes[b as usize];
            for i in (0..len).rev() {
                writer.write_bit((code >> i) & 1 == 1);
            }
        }
    }

    out.extend_from_slice(&(data.len() as u32).to_le_bytes());
    out.extend_from_slice(&k.to_le_bytes());
    for i in 0..256 {
        if lengths[i] > 0 {
            out.push(i as u8);
            out.push(lengths[i] as u8);
        }
    }
    out.extend_from_slice(&writer.finish());
    out
}
fn huffman_decompress(data: &[u8]) -> Result<Vec<u8>, ArchiveError> {
    let total = read_u32(data, 0)? as usize;
    if total == 0 {
        return Ok(Vec::new());
    }
    let k: usize =  read_u16(data, 4)? as usize;

    let mut lengths = [0u8; 256];
    let mut last_byte = 0u8;
    for i in 0..k {
        let start = 6 + i * 2;
        let byte = *data.get(start).ok_or(ArchiveError::Truncated)?;
        let length = *data.get(start+1).ok_or(ArchiveError::Truncated)?;
        if length == 0 {
            return Err(ArchiveError::Corrupted);
        }
        lengths[byte as usize] = length;
        last_byte = byte;
    }
    if k == 1 {
        return Ok(vec![last_byte; total]);
    }
    let codes = canonical_codes(&lengths)?;
    let mut table: HashMap<(u8, u64), u8> = HashMap::new();
    for i in 0..256 {
        if lengths[i] > 0 {
            table.insert((lengths[i], codes[i]), i as u8);
        }
    }

    let bytes = data.get(6 + k * 2..).ok_or(ArchiveError::Truncated)?;
    let mut reader = BitReader {bytes: bytes.to_vec(), pos: 0};
    let mut out: Vec<u8> = Vec::new();
    let mut code = 0u64;
    let mut code_len = 0u8;

    while out.len() < total {
        let bit = reader.read_bit().ok_or(ArchiveError::Corrupted)?;
        code = (code << 1) | bit as u64;
        code_len += 1;
        if let Some(&byte) = table.get(&(code_len, code)) {
            out.push(byte);
            code = 0;
            code_len = 0;
        } else if code_len >= 63 {
            return Err(ArchiveError::Corrupted);
        }
    } 
    Ok(out)
}
fn pack_entries(entries: &[(String, Vec<u8>)]) -> Vec<u8> {
    let mut out: Vec<u8> = Vec::new();
    out.extend_from_slice(&(entries.len() as u32).to_le_bytes());
    for (file_name, file_data) in entries.iter() {
        let packed = compress(&file_data);
        out.extend_from_slice(&(file_name.len() as u16).to_le_bytes());
        out.extend_from_slice(&file_name.as_bytes());
        out.extend_from_slice(&(packed.len() as u32).to_le_bytes());
        out.extend_from_slice(&packed);
    }
    out
}
fn unpack_entries(data: &[u8]) -> Result<Vec<(String, Vec<u8>)>, ArchiveError> {
    let mut entries: Vec<(String, Vec<u8>)> = Vec::new();
    let count = read_u32(data, 0)?;
    let mut cursor: usize = 4;
    for _ in 0..count {
        let name_len = read_u16(data, cursor)? as usize;
        cursor += 2;
        let name_bytes = data.get(cursor..cursor + name_len).ok_or(ArchiveError::Truncated)?;
        cursor += name_len;
        let name = std::str::from_utf8(name_bytes).map_err(|_| ArchiveError::InvalidName)?.to_string();
        if !is_safe_name(&name) { return Err(ArchiveError::InvalidName); }

        let file_len = read_u32(data, cursor)? as usize;
        cursor += 4;
        let file_data = data.get(cursor..cursor + file_len).ok_or(ArchiveError::Truncated)?;
        cursor += file_len;
        
        entries.push((name, decompress(&file_data)?));
    }
    Ok(entries)
}
fn collect_files(dir: &Path, prefix: &str, out: &mut Vec<(String, PathBuf)>) -> std::io::Result<()> {
    let mut items: Vec<fs::DirEntry> = fs::read_dir(dir)?.collect::<Result<_, _>>()?;
    items.sort_by_key(|e| e.file_name());
    for item in items {
        let name = format!("{}/{}", prefix, item.file_name().to_string_lossy());
        if item.file_type()?.is_dir() {
            collect_files(&item.path(), &name, out)?;
        } else {
            out.push((name, item.path()));
        }
    }
    Ok(())
}


// Main functions
fn compress(data: &[u8]) -> Vec<u8> {
    let packed = huffman_compress(data);
    let mut out: Vec<u8> = Vec::new();
    if packed.len() < data.len() {
        out.push(1);
        out.extend_from_slice(&crc32(data).to_le_bytes());
        out.extend_from_slice(&packed);
    } else {
        out.push(0);
        out.extend_from_slice(&crc32(data).to_le_bytes());
        out.extend_from_slice(&data);
    }
    out
}
fn decompress(data: &[u8]) -> Result<Vec<u8>, ArchiveError> {
    let mode = *data.first().ok_or(ArchiveError::Truncated)?;
    match mode {
        0 => {
            let crc = read_u32(data, 1)?;
            let restored = data[5..].to_vec();
            if crc32(&restored) != crc {
                return Err(ArchiveError::ChecksumMismatch);
            }
            Ok(restored)
        },
        1 => {
            let crc = read_u32(data, 1)?;
            let restored = huffman_decompress(&data[5..])?;
            if crc32(&restored) != crc {
                return Err(ArchiveError::ChecksumMismatch);
            }
            Ok(restored)
        },
        other => Err(ArchiveError::UnknownMode(other))
    }
}

fn pack(paths: &[String]) -> Result<Vec<u8>, ArchiveError> {
    let mut entries: Vec<(String, Vec<u8>)> = Vec::new();

    for path in paths {
        let path_obj = Path::new(path);
        let mut found: Vec<(String, PathBuf)> = Vec::new();
        let name = path_obj.file_name().ok_or(ArchiveError::InvalidName)?.to_string_lossy().to_string();
        if path_obj.is_dir() {
            collect_files(path_obj, &name, &mut found)?;
        } else {
            found.push((name, path_obj.to_path_buf()));
        }
        for (entry_name, entry_path) in found {
            let data = fs::read(entry_path)?;
            entries.push((entry_name, data));
        }
    }

    Ok(pack_entries(&entries))
}
fn unpack(archive_path: &String, out_dir: &String) -> Result<usize, ArchiveError>{
    let data = &fs::read(archive_path)?;
    let entries = unpack_entries(data)?;
    let count = entries.len();

    let out_dir = Path::new(out_dir);
    fs::create_dir_all(out_dir)?;
    for (file_name, file_data) in entries {
        let target = out_dir.join(&file_name);
        if let Some(parent) = target.parent() {
            fs::create_dir_all(parent)?;
        }
        fs::write(&target, file_data)?;
    }
    Ok(count)
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = std::env::args().collect();

    if args.len() >= 4 {
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
            "pack" => {
                match pack(&args[3..]) {
                    Ok(packed) => {
                        fs::write(&args[2], &packed)?;
                    },
                    Err(e) => {
                        eprintln!("Error: {:?}", e);
                        std::process::exit(1);
                    }
                }
            },
            "unpack" => {
                match unpack(&args[2], &args[3]) {
                    Ok(n) => println!("Files unpacked: {}", n),
                    Err(e) => {
                        eprintln!("Error: {:?}", e);
                        std::process::exit(1);
                    }
                }
            }
            _ => eprintln!("Usage: main.exe [(de)compress]/[(un)pack] ([input_path] [output_path])/([paths])")
        }
    } else {
        eprintln!("Usage: main.exe [(de)compress]/[(un)pack] ([input_path] [output_path])/([paths])")
    }
    Ok(())
}


// Tests
#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn empty_data() {
        assert_eq!(decompress(&compress(&[])).unwrap(), []);
        assert_eq!(decompress(&[]).err(), Some(ArchiveError::Truncated));

        let entries: Vec<(String, Vec<u8>)> = Vec::new();
        assert_eq!(unpack_entries(&pack_entries(&entries)).unwrap(), entries);
    }
    #[test]
    fn unknown_mode() {
        assert_eq!(decompress(&[7, 1, 2]).err(), Some(ArchiveError::UnknownMode(7)));
    }
    #[test]
    fn trunc_corrupt() {
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
    #[test]
    fn crc_check() {
        assert_eq!(crc32(b"123456789"), 0xCBF4_3926);
        assert_eq!(crc32(b""), 0);
    }
    #[test]
    fn pack_roundtrip() {
        let entries = vec![
            ("a.txt".to_string(), b"hello hello hello".to_vec()),
            ("b.bin".to_string(), vec!(1, 2, 3)),
            ("empty".to_string(), vec![])
        ];
        let archive = pack_entries(&entries);
        assert_eq!(unpack_entries(&archive).unwrap(), entries);
    }
    #[test]
    fn cutted_archive() {
        let entries = vec![
            ("a.txt".to_string(), b"hello hello hello".to_vec()),
            ("b.bin".to_string(), vec!(1, 2, 3)),
            ("empty".to_string(), vec![])
        ];
        let archive = pack_entries(&entries);
        assert_eq!(unpack_entries(&archive[..archive.len() - 3]).err(), Some(ArchiveError::Truncated));
    }
    #[test]
    fn decompress_err_truncated() {
        assert_eq!(decompress(&[1]).err(), Some(ArchiveError::Truncated));
    }
    #[test]
    fn test_mode() {
        let packed = compress(&[1, 2, 3]);
        assert_eq!(*packed.first().unwrap(), 0);
        assert_eq!(decompress(&packed).unwrap(), [1, 2, 3]);
    }
    #[test]
    fn invalid_name() {
        let entries = vec![
            ("../a.txt".to_string(), b"hello hello hello".to_vec()),
            ("b.bin".to_string(), vec!(1, 2, 3)),
            ("empty".to_string(), vec![])
        ];
        assert_eq!(unpack_entries(&pack_entries(&entries)).err(), Some(ArchiveError::InvalidName));
    }
    #[test]
    fn canonical_example() {
        let mut lengths = [0u8; 256];
        lengths[b'a' as usize] = 1;
        for &c in b"bcdr" {
            lengths[c as usize] = 3;
        }
        let codes = canonical_codes(&lengths).unwrap();
        assert_eq!(codes[b'a' as usize], 0b0);
        assert_eq!(codes[b'b' as usize], 0b100);
        assert_eq!(codes[b'c' as usize], 0b101);
        assert_eq!(codes[b'd' as usize], 0b110);
        assert_eq!(codes[b'r' as usize], 0b111);
    }
    #[test]
    fn actually_compresses() {
        let data = b"The Bayeux Tapestry (also known in France as a Tapestry of Queen Matilda)".repeat(100);
        let packed = compress(&data);
        assert_eq!(packed[0], 1);
        assert!(packed.len() < data.len() * 3 / 4);
    }
    #[test]
    fn length_tests() {
        let data = b"very, very big text";
        let mut freq = [0usize; 256];
        for &b in data {
            freq[b as usize] += 1;
        }
        let root: Node = build_tree(&freq);
        let mut lengths= [0u8; 256];
        fill_lengths(&root, 0, &mut lengths);
        let mut total = 0;
        for i in 0..256 {
            total += freq[i] * lengths[i] as usize;
        }
        assert_eq!(total, 64);

        let mut lengths = [0u8; 256];
        lengths[b'a' as usize] = 1;
        for &c in b"bcd" {
            lengths[c as usize] = 2;
        }
        assert_eq!(canonical_codes(&lengths).err(), Some(ArchiveError::Corrupted));

        let mut lengths = [0u8; 256];
        lengths[b'a' as usize] = 64;
        assert_eq!(canonical_codes(&lengths).err(), Some(ArchiveError::Corrupted))

    }
    #[test]
    fn roundtrip_tests() {
        let data1 = b"a";
        let data2 = b"ab";
        let data3 = b"aaaa";
        let data4 = b"abracadabra";
        let packed1 = huffman_compress(data1);
        let packed2 = huffman_compress(data2);
        let packed3 = huffman_compress(data3);
        let packed4 = huffman_compress(data4);
        assert!(huffman_decompress(&packed1).unwrap() == *data1);
        assert!(huffman_decompress(&packed2).unwrap() == *data2);
        assert!(huffman_decompress(&packed3).unwrap() == *data3);
        assert!(huffman_decompress(&packed4).unwrap() == *data4);
        let data5 = "The Bayeux Tapestry (also known in France as a Tapestry of Queen Matilda)".repeat(100);
        let packed5 = huffman_compress(data5.as_bytes());
        assert_eq!(huffman_decompress(&packed5).unwrap(), data5.as_bytes());
        let data6 = &b"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa".repeat(100);
        let packed6 = huffman_compress(data6);
        assert!(packed6.len() < 20);
        assert!(huffman_decompress(&packed6).unwrap() == *data6);
        let mut data7: Vec<u8> = Vec::new();
        for b in 0..=255u8 {
            data7.push(b);
        }
        let packed7 = huffman_compress(&data7);
        assert_eq!(huffman_decompress(&packed7).unwrap(), data7);
    }
    #[test]
    fn random_roundtrip() {
        let mut x: u32 = 12345;
        for len in 0..300 {
            let m = (len % 20 + 1) as u8;
            let mut data: Vec<u8> = Vec::new();
            for _ in 0..len {
                x = x.wrapping_mul(1664525).wrapping_add(1013904223);
                data.push((x >> 24) as u8 % m);
            }
            let packed = huffman_compress(&data);
            assert_eq!(huffman_decompress(&packed).unwrap(), data, "Error on lengths: {}", len)
        }
        let mut x1: u32 = 12345;
        for len in 0..300 {
            let m = (len % 20 + 1) as u8;
            let mut data: Vec<u8> = Vec::new();
            for _ in 0..len {
                x1 = x1.wrapping_mul(1664525).wrapping_add(1013904223);
                data.push((x1 >> 24) as u8 % m);
            }
            let packed = compress(&data);
            assert_eq!(decompress(&packed).unwrap(), data, "Error on lengths: {}", len)
        }
    }
}