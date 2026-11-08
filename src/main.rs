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
        let byte = self.bytes[self.pos/8];
        let bit = (byte >> 7 - self.pos % 8) & 1 == 1;
        self.pos+=1;
        Some(bit)
    }
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

fn main() {
    // Сбор частот
    let text = "very, very big text";
    let mut freq = [0usize; 256];
    let mut counts: Vec<(u8, usize)> = Vec::new();
    
    for b in text.bytes() {
        freq[b as usize] += 1;
    }
    for i in 0..256 {
        if freq[i] > 0 {
            counts.push((i as u8, freq[i]));
        }
    }

    // Создание дерева
    let mut nodes: Vec<Node> = counts.iter().map(|&(byte, freq)| Node::Leaf { byte, freq }).collect();
    while nodes.len() > 1 {
        let left: Node = pop_min(&mut nodes);
        let right: Node = pop_min(&mut nodes);
        nodes.push(Node::Internal { freq: left.freq()+right.freq(), left: Box::new(left), right: Box::new(right) })
    }
    let root: Node = pop_min(&mut nodes);

    // Создание кодов и конструирование битов
    let mut codes: Vec<String> = vec![String::new(); 256];
    build_codes(&root, String::new(), &mut codes);

    let mut writer = BitWriter::new();
    for b in text.bytes() {
        for c in codes[b as usize].chars() {
            writer.write_bit(c == '1');
        }
    }
    let packed = writer.finish();
    for byte in &packed {
        print!("{:08b} ", byte)
    }

    // Чтение
    let mut node: &Node = &root;
    let mut out: Vec<u8> = Vec::new();
    let mut reader = BitReader {bytes: packed, pos: 0};
    while out.len() < text.len() {
        match node {
            Node::Leaf { byte, .. } => {
                out.push(*byte);
                node = &root;
            },
            Node::Internal { left, right, .. } => {
                let bit = reader.read_bit().unwrap();
                node = if bit { right } else { left }
            }
        }
    }
    let restored = String::from_utf8(out).unwrap();
    println!("{}", restored == text)
}
