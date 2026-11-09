use std::fs;

// Базовые классы
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


// Вспомогательные функции
fn read_u32(data: &[u8], start: usize) -> u32 {
    u32::from_le_bytes(data[start..start+4].try_into().unwrap())
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


// Функции компрессии
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
fn huffman_decompress(data: &[u8]) -> Vec<u8> {
    let len = read_u32(data, 0) as usize;
    if len == 0 {
        return Vec::new();
    }
    let k: usize =  u16::from_le_bytes(data[4..6].try_into().unwrap()) as usize;
    let mut freq: [usize; 256] = [0usize; 256];
    for i in 0..k {
        freq[data[6 + i * 5] as usize] = read_u32(data, 6 + i * 5 + 1) as usize;
    }
    let root: Node = build_tree(&freq);
    let mut node: &Node = &root;
    let mut out: Vec<u8> = Vec::new();
    let mut reader = BitReader {bytes: data[6 + k * 5..].to_vec(), pos: 0};
    while out.len() < len {
        match node {
            Node::Leaf { byte, .. } => {
                out.push(*byte);
                node = &root;
            },
            Node::Internal { left, right, .. } => {
                let bit = reader.read_bit().unwrap();
                node = if bit { right } else { left };
            }
        }
    } 
    out
}


// Главные функции
fn compress(data: &[u8]) -> Vec<u8> {
    let packed = huffman_compress(data);
    let mut out: Vec<u8> = Vec::new();
    if packed.len() < data.len() {
        out.push(1);
        out.extend_from_slice(&packed);
    } else {
        out.push(0);
        out.extend_from_slice(&data);
    }
    out
}
fn decompress(data: &[u8]) -> Vec<u8> {
    match data[0] {
        0 => return data[1..].to_vec(),
        1 => return huffman_decompress(&data[1..]),
        _ => panic!("Неизвестный режим")
    }
}
fn main() -> std::io::Result<()> {
    // let text = "The Bayeux Tapestry\nThe Bayeux Tapestry (also known in France as a Tapestry of Queen Matilda) is a unique medieval artifact that dates back to the 11th century. Nearly 70 metres of embroidered1 cloth expand on the events that led up to the Norman conquest of England, culminating with the fateful Battle of Hastings.\nTechnically not a tapestry (as tapestries are woven, not embroidered), this exquisite piece of cloth shows about 70 historical scenes and is narrated with Latin tituli2. It’s origins and the history of creation are still hotly debated in scholarly circles, but the two main theories give the credit either to the Queen Matilda of Flanders who was a wife of William the Conqueror, or to a bishop Odo of Bayeux, who was William’s half-brother and eventually became a regent of England in his absence.\nThe tapestry is made largely of plain weave3 linen and embroidered with wool yarn. The woolen crewelwork4 is made in various shades of brown, blue and green, mainly terracotta, russet, and olive green. Later restorations have also added some brighter colours, such as orange and light yellow. Attempts at restoration of both the beginning and the end of the tapestry were made at some points, adding some missing tituli and numerals, although an ongoing debate disputes the validity of these restorations.\nThe events unfolding on a tapestry took place in the years 1064 to 1066. Anglo-Saxon earl Harold Godwinson is depicted receiving the English crown from Edward the Confessor, a deathly ill English monarch. An invading Norman force is then shown, which soon engages Saxon forces in a bloody battle. Ultimately king Harold is slain, and English forces flee the battlefield. The last part of the tapestry was supposedly lost and a newer piece was added in its place roughly in 1810.\nThe tapestry allows for an unique insight4 into the mind of a medieval craftsman, and, as it was commissioned by victorious Normans, gives us a chance to see how the medieval history was customarily5 chronicled by the winning side.\nSince 1945 the Tapestry rests in Bayeux Museum, although as recently as 2018 the plans were put in motion to move it to an exhibit6 of the British Museum in London before the end of 2022. If everything proceeds as planned, it will be the first time the Tapestry has left France in over 950 years.";
    // //let text = "At school\nLucas goes to school every day of the week. He has many subjects to go to each school day: English, art, science, mathematics, gym, and history. His mother packs a big backpack full of books and lunch for Lucas.\nHis first class is English, and he likes that teacher very much. His English teacher says that he is a good pupil, which Lucas knows means that she thinks he is a good student.\nHis next class is art. He draws on paper with crayons and pencils and sometimes uses a ruler. Lucas likes art. It is his favorite class.\nHis third class is science. This class is very hard for Lucas to figure out, but he gets to work with his classmates a lot, which he likes to do. His friend, Kyle, works with Lucas in science class, and they have fun.\nThen Lucas gets his break for lunch. He sits with Kyle while he eats. The principal, or the headmaster as some call him, likes to walk around and talk to students during lunch to check that they are all behaving.\nThe next class is mathematics, which most of the students just call math. Kyle has trouble getting a good grade in mathematics, but the teacher is very nice and helpful.\nHis fourth class is gym. It is just exercising.\nHistory is his last class of the day. Lucas has a hard time staying awake. Many lessons are boring, and he is very tired after doing gym.";
    // //let text = "very, very big text";

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
                let restored = decompress(&data);
                fs::write(&args[3], &restored)?;
                println!("{} -> {}", data.len(), restored.len());
            }
            _ => eprintln!("Usage: main.exe [(de)compress] [input_path] [output_path]")
        }
    } else {
        eprintln!("Usage: main.exe [(de)compress] [input_path] [output_path]")
    }
    Ok(())
}
