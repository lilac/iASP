use io::*;

fn main() {
    let in = io::stdin() as ReaderUtil;
    let mut nodes = 0;
    fn str_to_int(s: &str) -> uint {
        uint::from_str(s).get()
    }
    for in.each_line |line| {
        let words = str::words(line); //str::split(line, char::is_whitespace);
        if (words.len() > 0) {
            match words[0] {
                ~"c" => loop,
                ~"p" => {
                    let nodes = uint::from_str(words[2]).get();
                    for uint::range(1, nodes + 1) |i| {
                        io::println(fmt!("node(%u).", i));
                    }
                }
                ~"e" => {
                    let u = str_to_int(words[1]);
                    let v = str_to_int(words[2]);
                    io::println(fmt!("edge(%u, %u).", u, v));
                }
                _ => loop
            }
        }
    }
}

