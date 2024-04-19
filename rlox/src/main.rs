mod tree_walk;

use tree_walk::Tokenizer;

fn main() {
    let src = "{ ( ) } . + - =";
    let mut tokenizer = Tokenizer::new();

    let tokens = tokenizer.parse(src);

    println!("{:?}", tokens);
}
