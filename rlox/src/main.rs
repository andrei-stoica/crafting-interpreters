mod tree_walk;

use tree_walk::Tokenizer;

fn main() {
    let mut tokenizer = Tokenizer::new();
    let src = "{ ( ) } . + - =";

    let tokens = tokenizer.parse(src);

    println!("{:?}", tokens);
}
