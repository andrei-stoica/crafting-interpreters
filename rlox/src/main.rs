mod tree_walk;

use tree_walk::parser::Parser;
use tree_walk::tokenizer::Tokenizer;

fn main() {
    let mut tokenizer = Tokenizer::new();
    let src = "print \"this\"; 4 + 8; \n 4 > 5; true == false;\n9 * 1;";
    tokenizer.parse(src);

    let root = Parser::new(tokenizer.tokens).parse();

    println!("{:#?}", root);
}
