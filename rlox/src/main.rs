mod tree_walk;

use tree_walk::interpreter::evaluate;
use tree_walk::parser::Parser;
use tree_walk::tokenizer::Tokenizer;

fn main() {
    let mut tokenizer = Tokenizer::new();
    let src = "print \"this\"; print 4 + 8; \n print 4 > 5; print true == false;\n print 9 * 1;";
    tokenizer.parse(src);
    let root = Parser::new(tokenizer.tokens).parse();

    println!();
    evaluate(root);
}
