mod lox;
mod tree_walk;

use tree_walk::interpreter::Interpreter;
use tree_walk::parser::Parser;
use tree_walk::tokenizer::Tokenizer;

fn main() {
    let mut interpreter = Interpreter::new();
    let mut tokenizer = Tokenizer::new();
    let src = "";
    tokenizer.parse(src);
    let root = Parser::new(tokenizer.tokens).parse();

    println!();
    let _ = interpreter.evaluate(&root);
}
