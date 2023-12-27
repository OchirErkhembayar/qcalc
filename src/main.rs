use tc::{interpreter::interpret, parse::Parser, token::Tokenizer};

fn main() {
    run_repl();
}

fn run_repl() {
    use std::io::{self, Write};

    println!("Welcome to the TC REPL");
    println!("Press q to quit");
    let mut buffer = String::new();
    loop {
        print!("> ");
        io::stdout().flush().expect("Failed to flush output");
        io::stdin()
            .read_line(&mut buffer)
            .expect("Failed to read input");
        let input = buffer.trim();
        if input.to_lowercase() == "q" {
            break;
        }
        let tokens =
            Tokenizer::new(input.chars().collect::<Vec<_>>().as_slice()).collect::<Vec<_>>();
        let expr = Parser::new(tokens).parse().unwrap();
        let res = interpret(expr);
        println!("{res}");
        buffer.clear();
    }
}
