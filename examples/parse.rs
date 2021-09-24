use harriet::{ParseError, TurtleDocument};

pub fn main() {
    let ontology = std::fs::read_to_string(
        &std::env::args()
            .nth(1)
            .expect("Expected path to .ttl file as first argument"),
    )
    .unwrap();
    let result = TurtleDocument::parse_full(&ontology);
    if let Err(ParseError::NotFullyParsed(remainder)) = result {
        dbg!(remainder);
        println!("=======================================");
        println!("================= WARNING =============");
        println!("=======================================");
        println!("=== file has not been parsed to end ===");
        println!("=======================================");
        std::process::exit(1);
    }
    dbg!(result);
}
