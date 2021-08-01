use harriet::TurtleDocument;
use nom::error::VerboseError;

pub fn main() {
    let ontology = std::fs::read_to_string(
        &std::env::args()
            .nth(1)
            .expect("Expected path to .ttl file as first argument"),
    )
    .unwrap();
    dbg!(TurtleDocument::parse::<VerboseError<&str>>(&ontology)).unwrap();
}
