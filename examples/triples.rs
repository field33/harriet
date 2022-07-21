use harriet::TurtleDocument;
use harriet::triple_production::TripleProducer;

pub fn main() {
    let ontology = std::fs::read_to_string(
        &std::env::args()
            .nth(1)
            .expect("Expected path to .ttl file as first argument"),
    )
    .unwrap();
    let document = TurtleDocument::parse_full(&ontology).unwrap();
    dbg!(TripleProducer::produce_for_document(&document));
}
