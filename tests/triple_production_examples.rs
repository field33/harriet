use harriet::TurtleDocument;
use nom::error::VerboseError;
use harriet::triple_production::TripleProducer;

fn triples_example_file(file_name: &str) {
    let ontology =
        std::fs::read_to_string(&format!("./tests/reference_examples/{}", file_name)).unwrap();
    let document = TurtleDocument::parse_full(&ontology).unwrap();
    assert!(dbg!(TripleProducer::produce_for_document(&document)).is_ok());
}

fn triples_wildtype_file(file_name: &str) {
    let ontology =
        std::fs::read_to_string(&format!("./tests/wildtype_examples/{}", file_name)).unwrap();
    let document = TurtleDocument::parse_full(&ontology).unwrap();
    assert!(dbg!(TripleProducer::produce_for_document(&document)).is_ok());
}

#[test]
fn example1() {
    triples_example_file("example1.ttl");
}

#[test]
fn example2() {
    triples_example_file("example2.ttl");
}

#[test]
fn example3() {
    triples_example_file("example3.ttl");
}

#[test]
fn example4() {
    triples_example_file("example4.ttl");
}

#[test]
fn example5() {
    triples_example_file("example5.ttl");
}

#[test]
fn example6() {
    triples_example_file("example6.ttl");
}

#[test]
fn example7() {
    triples_example_file("example7.ttl");
}

#[test]
fn example8() {
    triples_example_file("example8.ttl");
}

#[test]
fn example9() {
    triples_example_file("example9.ttl");
}

#[test]
fn example10() {
    triples_example_file("example10.ttl");
}

#[test]
fn example11() {
    triples_example_file("example11.ttl");
}

#[test]
#[ignore]
// Number literals
fn example12() {
    triples_example_file("example12.ttl");
}

#[test]
#[ignore]
fn example13() {
    triples_example_file("example13.ttl");
}

#[test]
// Blank node
fn example14() {
    triples_example_file("example14.ttl");
}

#[test]
#[ignore]
fn example15() {
    triples_example_file("example15.ttl");
}

#[test]
#[ignore]
fn example16() {
    triples_example_file("example16.ttl");
}

#[test]
fn example17() {
    triples_example_file("example17.ttl");
}

#[test]
#[ignore]
fn example18() {
    triples_example_file("example18.ttl");
}

#[test]
#[ignore]
fn example19() {
    triples_example_file("example19.ttl");
}

#[test]
fn example20() {
    triples_example_file("example20.ttl");
}

#[test]
fn example21() {
    triples_example_file("example21.ttl");
}

#[test]
#[ignore]
// Multiline string in a single line via \n escape sequence
fn example22() {
    triples_example_file("example22.ttl");
}

#[test]
#[ignore]
fn example23() {
    triples_example_file("example23.ttl");
}

#[test]
#[ignore]
// Blank nodes + numbers
fn example24() {
    triples_example_file("example24.ttl");
}

#[test]
#[ignore]
fn example25() {
    triples_example_file("example25.ttl");
}

#[test]
#[ignore]
// Blank nodes + numbers
fn example26() {
    triples_example_file("example26.ttl");
}

#[test]
// Variant of reference example1 where "a" is replaced with "rdfs:type"
fn example1_without_a() {
    triples_wildtype_file("example1_without_a.ttl");
}

#[test]
#[ignore]
// Trimmed down example of nested blankNodePropertyList
fn example_nested_lists() {
    triples_wildtype_file("nested_lists.ttl");
}

#[test]
#[ignore]
// Slightly more expanded example of nested blankNodePropertyList
fn example_nested_lists2() {
    triples_wildtype_file("nested_lists2.ttl");
}

#[test]
fn example24_simple1() {
    triples_wildtype_file("example24_simple1.ttl");
}

#[test]
fn example24_simple2() {
    triples_wildtype_file("example24_simple2.ttl");
}
