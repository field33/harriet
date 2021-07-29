use harriet::TurtleDocument;
use nom::error::VerboseError;

fn parse_example_file(file_name: &str) {
    let ontology =
        std::fs::read_to_string(&format!("./tests/reference_examples/{}", file_name)).unwrap();
    assert!(TurtleDocument::parse::<VerboseError<&str>>(&ontology)
        .unwrap()
        .0
        .is_empty());
}
#[test]
#[ignore]
fn example1() {
    parse_example_file("example1.ttl");
}

#[test]
fn example2() {
    parse_example_file("example2.ttl");
}

#[test]
#[ignore]
fn example3() {
    parse_example_file("example3.ttl");
}

#[test]
fn example4() {
    parse_example_file("example4.ttl");
}

#[test]
#[ignore]
fn example5() {
    parse_example_file("example5.ttl");
}

#[test]
#[ignore]
fn example6() {
    parse_example_file("example6.ttl");
}

#[test]
fn example7() {
    parse_example_file("example7.ttl");
}

#[test]
#[ignore]
fn example8() {
    parse_example_file("example8.ttl");
}

#[test]
#[ignore]
fn example9() {
    parse_example_file("example9.ttl");
}

#[test]
#[ignore]
fn example10() {
    parse_example_file("example10.ttl");
}
