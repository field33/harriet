use harriet::TurtleDocument;
use nom::error::VerboseError;

fn parse_example_file(file_name: &str) {
    let ontology =
        std::fs::read_to_string(&format!("./tests/reference_examples/{}", file_name)).unwrap();
    assert!(dbg!(TurtleDocument::parse::<VerboseError<&str>>(&ontology))
        .unwrap()
        .0
        .is_empty());
}
#[test]
#[ignore]
// TODO: There is a comment in the middle of a statement (build custom parser for whitespace, and start treating comments as whitespace)
fn example1() {
    parse_example_file("example1.ttl");
}

#[test]
fn example2() {
    parse_example_file("example2.ttl");
}

#[test]
fn example3() {
    parse_example_file("example3.ttl");
}

#[test]
fn example4() {
    parse_example_file("example4.ttl");
}

#[test]
fn example5() {
    parse_example_file("example5.ttl");
}

#[test]
fn example6() {
    parse_example_file("example6.ttl");
}

#[test]
fn example7() {
    parse_example_file("example7.ttl");
}

#[test]
fn example8() {
    parse_example_file("example8.ttl");
}

#[test]
#[ignore]
// Doesn't handle special case of verb `a` as a replacement for `rdfs:type`
fn example9() {
    parse_example_file("example9.ttl");
}

#[test]
fn example10() {
    parse_example_file("example10.ttl");
}

#[test]
#[ignore]
fn example11() {
    parse_example_file("example11.ttl");
}

#[test]
#[ignore]
fn example12() {
    parse_example_file("example12.ttl");
}

#[test]
fn example13() {
    parse_example_file("example13.ttl");
}

#[test]
fn example14() {
    parse_example_file("example14.ttl");
}

#[test]
#[ignore]
fn example15() {
    parse_example_file("example15.ttl");
}

#[test]
#[ignore]
fn example16() {
    parse_example_file("example16.ttl");
}

#[test]
fn example17() {
    parse_example_file("example17.ttl");
}

#[test]
#[ignore]
fn example18() {
    parse_example_file("example18.ttl");
}

#[test]
fn example19() {
    parse_example_file("example19.ttl");
}
