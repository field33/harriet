use harriet::TurtleDocument;
use nom::error::VerboseError;
use pretty_assertions::{assert_eq};

fn roundtrip_example_file(file_name: &str) {
    let input_ontology =
        std::fs::read_to_string(&format!("./tests/reference_examples/{}", file_name)).unwrap();

    let parsed = TurtleDocument::parse::<VerboseError<&str>>(&input_ontology).unwrap().1;

    let mut mem: [u8; 10024] = [0; 10024];
    let buf = &mut mem[..];
    let (_, written_bytes) = cookie_factory::gen(
        TurtleDocument::gen(&parsed), buf
    )
        .unwrap();
    let rendered_ontology = std::str::from_utf8(&mem[..written_bytes as usize]).unwrap();

    assert_eq!(input_ontology, rendered_ontology);
}

fn roundtrip_wildtype_file(file_name: &str) {
    let input_ontology =
        std::fs::read_to_string(&format!("./tests/wildtype_examples/{}", file_name)).unwrap();

    let parsed = TurtleDocument::parse::<VerboseError<&str>>(&input_ontology).unwrap().1;

    let mut mem: [u8; 10024] = [0; 10024];
    let buf = &mut mem[..];
    let (_, written_bytes) = cookie_factory::gen(
        TurtleDocument::gen(&parsed), buf
    )
        .unwrap();
    let rendered_ontology = std::str::from_utf8(&mem[..written_bytes as usize]).unwrap();

    assert_eq!(input_ontology, rendered_ontology);
}

#[test]
fn example1() {
    roundtrip_example_file("example1.ttl");
}

#[test]
fn example2() {
    roundtrip_example_file("example2.ttl");
}

#[test]
fn example3() {
    roundtrip_example_file("example3.ttl");
}

#[test]
fn example4() {
    roundtrip_example_file("example4.ttl");
}

#[test]
fn example5() {
    roundtrip_example_file("example5.ttl");
}

#[test]
fn example6() {
    roundtrip_example_file("example6.ttl");
}

#[test]
fn example7() {
    roundtrip_example_file("example7.ttl");
}

#[test]
fn example8() {
    roundtrip_example_file("example8.ttl");
}

#[test]
fn example9() {
    roundtrip_example_file("example9.ttl");
}

#[test]
fn example10() {
    roundtrip_example_file("example10.ttl");
}

#[test]
fn example11() {
    roundtrip_example_file("example11.ttl");
}

#[test]
// Number literals
fn example12() {
    roundtrip_example_file("example12.ttl");
}

#[test]
fn example13() {
    roundtrip_example_file("example13.ttl");
}

#[test]
fn example14() {
    roundtrip_example_file("example14.ttl");
}

#[test]
#[ignore]
fn example15() {
    roundtrip_example_file("example15.ttl");
}

#[test]
fn example16() {
    roundtrip_example_file("example16.ttl");
}

#[test]
fn example17() {
    roundtrip_example_file("example17.ttl");
}

#[test]
#[ignore]
fn example18() {
    roundtrip_example_file("example18.ttl");
}

#[test]
fn example19() {
    roundtrip_example_file("example19.ttl");
}

#[test]
#[ignore]
fn example20() {
    roundtrip_example_file("example20.ttl");
}

#[test]
fn example21() {
    roundtrip_example_file("example21.ttl");
}

#[test]
// Multiline string in a single line via \n escape sequence
fn example22() {
    roundtrip_example_file("example22.ttl");
}

#[test]
fn example23() {
    roundtrip_example_file("example23.ttl");
}

#[test]
fn example24() {
    roundtrip_example_file("example24.ttl");
}

#[test]
#[ignore]
fn example25() {
    roundtrip_example_file("example25.ttl");
}

#[test]
fn example26() {
    roundtrip_example_file("example26.ttl");
}

#[test]
// Variant of reference example1 where "a" is replaced with "rdfs:type"
fn example1_without_a() {
    roundtrip_wildtype_file("example1_without_a.ttl");
}

#[test]
fn example12_only_integer() {
    roundtrip_wildtype_file("example12_only_integer.ttl");
}

#[test]
fn example12_only_decimal() {
    roundtrip_wildtype_file("example12_only_decimal.ttl");
}

#[test]
fn example12_only_double() {
    roundtrip_wildtype_file("example12_only_double.ttl");
}

#[test]
// Trimmed down example of nested blankNodePropertyList
fn example_nested_lists() {
    roundtrip_wildtype_file("nested_lists.ttl");
}

#[test]
#[ignore]
// Slightly more expanded example of nested blankNodePropertyList
fn example_nested_lists2() {
    roundtrip_wildtype_file("nested_lists2.ttl");
}

#[test]
fn example24_simple1() {
    roundtrip_wildtype_file("example24_simple1.ttl");
}

#[test]
fn example24_simple2() {
    roundtrip_wildtype_file("example24_simple2.ttl");
}
