use harriet::{Directive, Statement, TurtleDocument};
use nom::error::VerboseError;

fn parse_example_file(file_name: &str) {
    let ontology =
        std::fs::read_to_string(&format!("./tests/reference_examples/{}", file_name)).unwrap();
    assert!(dbg!(TurtleDocument::parse::<VerboseError<&str>>(&ontology))
        .unwrap()
        .0
        .is_empty());
}

fn parse_wildtype_file(file_name: &str) {
    let ontology =
        std::fs::read_to_string(&format!("./tests/wildtype_examples/{}", file_name)).unwrap();
    assert!(dbg!(TurtleDocument::parse::<VerboseError<&str>>(&ontology))
        .unwrap()
        .0
        .is_empty());
}

#[test]
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
fn example9() {
    parse_example_file("example9.ttl");
}

#[test]
fn example10() {
    parse_example_file("example10.ttl");
}

#[test]
fn example11() {
    parse_example_file("example11.ttl");
}

#[test]
// Number literals
fn example12() {
    parse_example_file("example12.ttl");
}

#[test]
fn example13() {
    parse_example_file("example13.ttl");
}

#[test]
// Blank node
fn example14() {
    parse_example_file("example14.ttl");
}

#[test]
fn example15() {
    parse_example_file("example15.ttl");
}

#[test]
fn example16() {
    parse_example_file("example16.ttl");
}

#[test]
fn example17() {
    parse_example_file("example17.ttl");
}

#[test]
#[ignore]
// TODO: empty collection
fn example18() {
    parse_example_file("example18.ttl");
}

#[test]
fn example19() {
    parse_example_file("example19.ttl");
}

#[test]
fn example20() {
    parse_example_file("example20.ttl");
}

#[test]
fn example21() {
    parse_example_file("example21.ttl");
}

#[test]
// Multiline string in a single line via \n escape sequence
fn example22() {
    parse_example_file("example22.ttl");
}

#[test]
fn example23() {
    parse_example_file("example23.ttl");
}

#[test]
// Blank nodes + numbers
fn example24() {
    parse_example_file("example24.ttl");
}

#[test]
#[ignore]
// TODO: nested parens
fn example25() {
    parse_example_file("example25.ttl");
}

#[test]
// Blank nodes + numbers
fn example26() {
    parse_example_file("example26.ttl");
}

#[test]
// Variant of reference example1 where "a" is replaced with "rdfs:type"
fn example1_without_a() {
    parse_wildtype_file("example1_without_a.ttl");
}

#[test]
fn example12_only_integer() {
    parse_wildtype_file("example12_only_integer.ttl");
}

#[test]
fn example12_only_decimal() {
    parse_wildtype_file("example12_only_decimal.ttl");
}

#[test]
fn example12_only_double() {
    parse_wildtype_file("example12_only_double.ttl");
}

#[test]
// Trimmed down example of nested blankNodePropertyList
fn example_nested_lists() {
    parse_wildtype_file("nested_lists.ttl");
}

#[test]
// Slightly more expanded example of nested blankNodePropertyList
fn example_nested_lists2() {
    parse_wildtype_file("nested_lists2.ttl");
}

#[test]
fn example24_simple1() {
    parse_wildtype_file("example24_simple1.ttl");
}

#[test]
fn example24_simple2() {
    parse_wildtype_file("example24_simple2.ttl");
}

#[test]
fn wildtype_rdf_ontology() {
    parse_wildtype_file("rdf.ttl");
}

#[test]
fn wildtype_rdfs_ontology() {
    parse_wildtype_file("rdfs.ttl");
}

#[test]
fn wildtype_owl_ontology() {
    parse_wildtype_file("owl.ttl");
}

#[test]
fn wildtype_dublin_core_elements_ontology() {
    parse_wildtype_file("owl.ttl");
}

#[test]
fn wildtype_bibo_ontology() {
    parse_wildtype_file("bibo.ttl");
}

#[test]
fn leading_whitespace_base() {
    let file_name = "leading_whitespace_base.ttl";
    let ontology =
        std::fs::read_to_string(&format!("./tests/wildtype_examples/{}", file_name)).unwrap();
    let document = TurtleDocument::parse::<VerboseError<&str>>(&ontology)
        .unwrap()
        .1;
    let mut count = 0;
    for statement in document.statements {
        if let Statement::Directive(Directive::Base(_)) = statement {
            count += 1;
        }
    }
    assert_eq!(1, count);
}
