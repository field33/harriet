use cookie_factory::GenError;
use cookie_factory::{do_gen, gen_slice};
use nom::alt;
use nom::branch::alt;
use nom::bytes::complete::{is_not, tag};
use nom::character::complete::{char, multispace1};
use nom::combinator::{map, map_res, opt};
use nom::error::{FromExternalError, ParseError};
use nom::multi::many1;
use nom::sequence::{delimited, tuple};
use nom::IResult;
use std::borrow::Cow;

#[derive(Debug, PartialEq, Eq)]
pub struct TurtleDocument<'a> {
    items: Vec<Item<'a>>,
}

impl<'a> TurtleDocument<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(
            many1(alt((map(Item::parse, Some), map(multispace1, |_| None)))),
            |maybe_statements| Self {
                items: maybe_statements.into_iter().filter_map(|n| n).collect(),
            },
        )(input)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Item<'a> {
    Statement(Statement<'a>),
    Comment(Comment<'a>),
}

impl<'a> Item<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        alt((
            map(Comment::parse, Item::Comment),
            map(Statement::parse, Item::Statement),
        ))(input)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Statement<'a> {
    Directive(Directive<'a>),
    // TODO: Triples
}

impl<'a> Statement<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(Directive::parse, Self::Directive)(input)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Comment<'a> {
    comment: Cow<'a, str>,
}

impl<'a> Comment<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map_res(Self::parse_raw, |comment_raw: &'a str| {
            let comment = Cow::Borrowed(comment_raw);

            Ok(Self { comment })
        })(input)
    }

    fn parse_raw<E>(input: &'a str) -> IResult<&'a str, &str, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        delimited(char('#'), is_not("\n"), char('\n'))(input)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Triples<'a> {
    Labeled(Subject<'a>, PredicateObjectList<'a>),
    // Labeled()
    // Labeled()
}

impl<'a> Triples<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(
            tuple((
                map_res(
                    tuple((Subject::parse, multispace1, PredicateObjectList::parse)),
                    |(subject, _, list)| Ok(Self::Labeled(subject, list)),
                ),
                multispace1,
                char('.'),
            )),
            |(triples, _, _)| triples,
        )(input)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Subject<'a> {
    IRI(IRI<'a>),
    // TOOD: BlankNode
    // TOOD: Collection
}

impl<'a> Subject<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(IRI::parse, Self::IRI)(input)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum IRI<'a> {
    IRIReference(IRIReference<'a>),
    // TODO: PrefixedName
}

impl<'a> IRI<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(IRIReference::parse, Self::IRIReference)(input)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct PredicateObjectList<'a> {
    // TODO: should be "Verb" - Enum between IRI and literal "a"
    verb: IRI<'a>,
    objectList: ObjectList<'a>,
}

impl<'a> PredicateObjectList<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(
            tuple((IRI::parse, multispace1, ObjectList::parse)),
            |(verb, _, list)| Self {
                verb,
                objectList: list,
            },
        )(input)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ObjectList<'a> {
    list: Vec<Object<'a>>,
}

impl<'a> ObjectList<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(
            many1(alt((
                // First item
                map(Object::parse, Some),
                // Subsequent items delimited by whitespace and ','
                map(
                    tuple((multispace1, char(','), multispace1, Object::parse)),
                    |(_, _, _, object)| Some(object),
                ),
            ))),
            |maybe_items| Self {
                list: maybe_items.into_iter().filter_map(|n| n).collect(),
            },
        )(input)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Object<'a> {
    IRI(IRI<'a>), // TODO: more variants
}

impl<'a> Object<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(IRI::parse, Self::IRI)(input)
    }
}

#[derive(Debug, PartialEq, Eq)]
/// One of multiple kinds of directives.
///
/// Parsing reference: <https://www.w3.org/TR/turtle/#grammar-production-directive>
pub enum Directive<'a> {
    Base(BaseDirective<'a>),
    // TODO: Base SPARQL
    Prefix(PrefixDirective<'a>),
    // TODO: Prefix SPARQL
}

impl<'a> Directive<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        alt((
            map(BaseDirective::parse, Self::Base),
            map(PrefixDirective::parse, Self::Prefix),
        ))(input)
    }
}

/// A directive specifying the base for relative IRIs. E.g. `@base <http://example.com> .`
///
/// Parsing reference: <https://www.w3.org/TR/turtle/#grammar-production-base>
#[derive(Debug, Eq, PartialEq)]
pub struct BaseDirective<'a> {
    iri: IRIReference<'a>,
}

impl<'a> BaseDirective<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map_res(Self::parse_raw, |iri_ref| Ok(Self { iri: iri_ref }))(input)
    }

    fn parse_raw<E>(input: &'a str) -> IResult<&'a str, IRIReference, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        tuple((
            tag("@base"),
            multispace1,
            IRIReference::parse,
            multispace1,
            char('.'),
        ))(input)
        .map(|(remainder, (_, _, iri, _, _))| (remainder, iri))
    }
}

/// A directive specifying the base for relative prefixed IRIs. E.g. `@prefix owl: <http://www.w3.org/2002/07/owl#> .`
///
/// Parsing reference: <https://www.w3.org/TR/turtle/#grammar-production-prefixID>
#[derive(Debug, Eq, PartialEq)]
pub struct PrefixDirective<'a> {
    prefix: Option<Cow<'a, str>>,
    iri: IRIReference<'a>,
}

impl<'a> PrefixDirective<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map_res(Self::parse_raw, |(prefix, iri_ref)| {
            Ok(Self {
                prefix: prefix.map(|n| Cow::Borrowed(n)),
                iri: iri_ref,
            })
        })(input)
    }

    fn parse_raw<E>(input: &'a str) -> IResult<&'a str, (Option<&'a str>, IRIReference), E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        tuple((
            tag("@prefix"),
            multispace1,
            opt(is_not(":")),
            char(':'),
            multispace1,
            IRIReference::parse,
            multispace1,
            char('.'),
        ))(input)
        .map(|(remainder, (_, _, prefix, _, _, iri, _, _))| (remainder, (prefix, iri)))
    }
}

/// A IRI wrapped by angle brackets. E.g. `<http://example.com/foo>`
///
/// Grammar reference: <https://www.w3.org/TR/turtle/#sec-iri-references>
///
/// Parsing reference: <https://www.w3.org/TR/turtle/#grammar-production-IRIREF>
#[derive(Debug, Eq, PartialEq)]
pub struct IRIReference<'a> {
    iri: Cow<'a, str>,
}

impl<'a> IRIReference<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map_res(Self::parse_raw, |iri: &'a str| {
            let iri_utf8 = Cow::Borrowed(iri);

            Ok(Self { iri: iri_utf8 })
        })(input)
    }

    fn parse_raw<E>(input: &'a str) -> IResult<&'a str, &'a str, E>
    where
        E: ParseError<&'a str>,
    {
        delimited(char('<'), is_not(">"), char('>'))(input)
    }

    fn render(
        i: (&'a mut [u8], usize),
        iri_ref: &'a Self,
    ) -> Result<(&'a mut [u8], usize), GenError> {
        do_gen!(
            (i.0, i.1),
            gen_slice!(b"<") >> gen_slice!(iri_ref.iri.as_bytes()) >> gen_slice!(b">")
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use cookie_factory::gen_simple;
    use nom::error::VerboseError;
    use std::io::Cursor;

    #[test]
    fn parse_iri_reference() {
        assert_eq!(
            Ok((
                "",
                IRIReference {
                    iri: Cow::Borrowed("http://example.com/ontology")
                }
            )),
            IRIReference::parse::<VerboseError<&str>>("<http://example.com/ontology>")
        );
        assert!(IRIReference::parse::<VerboseError<&str>>("<http://example.com/ontology").is_err());
    }

    #[test]
    fn render_iri_reference() {
        let mut mem: [u8; 1024] = [0; 1024];
        let buf = &mut mem[..];
        let (_, written_bytes) = IRIReference::render(
            (buf, 0),
            &IRIReference {
                iri: Cow::Borrowed("http://example.com/ontology"),
            },
        )
        .unwrap();
        assert_eq!(
            "<http://example.com/ontology>".as_bytes(),
            &mem[..written_bytes]
        );
    }

    #[test]
    fn parse_base_directive() {
        assert_eq!(
            Ok((
                "",
                BaseDirective {
                    iri: IRIReference {
                        iri: Cow::Borrowed("http://example.com/ontology")
                    }
                }
            )),
            BaseDirective::parse::<VerboseError<&str>>("@base <http://example.com/ontology> .")
        );
        // Some whitespace
        assert_eq!(
            Ok((
                "",
                BaseDirective {
                    iri: IRIReference {
                        iri: Cow::Borrowed("http://example.com/ontology")
                    }
                }
            )),
            BaseDirective::parse::<VerboseError<&str>>(
                "@base  \n <http://example.com/ontology>    ."
            )
        );
    }

    #[test]
    fn parse_prefix_directive() {
        assert_eq!(
            Ok((
                "",
                PrefixDirective {
                    prefix: Some(Cow::Borrowed("owl")),
                    iri: IRIReference {
                        iri: Cow::Borrowed("http://example.com/ontology")
                    }
                }
            )),
            PrefixDirective::parse::<VerboseError<&str>>(
                "@prefix owl: <http://example.com/ontology> ."
            )
        );
        assert_eq!(
            Ok((
                "",
                PrefixDirective {
                    prefix: None,
                    iri: IRIReference {
                        iri: Cow::Borrowed("http://example.com/ontology")
                    }
                }
            )),
            PrefixDirective::parse::<VerboseError<&str>>(
                "@prefix : <http://example.com/ontology> ."
            )
        );
    }

    #[test]
    fn parse_directive() {
        assert_eq!(
            Ok((
                "",
                Directive::Base(BaseDirective {
                    iri: IRIReference {
                        iri: Cow::Borrowed("http://example.com/ontology")
                    }
                })
            )),
            Directive::parse::<VerboseError<&str>>("@base <http://example.com/ontology> .")
        );
        assert_eq!(
            Ok((
                "",
                Directive::Prefix(PrefixDirective {
                    prefix: Some(Cow::Borrowed("owl")),
                    iri: IRIReference {
                        iri: Cow::Borrowed("http://example.com/ontology")
                    }
                })
            )),
            Directive::parse::<VerboseError<&str>>("@prefix owl: <http://example.com/ontology> .")
        );
    }

    #[test]
    fn parse_simple_triple() {
        assert_eq!(
            Ok((
                "",
                Triples::Labeled(
                    Subject::IRI(IRI::IRIReference(IRIReference {
                        iri: Cow::Borrowed("http://example.org/#spiderman")
                    })),
                    PredicateObjectList {
                        verb: IRI::IRIReference(IRIReference {
                            iri: Cow::Borrowed(
                                "http://www.perceive.net/schemas/relationship/enemyOf"
                            )
                        }),
                        objectList: ObjectList {
                            list: vec![Object::IRI(IRI::IRIReference(IRIReference {
                                iri: Cow::Borrowed("http://example.org/#green-goblin")
                            }))]
                        }
                    }
                )
            )),
            Triples::parse::<VerboseError<&str>>(include_str!(
                "../tests/reference_examples/example2.ttl"
            ))
        );
    }

    #[test]
    fn parse_document() {
        assert_eq!(
            Ok((
                "",
                TurtleDocument {
                    items: vec![
                        Item::Statement(Statement::Directive(Directive::Base(BaseDirective {
                            iri: IRIReference {
                                iri: Cow::Borrowed("http://example.com/ontology")
                            }
                        }))),
                        Item::Statement(Statement::Directive(Directive::Prefix(PrefixDirective {
                            prefix: None,
                            iri: IRIReference {
                                iri: Cow::Borrowed("http://example.com/ontology")
                            }
                        }))),
                        Item::Statement(Statement::Directive(Directive::Prefix(PrefixDirective {
                            prefix: Some(Cow::Borrowed("owl")),
                            iri: IRIReference {
                                iri: Cow::Borrowed("http://example.com/ontology")
                            }
                        })))
                    ]
                }
            )),
            TurtleDocument::parse::<VerboseError<&str>>(
                r#"
                @base <http://example.com/ontology> .
                
                @prefix : <http://example.com/ontology> .
                @prefix owl: <http://example.com/ontology> .
            "#
            )
        );
    }

    #[test]
    #[ignore]
    fn parse_document2() {
        let ontology = r#"
                @base <http://example.com/ontology> .
                
                @prefix : <http://example.com/ontology> .
                @prefix owl: <http://example.com/ontology> .
                
                <http://example.com/ontology> rdf:type owl:Ontology .
            "#;
        assert!(TurtleDocument::parse::<VerboseError<&str>>(ontology).is_ok());
        assert!(TurtleDocument::parse::<VerboseError<&str>>(ontology)
            .unwrap()
            .0
            .is_empty());
    }

    #[test]
    fn parse_document_with_comment() {
        let ontology = r#"
                # The base here
                @base <http://example.com/ontology> .
                # And some prefixes here
                @prefix : <http://example.com/ontology> .
                @prefix owl: <http://example.com/ontology> .
            "#;
        assert!(TurtleDocument::parse::<VerboseError<&str>>(ontology).is_ok());
        assert!(TurtleDocument::parse::<VerboseError<&str>>(ontology)
            .unwrap()
            .0
            .is_empty());
    }
}
