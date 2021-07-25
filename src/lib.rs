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
    statements: Vec<Statement<'a>>,
}

impl<'a> TurtleDocument<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(
            many1(alt((
                map(Statement::parse, Some),
                map(multispace1, |_| None),
            ))),
            |maybe_statements| Self {
                statements: maybe_statements.into_iter().filter_map(|n| n).collect(),
            },
        )(input)
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
    fn parse_document() {
        assert_eq!(
            Ok((
                "",
                TurtleDocument {
                    statements: vec![
                        Statement::Directive(Directive::Base(BaseDirective {
                            iri: IRIReference {
                                iri: Cow::Borrowed("http://example.com/ontology")
                            }
                        })),
                        Statement::Directive(Directive::Prefix(PrefixDirective {
                            prefix: None,
                            iri: IRIReference {
                                iri: Cow::Borrowed("http://example.com/ontology")
                            }
                        })),
                        Statement::Directive(Directive::Prefix(PrefixDirective {
                            prefix: Some(Cow::Borrowed("owl")),
                            iri: IRIReference {
                                iri: Cow::Borrowed("http://example.com/ontology")
                            }
                        }))
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
}
