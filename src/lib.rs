#![allow(dead_code)]

use cookie_factory::combinator::string as cf_string;
use cookie_factory::lib::std::io::Write;
use cookie_factory::multi::separated_list as cf_separated_list;
use cookie_factory::sequence::tuple as cf_tuple;
use cookie_factory::SerializeFn;
use nom::branch::alt;
use nom::bytes::complete::{is_not, tag};
use nom::character::complete::{alphanumeric1, char, multispace0, multispace1};
use nom::combinator::{map, opt};
use nom::error::{FromExternalError, ParseError};
use nom::multi::{many1, separated_list1};
use nom::sequence::{delimited, tuple};
use nom::IResult;
use std::borrow::Cow;

#[derive(Debug, PartialEq, Eq)]
pub struct TurtleDocument<'a> {
    pub items: Vec<Item<'a>>,
}

impl<'a> TurtleDocument<'a> {
    pub fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
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

    pub fn gen<W: Write + 'a>(subject: &'a Self) -> impl SerializeFn<W> + 'a {
        cf_separated_list(cf_string("\n"), subject.items.iter().map(Item::gen))
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

    fn gen<W: Write + 'a>(subject: &'a Self) -> Box<dyn SerializeFn<W> + 'a> {
        match subject {
            Self::Statement(statement) => Box::new(Statement::gen(statement)),
            Self::Comment(comment) => Box::new(Comment::gen(comment)),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Statement<'a> {
    Directive(Directive<'a>),
    Triples(Triples<'a>),
}

impl<'a> Statement<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        alt((
            map(Directive::parse, Self::Directive),
            map(Triples::parse, Self::Triples),
        ))(input)
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> Box<dyn SerializeFn<W> + 'a> {
        match subject {
            Self::Directive(directive) => Box::new(Directive::gen(directive)),
            Self::Triples(triples) => Box::new(Triples::gen(triples)),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Comment<'a> {
    pub comment: Cow<'a, str>,
}

impl<'a> Comment<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(Self::parse_raw, |comment_raw: &'a str| {
            let comment = Cow::Borrowed(comment_raw);

            Self { comment }
        })(input)
    }

    fn parse_raw<E>(input: &'a str) -> IResult<&'a str, &str, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        delimited(char('#'), is_not("\n"), char('\n'))(input)
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> impl SerializeFn<W> + 'a {
        cf_tuple((cf_string("#"), cf_string(&subject.comment), cf_string("\n")))
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
                map(
                    tuple((Subject::parse, multispace1, PredicateObjectList::parse)),
                    |(subject, _, list)| Self::Labeled(subject, list),
                ),
                multispace1,
                char('.'),
            )),
            |(triples, _, _)| triples,
        )(input)
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> Box<dyn SerializeFn<W> + 'a> {
        match subject {
            Self::Labeled(subject, predicate_object_list) => Box::new(cf_tuple((
                Subject::gen(subject),
                cf_string(" "),
                PredicateObjectList::gen(predicate_object_list),
                cf_string(" ."),
            ))),
            #[allow(unreachable_patterns)]
            _ => todo!(),
        }
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

    fn gen<W: Write + 'a>(subject: &'a Self) -> impl SerializeFn<W> + 'a {
        match subject {
            Self::IRI(iri) => IRI::gen(iri),
            #[allow(unreachable_patterns)]
            _ => todo!(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum IRI<'a> {
    IRIReference(IRIReference<'a>),
    PrefixedName(PrefixedName<'a>),
}

impl<'a> IRI<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        alt((
            map(IRIReference::parse, Self::IRIReference),
            map(PrefixedName::parse, Self::PrefixedName),
        ))(input)
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> Box<dyn SerializeFn<W> + 'a> {
        match subject {
            Self::IRIReference(iri) => Box::new(IRIReference::gen(iri)),
            Self::PrefixedName(prefixed_name) => Box::new(PrefixedName::gen(prefixed_name)),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct PredicateObjectList<'a> {
    // TODO: IRI should be "Verb" - Enum between IRI and literal "a"
    pub list: Vec<(IRI<'a>, ObjectList<'a>)>,
}

impl<'a> PredicateObjectList<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(
            separated_list1(
                tuple((multispace1, tag(";"), multispace1)),
                map(
                    tuple((IRI::parse, multispace1, ObjectList::parse)),
                    |(verb, _, list)| (verb, list),
                ),
            ),
            |list| Self { list },
        )(input)
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> impl SerializeFn<W> + 'a {
        cf_separated_list(
            cf_string(" ; "),
            subject.list.iter().map(|(verb, object_list)| {
                cf_tuple((IRI::gen(verb), cf_string(" "), ObjectList::gen(object_list)))
            }),
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ObjectList<'a> {
    pub list: Vec<Object<'a>>,
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
                    tuple((multispace0, char(','), multispace0, Object::parse)),
                    |(_, _, _, object)| Some(object),
                ),
            ))),
            |maybe_items| Self {
                list: maybe_items.into_iter().filter_map(|n| n).collect(),
            },
        )(input)
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> impl SerializeFn<W> + 'a {
        cf_separated_list(cf_string(" , "), subject.list.iter().map(Object::gen))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Object<'a> {
    IRI(IRI<'a>),
    // TODO: BlankNode
    // TODO: collection
    // TODO: blankNodePropertyList
    Literal(Literal<'a>),
}

impl<'a> Object<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        alt((
            map(IRI::parse, Self::IRI),
            map(Literal::parse, Self::Literal),
        ))(input)
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> Box<dyn SerializeFn<W> + 'a> {
        match subject {
            Self::IRI(iri) => Box::new(IRI::gen(iri)),
            Self::Literal(literal) => Box::new(Literal::gen(literal)),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
/// One of multiple kinds of directives.
///
/// Parsing reference: <https://www.w3.org/TR/turtle/#grammar-production-directive>
pub enum Directive<'a> {
    Base(BaseDirective<'a>),
    Prefix(PrefixDirective<'a>),
    SparqlBase(SparqlBaseDirective<'a>),
    SparqlPrefix(SparqlPrefixDirective<'a>),
}

impl<'a> Directive<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        alt((
            map(BaseDirective::parse, Self::Base),
            map(PrefixDirective::parse, Self::Prefix),
            map(SparqlBaseDirective::parse, Self::SparqlBase),
            map(SparqlPrefixDirective::parse, Self::SparqlPrefix),
        ))(input)
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> Box<dyn SerializeFn<W> + 'a> {
        match subject {
            Self::Base(directive) => Box::new(BaseDirective::gen(directive)),
            Self::Prefix(directive) => Box::new(PrefixDirective::gen(directive)),
            Self::SparqlBase(directive) => Box::new(SparqlBaseDirective::gen(directive)),
            Self::SparqlPrefix(directive) => Box::new(SparqlPrefixDirective::gen(directive)),
        }
    }
}

/// A directive specifying the base for relative IRIs. E.g. `@base <http://example.com> .`
///
/// Parsing reference: <https://www.w3.org/TR/turtle/#grammar-production-base>
#[derive(Debug, Eq, PartialEq)]
pub struct BaseDirective<'a> {
    pub iri: IRIReference<'a>,
}

impl<'a> BaseDirective<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(Self::parse_raw, |iri_ref| Self { iri: iri_ref })(input)
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

    fn gen<W: Write + 'a>(subject: &'a Self) -> impl SerializeFn<W> + 'a {
        cf_tuple((
            cf_string("@base"),
            cf_string(" "),
            IRIReference::gen(&subject.iri),
            cf_string(" "),
            cf_string("."),
        ))
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct SparqlBaseDirective<'a> {
    pub iri: IRIReference<'a>,
}

impl<'a> SparqlBaseDirective<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(Self::parse_raw, |iri_ref| Self { iri: iri_ref })(input)
    }

    fn parse_raw<E>(input: &'a str) -> IResult<&'a str, IRIReference, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        tuple((tag("BASE"), multispace1, IRIReference::parse))(input)
            .map(|(remainder, (_, _, iri))| (remainder, iri))
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> impl SerializeFn<W> + 'a {
        cf_tuple((
            cf_string("BASE"),
            cf_string(" "),
            IRIReference::gen(&subject.iri),
        ))
    }
}

/// A directive specifying the base for relative prefixed IRIs. E.g. `@prefix owl: <http://www.w3.org/2002/07/owl#> .`
///
/// Parsing reference: <https://www.w3.org/TR/turtle/#grammar-production-prefixID>
#[derive(Debug, Eq, PartialEq)]
pub struct PrefixDirective<'a> {
    pub prefix: Option<Cow<'a, str>>,
    pub iri: IRIReference<'a>,
}

impl<'a> PrefixDirective<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(Self::parse_raw, |(prefix, iri_ref)| Self {
            prefix: prefix.map(|n| Cow::Borrowed(n)),
            iri: iri_ref,
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

    fn gen<W: Write + 'a>(subject: &'a Self) -> impl SerializeFn<W> + 'a {
        cf_tuple((
            cf_string("@prefix"),
            cf_string(" "),
            gen_option_cow_str(&subject.prefix),
            cf_string(":"),
            cf_string(" "),
            IRIReference::gen(&subject.iri),
            cf_string(" "),
            cf_string("."),
        ))
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct SparqlPrefixDirective<'a> {
    pub prefix: Option<Cow<'a, str>>,
    pub iri: IRIReference<'a>,
}

impl<'a> SparqlPrefixDirective<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(Self::parse_raw, |(prefix, iri_ref)| Self {
            prefix: prefix.map(|n| Cow::Borrowed(n)),
            iri: iri_ref,
        })(input)
    }

    fn parse_raw<E>(input: &'a str) -> IResult<&'a str, (Option<&'a str>, IRIReference), E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        tuple((
            tag("PREFIX"),
            multispace1,
            opt(is_not(":")),
            char(':'),
            multispace1,
            IRIReference::parse,
        ))(input)
        .map(|(remainder, (_, _, prefix, _, _, iri))| (remainder, (prefix, iri)))
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> impl SerializeFn<W> + 'a {
        cf_tuple((
            cf_string("PREFIX"),
            cf_string(" "),
            gen_option_cow_str(&subject.prefix),
            cf_string(":"),
            cf_string(" "),
            IRIReference::gen(&subject.iri),
        ))
    }
}

/// A IRI wrapped by angle brackets. E.g. `<http://example.com/foo>`
///
/// Grammar reference: <https://www.w3.org/TR/turtle/#sec-iri-references>
///
/// Parsing reference: <https://www.w3.org/TR/turtle/#grammar-production-IRIREF>
#[derive(Debug, Eq, PartialEq)]
pub struct IRIReference<'a> {
    pub iri: Cow<'a, str>,
}

impl<'a> IRIReference<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(Self::parse_raw, |iri: &'a str| {
            let iri_utf8 = Cow::Borrowed(iri);

            Self { iri: iri_utf8 }
        })(input)
    }

    fn parse_raw<E>(input: &'a str) -> IResult<&'a str, &'a str, E>
    where
        E: ParseError<&'a str>,
    {
        delimited(char('<'), is_not(">"), char('>'))(input)
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> impl SerializeFn<W> + 'a {
        cf_tuple((cf_string("<"), cf_string(&subject.iri), cf_string(">")))
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct PrefixedName<'a> {
    pub prefix: Option<Cow<'a, str>>,
    pub name: Option<Cow<'a, str>>,
    // TODO: locale
}

impl<'a> PrefixedName<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(
            tuple((opt(is_not(" \t\r\n:")), char(':'), opt(is_not(" \t\r\n")))),
            |(prefix, _, name)| Self {
                prefix: prefix.map(Cow::Borrowed),
                name: name.map(Cow::Borrowed),
            },
        )(input)
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> impl SerializeFn<W> + 'a {
        cf_tuple((
            gen_option_cow_str(&subject.prefix),
            cf_string(":"),
            gen_option_cow_str(&subject.name),
        ))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Literal<'a> {
    RDFLiteral(RDFLiteral<'a>),
    // NumericLiteral(NumericLiteral<'a>),
    BooleanLiteral(BooleanLiteral),
}

impl<'a> Literal<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        alt((
            map(RDFLiteral::parse, Self::RDFLiteral),
            map(BooleanLiteral::parse, Self::BooleanLiteral),
        ))(input)
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> Box<dyn SerializeFn<W> + 'a> {
        match subject {
            Self::RDFLiteral(literal) => Box::new(RDFLiteral::gen(literal)),
            Self::BooleanLiteral(bool) => Box::new(BooleanLiteral::gen(bool)),
            #[allow(unreachable_patterns)]
            _ => todo!(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct RDFLiteral<'a> {
    pub string: TurtleString<'a>,
    pub language_tag: Option<Cow<'a, str>>,
    // TODO: language_tag or IRI (for datatype?)
}

impl<'a> RDFLiteral<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(
            tuple((
                TurtleString::parse,
                map(
                    opt(tuple((char('@'), alphanumeric1))),
                    |language_tag_tuple| {
                        language_tag_tuple.map(|(_, language_tag)| Cow::Borrowed(language_tag))
                    },
                ),
            )),
            |(string, language_tag)| Self {
                string,
                language_tag,
            },
        )(input)
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> impl SerializeFn<W> + 'a {
        TurtleString::gen(&subject.string)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct BooleanLiteral {
    pub bool: bool,
}

impl BooleanLiteral {
    fn parse<'a, E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        alt((
            map(tag("true"), |_| Self { bool: true }),
            map(tag("false"), |_| Self { bool: false }),
        ))(input)
    }

    fn gen<'a, W: Write + 'a>(subject: &'a Self) -> Box<dyn SerializeFn<W> + 'a> {
        match subject {
            Self { bool: true } => Box::new(cf_string("true")),
            Self { bool: false } => Box::new(cf_string("false")),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum TurtleString<'a> {
    StringLiteralQuote(StringLiteralQuote<'a>),
    // TODO: other quoting variants
}

impl<'a> TurtleString<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(StringLiteralQuote::parse, Self::StringLiteralQuote)(input)
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> Box<dyn SerializeFn<W> + 'a> {
        match subject {
            Self::StringLiteralQuote(string) => Box::new(StringLiteralQuote::gen(string)),
            #[allow(unreachable_patterns)]
            _ => todo!(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct StringLiteralQuote<'a> {
    pub string: Cow<'a, str>,
}

impl<'a> StringLiteralQuote<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(delimited(char('\"'), is_not("\""), char('\"')), |string| {
            Self {
                string: Cow::Borrowed(string),
            }
        })(input)
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> impl SerializeFn<W> + 'a {
        cf_tuple((cf_string("\""), cf_string(&subject.string), cf_string("\"")))
    }
}

fn gen_option_cow_str<'a, W: Write + 'a>(
    prefix: &'a Option<Cow<str>>,
) -> Box<dyn SerializeFn<W> + 'a> {
    match prefix {
        Some(prefix) => Box::new(cf_string(prefix)),
        None => Box::new(cf_string("")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::error::VerboseError;

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
        let (_, written_bytes) = cookie_factory::gen(
            IRIReference::gen(&IRIReference {
                iri: Cow::Borrowed("http://example.com/ontology"),
            }),
            buf,
        )
        .unwrap();
        assert_eq!(
            "<http://example.com/ontology>".as_bytes(),
            &mem[..written_bytes as usize]
        );
    }

    #[test]
    fn parse_prefixed_name() {
        assert_eq!(
            Ok((
                "",
                PrefixedName {
                    prefix: Some(Cow::Borrowed("owl")),
                    name: Some(Cow::Borrowed("Thing")),
                }
            )),
            PrefixedName::parse::<VerboseError<&str>>("owl:Thing")
        );
        assert_eq!(
            Ok((
                "",
                PrefixedName {
                    prefix: Some(Cow::Borrowed("owl")),
                    name: None,
                }
            )),
            PrefixedName::parse::<VerboseError<&str>>("owl:")
        );
        assert_eq!(
            Ok((
                "",
                PrefixedName {
                    prefix: None,
                    name: Some(Cow::Borrowed("Thing")),
                }
            )),
            PrefixedName::parse::<VerboseError<&str>>(":Thing")
        );
    }

    #[test]
    fn render_prefixed_name() {
        let mut mem: [u8; 1024] = [0; 1024];
        let buf = &mut mem[..];
        let (_, written_bytes) = cookie_factory::gen(
            PrefixedName::gen(&PrefixedName {
                prefix: Some(Cow::Borrowed("owl")),
                name: Some(Cow::Borrowed("Thing")),
            }),
            buf,
        )
        .unwrap();
        assert_eq!("owl:Thing".as_bytes(), &mem[..written_bytes as usize]);
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
                        list: vec![(
                            IRI::IRIReference(IRIReference {
                                iri: Cow::Borrowed(
                                    "http://www.perceive.net/schemas/relationship/enemyOf"
                                )
                            }),
                            ObjectList {
                                list: vec![Object::IRI(IRI::IRIReference(IRIReference {
                                    iri: Cow::Borrowed("http://example.org/#green-goblin")
                                }))]
                            }
                        )]
                    }
                )
            )),
            Triples::parse::<VerboseError<&str>>(include_str!(
                "../tests/reference_examples/example2.ttl"
            ))
        );
    }

    #[test]
    fn parse_string_literal_quote() {
        assert_eq!(
            Ok((
                "",
                StringLiteralQuote {
                    string: Cow::Borrowed("SomeString"),
                }
            )),
            StringLiteralQuote::parse::<VerboseError<&str>>("\"SomeString\"")
        );
    }

    #[test]
    fn parse_rdf_literal() {
        assert_eq!(
            Ok((
                "",
                RDFLiteral {
                    string: TurtleString::StringLiteralQuote(StringLiteralQuote {
                        string: Cow::Borrowed("SomeString"),
                    }),
                    language_tag: Some(Cow::Borrowed("en")),
                }
            )),
            RDFLiteral::parse::<VerboseError<&str>>("\"SomeString\"@en")
        );
    }

    #[test]
    fn render_triples() {
        let mut mem: [u8; 1024] = [0; 1024];
        let buf = &mut mem[..];
        let (_, written_bytes) = cookie_factory::gen(
            Triples::gen(&Triples::Labeled(
                Subject::IRI(IRI::IRIReference(IRIReference {
                    iri: Cow::Borrowed("http://example.com/"),
                })),
                PredicateObjectList {
                    list: vec![(
                        IRI::PrefixedName(PrefixedName {
                            prefix: Some(Cow::Borrowed("rdf")),
                            name: Some(Cow::Borrowed("type")),
                        }),
                        ObjectList {
                            list: vec![Object::IRI(IRI::PrefixedName(PrefixedName {
                                prefix: Some(Cow::Borrowed("owl")),
                                name: Some(Cow::Borrowed("Ontology")),
                            }))],
                        },
                    )],
                },
            )),
            buf,
        )
        .unwrap();
        assert_eq!(
            r#"<http://example.com/> rdf:type owl:Ontology ."#,
            std::str::from_utf8(&mem[..written_bytes as usize]).unwrap()
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
    fn render_document() {
        let mut mem: [u8; 1024] = [0; 1024];
        let buf = &mut mem[..];
        let (_, written_bytes) = cookie_factory::gen(
            TurtleDocument::gen(&TurtleDocument {
                items: vec![
                    Item::Statement(Statement::Directive(Directive::Base(BaseDirective {
                        iri: IRIReference {
                            iri: Cow::Borrowed("http://example.com/ontology"),
                        },
                    }))),
                    Item::Statement(Statement::Directive(Directive::Prefix(PrefixDirective {
                        prefix: None,
                        iri: IRIReference {
                            iri: Cow::Borrowed("http://example.com/ontology"),
                        },
                    }))),
                    Item::Statement(Statement::Directive(Directive::Prefix(PrefixDirective {
                        prefix: Some(Cow::Borrowed("owl")),
                        iri: IRIReference {
                            iri: Cow::Borrowed("http://example.com/ontology"),
                        },
                    }))),
                ],
            }),
            buf,
        )
        .unwrap();
        assert_eq!(
            r#"@base <http://example.com/ontology> .
@prefix : <http://example.com/ontology> .
@prefix owl: <http://example.com/ontology> ."#,
            std::str::from_utf8(&mem[..written_bytes as usize]).unwrap()
        );
    }

    #[test]
    fn parse_document2() {
        let ontology = r#"
                @base <http://example.com/ontology> .
                
                @prefix : <http://example.com/ontology> .
                @prefix owl: <http://example.com/ontology> .
                
                <http://example.com/ontology> rdf:type owl:Ontology ."#;
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
