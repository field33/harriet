#![allow(dead_code)]

use crate::ParseError::NotFullyParsed;
use cookie_factory::combinator::string as cf_string;
use cookie_factory::lib::std::io::Write;
use cookie_factory::multi::all as cf_all;
use cookie_factory::multi::separated_list as cf_separated_list;
use cookie_factory::sequence::tuple as cf_tuple;
use cookie_factory::SerializeFn;
use nom::branch::alt;
use nom::bytes::complete::{
    escaped_transform, is_not, tag, take_till, take_until, take_while1,
};
use nom::character::complete::{alpha1, alphanumeric1, char, multispace0, multispace1, satisfy};
use nom::combinator::{map, map_parser, opt, value};
use nom::error::{FromExternalError, ParseError as NomParseError, VerboseError};
use nom::multi::{many0, many1, separated_list1};
use nom::sequence::{delimited, preceded, terminated, tuple};
use nom::IResult;
use std::borrow::Cow;
use std::io::{Cursor, Read, Seek, SeekFrom};

#[derive(Debug, PartialEq, Eq)]
pub struct TurtleDocument<'a> {
    pub statements: Vec<Statement<'a>>,
    pub trailing_whitespace: Option<Whitespace<'a>>,
}

#[derive(Debug)]
pub enum ParseError<'a> {
    ParseError(nom::Err<VerboseError<&'a str>>),
    NotFullyParsed(&'a str),
}

impl<'a> TurtleDocument<'a> {
    pub fn parse_full(input: &'a str) -> Result<Self, ParseError> {
        let (remaining, document) =
            TurtleDocument::parse::<VerboseError<&str>>(input).map_err(ParseError::ParseError)?;
        if !remaining.is_empty() {
            return Err(NotFullyParsed(remaining));
        }
        return Ok(document);
    }

    pub fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(
            tuple((many0(Statement::parse), opt(Whitespace::parse))),
            |(statements, trailing)| Self {
                statements,
                trailing_whitespace: trailing,
            },
        )(input)
    }

    pub fn gen<W: Write + 'a>(subject: &'a Self) -> impl SerializeFn<W> + 'a {
        cf_tuple((
            cf_all(subject.statements.iter().map(Statement::gen)),
            Whitespace::gen_option(&subject.trailing_whitespace),
        ))
    }
}

impl ToString for TurtleDocument<'_> {
    fn to_string(&self) -> String {
        let mut buf = Cursor::new(Vec::new());

        cookie_factory::gen(TurtleDocument::gen(&self), &mut buf)
            .expect("Unable to write TurtleDocument to buffer.");
        buf.seek(SeekFrom::Start(0)).unwrap();

        let mut out = Vec::new();
        buf.read_to_end(&mut out).unwrap();

        std::str::from_utf8(&out).unwrap().to_owned()
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
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Whitespace<'a> {
    pub whitespace: Cow<'a, str>,
}

impl<'a> Whitespace<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(Self::parse_raw, |whitespace_raw: Vec<String>| {
            let whitespace = Cow::Owned(whitespace_raw.join(""));

            Self { whitespace }
        })(input)
    }

    fn parse_raw<E>(input: &'a str) -> IResult<&'a str, Vec<String>, E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        many1(alt((
            // WS
            map(take_while1(Whitespace::is_ws_char), |ws: &str| {
                ws.to_string()
            }),
            // Comment starting with '#' and until end of the line; Mentioned in the spec, but doesn't seem to be in the grammar
            map(
                tuple((char('#'), take_till(|c| c == '\n'))),
                // TODO: replace format with something more efficient?
                |(pound, comment): (char, &str)| format!("{}{}", pound, comment),
            ),
        )))(input)
    }

    /// [161s] 	WS 	::= 	#x20 | #x9 | #xD | #xA /* #x20=space #x9=character tabulation #xD=carriage return #xA=new line */
    fn is_ws_char(chchar: char) -> bool {
        matches!(chchar, '\u{20}' | '\u{9}' | '\u{D}' | '\u{A}')
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> impl SerializeFn<W> + 'a {
        cf_string(&subject.whitespace)
    }

    fn gen_option<'b, W: Write + 'b>(
        whitespace_opt: &'b Option<Self>,
    ) -> Box<dyn SerializeFn<W> + 'b> {
        match whitespace_opt {
            Some(whitespace) => Box::new(cf_string(&whitespace.whitespace)),
            None => Box::new(cf_string("")),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Triples<'a> {
    Labeled(Option<Whitespace<'a>>, Subject<'a>, PredicateObjectList<'a>),
    // Labeled()
    // Labeled()
}

impl<'a> Triples<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(
            tuple((
                map(
                    tuple((opt(Whitespace::parse), Subject::parse, multispace1, PredicateObjectList::parse)),
                    |(leading, subject, _, list)| Self::Labeled(leading, subject, list),
                ),
                multispace1,
                char('.'),
            )),
            |(triples, _, _)| triples,
        )(input)
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> Box<dyn SerializeFn<W> + 'a> {
        match subject {
            Self::Labeled(leading, subject, predicate_object_list) => Box::new(cf_tuple((
                Whitespace::gen_option(leading),
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
    Collection(Collection<'a>),
}

impl<'a> Subject<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        alt((
            map(IRI::parse, Self::IRI),
            map(Collection::parse, Self::Collection),
        ))(input)
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> Box<dyn SerializeFn<W> + 'a> {
        match subject {
            Self::IRI(iri) => Box::new(IRI::gen(iri)),
            Self::Collection(collection) => Box::new(Collection::gen(collection)),
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
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
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
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(
            separated_list1(
                tuple((multispace0, tag(";"), multispace0)),
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
pub struct BlankNodePropertyList<'a> {
    pub list: PredicateObjectList<'a>,
}

impl<'a> BlankNodePropertyList<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(
            delimited(
                tuple((char('['), multispace0)),
                PredicateObjectList::parse,
                tuple((multispace0, char(']'))),
            ),
            |list| Self { list },
        )(input)
    }

    fn parse_parens<E>(input: &'a str) -> IResult<&'a str, &'a str, E>
    where
        E: NomParseError<&'a str>,
    {
        delimited(
            tuple((char('['), multispace0)),
            is_not("]"),
            tuple((multispace0, char(']'))),
        )(input)
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> impl SerializeFn<W> + 'a {
        cf_tuple((
            cf_string("[\n"),
            PredicateObjectList::gen(&subject.list),
            cf_string("]"),
        ))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ObjectList<'a> {
    pub list: Vec<Object<'a>>,
}

impl<'a> ObjectList<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
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
    Collection(Collection<'a>),
    BlankNodePropertyList(BlankNodePropertyList<'a>),
    Literal(Literal<'a>),
}

impl<'a> Object<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        alt((
            map(IRI::parse, Self::IRI),
            map(Collection::parse, Self::Collection),
            map(BlankNodePropertyList::parse, Self::BlankNodePropertyList),
            map(Literal::parse, Self::Literal),
        ))(input)
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> Box<dyn SerializeFn<W> + 'a> {
        match subject {
            Self::IRI(iri) => Box::new(IRI::gen(iri)),
            Self::Collection(collection) => Box::new(Collection::gen(collection)),
            Self::BlankNodePropertyList(list) => Box::new(BlankNodePropertyList::gen(list)),
            Self::Literal(literal) => Box::new(Literal::gen(literal)),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Collection<'a> {
    pub list: Vec<Object<'a>>,
}

impl<'a> Collection<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map_parser(
            Self::parse_parens,
            map(separated_list1(multispace1, Object::parse), |list| Self {
                list,
            }),
        )(input)
    }

    fn parse_parens<E>(input: &'a str) -> IResult<&'a str, &'a str, E>
    where
        E: NomParseError<&'a str>,
    {
        delimited(
            tuple((char('('), multispace0)),
            is_not(")"),
            tuple((multispace0, char(')'))),
        )(input)
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> impl SerializeFn<W> + 'a {
        cf_tuple((
            cf_string("("),
            cf_separated_list(
                cf_string(" "),
                subject.list.iter().map(|object| Object::gen(object)),
            ),
            cf_string(")"),
        ))
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
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
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
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
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct BaseDirective<'a> {
    pub leading_whitespace: Option<Whitespace<'a>>,
    pub iri: IRIReference<'a>,
}

impl<'a> BaseDirective<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(Self::parse_raw, |(leading, iri_ref)| Self {
            leading_whitespace: leading,
            iri: iri_ref,
        })(input)
    }

    fn parse_raw<E>(input: &'a str) -> IResult<&'a str, (Option<Whitespace>, IRIReference), E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        tuple((
            opt(Whitespace::parse),
            tag("@base"),
            multispace1,
            IRIReference::parse,
            multispace1,
            char('.'),
        ))(input)
        .map(|(remainder, (leading, _, _, iri, _, _))| (remainder, (leading, iri)))
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

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct SparqlBaseDirective<'a> {
    // TODO: leading
    pub iri: IRIReference<'a>,
}

impl<'a> SparqlBaseDirective<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(Self::parse_raw, |iri_ref| Self { iri: iri_ref })(input)
    }

    fn parse_raw<E>(input: &'a str) -> IResult<&'a str, IRIReference, E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
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
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct PrefixDirective<'a> {
    pub leading_whitespace: Option<Whitespace<'a>>,
    pub prefix: Option<Cow<'a, str>>,
    pub iri: IRIReference<'a>,
}

impl<'a> PrefixDirective<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(Self::parse_raw, |(leading, prefix, iri_ref)| Self {
            leading_whitespace: leading,
            prefix: prefix.map(|n| Cow::Borrowed(n)),
            iri: iri_ref,
        })(input)
    }

    fn parse_raw<E>(input: &'a str) -> IResult<&'a str, (Option<Whitespace>, Option<&'a str>, IRIReference), E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        tuple((
            opt(Whitespace::parse),
            tag("@prefix"),
            multispace1,
            opt(is_not(":")),
            char(':'),
            multispace1,
            IRIReference::parse,
            multispace1,
            char('.'),
        ))(input)
        .map(|(remainder, (leading, _, _, prefix, _, _, iri, _, _))| (remainder, (leading, prefix, iri)))
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> impl SerializeFn<W> + 'a {
        cf_tuple((
            Whitespace::gen_option(&subject.leading_whitespace),
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

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct SparqlPrefixDirective<'a> {
    // TODO: leading
    pub prefix: Option<Cow<'a, str>>,
    pub iri: IRIReference<'a>,
}

impl<'a> SparqlPrefixDirective<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(Self::parse_raw, |(prefix, iri_ref)| Self {
            prefix: prefix.map(|n| Cow::Borrowed(n)),
            iri: iri_ref,
        })(input)
    }

    fn parse_raw<E>(input: &'a str) -> IResult<&'a str, (Option<&'a str>, IRIReference), E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
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
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct IRIReference<'a> {
    pub iri: Cow<'a, str>,
}

impl<'a> IRIReference<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(Self::parse_raw, |iri: &'a str| {
            let iri_utf8 = Cow::Borrowed(iri);

            Self { iri: iri_utf8 }
        })(input)
    }

    fn parse_raw<E>(input: &'a str) -> IResult<&'a str, &'a str, E>
    where
        E: NomParseError<&'a str>,
    {
        delimited(char('<'), is_not(">"), char('>'))(input)
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> impl SerializeFn<W> + 'a {
        cf_tuple((cf_string("<"), cf_string(&subject.iri), cf_string(">")))
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct PrefixedName<'a> {
    pub prefix: Option<Cow<'a, str>>,
    pub name: Option<Cow<'a, str>>,
    // TODO: locale
}

impl<'a> PrefixedName<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(
            tuple((
                // TODO: proper order instead of just is_pn_chars
                // (there are restrictions for first and last character)
                opt(many1(satisfy(Self::is_pn_chars))),
                char(':'),
                opt(is_not(" \t\r\n")),
            )),
            |(prefix, _, name)| Self {
                prefix: prefix.map(|chars| Cow::Owned(chars.into_iter().collect())),
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

    // [163s] 	PN_CHARS_BASE 	::= 	[A-Z] | [a-z] | [#x00C0-#x00D6] | [#x00D8-#x00F6] | [#x00F8-#x02FF] | [#x0370-#x037D] | [#x037F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
    fn is_pn_chars_base(chchar: char) -> bool {
        matches!(chchar,
        'A'..='Z'
        | 'a'..='z'
        | '\u{00C0}'..='\u{00D6}'
        | '\u{00D8}'..='\u{00F6}'
        | '\u{00F8}'..='\u{02FF}'
        | '\u{0370}'..='\u{037D}'
        | '\u{037F}'..='\u{1FFF}'
        | '\u{200C}'..='\u{200D}'
        | '\u{2070}'..='\u{218F}'
        | '\u{2C00}'..='\u{2FEF}'
        | '\u{3001}'..='\u{D7FF}'
        | '\u{F900}'..='\u{FDCF}'
        | '\u{FDF0}'..='\u{FFFD}'
        | '\u{10000}'..='\u{EFFFF}')
    }

    // [164s] 	PN_CHARS_U 	::= 	PN_CHARS_BASE | '_'
    fn is_pn_chars_base_with_underscore(khar: char) -> bool {
        Self::is_pn_chars_base(khar) || matches!(khar, '_')
    }

    // [166s] 	PN_CHARS 	::= 	PN_CHARS_U | '-' | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040]
    fn is_pn_chars(khar: char) -> bool {
        Self::is_pn_chars_base_with_underscore(khar)
            || matches!(khar,
                '-'
                | '0'..='9'
                | '\u{00B7}'
                | '\u{0300}'..='\u{036F}'
                | '\u{203F}'..='\u{2040}'
            )
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
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
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
    pub iri: Option<IRI<'a>>,
}

impl<'a> RDFLiteral<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(
            tuple((
                TurtleString::parse,
                opt(alt((
                    map(Self::parse_language_tag, |(first, repeating)| {
                        let mut all_parts = vec![first];
                        all_parts.extend_from_slice(&repeating);
                        either::Either::Left(Cow::Owned(all_parts.join("-")))
                    }),
                    map(tuple((tag("^^"), IRI::parse)), |(_, iri)| {
                        either::Either::Right(iri)
                    }),
                ))),
            )),
            |(string, language_tag_or_iri)| match language_tag_or_iri {
                None => Self {
                    string,
                    language_tag: None,
                    iri: None,
                },
                Some(either::Either::Left(language_tag)) => Self {
                    string,
                    language_tag: Some(language_tag),
                    iri: None,
                },
                Some(either::Either::Right(iri)) => Self {
                    string,
                    language_tag: None,
                    iri: Some(iri),
                },
            },
        )(input)
    }

    fn parse_language_tag<E>(input: &'a str) -> IResult<&'a str, (&'a str, Vec<&'a str>), E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        tuple((char('@'), alpha1, many0(tuple((char('-'), alphanumeric1)))))(input).map(
            |(remainder, (_, first, repeating))| {
                (
                    remainder,
                    (first, repeating.into_iter().map(|n| n.1).collect()),
                )
            },
        )
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> Box<dyn SerializeFn<W> + 'a> {
        match (&subject.language_tag, &subject.iri) {
            (None, None) => Box::new(TurtleString::gen(&subject.string)),
            (Some(language_tag), None) => Box::new(cf_tuple((
                TurtleString::gen(&subject.string),
                cf_string("@"),
                cf_string(language_tag),
            ))),
            (None, Some(iri)) => Box::new(cf_tuple((
                TurtleString::gen(&subject.string),
                cf_string("^^"),
                IRI::gen(iri),
            ))),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct BooleanLiteral {
    pub bool: bool,
}

impl BooleanLiteral {
    fn parse<'a, E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
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
    StringLiteralSingleQuote(StringLiteralSingleQuote<'a>),
    StringLiteralLongQuote(StringLiteralLongQuote<'a>),
    StringLiteralLongSingleQuote(StringLiteralLongSingleQuote<'a>),
}

impl<'a> TurtleString<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        alt((
            map(StringLiteralLongQuote::parse, Self::StringLiteralLongQuote),
            map(
                StringLiteralLongSingleQuote::parse,
                Self::StringLiteralLongSingleQuote,
            ),
            map(StringLiteralQuote::parse, Self::StringLiteralQuote),
            map(
                StringLiteralSingleQuote::parse,
                Self::StringLiteralSingleQuote,
            ),
        ))(input)
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> Box<dyn SerializeFn<W> + 'a> {
        match subject {
            Self::StringLiteralQuote(string) => Box::new(StringLiteralQuote::gen(string)),
            Self::StringLiteralSingleQuote(string) => {
                Box::new(StringLiteralSingleQuote::gen(string))
            }
            Self::StringLiteralLongQuote(string) => Box::new(StringLiteralLongQuote::gen(string)),
            Self::StringLiteralLongSingleQuote(string) => {
                Box::new(StringLiteralLongSingleQuote::gen(string))
            }
        }
    }
}

impl<'a> ToString for TurtleString<'a> {
    fn to_string(&self) -> String {
        match self {
            TurtleString::StringLiteralQuote(inner) => inner.string.to_string(),
            TurtleString::StringLiteralSingleQuote(inner) => inner.string.to_string(),
            TurtleString::StringLiteralLongQuote(inner) => inner.string.to_string(),
            TurtleString::StringLiteralLongSingleQuote(inner) => inner.string.to_string(),
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
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(Self::parse_str, |string| Self {
            string: Cow::Owned(string),
        })(input)
    }

    fn parse_str<E: NomParseError<&'a str>>(i: &'a str) -> IResult<&'a str, String, E> {
        preceded(
            char('\"'),
            terminated(
                // Inner string
                escaped_transform(
                    satisfy(|c| {
                        c.is_alphanumeric()
                            || c.is_whitespace()
                            || (c.is_ascii_punctuation() && c != '"' && c != '\\')
                    }),
                    '\\',
                    alt((
                        value("\\", tag("\\")),
                        value("\"", tag("\"")),
                        value("\n", tag("\n")),
                    )),
                ),
                char('\"'),
            ),
        )(i)
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> impl SerializeFn<W> + 'a {
        cf_tuple((cf_string("\""), cf_string(&subject.string), cf_string("\"")))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct StringLiteralSingleQuote<'a> {
    pub string: Cow<'a, str>,
}

impl<'a> StringLiteralSingleQuote<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(Self::parse_str, |string| Self {
            string: Cow::Owned(string),
        })(input)
    }

    /// Inner logic should be identical to [StringLiteralQuote::parse_str].
    fn parse_str<E: NomParseError<&'a str>>(i: &'a str) -> IResult<&'a str, String, E> {
        preceded(
            char('\''),
            terminated(
                // Inner string
                escaped_transform(
                    satisfy(|c| {
                        c.is_alphanumeric()
                            || c.is_whitespace()
                            || (c.is_ascii_punctuation() && c != '\'' && c != '\\')
                    }),
                    '\\',
                    alt((
                        value("\\", tag("\\")),
                        value("\'", tag("\'")),
                        value("\n", tag("\n")),
                    )),
                ),
                char('\''),
            ),
        )(i)
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> impl SerializeFn<W> + 'a {
        cf_tuple((cf_string("\'"), cf_string(&subject.string), cf_string("\'")))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct StringLiteralLongQuote<'a> {
    pub string: Cow<'a, str>,
}

impl<'a> StringLiteralLongQuote<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(Self::parse_str, |string| Self {
            string: Cow::Borrowed(string),
        })(input)
    }

    fn parse_str<E: NomParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
        preceded(
            tag("\"\"\""),
            terminated(
                // Inner string
                take_until("\"\"\""),
                tag("\"\"\""),
            ),
        )(i)
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> impl SerializeFn<W> + 'a {
        cf_tuple((
            cf_string("\"\"\""),
            cf_string(&subject.string),
            cf_string("\"\"\""),
        ))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct StringLiteralLongSingleQuote<'a> {
    pub string: Cow<'a, str>,
}

impl<'a> StringLiteralLongSingleQuote<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(Self::parse_str, |string| Self {
            string: Cow::Borrowed(string),
        })(input)
    }

    fn parse_str<E: NomParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
        preceded(
            tag("'''"),
            terminated(
                // Inner string
                take_until("'''"),
                tag("'''"),
            ),
        )(i)
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> impl SerializeFn<W> + 'a {
        cf_tuple((
            cf_string("'''"),
            cf_string(&subject.string),
            cf_string("'''"),
        ))
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
    fn parse_whitespace() {
        assert_eq!(
            Ok((
                "",
                Whitespace {
                    whitespace: Cow::Borrowed("   ")
                }
            )),
            Whitespace::parse::<VerboseError<&str>>("   ")
        );
        assert_eq!(
            Ok((
                ".",
                Whitespace {
                    whitespace: Cow::Borrowed("   ")
                }
            )),
            Whitespace::parse::<VerboseError<&str>>("   .")
        );
        assert_eq!(
            Ok((
                "",
                Whitespace {
                    whitespace: Cow::Borrowed("   \n  ")
                }
            )),
            Whitespace::parse::<VerboseError<&str>>("   \n  ")
        );
        // Second line with immediate comment
        assert_eq!(
            Ok((
                "",
                Whitespace {
                    whitespace: Cow::Borrowed("   \n#Test")
                }
            )),
            Whitespace::parse::<VerboseError<&str>>("   \n#Test")
        );
        // Second line with empty comment
        assert_eq!(
            Ok((
                "",
                Whitespace {
                    whitespace: Cow::Borrowed("   \n#")
                }
            )),
            Whitespace::parse::<VerboseError<&str>>("   \n#")
        );
        // Empty comment
        assert_eq!(
            Ok((
                "",
                Whitespace {
                    whitespace: Cow::Borrowed("#")
                }
            )),
            Whitespace::parse::<VerboseError<&str>>("#")
        );
    }

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
        assert!(PrefixedName::parse::<VerboseError<&str>>("\"http://example.com").is_err());
        // Some examples from the reference
        assert_eq!(
            Ok((
                "",
                PrefixedName {
                    prefix: Some(Cow::Borrowed("leg")),
                    name: Some(Cow::Borrowed("3032571")),
                }
            )),
            PrefixedName::parse::<VerboseError<&str>>("leg:3032571")
        );
        assert_eq!(
            Ok((
                "",
                PrefixedName {
                    prefix: Some(Cow::Borrowed("isbn13")),
                    name: Some(Cow::Borrowed("9780136019701")),
                }
            )),
            PrefixedName::parse::<VerboseError<&str>>("isbn13:9780136019701")
        );
        assert_eq!(
            Ok((
                "",
                PrefixedName {
                    prefix: Some(Cow::Borrowed("og")),
                    name: Some(Cow::Borrowed("video:height")),
                }
            )),
            PrefixedName::parse::<VerboseError<&str>>("og:video:height")
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
    fn parse_object() {
        assert_eq!(
            Ok((
                "",
                Object::Literal(Literal::RDFLiteral(RDFLiteral {
                    string: TurtleString::StringLiteralQuote(StringLiteralQuote {
                        string: Cow::Borrowed("http://example.com")
                    }),
                    language_tag: None,
                    iri: None
                }))
            )),
            Object::parse::<VerboseError<&str>>(r#""http://example.com""#)
        );
    }

    #[test]
    fn parse_base_directive() {
        assert_eq!(
            Ok((
                "",
                BaseDirective {
                    leading_whitespace: None,
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
                    leading_whitespace: Some(Whitespace {
                        whitespace: Cow::Borrowed("   ")
                    }),
                    iri: IRIReference {
                        iri: Cow::Borrowed("http://example.com/ontology")
                    }
                }
            )),
            BaseDirective::parse::<VerboseError<&str>>(
                "   @base  \n <http://example.com/ontology>    ."
            )
        );
    }

    #[test]
    fn parse_prefix_directive() {
        assert_eq!(
            Ok((
                "",
                PrefixDirective {
                    leading_whitespace: None,
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
                    leading_whitespace: None,
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
    fn parse_collection() {
        assert_eq!(
            Ok((
                "",
                Collection {
                    list: vec![
                        Object::IRI(IRI::PrefixedName(PrefixedName {
                            prefix: None,
                            name: Some("Entity1".into())
                        })),
                        Object::IRI(IRI::PrefixedName(PrefixedName {
                            prefix: None,
                            name: Some("Entity2".into())
                        })),
                        Object::IRI(IRI::PrefixedName(PrefixedName {
                            prefix: None,
                            name: Some("Entity3".into())
                        }))
                    ]
                }
            )),
            Collection::parse::<VerboseError<&str>>("(:Entity1 :Entity2 :Entity3)")
        );
        // Some whitespace
        assert_eq!(
            Ok((
                "",
                Collection {
                    list: vec![
                        Object::IRI(IRI::PrefixedName(PrefixedName {
                            prefix: None,
                            name: Some("Entity1".into())
                        })),
                        Object::IRI(IRI::PrefixedName(PrefixedName {
                            prefix: None,
                            name: Some("Entity2".into())
                        })),
                        Object::IRI(IRI::PrefixedName(PrefixedName {
                            prefix: None,
                            name: Some("Entity3".into())
                        }))
                    ]
                }
            )),
            Collection::parse::<VerboseError<&str>>("( :Entity1 \n:Entity2 :Entity3 )")
        );
    }

    #[test]
    fn render_collection() {
        let mut mem: [u8; 1024] = [0; 1024];
        let buf = &mut mem[..];
        let (_, written_bytes) = cookie_factory::gen(
            Collection::gen(&Collection {
                list: vec![
                    Object::IRI(IRI::PrefixedName(PrefixedName {
                        prefix: None,
                        name: Some("Entity1".into()),
                    })),
                    Object::IRI(IRI::PrefixedName(PrefixedName {
                        prefix: None,
                        name: Some("Entity2".into()),
                    })),
                    Object::IRI(IRI::PrefixedName(PrefixedName {
                        prefix: None,
                        name: Some("Entity3".into()),
                    })),
                ],
            }),
            buf,
        )
        .unwrap();
        assert_eq!(
            r#"(:Entity1 :Entity2 :Entity3)"#,
            std::str::from_utf8(&mem[..written_bytes as usize]).unwrap()
        );
    }

    #[test]
    fn parse_directive() {
        assert_eq!(
            Ok((
                "",
                Directive::Base(BaseDirective {
                    leading_whitespace: None,
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
                    leading_whitespace: None,
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
                    None,
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
            StringLiteralQuote::parse::<VerboseError<&str>>(r#""SomeString""#)
        );
        // Quote in quote
        assert_eq!(
            Ok((
                "",
                StringLiteralQuote {
                    string: Cow::Borrowed(r#"Dwayne "The Rock" Johnson"#),
                }
            )),
            StringLiteralQuote::parse::<VerboseError<&str>>(r#""Dwayne \"The Rock\" Johnson""#)
        );
        // URI
        assert_eq!(
            Ok((
                "",
                StringLiteralQuote {
                    string: Cow::Borrowed(r#"http://example.com"#),
                }
            )),
            StringLiteralQuote::parse::<VerboseError<&str>>(r#""http://example.com""#)
        );
    }

    #[test]
    fn parse_string_literal_single_quote() {
        assert_eq!(
            Ok((
                "",
                StringLiteralSingleQuote {
                    string: Cow::Borrowed("SomeString"),
                }
            )),
            StringLiteralSingleQuote::parse::<VerboseError<&str>>(r#"'SomeString'"#)
        );
    }

    #[test]
    fn parse_string_literal_long_quote() {
        assert_eq!(
            Ok((
                "",
                StringLiteralLongQuote {
                    string: Cow::Borrowed("SomeString"),
                }
            )),
            StringLiteralLongQuote::parse::<VerboseError<&str>>(r#""""SomeString""""#)
        );
        // Empty
        assert_eq!(
            Ok((
                "",
                StringLiteralLongQuote {
                    string: Cow::Borrowed(""),
                }
            )),
            StringLiteralLongQuote::parse::<VerboseError<&str>>(r#""""""""#)
        );
        // Contains quote
        assert_eq!(
            Ok((
                "",
                StringLiteralLongQuote {
                    string: Cow::Borrowed("SomeString\"OtherString"),
                }
            )),
            StringLiteralLongQuote::parse::<VerboseError<&str>>(r#""""SomeString"OtherString""""#)
        );
    }

    #[test]
    fn parse_string_literal_long_single_quote() {
        assert_eq!(
            Ok((
                "",
                StringLiteralLongSingleQuote {
                    string: Cow::Borrowed("SomeString"),
                }
            )),
            StringLiteralLongSingleQuote::parse::<VerboseError<&str>>(r#"'''SomeString'''"#)
        );
    }

    #[test]
    fn parse_blank_node_property_list() {
        assert_eq!(
            Ok((
                "",
                BlankNodePropertyList {
                    list: PredicateObjectList {
                        list: vec![
                            (
                                IRI::PrefixedName(PrefixedName {
                                    prefix: Some("ex".into()),
                                    name: Some("fullname".into())
                                }),
                                ObjectList {
                                    list: vec![Object::Literal(Literal::RDFLiteral(RDFLiteral {
                                        string: TurtleString::StringLiteralQuote(
                                            StringLiteralQuote {
                                                string: "Dave Beckett".into()
                                            }
                                        ),
                                        language_tag: None,
                                        iri: None
                                    }))]
                                }
                            ),
                            (
                                IRI::PrefixedName(PrefixedName {
                                    prefix: Some("ex".into()),
                                    name: Some("homePage".into())
                                }),
                                ObjectList {
                                    list: vec![Object::IRI(IRI::IRIReference(IRIReference {
                                        iri: "http://purl.org/net/dajobe/".into()
                                    }))]
                                }
                            )
                        ]
                    }
                }
            )),
            BlankNodePropertyList::parse::<VerboseError<&str>>(
                r#"[
                        ex:fullname "Dave Beckett";
                        ex:homePage <http://purl.org/net/dajobe/>
                      ]"#
            )
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
                    iri: None
                }
            )),
            RDFLiteral::parse::<VerboseError<&str>>("\"SomeString\"@en")
        );
        assert_eq!(
            Ok((
                "",
                RDFLiteral {
                    string: TurtleString::StringLiteralQuote(StringLiteralQuote {
                        string: Cow::Borrowed("SomeString"),
                    }),
                    language_tag: None,
                    iri: Some(IRI::PrefixedName(PrefixedName {
                        prefix: Some(Cow::Borrowed("xsd")),
                        name: Some(Cow::Borrowed("boolean")),
                    }))
                }
            )),
            RDFLiteral::parse::<VerboseError<&str>>("\"SomeString\"^^xsd:boolean")
        );
        // Non-trivial language tags
        assert_eq!(
            Ok((
                "",
                RDFLiteral {
                    string: TurtleString::StringLiteralQuote(StringLiteralQuote {
                        string: Cow::Borrowed("SomeString"),
                    }),
                    language_tag: Some(Cow::Borrowed("es-419")),
                    iri: None
                }
            )),
            RDFLiteral::parse::<VerboseError<&str>>("\"SomeString\"@es-419")
        );
        assert_eq!(
            Ok((
                "",
                RDFLiteral {
                    string: TurtleString::StringLiteralQuote(StringLiteralQuote {
                        string: Cow::Borrowed("SomeString"),
                    }),
                    language_tag: Some(Cow::Borrowed("nan-Hant-TW")),
                    iri: None
                }
            )),
            RDFLiteral::parse::<VerboseError<&str>>("\"SomeString\"@nan-Hant-TW")
        );
    }

    #[test]
    fn render_rdf_literal() {
        let mut mem: [u8; 1024] = [0; 1024];
        let buf = &mut mem[..];
        let (_, written_bytes) = cookie_factory::gen(
            RDFLiteral::gen(&RDFLiteral {
                string: TurtleString::StringLiteralQuote(StringLiteralQuote {
                    string: Cow::Borrowed("SomeString"),
                }),
                language_tag: None,
                iri: None,
            }),
            buf,
        )
        .unwrap();
        assert_eq!(
            r#""SomeString""#,
            std::str::from_utf8(&mem[..written_bytes as usize]).unwrap()
        );
        let mut mem: [u8; 1024] = [0; 1024];
        let buf = &mut mem[..];
        let (_, written_bytes) = cookie_factory::gen(
            RDFLiteral::gen(&RDFLiteral {
                string: TurtleString::StringLiteralQuote(StringLiteralQuote {
                    string: Cow::Borrowed("SomeString"),
                }),
                language_tag: Some(Cow::Borrowed("en")),
                iri: None,
            }),
            buf,
        )
        .unwrap();
        assert_eq!(
            r#""SomeString"@en"#,
            std::str::from_utf8(&mem[..written_bytes as usize]).unwrap()
        );
        let mut mem: [u8; 1024] = [0; 1024];
        let buf = &mut mem[..];
        let (_, written_bytes) = cookie_factory::gen(
            RDFLiteral::gen(&RDFLiteral {
                string: TurtleString::StringLiteralQuote(StringLiteralQuote {
                    string: Cow::Borrowed("SomeString"),
                }),
                language_tag: None,
                iri: Some(IRI::PrefixedName(PrefixedName {
                    prefix: Some(Cow::Borrowed("xsd")),
                    name: Some(Cow::Borrowed("boolean")),
                })),
            }),
            buf,
        )
        .unwrap();
        assert_eq!(
            r#""SomeString"^^xsd:boolean"#,
            std::str::from_utf8(&mem[..written_bytes as usize]).unwrap()
        );
    }

    #[test]
    fn render_triples() {
        let mut mem: [u8; 1024] = [0; 1024];
        let buf = &mut mem[..];
        let (_, written_bytes) = cookie_factory::gen(
            Triples::gen(&Triples::Labeled(
                None,
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
                    statements: vec![
                        Statement::Directive(Directive::Base(BaseDirective {
                            leading_whitespace: None,
                            iri: IRIReference {
                                iri: Cow::Borrowed("http://example.com/ontology")
                            }
                        })),
                        Statement::Directive(Directive::Prefix(PrefixDirective {
                            leading_whitespace: Some(Whitespace {
                                whitespace: Cow::Borrowed("\n                \n                ")
                            }),
                            prefix: None,
                            iri: IRIReference {
                                iri: Cow::Borrowed("http://example.com/ontology")
                            }
                        })),
                        Statement::Directive(Directive::Prefix(PrefixDirective {
                            leading_whitespace: Some(Whitespace {
                                whitespace: Cow::Borrowed("\n                ")
                            }),
                            prefix: Some(Cow::Borrowed("owl")),
                            iri: IRIReference {
                                iri: Cow::Borrowed("http://example.com/ontology")
                            }
                        }))
                    ],
                    trailing_whitespace: None,
                }
            )),
            TurtleDocument::parse::<VerboseError<&str>>(
                r#"
                @base <http://example.com/ontology> .
                
                @prefix : <http://example.com/ontology> .
                @prefix owl: <http://example.com/ontology> .
            "#
                .trim()
            )
        );
    }

    #[test]
    fn render_document() {
        let mut mem: [u8; 1024] = [0; 1024];
        let buf = &mut mem[..];
        let (_, written_bytes) = cookie_factory::gen(
            TurtleDocument::gen(&TurtleDocument {
                statements: vec![
                    Statement::Directive(Directive::Base(BaseDirective {
                        leading_whitespace: None,
                        iri: IRIReference {
                            iri: Cow::Borrowed("http://example.com/ontology"),
                        },
                    })),
                    Statement::Directive(Directive::Prefix(PrefixDirective {
                        leading_whitespace: Some(Whitespace {
                            whitespace: Cow::Borrowed("\n")
                        }),
                        prefix: None,
                        iri: IRIReference {
                            iri: Cow::Borrowed("http://example.com/ontology"),
                        },
                    })),
                    Statement::Directive(Directive::Prefix(PrefixDirective {
                        leading_whitespace: Some(Whitespace {
                            whitespace: Cow::Borrowed("\n")
                        }),
                        prefix: Some(Cow::Borrowed("owl")),
                        iri: IRIReference {
                            iri: Cow::Borrowed("http://example.com/ontology"),
                        },
                    })),
                ],
                trailing_whitespace: None,
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
