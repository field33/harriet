#![allow(dead_code)]
pub mod triple_production;

use crate::ParseError::NotFullyParsed;
use cookie_factory::combinator::string as cf_string;
use cookie_factory::lib::std::io::Write;
use cookie_factory::multi::all as cf_all;
use cookie_factory::multi::separated_list as cf_separated_list;
use cookie_factory::sequence::tuple as cf_tuple;
use cookie_factory::SerializeFn;
use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take_till, take_while1};
use nom::character::complete::{alpha1, alphanumeric1, char, multispace0, multispace1, satisfy};
use nom::combinator::{map, map_parser, opt};
use nom::error::{ErrorKind, FromExternalError, ParseError as NomParseError, VerboseError};
use nom::multi::{many0, many1};
use nom::sequence::{delimited, tuple};
use nom::Err;
use nom::{IResult, Needed};
use std::borrow::Cow;
use std::borrow::Cow::Borrowed;
use std::io::{Cursor, Read, Seek, SeekFrom};
use std::str::Chars;

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
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

/// Whitespace. May be one of four characters, or a comment (starting with `#`) until the end of a line.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Whitespace<'a> {
    pub whitespace: Cow<'a, str>,
}

impl<'a> Whitespace<'a> {
    pub fn space() -> Self {
        Self {
            whitespace: Borrowed(" "),
        }
    }

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Triples<'a> {
    Labeled(Option<Whitespace<'a>>, Subject<'a>, PredicateObjectList<'a>),
    Blank(
        Option<Whitespace<'a>>,
        BlankNodePropertyList<'a>,
        Option<PredicateObjectList<'a>>,
    ),
}

impl<'a> Triples<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(
            tuple((
                alt((
                    map(
                        tuple((
                            opt(Whitespace::parse),
                            Subject::parse,
                            PredicateObjectList::parse,
                        )),
                        |(leading, subject, list)| Self::Labeled(leading, subject, list),
                    ),
                    map(
                        tuple((
                            opt(Whitespace::parse),
                            BlankNodePropertyList::parse,
                            opt(PredicateObjectList::parse),
                        )),
                        |(leading, blank_node_property_list, predicate_object_list)| {
                            Self::Blank(leading, blank_node_property_list, predicate_object_list)
                        },
                    ),
                )),
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
                PredicateObjectList::gen(predicate_object_list),
                cf_string(" ."),
            ))),
            Self::Blank(leading, blank_node_property_list, predicate_object_list) => {
                Box::new(cf_tuple((
                    Whitespace::gen_option(leading),
                    BlankNodePropertyList::gen(blank_node_property_list),
                    PredicateObjectList::gen_option(predicate_object_list),
                    cf_string(" ."),
                )))
            }
            #[allow(unreachable_patterns)]
            _ => todo!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Subject<'a> {
    IRI(IRI<'a>),
    BlankNode(BlankNode<'a>),
    Collection(Collection<'a>),
}

impl<'a> Subject<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        alt((
            // BlankNode should be parsed before IRI, or it might be mis-parsed as PrefixedName
            map(BlankNode::parse, Self::BlankNode),
            map(IRI::parse, Self::IRI),
            map(Collection::parse, Self::Collection),
        ))(input)
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> Box<dyn SerializeFn<W> + 'a> {
        match subject {
            Self::BlankNode(blank_node) => Box::new(BlankNode::gen(blank_node)),
            Self::IRI(iri) => Box::new(IRI::gen(iri)),
            Self::Collection(collection) => Box::new(Collection::gen(collection)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
/// A "Verb" can be used for inside a [PredicateObjectList]. It allows for using the character 'a'
/// as a placeholder for `rdfs:type`.
///
/// Parsing reference: <https://www.w3.org/TR/turtle/#grammar-production-verb>
pub enum Verb<'a> {
    A,
    IRI(IRI<'a>),
}

impl<'a> Verb<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        alt((map(IRI::parse, Self::IRI), map(char('a'), |_| Self::A)))(input)
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> Box<dyn SerializeFn<W> + 'a> {
        match subject {
            Self::IRI(iri) => Box::new(IRI::gen(iri)),
            Self::A => Box::new(cf_string("a")),
        }
    }
}

impl<'a> From<IRI<'a>> for Verb<'a> {
    fn from(original: IRI<'a>) -> Self {
        Verb::IRI(original)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

/// RDF Blank Nodes
///
/// May either be a [BlankNodeLabel] or an anonymous blank node
///
/// Parsing: <https://www.w3.org/TR/turtle/#grammar-production-BlankNode>
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BlankNode<'a> {
    Labeled(BlankNodeLabel<'a>),
    Anonymous(BlankNodeAnonymous<'a>),
}

impl<'a> BlankNode<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        alt((
            map(BlankNodeLabel::parse, Self::Labeled),
            map(BlankNodeAnonymous::parse, Self::Anonymous),
        ))(input)
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> Box<dyn SerializeFn<W> + 'a> {
        match subject {
            Self::Labeled(blank_node) => Box::new(BlankNodeLabel::gen(blank_node)),
            Self::Anonymous(blank_node) => Box::new(BlankNodeAnonymous::gen(blank_node)),
        }
    }
}

/// Labled RDF Blank Nodes
///
/// Reference section: <https://www.w3.org/TR/turtle/#BNodes>
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BlankNodeLabel<'a> {
    /// Parsing:
    /// <https://www.w3.org/TR/turtle/#grammar-production-BLANK_NODE_LABEL>
    /// `[141s] BLANK_NODE_LABEL ::= '_:' (PN_CHARS_U | [0-9]) ((PN_CHARS | '.')* PN_CHARS)?`
    pub label: Cow<'a, str>,
}

impl<'a> BlankNodeLabel<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(
            tuple((
                tag("_:"),
                tuple((
                    many1(satisfy(Self::is_blank_node_label_first_char)),
                    many0(satisfy(Self::is_blank_node_label_middle_char)),
                    many0(satisfy(Self::is_blank_node_label_end_char)),
                )),
            )),
            |(_tag, (first_label_char, middle_label_chars, end_label_char))| Self {
                label: Cow::Owned(
                    first_label_char
                        .into_iter()
                        .chain(middle_label_chars.into_iter())
                        .chain(end_label_char.into_iter())
                        .collect(),
                ),
            },
        )(input)
    }

    pub fn is_blank_node_label_first_char(khar: char) -> bool {
        PrefixedName::is_pn_chars_base_with_underscore(khar) || matches!(khar, '0'..='9')
    }

    pub fn is_blank_node_label_middle_char(khar: char) -> bool {
        Self::is_blank_node_char_with_additional_permitted(khar) || matches!(khar, '.')
    }

    pub fn is_blank_node_label_end_char(khar: char) -> bool {
        Self::is_blank_node_char_with_additional_permitted(khar)
    }

    /// > The characters `-`, `U+00B7`, `U+0300` to `U+036F` and `U+203F` to `U+2040` are permitted anywhere except the first character.
    ///
    /// This is equivalent to `PN_CHARS`
    pub fn is_blank_node_char_with_additional_permitted(khar: char) -> bool {
        PrefixedName::is_pn_chars(khar)
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> Box<dyn SerializeFn<W> + 'a> {
        Box::new(cf_tuple((cf_string("_:"), cf_string(&subject.label))))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BlankNodeAnonymous<'a> {
    pub whitespace: Option<Whitespace<'a>>,
}

impl<'a> BlankNodeAnonymous<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(
            tuple((tag("["), opt(Whitespace::parse), tag("]"))),
            |(_bracket_open, whitespace_opt, _bracket_close)| Self {
                whitespace: whitespace_opt,
            },
        )(input)
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> Box<dyn SerializeFn<W> + 'a> {
        Box::new(cf_tuple((
            cf_string("["),
            Whitespace::gen_option(&subject.whitespace),
            cf_string("]"),
        )))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PredicateObjectList<'a> {
    pub list: Vec<(
        Whitespace<'a>,
        Verb<'a>,
        ObjectList<'a>,
        Option<Whitespace<'a>>, /* Token: ';' */
    )>,
}

impl<'a> PredicateObjectList<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(
            many1(map(
                tuple((
                    Whitespace::parse,
                    Verb::parse,
                    ObjectList::parse,
                    opt(tuple((opt(Whitespace::parse), tag(";")))),
                )),
                |(leading, verb, list, ws_pre_semicolon_opt)| {
                    (
                        leading,
                        verb,
                        list,
                        ws_pre_semicolon_opt.map(|(ws, _sem)| ws).flatten(),
                    )
                },
            )),
            |list| Self { list },
        )(input)
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> impl SerializeFn<W> + 'a {
        cf_separated_list(
            cf_string(";"),
            subject
                .list
                .iter()
                .map(|(leading, verb, object_list, ws_2)| {
                    cf_tuple((
                        Whitespace::gen(leading),
                        Verb::gen(verb),
                        ObjectList::gen(object_list),
                        Whitespace::gen_option(ws_2),
                    ))
                }),
        )
    }

    fn gen_option<'b: 'a, W: Write + 'b>(
        subject_opt: &'b Option<Self>,
    ) -> Box<dyn SerializeFn<W> + 'b> {
        match subject_opt {
            Some(inner) => Box::new(Self::gen(&inner)),
            None => Box::new(cf_string("")),
        }
    }

    fn gen_opt_semicolon<W: Write + 'a>(
        ws_opt: &'a Option<Whitespace<'a>>,
    ) -> Box<dyn SerializeFn<W> + 'a> {
        match ws_opt {
            Some(_) => Box::new(cf_tuple((Whitespace::gen_option(ws_opt), cf_string(";")))),
            None => Box::new(cf_string("")),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BlankNodePropertyList<'a> {
    pub list: PredicateObjectList<'a>,
    pub trailing_whitespace: Option<Whitespace<'a>>,
}

impl<'a> BlankNodePropertyList<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(
            delimited(
                char('['),
                tuple((PredicateObjectList::parse, opt(Whitespace::parse))),
                char(']'),
            ),
            |(list, ws)| Self {
                list,
                trailing_whitespace: ws,
            },
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
            cf_string("["),
            PredicateObjectList::gen(&subject.list),
            Whitespace::gen_option(&subject.trailing_whitespace),
            cf_string("]"),
        ))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ObjectList<'a> {
    pub list: Vec<(
        // Whitespace before separator (will be None on first element)
        Option<Whitespace<'a>>,
        // Whitespace after separator
        Option<Whitespace<'a>>,
        Object<'a>,
    )>,
}

impl<'a> ObjectList<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(
            many1(alt((
                // First item
                map(
                    tuple((opt(Whitespace::parse), Object::parse)),
                    |(ws, object)| (None, ws, object),
                ),
                // Subsequent items delimited by whitespace and ','
                map(
                    tuple((
                        opt(Whitespace::parse),
                        char(','),
                        opt(Whitespace::parse),
                        Object::parse,
                    )),
                    |(whitespace_before, _, whitespace_after, object)| {
                        (whitespace_before, whitespace_after, object)
                    },
                ),
            ))),
            |maybe_items| Self {
                list: maybe_items
                    .into_iter()
                    .map(|(whitespace_before, whitespace_after, object)| {
                        (whitespace_before, whitespace_after, object)
                    })
                    .collect(),
            },
        )(input)
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> impl SerializeFn<W> + 'a {
        cf_all(subject.list.iter().enumerate().map(
            |(i, (whitespace_before_seperator, whitespace_after_seperator, object))| {
                cf_tuple((
                    Whitespace::gen_option(whitespace_before_seperator),
                    match i == 0 {
                        false => Box::new(cf_string(",")),
                        true => Box::new(cf_string("")),
                    },
                    Whitespace::gen_option(whitespace_after_seperator),
                    Object::gen(object),
                ))
            },
        ))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Object<'a> {
    IRI(IRI<'a>),
    BlankNode(BlankNode<'a>),
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
            // BlankNode should be parsed before IRI, or it might be mis-parsed as PrefixedName
            map(BlankNode::parse, Self::BlankNode),
            map(IRI::parse, Self::IRI),
            map(Collection::parse, Self::Collection),
            map(BlankNodePropertyList::parse, Self::BlankNodePropertyList),
            map(Literal::parse, Self::Literal),
        ))(input)
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> Box<dyn SerializeFn<W> + 'a> {
        match subject {
            Self::BlankNode(blank_node) => Box::new(BlankNode::gen(blank_node)),
            Self::IRI(iri) => Box::new(IRI::gen(iri)),
            Self::Collection(collection) => Box::new(Collection::gen(collection)),
            Self::BlankNodePropertyList(list) => Box::new(BlankNodePropertyList::gen(list)),
            Self::Literal(literal) => Box::new(Literal::gen(literal)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Collection<'a> {
    pub list: Vec<(
        // Leading whitespace
        Option<Whitespace<'a>>,
        Object<'a>,
        // Trailing whitespace
        Option<Whitespace<'a>>,
    )>,
}

impl<'a> Collection<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(
            map_parser(
                Self::parse_parens,
                many1(map(
                    tuple((
                        opt(Whitespace::parse),
                        Object::parse,
                        opt(Whitespace::parse),
                    )),
                    |(whitespace_before, object, whitespace_after)| {
                        (whitespace_before, object, whitespace_after)
                    },
                )),
            ),
            |list| Self { list },
        )(input)
    }

    fn parse_parens<E>(input: &'a str) -> IResult<&'a str, &'a str, E>
    where
        E: NomParseError<&'a str>,
    {
        delimited(char('('), is_not(")"), char(')'))(input)
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> impl SerializeFn<W> + 'a {
        cf_tuple((
            cf_string("("),
            cf_all(
                subject
                    .list
                    .iter()
                    .map(|(whitespace_leading, object, whitespace_trailing)| {
                        cf_tuple((
                            Whitespace::gen_option(whitespace_leading),
                            Object::gen(object),
                            Whitespace::gen_option(whitespace_trailing),
                        ))
                    }),
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
            Whitespace::gen_option(&subject.leading_whitespace),
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
    pub leading_whitespace: Option<Whitespace<'a>>,
    pub iri: IRIReference<'a>,
}

impl<'a> SparqlBaseDirective<'a> {
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
            tag("BASE"),
            multispace1,
            IRIReference::parse,
        ))(input)
        .map(|(remainder, (leading, _, _, iri))| (remainder, (leading, iri)))
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> impl SerializeFn<W> + 'a {
        cf_tuple((
            Whitespace::gen_option(&subject.leading_whitespace),
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

    fn parse_raw<E>(
        input: &'a str,
    ) -> IResult<&'a str, (Option<Whitespace>, Option<&'a str>, IRIReference), E>
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
        .map(|(remainder, (leading, _, _, prefix, _, _, iri, _, _))| {
            (remainder, (leading, prefix, iri))
        })
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
    pub leading_whitespace: Option<Whitespace<'a>>,
    pub prefix: Option<Cow<'a, str>>,
    pub iri: IRIReference<'a>,
}

impl<'a> SparqlPrefixDirective<'a> {
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

    fn parse_raw<E>(
        input: &'a str,
    ) -> IResult<&'a str, (Option<Whitespace>, Option<&'a str>, IRIReference), E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        tuple((
            opt(Whitespace::parse),
            tag("PREFIX"),
            multispace1,
            opt(is_not(":")),
            char(':'),
            multispace1,
            IRIReference::parse,
        ))(input)
        .map(|(remainder, (leading, _, _, prefix, _, _, iri))| (remainder, (leading, prefix, iri)))
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> impl SerializeFn<W> + 'a {
        cf_tuple((
            Whitespace::gen_option(&subject.leading_whitespace),
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
                // TODO: proper implementation of PN_LOCAL
                opt(is_not(" \t\r\n,")),
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
    pub fn is_pn_chars_base(chchar: char) -> bool {
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
    pub fn is_pn_chars_base_with_underscore(khar: char) -> bool {
        Self::is_pn_chars_base(khar) || matches!(khar, '_')
    }

    // [166s] 	PN_CHARS 	::= 	PN_CHARS_U | '-' | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040]
    pub fn is_pn_chars(khar: char) -> bool {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal<'a> {
    RDFLiteral(RDFLiteral<'a>),
    NumericLiteral(NumericLiteral<'a>),
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
            map(NumericLiteral::parse, Self::NumericLiteral),
        ))(input)
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> Box<dyn SerializeFn<W> + 'a> {
        match subject {
            Self::RDFLiteral(literal) => Box::new(RDFLiteral::gen(literal)),
            Self::BooleanLiteral(bool) => Box::new(BooleanLiteral::gen(bool)),
            Self::NumericLiteral(numeric) => Box::new(NumericLiteral::gen(numeric)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NumericLiteral<'a> {
    Integer(Integer<'a>),
    Decimal(Decimal<'a>),
    Double(Double<'a>),
}

impl<'a> NumericLiteral<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        alt((
            map(Double::parse, Self::Double),
            map(Decimal::parse, Self::Decimal),
            map(Integer::parse, Self::Integer),
        ))(input)
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> Box<dyn SerializeFn<W> + 'a> {
        match subject {
            Self::Integer(number) => Box::new(Integer::gen(number)),
            Self::Decimal(number) => Box::new(Decimal::gen(number)),
            Self::Double(number) => Box::new(Double::gen(number)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Integer<'a> {
    pub sign: Option<Cow<'a, str>>,
    pub number_literal: Cow<'a, str>,
}

impl<'a> Integer<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(
            tuple((
                opt(satisfy(Self::is_number_sign)),
                many1(satisfy(Self::is_number)),
            )),
            |(sign, number_literal)| Self {
                sign: sign.map(|c| Cow::Owned(c.to_string())),
                number_literal: Cow::Owned(number_literal.into_iter().collect()),
            },
        )(input)
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> Box<dyn SerializeFn<W> + 'a> {
        Box::new(cf_tuple((
            gen_option_cow_str(&subject.sign),
            cf_string(&subject.number_literal),
        )))
    }

    pub fn lexical_form(&self) -> String {
        format!(
            "{sign}{integer}",
            sign = self.sign.as_ref().unwrap_or(&Cow::Borrowed("")),
            integer = self.number_literal
        )
    }

    pub fn is_number_sign(khar: char) -> bool {
        matches!(khar, '+' | '-')
    }

    pub fn is_number(khar: char) -> bool {
        matches!(khar, '0'..='9')
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Decimal<'a> {
    pub sign: Option<Cow<'a, str>>,
    pub integer: Option<Cow<'a, str>>,
    pub fractional: Cow<'a, str>,
}

impl<'a> Decimal<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(
            tuple((
                opt(satisfy(Integer::is_number_sign)),
                opt(many1(satisfy(Integer::is_number))),
                tag("."),
                many1(satisfy(Integer::is_number)),
            )),
            |(sign, integer, _point, fractional)| Self {
                sign: sign.map(|c| Cow::Owned(c.to_string())),
                integer: integer.map(|chars| Cow::Owned(chars.into_iter().collect())),
                fractional: Cow::Owned(fractional.into_iter().collect()),
            },
        )(input)
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> Box<dyn SerializeFn<W> + 'a> {
        Box::new(cf_tuple((
            gen_option_cow_str(&subject.sign),
            gen_option_cow_str(&subject.integer),
            cf_string("."),
            cf_string(&subject.fractional),
        )))
    }

    pub fn lexical_form(&self) -> String {
        format!(
            "{sign}{integer}.{fractional}",
            sign = self.sign.as_ref().unwrap_or(&Cow::Borrowed("")),
            integer = self.integer.as_ref().unwrap_or(&Cow::Borrowed("")),
            fractional = self.fractional,
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Double<'a> {
    pub sign: Option<Cow<'a, str>>,
    pub integer: Option<Cow<'a, str>>,
    pub fractional_dot: Option<Cow<'a, str>>,
    pub fractional: Option<Cow<'a, str>>,
    pub exponent_char: Cow<'a, str>,
    pub exponent_sign: Option<Cow<'a, str>>,
    pub exponent_integer: Cow<'a, str>,
}

impl<'a> Double<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        alt((
            map(
                tuple((
                    opt(satisfy(Integer::is_number_sign)),
                    many1(satisfy(Integer::is_number)),
                    tag("."),
                    opt(many1(satisfy(Integer::is_number))),
                    Self::parse_exponent,
                )),
                |(
                    sign,
                    integer,
                    fractional_dot,
                    fractional,
                    (exponent_char, exponent_sign, exponent_integer),
                )| Self {
                    sign: sign.map(|c| Cow::Owned(c.to_string())),
                    integer: Some(Cow::Owned(integer.into_iter().collect())),
                    fractional_dot: Some(Cow::Borrowed(fractional_dot)),
                    fractional: fractional.map(|chars| Cow::Owned(chars.into_iter().collect())),
                    exponent_char,
                    exponent_sign,
                    exponent_integer,
                },
            ),
            map(
                tuple((
                    opt(satisfy(Integer::is_number_sign)),
                    tag("."),
                    many1(satisfy(Integer::is_number)),
                    Self::parse_exponent,
                )),
                |(
                    sign,
                    fractional_dot,
                    fractional,
                    (exponent_char, exponent_sign, exponent_integer),
                )| Self {
                    sign: sign.map(|c| Cow::Owned(c.to_string())),
                    integer: None,
                    fractional_dot: Some(Cow::Borrowed(fractional_dot)),
                    fractional: Some(Cow::Owned(fractional.into_iter().collect())),
                    exponent_char,
                    exponent_sign,
                    exponent_integer,
                },
            ),
            map(
                tuple((
                    opt(satisfy(Integer::is_number_sign)),
                    opt(many1(satisfy(Integer::is_number))),
                    Self::parse_exponent,
                )),
                |(sign, integer, (exponent_char, exponent_sign, exponent_integer))| Self {
                    sign: sign.map(|c| Cow::Owned(c.to_string())),
                    integer: integer.map(|chars| Cow::Owned(chars.into_iter().collect())),
                    fractional_dot: None,
                    fractional: None,
                    exponent_char,
                    exponent_sign,
                    exponent_integer,
                },
            ),
        ))(input)
    }

    fn parse_exponent<E>(
        input: &'a str,
    ) -> IResult<&'a str, (Cow<'a, str>, Option<Cow<'a, str>>, Cow<'a, str>), E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(
            tuple((
                alt((
                    map(tag("e"), |khar| Cow::Borrowed(khar)),
                    map(tag("E"), |khar| Cow::Borrowed(khar)),
                )),
                opt(satisfy(Integer::is_number_sign)),
                many1(satisfy(Integer::is_number)),
            )),
            |(exponent_char, sign, integer)| {
                (
                    exponent_char,
                    sign.map(|c| Cow::Owned(c.to_string())),
                    Cow::Owned(integer.into_iter().collect()),
                )
            },
        )(input)
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> Box<dyn SerializeFn<W> + 'a> {
        Box::new(cf_tuple((
            gen_option_cow_str(&subject.sign),
            gen_option_cow_str(&subject.integer),
            gen_option_cow_str(&subject.fractional_dot),
            gen_option_cow_str(&subject.fractional),
            cf_string(&subject.exponent_char),
            gen_option_cow_str(&subject.exponent_sign),
            cf_string(&subject.exponent_integer),
        )))
    }

    pub fn lexical_form(&self) -> String {
        format!(
                "{sign}{integer}{fractional_dot}{fractional}{exponent_char}{exponent_sign}{exponent_integer}",
                sign = self.sign.as_ref().unwrap_or(&Cow::Borrowed("")),
                integer = self.integer.as_ref().unwrap_or(&Cow::Borrowed("")),
                fractional_dot = self.fractional_dot.as_ref().unwrap_or(&Cow::Borrowed("")),
                fractional = self.fractional.as_ref().unwrap_or(&Cow::Borrowed("")),
                exponent_char = self.exponent_char,
                exponent_sign = self.exponent_sign.as_ref().unwrap_or(&Cow::Borrowed("")),
                exponent_integer = self.exponent_integer,
            )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

fn string_literal_inner<'a, E>(
    str: &'a str,
    ql: usize,
    starts_with: fn(&'a str) -> bool,
    find: fn(&'a str) -> Option<usize>,
) -> IResult<&'a str, &'a str, E>
where
    E: NomParseError<&'a str>,
{
    if !starts_with(str) {
        return Err(nom::Err::Error(NomParseError::from_error_kind(
            str,
            ErrorKind::Tag,
        )));
    }
    let hay = &str[ql..];
    if starts_with(hay) {
        return Ok((&hay[ql..], ""));
    }
    let mut offset = 0;
    loop {
        let left = &hay[offset..];
        if let Some(i) = find(left) {
            offset += i;
            if !escaped(hay.as_bytes(), offset) {
                break;
            }
            offset += 1;
        } else {
            return Err(Err::Incomplete(Needed::Unknown));
        }
    }
    Ok((&hay[offset + ql..], &hay[..offset]))
}

/// Escapes parts of the inner string literal during parsing
fn escaped(hay: &[u8], offset: usize) -> bool {
    let mut p = offset;
    while p != 0 && hay[p - 1] == b'\\' {
        p -= 1;
    }
    (offset - p) % 2 == 1
}

pub(crate) fn unescape(s: &str) -> Result<String, &'static str> {
    let mut result = String::new();
    let mut chars = s.chars();
    while let Some(ch) = chars.next() {
        if ch == '\\' {
            let r = match chars.next() {
                Some('u') => hex_to_char(&mut chars, 4),
                Some('U') => hex_to_char(&mut chars, 8),
                Some('t') => Some('\t'),
                Some('b') => Some('\x08'),
                Some('n') => Some('\n'),
                Some('r') => Some('\r'),
                Some('f') => Some('\x0c'),
                Some('\'') => Some('\''),
                Some('"') => Some('"'),
                Some('\\') => Some('\\'),
                _ => return Err("Invalid escape sequence"),
            };
            match r {
                Some(v) => result.push(v),
                None => return Err("Unclosed escape sequence"),
            }
        } else {
            result.push(ch)
        }
    }
    Ok(result)
}

fn hex_to_char(chars: &mut Chars, n: u8) -> Option<char> {
    chars
        .by_ref()
        .take(n as usize)
        .fold(Some((0, 0)), |acc, c| {
            acc.and_then(|(acc, n)| c.to_digit(16).map(|c| ((acc << 4) + c, n + 1)))
        })
        .and_then(|(ch, count)| if count == n { char::from_u32(ch) } else { None })
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

    pub fn lexical_form(&self) -> Result<String, &str> {
        match self {
            Self::StringLiteralQuote(inner) => inner.lexical_form(),
            Self::StringLiteralSingleQuote(inner) => inner.lexical_form(),
            Self::StringLiteralLongQuote(inner) => inner.lexical_form(),
            Self::StringLiteralLongSingleQuote(inner) => inner.lexical_form(),
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StringLiteralQuote<'a> {
    pub string: Cow<'a, str>,
}

impl<'a> StringLiteralQuote<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(Self::parse_str, |string| Self {
            string: Cow::Borrowed(string),
        })(input)
    }

    fn parse_str<E: NomParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
        string_literal_inner(i, 1, |s| s.starts_with('"'), |s| s.find('"'))
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> impl SerializeFn<W> + 'a {
        cf_tuple((cf_string("\""), cf_string(&subject.string), cf_string("\"")))
    }

    pub fn lexical_form(&self) -> Result<String, &str> {
        unescape(&self.string.as_ref())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StringLiteralSingleQuote<'a> {
    pub string: Cow<'a, str>,
}

impl<'a> StringLiteralSingleQuote<'a> {
    fn parse<E>(input: &'a str) -> IResult<&'a str, Self, E>
    where
        E: NomParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
    {
        map(Self::parse_str, |string| Self {
            string: Cow::Borrowed(string),
        })(input)
    }

    /// Inner logic should be identical to [StringLiteralQuote::parse_str].
    fn parse_str<E: NomParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
        string_literal_inner(i, 1, |s| s.starts_with('\''), |s| s.find('\''))
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> impl SerializeFn<W> + 'a {
        cf_tuple((cf_string("\'"), cf_string(&subject.string), cf_string("\'")))
    }

    pub fn lexical_form(&self) -> Result<String, &str> {
        unescape(&self.string.as_ref())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
        string_literal_inner(i, 3, |s| s.starts_with("\"\"\""), |s| s.find("\"\"\""))
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> impl SerializeFn<W> + 'a {
        cf_tuple((
            cf_string("\"\"\""),
            cf_string(&subject.string),
            cf_string("\"\"\""),
        ))
    }

    pub fn lexical_form(&self) -> Result<String, &str> {
        unescape(&self.string.as_ref())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
        string_literal_inner(i, 3, |s| s.starts_with("'''"), |s| s.find("'''"))
    }

    fn gen<W: Write + 'a>(subject: &'a Self) -> impl SerializeFn<W> + 'a {
        cf_tuple((
            cf_string("'''"),
            cf_string(&subject.string),
            cf_string("'''"),
        ))
    }

    pub fn lexical_form(&self) -> Result<String, &str> {
        unescape(&self.string.as_ref())
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
    use pretty_assertions::assert_eq;

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
                    whitespace: Cow::Borrowed("\n")
                }
            )),
            Whitespace::parse::<VerboseError<&str>>("\n.")
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
    fn parse_verb() {
        assert_eq!(
            Ok((
                "",
                Verb::IRI(IRI::IRIReference(IRIReference {
                    iri: Cow::Borrowed("http://example.com/ontology")
                }))
            )),
            Verb::parse::<VerboseError<&str>>("<http://example.com/ontology>")
        );
        assert_eq!(Ok(("", Verb::A)), Verb::parse::<VerboseError<&str>>("a"));
        assert_eq!(
            Ok((
                "",
                Verb::IRI(IRI::PrefixedName(PrefixedName {
                    prefix: Some(Cow::Borrowed("abc")),
                    name: Some(Cow::Borrowed("def"))
                }))
            )),
            Verb::parse::<VerboseError<&str>>("abc:def")
        );
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
    fn parse_blank_node() {
        assert_eq!(
            Ok((
                "",
                BlankNode::Labeled(BlankNodeLabel {
                    label: Cow::Borrowed("blankNode")
                })
            )),
            BlankNode::parse::<VerboseError<&str>>(r#"_:blankNode"#)
        );
        // "The characters _ and digits may appear anywhere in a blank node label."
        assert_eq!(
            Ok((
                "",
                BlankNode::Labeled(BlankNodeLabel {
                    label: Cow::Borrowed("_blank_Node_")
                })
            )),
            BlankNode::parse::<VerboseError<&str>>(r#"_:_blank_Node_"#)
        );
        assert_eq!(
            Ok((
                "",
                BlankNode::Labeled(BlankNodeLabel {
                    label: Cow::Borrowed("1blank1Node1")
                })
            )),
            BlankNode::parse::<VerboseError<&str>>(r#"_:1blank1Node1"#)
        );
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
                        (
                            None,
                            Object::IRI(IRI::PrefixedName(PrefixedName {
                                prefix: None,
                                name: Some("Entity1".into())
                            })),
                            Some(Whitespace::space())
                        ),
                        (
                            None,
                            Object::IRI(IRI::PrefixedName(PrefixedName {
                                prefix: None,
                                name: Some("Entity2".into())
                            })),
                            Some(Whitespace::space())
                        ),
                        (
                            None,
                            Object::IRI(IRI::PrefixedName(PrefixedName {
                                prefix: None,
                                name: Some("Entity3".into())
                            })),
                            None
                        ),
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
                        (
                            Some(Whitespace::space()),
                            Object::IRI(IRI::PrefixedName(PrefixedName {
                                prefix: None,
                                name: Some("Entity1".into())
                            })),
                            Some(Whitespace {
                                whitespace: Cow::Borrowed(" \n")
                            })
                        ),
                        (
                            None,
                            Object::IRI(IRI::PrefixedName(PrefixedName {
                                prefix: None,
                                name: Some("Entity2".into())
                            })),
                            Some(Whitespace::space())
                        ),
                        (
                            None,
                            Object::IRI(IRI::PrefixedName(PrefixedName {
                                prefix: None,
                                name: Some("Entity3".into())
                            })),
                            Some(Whitespace::space())
                        ),
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
                    (
                        None,
                        Object::IRI(IRI::PrefixedName(PrefixedName {
                            prefix: None,
                            name: Some("Entity1".into()),
                        })),
                        Some(Whitespace::space()),
                    ),
                    (
                        None,
                        Object::IRI(IRI::PrefixedName(PrefixedName {
                            prefix: None,
                            name: Some("Entity2".into()),
                        })),
                        Some(Whitespace::space()),
                    ),
                    (
                        None,
                        Object::IRI(IRI::PrefixedName(PrefixedName {
                            prefix: None,
                            name: Some("Entity3".into()),
                        })),
                        None,
                    ),
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
    fn parse_object_list() {
        // Single object
        assert_eq!(
            Ok((
                "",
                ObjectList {
                    list: vec![(
                        None,
                        None,
                        Object::IRI(IRI::IRIReference(IRIReference {
                            iri: Cow::Borrowed("http://example.org/#green-goblin")
                        }))
                    )]
                },
            )),
            ObjectList::parse::<VerboseError<&str>>("<http://example.org/#green-goblin>")
        );
        assert_eq!(
            Ok((
                "",
                ObjectList {
                    list: vec![(
                        None,
                        Some(Whitespace::space()),
                        Object::IRI(IRI::IRIReference(IRIReference {
                            iri: Cow::Borrowed("http://example.org/#green-goblin")
                        }))
                    )]
                },
            )),
            ObjectList::parse::<VerboseError<&str>>(" <http://example.org/#green-goblin>")
        );
        // Two objects
        assert_eq!(
            Ok((
                "",
                ObjectList {
                    list: vec![
                        (
                            None,
                            None,
                            Object::IRI(IRI::IRIReference(IRIReference {
                                iri: Cow::Borrowed("http://example.org/#green-goblin")
                            }))
                        ),
                        (
                            Some(Whitespace {
                                whitespace: Cow::Borrowed(" ")
                            }),
                            Some(Whitespace {
                                whitespace: Cow::Borrowed("  ")
                            }),
                            Object::IRI(IRI::IRIReference(IRIReference {
                                iri: Cow::Borrowed("http://example.org/#blue-goblin")
                            }))
                        )
                    ]
                },
            )),
            ObjectList::parse::<VerboseError<&str>>(
                "<http://example.org/#green-goblin> ,  <http://example.org/#blue-goblin>"
            )
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
                            Whitespace::space(),
                            IRI::IRIReference(IRIReference {
                                iri: Cow::Borrowed(
                                    "http://www.perceive.net/schemas/relationship/enemyOf"
                                )
                            })
                            .into(),
                            ObjectList {
                                list: vec![(
                                    None,
                                    Some(Whitespace::space()),
                                    Object::IRI(IRI::IRIReference(IRIReference {
                                        iri: Cow::Borrowed("http://example.org/#green-goblin")
                                    }))
                                )]
                            },
                            None,
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
                    string: Cow::Borrowed(r#"Dwayne \"The Rock\" Johnson"#),
                }
            )),
            StringLiteralQuote::parse::<VerboseError<&str>>(r#""Dwayne \"The Rock\" Johnson""#)
        );
        assert_eq!(
            r#"Dwayne "The Rock" Johnson"#,
            StringLiteralQuote::parse::<VerboseError<&str>>(r#""Dwayne \"The Rock\" Johnson""#)
                .unwrap()
                .1
                .lexical_form()
                .unwrap()
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
    fn parse_string_literal_quote_special_chars() {
        assert_eq!(
            Ok((
                "",
                StringLiteralQuote {
                    string: Cow::Borrowed("test® Group GmbH"),
                }
            )),
            StringLiteralQuote::parse::<VerboseError<&str>>(r#""test® Group GmbH""#)
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
                                Whitespace {
                                    whitespace: Cow::Borrowed("\n                        ")
                                },
                                IRI::PrefixedName(PrefixedName {
                                    prefix: Some("ex".into()),
                                    name: Some("fullname".into())
                                })
                                .into(),
                                ObjectList {
                                    list: vec![(
                                        None,
                                        Some(Whitespace::space()),
                                        Object::Literal(Literal::RDFLiteral(RDFLiteral {
                                            string: TurtleString::StringLiteralQuote(
                                                StringLiteralQuote {
                                                    string: "Dave Beckett".into()
                                                }
                                            ),
                                            language_tag: None,
                                            iri: None
                                        }))
                                    )]
                                },
                                None,
                            ),
                            (
                                Whitespace {
                                    whitespace: Cow::Borrowed("\n                        ")
                                },
                                IRI::PrefixedName(PrefixedName {
                                    prefix: Some("ex".into()),
                                    name: Some("homePage".into())
                                })
                                .into(),
                                ObjectList {
                                    list: vec![(
                                        None,
                                        Some(Whitespace::space()),
                                        Object::IRI(IRI::IRIReference(IRIReference {
                                            iri: "http://purl.org/net/dajobe/".into()
                                        }))
                                    )]
                                },
                                None,
                            )
                        ]
                    },
                    trailing_whitespace: Some(Whitespace {
                        whitespace: Cow::Borrowed("\n                      ")
                    })
                },
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
    fn parse_predicate_property_list() {
        assert_eq!(
            Ok((
                "",
                PredicateObjectList {
                    list: vec![
                        (
                            Whitespace::space(),
                            IRI::PrefixedName(PrefixedName {
                                prefix: Some("ex".into()),
                                name: Some("fullname".into())
                            })
                            .into(),
                            ObjectList {
                                list: vec![(
                                    None,
                                    Some(Whitespace::space()),
                                    Object::Literal(Literal::RDFLiteral(RDFLiteral {
                                        string: TurtleString::StringLiteralQuote(
                                            StringLiteralQuote {
                                                string: "Dave Beckett".into()
                                            }
                                        ),
                                        language_tag: None,
                                        iri: None
                                    }))
                                )]
                            },
                            None,
                        ),
                        (
                            Whitespace {
                                whitespace: Cow::Borrowed("\n")
                            },
                            IRI::PrefixedName(PrefixedName {
                                prefix: Some("ex".into()),
                                name: Some("homePage".into())
                            })
                            .into(),
                            ObjectList {
                                list: vec![(
                                    None,
                                    Some(Whitespace::space()),
                                    Object::IRI(IRI::IRIReference(IRIReference {
                                        iri: "http://purl.org/net/dajobe/".into()
                                    }))
                                )]
                            },
                            None,
                        )
                    ]
                }
            )),
            PredicateObjectList::parse::<VerboseError<&str>>(
                " ex:fullname \"Dave Beckett\";\nex:homePage <http://purl.org/net/dajobe/>"
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
    fn parse_numeric_literal() {
        assert_eq!(
            Ok((
                "",
                Integer {
                    sign: None,
                    number_literal: Cow::Borrowed("12345")
                }
            )),
            Integer::parse::<VerboseError<&str>>("12345")
        );
        assert_eq!(
            Ok((
                "",
                NumericLiteral::Integer(Integer {
                    sign: None,
                    number_literal: Cow::Borrowed("12345")
                })
            )),
            NumericLiteral::parse::<VerboseError<&str>>("12345")
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
                        Whitespace::space(),
                        IRI::PrefixedName(PrefixedName {
                            prefix: Some(Cow::Borrowed("rdf")),
                            name: Some(Cow::Borrowed("type")),
                        })
                        .into(),
                        ObjectList {
                            list: vec![(
                                None,
                                Some(Whitespace::space()),
                                Object::IRI(IRI::PrefixedName(PrefixedName {
                                    prefix: Some(Cow::Borrowed("owl")),
                                    name: Some(Cow::Borrowed("Ontology")),
                                })),
                            )],
                        },
                        None,
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
                            whitespace: Cow::Borrowed("\n"),
                        }),
                        prefix: None,
                        iri: IRIReference {
                            iri: Cow::Borrowed("http://example.com/ontology"),
                        },
                    })),
                    Statement::Directive(Directive::Prefix(PrefixDirective {
                        leading_whitespace: Some(Whitespace {
                            whitespace: Cow::Borrowed("\n"),
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
