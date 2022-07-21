use crate::{
    Directive, IRIReference, Object, Statement, Subject, Triples, TurtleDocument, Verb, IRI,
};
use anyhow::{anyhow, Context, Error};
use std::borrow::Cow;
use std::collections::HashMap;

/// A triple producer, produces is able to produce a stream of RDF Triples based on a parsed Turtle document.
///
/// See <https://www.w3.org/TR/turtle/#sec-parsing>
#[derive(Debug)]
pub struct TripleProducer;

#[derive(Debug, Default)]
struct ProducerState<'a> {
    /// Tracks the current base URI.
    base_uri: Option<String>,
    /// Tracks the current namespaces (= prefix to prefix-URI mapping).
    namespaces: HashMap<String, String>,
    /// Tracks the current set of blank node labels.
    blank_node_labels: HashMap<String, RdfBlankNode>,
    /// Tracks the current production subject.
    current_subject: Option<RdfSubject<'a>>,
    /// Tracks the current production predicate.
    current_predicate: Option<RdfPredicate<'a>>,
}

impl TripleProducer {
    pub fn produce_for_document<'a>(
        document: &TurtleDocument<'a>,
    ) -> Result<Vec<RdfTriple<'a>>, Error> {
        let mut state = ProducerState::default();
        let mut triples: Vec<RdfTriple> = vec![];

        for statement in document.statements.clone() {
            match statement {
                Statement::Directive(directive) => state.apply_directive(directive),
                Statement::Triples(stmt_triples) => {
                    // Reset the subject; Shouldn't be necessary, but maybe keeps a few bugs out.
                    state.current_subject = None;
                    state.current_predicate = None;
                    match stmt_triples {
                        Triples::Labeled(_, subject, predicate_object_list) => {
                            match subject {
                                Subject::IRI(iri) => state
                                    .set_current_subject(RdfSubject::IRI(state.convert_iri(iri)?)),
                                Subject::BlankNode(_) => {
                                    // TODO
                                    Err(anyhow!(
                                        "BlankNodes are not supported in TripleProducer yet."
                                    ))?;
                                }
                                Subject::Collection(_) => {
                                    // TODO
                                    Err(anyhow!(
                                        "Collections are not supported in TripleProducer yet."
                                    ))?;
                                }
                            }
                            for (_, verb, object_list, _) in predicate_object_list.list {
                                state.set_current_predicate(state.convert_verb(verb)?);
                                for (_, _, object) in object_list.list {
                                    match object {
                                        Object::IRI(iri) => state.produce_triple(
                                            &mut triples,
                                            RdfObject::IRI(state.convert_iri(iri)?),
                                        )?,
                                        Object::Literal(_) => {}
                                        Object::BlankNode(_) => {}
                                        Object::Collection(_) => {}
                                        Object::BlankNodePropertyList(_) => {}
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        Ok(triples)
    }
}

impl<'a> ProducerState<'a> {
    fn set_base_uri<U: Into<String>>(&mut self, base_uri: U) {
        self.base_uri = Some(base_uri.into());
    }

    fn set_namespace<P: Into<String>, U: Into<String>>(&mut self, prefix: P, uri: U) {
        self.namespaces.insert(prefix.into(), uri.into());
    }

    fn set_current_subject<S: Into<RdfSubject<'a>>>(&mut self, subject: S) {
        self.current_subject = Some(subject.into())
    }

    fn set_current_predicate<P: Into<RdfPredicate<'a>>>(&mut self, predicate: P) {
        self.current_predicate = Some(predicate.into())
    }

    fn apply_directive(&mut self, directive: Directive) {
        match directive {
            Directive::Base(base_dir) => self.set_base_uri(base_dir.iri.iri),
            Directive::SparqlBase(base_dir) => self.set_base_uri(base_dir.iri.iri),
            Directive::Prefix(prefix_dir) => self.set_namespace(
                prefix_dir.prefix.unwrap_or(Cow::Borrowed("")),
                prefix_dir.iri.iri,
            ),
            Directive::SparqlPrefix(prefix_dir) => self.set_namespace(
                prefix_dir.prefix.unwrap_or(Cow::Borrowed("")),
                prefix_dir.iri.iri,
            ),
        }
    }

    fn produce_triple<O: Into<RdfObject<'a>>>(
        &self,
        triples: &mut Vec<RdfTriple<'a>>,
        object: O,
    ) -> Result<(), Error> {
        let triple = RdfTriple {
            subject: self
                .current_subject
                .clone()
                .context("Trying to produce triple without current subject")?,
            predicate: self
                .current_predicate
                .clone()
                .context("Trying to produce triple without current predicate")?,
            object: object.into(),
        };
        triples.push(triple);
        Ok(())
    }

    fn convert_iri(&self, iri: IRI<'a>) -> Result<RdfIri<'a>, Error> {
        Ok(match iri {
            IRI::IRIReference(iri_ref) => RdfIri {
                iri: self.resolve_iri(iri_ref),
            },
            IRI::PrefixedName(prefixed_name) => RdfIri {
                iri: format!(
                    "{prefix}{local_name}",
                    prefix = self.resolve_prefix(prefixed_name.prefix.as_deref())?,
                    local_name = prefixed_name
                        .name
                        .context("Empty local_name part of PrefixedName")?
                )
                .into(),
            },
        })
    }

    fn convert_verb(&self, verb: Verb<'a>) -> Result<RdfPredicate<'a>, Error> {
        Ok(match verb {
            Verb::A => {
                // TODO: implement
                Err(anyhow!("Conversion of `a` verb not implemented yet."))?
            }
            Verb::IRI(iri) => RdfPredicate::IRI(self.convert_iri(iri)?),
        })
    }

    // TODO: resolution of relative IRIs relative to the base
    // See https://www.ietf.org/rfc/rfc3986.txt - Section 5.2 - Requires parsing the base and the iri
    fn resolve_iri(&self, iri_ref: IRIReference<'a>) -> Cow<'a, str> {
        match &self.base_uri {
            None => iri_ref.iri,
            Some(base) => format!("{base}{relative_iri}", relative_iri = iri_ref.iri).into(),
        }
    }

    fn resolve_prefix(&self, prefix: Option<&str>) -> Result<&String, Error> {
        self.namespaces
            .get(prefix.unwrap_or(""))
            .context("Unable to resolve prefix `{prefix}`")
    }
}

#[derive(Debug, Clone)]
pub struct RdfTriple<'a> {
    pub subject: RdfSubject<'a>,
    pub predicate: RdfPredicate<'a>,
    pub object: RdfObject<'a>,
}

#[derive(Debug, Clone)]
pub enum RdfSubject<'a> {
    IRI(RdfIri<'a>),
    BlankNode(RdfBlankNode),
}

#[derive(Debug, Clone)]
pub enum RdfPredicate<'a> {
    IRI(RdfIri<'a>),
}

#[derive(Debug, Clone)]
pub enum RdfObject<'a> {
    IRI(RdfIri<'a>),
    BlankNode(RdfBlankNode),
    Literal(RdfLiteral<'a>),
}

#[derive(Debug, Clone)]
pub struct RdfIri<'a> {
    pub iri: Cow<'a, str>,
}

#[derive(Debug, Clone)]
pub struct RdfLiteral<'a> {
    pub lexical_form: Cow<'a, str>,
    pub datatype_iri: Cow<'a, str>,
    pub language_tag: Option<Cow<'a, str>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RdfBlankNode {
    internal_id: u32,
}
