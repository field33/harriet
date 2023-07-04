use crate::{
    BlankNode, BlankNodeLabel, Collection, Directive, IRIReference, Literal, NumericLiteral,
    Object, PredicateObjectList, Statement, Subject, Triples, TurtleDocument, Verb, IRI,
};
use anyhow::{anyhow, bail, Context, Error};
use either::Either;
use oxiri::Iri;
use snowflake::ProcessUniqueId;
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
    base_uri: Option<Iri<String>>,
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
                Statement::Directive(directive) => state.apply_directive(directive)?,
                Statement::Triples(stmt_triples) => {
                    // Reset the subject; Shouldn't be necessary, but maybe keeps a few bugs out.
                    state.current_subject = None;
                    state.current_predicate = None;
                    match stmt_triples {
                        Triples::Labeled(_, subject, predicate_object_list) => {
                            match subject {
                                Subject::IRI(iri) => state
                                    .set_current_subject(RdfSubject::IRI(state.convert_iri(iri)?)),
                                Subject::BlankNode(blank_node) => {
                                    let current_blank_node = match blank_node {
                                        BlankNode::Anonymous(_) => state.allocate_blank_node(),
                                        BlankNode::Labeled(labled) => {
                                            state.allocate_labeled_blank_node(labled)
                                        }
                                    };
                                    state.set_current_subject(RdfSubject::BlankNode(
                                        current_blank_node,
                                    ))
                                }
                                Subject::Collection(_) => {
                                    // TODO
                                    Err(anyhow!(
                                        "Collections are not supported in TripleProducer yet."
                                    ))?;
                                }
                            }
                            Self::produce_predicate_object_list(
                                &mut state,
                                &mut triples,
                                predicate_object_list,
                            )?;
                        }
                        Triples::Blank(_, _, _) => {
                            // TODO
                            bail!("Production of triple statements with blankNodePropertyList as subject not supported yet.")
                        }
                    }
                }
            }
        }

        Ok(triples)
    }

    fn produce_predicate_object_list<'a>(
        state: &mut ProducerState<'a>,
        mut triples: &mut Vec<RdfTriple<'a>>,
        predicate_object_list: PredicateObjectList<'a>,
    ) -> Result<(), Error> {
        for (_, verb, object_list, _) in predicate_object_list.list {
            state.set_current_predicate(state.convert_verb(verb)?);
            for (_, _, object) in object_list.list {
                let rdf_object = Self::produce_object(state, triples, object)?;
                state.produce_triple(&mut triples, rdf_object)?;
            }
        }
        Ok(())
    }

    fn produce_object<'a>(
        state: &mut ProducerState<'a>,
        triples: &mut Vec<RdfTriple<'a>>,
        object: Object<'a>,
    ) -> Result<RdfObject<'a>, Error> {
        Ok(match object {
            Object::IRI(iri) => RdfObject::IRI(state.convert_iri(iri)?),
            Object::Literal(literal) => match literal {
                Literal::RDFLiteral(rdf_literal) => RdfObject::Literal(RdfLiteral {
                    lexical_form: Cow::Owned(
                        rdf_literal
                            .string
                            .lexical_form()
                            .map_err(|e| anyhow!(e.to_owned()))
                            .unwrap(),
                    ),
                    datatype_iri: rdf_literal
                        .iri
                        .map(|n| state.convert_iri(n))
                        .transpose()?
                        .or(Some(iri_constants::XSD_STRING)),
                    language_tag: rdf_literal.language_tag,
                }),
                Literal::BooleanLiteral(boolean_literal) => RdfObject::Literal(RdfLiteral {
                    lexical_form: Cow::Borrowed(match boolean_literal.bool {
                        true => "true",
                        false => "false",
                    }),
                    datatype_iri: Some(iri_constants::XSD_BOOLEAN),
                    language_tag: None,
                }),
                Literal::NumericLiteral(numeric_literal) => {
                    RdfObject::Literal(match numeric_literal {
                        NumericLiteral::Integer(integer_literal) => RdfLiteral {
                            lexical_form: integer_literal.lexical_form().into(),
                            datatype_iri: Some(iri_constants::XSD_INTEGER),
                            language_tag: None,
                        },
                        NumericLiteral::Decimal(decimal_literal) => RdfLiteral {
                            lexical_form: decimal_literal.lexical_form().into(),
                            datatype_iri: Some(iri_constants::XSD_DECIMAL),
                            language_tag: None,
                        },
                        NumericLiteral::Double(double_literal) => RdfLiteral {
                            lexical_form: double_literal.lexical_form().into(),
                            datatype_iri: Some(iri_constants::XSD_DOUBLE),
                            language_tag: None,
                        },
                    })
                }
            },
            Object::BlankNode(blank_node) => {
                let current_blank_node = match blank_node {
                    BlankNode::Anonymous(_) => state.allocate_blank_node(),
                    BlankNode::Labeled(labled) => state.allocate_labeled_blank_node(labled),
                };
                RdfObject::BlankNode(current_blank_node)
            }
            Object::Collection(collection) => {
                match Self::produce_collection(state, triples, collection)? {
                    Either::Left(iri) => RdfObject::IRI(iri),
                    Either::Right(blank_node) => RdfObject::BlankNode(blank_node),
                }
            }
            Object::BlankNodePropertyList(blank_node_property_list) => {
                let mut sub_triples = vec![];

                let blank_node = state.allocate_blank_node();

                let stashed_subject = state.current_subject.clone();
                let stashed_predicate = state.current_predicate.clone();

                state.set_current_subject(RdfSubject::BlankNode(blank_node.clone()));

                Self::produce_predicate_object_list(
                    state,
                    &mut sub_triples,
                    blank_node_property_list.list,
                )?;

                state.current_subject = stashed_subject;
                state.current_predicate = stashed_predicate;

                triples.append(&mut sub_triples);

                RdfObject::BlankNode(blank_node.clone())
            }
        })
    }

    /// See <https://www.w3.org/TR/turtle/#collection>
    fn produce_collection<'a>(
        state: &mut ProducerState<'a>,
        triples: &mut Vec<RdfTriple<'a>>,
        collection: Collection<'a>,
    ) -> Result<Either<RdfIri<'a>, RdfBlankNode>, Error> {
        let stashed_subject = state.current_subject.clone();
        let stashed_predicate = state.current_predicate.clone();

        let return_node = match collection.list.len() {
            0 => Either::Left(iri_constants::RDF_NIL),
            _ => {
                let mut first_blank_node: Option<RdfBlankNode> = None;

                let mut previous_blank_node = None;
                for (_, object, _) in collection.list {
                    let current_blank_node = state.allocate_blank_node();
                    if matches!(first_blank_node, None) {
                        first_blank_node = Some(current_blank_node.clone());
                    }
                    if let Some(_) = previous_blank_node {
                        state.set_current_predicate(RdfPredicate::IRI(iri_constants::RDF_REST));
                        state.produce_triple(
                            triples,
                            RdfObject::BlankNode(current_blank_node.clone()),
                        )?;
                    }

                    state.set_current_subject(RdfSubject::BlankNode(current_blank_node.clone()));
                    state.set_current_predicate(RdfPredicate::IRI(iri_constants::RDF_FIRST));

                    let rdf_object = Self::produce_object(state, triples, object)?;
                    state.produce_triple(triples, rdf_object)?;

                    previous_blank_node = Some(current_blank_node.clone());
                }
                state.set_current_predicate(RdfPredicate::IRI(iri_constants::RDF_REST));
                state.produce_triple(triples, RdfObject::IRI(iri_constants::RDF_NIL))?;

                Either::Right(
                    first_blank_node
                        .expect("Tried to produce collection without returning a blank node"),
                )
            }
        };

        state.current_subject = stashed_subject;
        state.current_predicate = stashed_predicate;

        Ok(return_node)
    }
}

impl<'a> ProducerState<'a> {
    fn set_base_uri<U: Into<String>>(&mut self, base_uri: U) -> Result<(), Error> {
        self.base_uri = Some(Iri::parse(base_uri.into())?);
        Ok(())
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

    fn apply_directive(&mut self, directive: Directive) -> Result<(), Error> {
        match directive {
            Directive::Base(base_dir) => self.set_base_uri(base_dir.iri.iri)?,
            Directive::SparqlBase(base_dir) => self.set_base_uri(base_dir.iri.iri)?,
            Directive::Prefix(prefix_dir) => self.set_namespace(
                prefix_dir.prefix.unwrap_or(Cow::Borrowed("")),
                prefix_dir.iri.iri,
            ),
            Directive::SparqlPrefix(prefix_dir) => self.set_namespace(
                prefix_dir.prefix.unwrap_or(Cow::Borrowed("")),
                prefix_dir.iri.iri,
            ),
        }
        Ok(())
    }

    fn allocate_labeled_blank_node(&mut self, blank_node: BlankNodeLabel) -> RdfBlankNode {
        // PERFORMANCE: May be optimized
        let possible_new_blank_node = self.allocate_blank_node();
        self.blank_node_labels
            .entry(blank_node.label.to_string())
            .or_insert(possible_new_blank_node)
            .to_owned()
    }

    fn allocate_blank_node(&self) -> RdfBlankNode {
        RdfBlankNode::new()
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
                iri: self.resolve_iri(iri_ref)?,
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
            Verb::A => RdfPredicate::IRI(iri_constants::RDF_TYPE),
            Verb::IRI(iri) => RdfPredicate::IRI(self.convert_iri(iri)?),
        })
    }

    // See https://www.ietf.org/rfc/rfc3986.txt - Section 5.2 - Requires parsing the base and the iri
    fn resolve_iri(&self, iri_ref: IRIReference<'a>) -> Result<Cow<'a, str>, Error> {
        Ok(match &self.base_uri {
            None => iri_ref.iri,
            Some(base) => Cow::Owned(base.resolve(iri_ref.iri.as_ref())?.as_str().to_string()),
        })
    }

    fn resolve_prefix(&self, prefix: Option<&str>) -> Result<&String, Error> {
        self.namespaces.get(prefix.unwrap_or("")).context(format!(
            "Unable to resolve prefix `{prefix}`",
            prefix = prefix.unwrap_or("")
        ))
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
    pub datatype_iri: Option<RdfIri<'a>>,
    pub language_tag: Option<Cow<'a, str>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct RdfBlankNode {
    pub internal_id: ProcessUniqueId,
}

impl RdfBlankNode {
    pub fn new() -> Self {
        Self {
            internal_id: ProcessUniqueId::new(),
        }
    }
}

mod iri_constants {
    use crate::triple_production::RdfIri;
    use std::borrow::Cow;

    pub const RDF_TYPE: RdfIri = RdfIri {
        iri: Cow::Borrowed("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
    };

    pub const RDF_FIRST: RdfIri = RdfIri {
        iri: Cow::Borrowed("http://www.w3.org/1999/02/22-rdf-syntax-ns#first"),
    };

    pub const RDF_REST: RdfIri = RdfIri {
        iri: Cow::Borrowed("http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"),
    };

    pub const RDF_NIL: RdfIri = RdfIri {
        iri: Cow::Borrowed("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"),
    };

    pub const XSD_STRING: RdfIri = RdfIri {
        iri: Cow::Borrowed("http://www.w3.org/2001/XMLSchema#string"),
    };

    pub const XSD_BOOLEAN: RdfIri = RdfIri {
        iri: Cow::Borrowed("http://www.w3.org/2001/XMLSchema#boolean"),
    };

    pub const XSD_INTEGER: RdfIri = RdfIri {
        iri: Cow::Borrowed("http://www.w3.org/2001/XMLSchema#integer"),
    };

    pub const XSD_DECIMAL: RdfIri = RdfIri {
        iri: Cow::Borrowed("http://www.w3.org/2001/XMLSchema#decimal"),
    };

    pub const XSD_DOUBLE: RdfIri = RdfIri {
        iri: Cow::Borrowed("http://www.w3.org/2001/XMLSchema#double"),
    };
}
