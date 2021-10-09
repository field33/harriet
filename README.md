# Harriet

Harriet is a parser for the [Turtle][turtle] document format, which is a format
"that allows an RDF graph to be completely written in a compact and natural text form".

## Goals

- Provide a direct 1:1 AST mapping of a Turtle document
- Provide abilities to easily edit the AST
- Preserve the format: Parsing a document and then writing it, should yield the input document, including all whitespace and comments

## Non-Goals

- The main `harriet` crate doesn't aim to produce a RDF graph via interpreting the contents of a document.
  This is left to optional crates, that convert the AST into a specific RDF representation (e.g. `rdftk`)

[turtle]: https://www.w3.org/TR/turtle/