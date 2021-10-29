Harriet
===========================

[<img alt="github" src="https://img.shields.io/badge/github-field33/harriet-8da0cb?style=for-the-badge&labelColor=555555&logo=github" height="20">](https://github.com/field33/harriet)
[<img alt="crates.io" src="https://img.shields.io/crates/v/harriet.svg?style=for-the-badge&color=fc8d62&logo=rust" height="20">](https://crates.io/crates/harriet)
[<img alt="docs.rs" src="https://img.shields.io/badge/docs.rs-harriet-66c2a5?style=for-the-badge&labelColor=555555&logoColor=white&logo=data:image/svg+xml;base64,PHN2ZyByb2xlPSJpbWciIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIgdmlld0JveD0iMCAwIDUxMiA1MTIiPjxwYXRoIGZpbGw9IiNmNWY1ZjUiIGQ9Ik00ODguNiAyNTAuMkwzOTIgMjE0VjEwNS41YzAtMTUtOS4zLTI4LjQtMjMuNC0zMy43bC0xMDAtMzcuNWMtOC4xLTMuMS0xNy4xLTMuMS0yNS4zIDBsLTEwMCAzNy41Yy0xNC4xIDUuMy0yMy40IDE4LjctMjMuNCAzMy43VjIxNGwtOTYuNiAzNi4yQzkuMyAyNTUuNSAwIDI2OC45IDAgMjgzLjlWMzk0YzAgMTMuNiA3LjcgMjYuMSAxOS45IDMyLjJsMTAwIDUwYzEwLjEgNS4xIDIyLjEgNS4xIDMyLjIgMGwxMDMuOS01MiAxMDMuOSA1MmMxMC4xIDUuMSAyMi4xIDUuMSAzMi4yIDBsMTAwLTUwYzEyLjItNi4xIDE5LjktMTguNiAxOS45LTMyLjJWMjgzLjljMC0xNS05LjMtMjguNC0yMy40LTMzLjd6TTM1OCAyMTQuOGwtODUgMzEuOXYtNjguMmw4NS0zN3Y3My4zek0xNTQgMTA0LjFsMTAyLTM4LjIgMTAyIDM4LjJ2LjZsLTEwMiA0MS40LTEwMi00MS40di0uNnptODQgMjkxLjFsLTg1IDQyLjV2LTc5LjFsODUtMzguOHY3NS40em0wLTExMmwtMTAyIDQxLjQtMTAyLTQxLjR2LS42bDEwMi0zOC4yIDEwMiAzOC4ydi42em0yNDAgMTEybC04NSA0Mi41di03OS4xbDg1LTM4Ljh2NzUuNHptMC0xMTJsLTEwMiA0MS40LTEwMi00MS40di0uNmwxMDItMzguMiAxMDIgMzguMnYuNnoiPjwvcGF0aD48L3N2Zz4K" height="20">](https://docs.rs/harriet)

Harriet is a parser for the [Turtle][turtle] document format, which is a format
"that allows an [RDF][rdf-wiki] graph to be completely written in a compact and natural text form".

## Installation

Add harriet to your project (assuming [cargo-edit][cargo-edit] is installed) via:
```bash
cargo add harriet
```

## Goals

- Provide a direct 1:1 AST mapping of a Turtle document
- Provide abilities to easily navigate and edit the AST
- Preserve the format: Parsing a document and then writing it, should yield the input document, including all whitespace and comments

## Non-Goals

- The main `harriet` crate doesn't aim to produce a RDF graph via interpreting the contents of a document.
  This is left to optional crates, that convert the AST into a specific RDF representation (e.g. `rdftk`)

## Contributing

We are happy about any contributions!

To get started you can take a look at our [Github issues](https://github.com/field33/harriet/issues).

Unless you explicitly state otherwise, any contribution intentionally
submitted for inclusion in the work by you, as defined in the Apache-2.0
license, shall be dual licensed as below, without any additional terms or
conditions.

## License

Licensed under either of

* Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
* MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

[turtle]: https://www.w3.org/TR/turtle/
[rdf-wiki]: https://en.wikipedia.org/wiki/Resource_Description_Framework
[cargo-edit]: https://github.com/killercup/cargo-edit