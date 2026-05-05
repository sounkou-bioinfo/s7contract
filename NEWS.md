# s7contract 0.1.0.9000

* Added references to the S7 traits discussion in RConsortium/S7#34.

# s7contract 0.1.0

* Renamed the primary `new_interface()` requirement argument to `generics`; `methods` remains a compatibility alias.
* Tightened interface and trait requirements to require S7 generics created with `S7::new_generic()`.
* Added optional progressive argument and return checks for interface requirements and trait methods, including `with()` and `%::%` evaluation syntax.
* Reworked contract descriptor objects as internal S7 classes throughout.
* Added a Haskell-style `Maybe`/monad dictionary example to the interface and trait vignette.
* Added CRAN-facing vignettes with number-like, vector-like, and bioinformatics container examples.
* Added CRAN submission comments and metadata updates for vignette building.
* Fixed `impl_trait()` so failed S7 method registration no longer leaves a stale trait implementation record.
* Preserved explicit `NULL` associated item values and allowed subtraits to retrieve inherited associated items.
* Added regression tests for trait registration failure and associated metadata edge cases.
* Simplified the README to keep the Go/Rust analogies clearly scoped to runtime S7 helpers.
