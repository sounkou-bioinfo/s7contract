# s7contract 0.1.0

* Reworked contract descriptor objects as internal S7 classes throughout.
* Added CRAN-facing vignettes with number-like, vector-like, and bioinformatics container examples.
* Added CRAN submission comments and metadata updates for vignette building.
* Fixed `impl_trait()` so failed S7 method registration no longer leaves a stale trait implementation record.
* Preserved explicit `NULL` associated item values and allowed subtraits to retrieve inherited associated items.
* Added regression tests for trait registration failure and associated metadata edge cases.
* Simplified the README to keep the Go/Rust analogies clearly scoped to runtime S7 helpers.
