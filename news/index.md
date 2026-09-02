# Changelog

## s7contract 0.1.0.9000

- Added S7-backed generative laws with composable generators, integrated
  shrinking, deterministic framework-neutral checks, bounded
  precondition discards, counterexample diagnostics, and one-result
  tinytest integration.

- Discarded cases now advance generator size, accepted shrinks are
  retained at the evaluation budget boundary, and
  [`gen_vector()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_constant.md)
  preserves element-count bounds by treating nonscalar draws as list
  elements and validating atomic element prototypes. Zero shrink budgets
  no longer expand shrink trees, and integer bounds and seeds reject R’s
  reserved missing-integer sentinel.

- Added references to the S7 traits discussion in RConsortium/S7#34.

## s7contract 0.1.0

CRAN release: 2026-05-07

- Renamed the primary
  [`new_interface()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_interface.md)
  requirement argument to `generics`; `methods` remains a compatibility
  alias.
- Tightened interface and trait requirements to require S7 generics
  created with
  [`S7::new_generic()`](https://rconsortium.github.io/S7/reference/new_generic.html).
- Added optional progressive argument and return checks for interface
  requirements and trait methods, including
  [`with()`](https://rdrr.io/r/base/with.html) and `%::%` evaluation
  syntax.
- Reworked contract descriptor objects as internal S7 classes
  throughout.
- Added a Haskell-style `Maybe`/monad dictionary example to the
  interface and trait vignette.
- Added CRAN-facing vignettes with number-like, vector-like, and
  bioinformatics container examples.
- Added CRAN submission comments and metadata updates for vignette
  building.
- Fixed
  [`impl_trait()`](https://sounkou-bioinfo.github.io/s7contract/reference/trait_methods.md)
  so failed S7 method registration no longer leaves a stale trait
  implementation record.
- Preserved explicit `NULL` associated item values and allowed subtraits
  to retrieve inherited associated items.
- Added regression tests for trait registration failure and associated
  metadata edge cases.
- Simplified the README to keep the Go/Rust analogies clearly scoped to
  runtime S7 helpers.
