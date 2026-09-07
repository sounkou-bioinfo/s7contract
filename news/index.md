# Changelog

## s7contract 0.2.0.9000

- Added case classification and minimum observed coverage to
  [`new_law()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_law.md)
  and
  [`new_state_law()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_state_law.md).
  Results retain counts and proportions; unmet requirements produce
  `insufficient_coverage` after the requested passing cases. Discards,
  errors, and shrink evaluations do not contribute to coverage.

- Added sequential protocol laws with
  [`new_command()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_command.md),
  [`gen_commands()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_commands.md),
  and
  [`new_state_law()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_state_law.md).
  Commands run against a reference model with fresh fixtures,
  dependency-preserving shrinking, and original and reduced failure
  traces. The key/value example tests two S7 implementations and a
  faulty reset method.

- Shortened the introductory vignette around one vector protocol. Moved
  the full vector-law suite and Maybe dictionary into separate articles.

- Added a reusable `VectorLike` law example shared by numeric vectors
  and `ReadDepth`, with a faulty implementation that passes structural
  checks but fails a slicing law. The vignette and tests execute the
  same installed script.

- Clarified how interfaces, traits, checked calls, and generative laws
  describe and test behavioral protocols. Updated the package title to
  reflect that scope.

- Added dependent generation with
  [`gen_bind()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_bind.md),
  weighted
  [`gen_element()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_element.md)
  and
  [`gen_choice()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_element.md),
  and size control with
  [`gen_sized()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_bind.md)
  and
  [`gen_resize()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_bind.md).
  Dependent shrinks rebuild valid inputs with a captured local seed.

- Added
  [`gen_recursive()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_bind.md)
  for structures with decreasing recursive size,
  [`gen_example()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_example.md)
  for reproducible inspection, and
  [`gen_no_shrink()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_example.md).

## s7contract 0.2.0

- Fixed nested vector generator composition. Vector shrinking now
  removes contiguous chunks throughout a vector while preserving its
  length bounds.

- Shrink candidates are constructed and mapped only when visited.
  Evaluation budgets no longer force unused siblings or later product
  components. Custom shrink functions still construct their own
  candidate lists.

- Retained original and last failing examples when shrinking errors or
  warns. Results report `shrink_status` and `shrink_condition`, and
  diagnostics describe the smallest counterexample found rather than
  claiming a global minimum. Generator warnings now produce structured
  runner errors.

- Standardized law runs on Mersenne-Twister with Inversion normals and
  Rejection sampling, and recorded run parameters for replay. Box-Muller
  callers are rejected before changing RNG state because its cached
  normal draw cannot be restored through R’s public API.

- Consolidated integer-bound and named-generator admission checks,
  including independent validation of each scalar bound.

- Documented the relationship to R and Haskell Hedgehog, current
  generator composition, and limits of replay and shrinking.

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
