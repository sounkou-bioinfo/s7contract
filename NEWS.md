# s7contract (development version)

# s7contract 0.2.3

* Source builds exclude local `test.arrows` data files.

# s7contract 0.2.2

* Union requirements cover every concrete dispatch signature. Checked calls
  preserve lexical generic defaults, shared argument promises, and return
  visibility.

* Trait registrations use descriptor identity. Deserialized descriptors require
  registration in the current session; register union targets one member at a
  time.

* Added generative laws with `new_law()`, structured results from `check_law()`,
  and one-result tinytest integration through `expect_law()`.

* Generators support integers, finite doubles, constants, mapping, independent
  products, and nested vectors. `gen_sample()` selects positions without
  replacement; `gen_subsequence()` preserves source order.

* Added dependent generation with `gen_bind()`, weighted `gen_element()` and
  `gen_choice()`, and size control with `gen_sized()` and `gen_resize()`.
  Dependent shrinks rebuild valid inputs with a captured local seed.

* Added `gen_recursive()` for nested structures, `gen_example()` for reproducible
  inspection, and `gen_no_shrink()` to hold values fixed during shrinking.

* Shrinking constructs candidates lazily and preserves generator constraints.
  Results report the last accepted failing candidate, `shrink_status`, and
  `shrink_condition`. Errors during shrinking preserve the original and last
  failing examples. Vector shrinking removes chunks before shrinking elements;
  double shrinking moves toward an explicit origin until rounding stops progress.

* Added case classification and minimum observed coverage to `new_law()` and
  `new_state_law()`. Results retain counts and proportions; unmet requirements
  produce `insufficient_coverage` after the requested passing cases. Discards,
  errors, and shrink evaluations do not contribute to coverage.

* Added sequential protocol laws with `new_command()`, `gen_commands()`, and
  `new_state_law()`. Commands run against a reference model with fresh fixtures,
  dependency-preserving shrinking, and original and reduced failure traces.
  The key/value example tests two S7 implementations and a faulty reset method.

* Standardized law runs on Mersenne-Twister with Inversion normals and Rejection
  sampling, and recorded run parameters for replay. Box-Muller callers are
  rejected before changing RNG state because its cached normal draw cannot be
  restored through R's public API.

* Integer counts, bounds, and seeds are validated before conversion. Discarded
  cases advance generator size and count toward the discard budget.

* Added reusable laws for numeric vectors and `ReadDepth`, generated Maybe
  values and functions, UTF-8 store keys, and whole-day calendar intervals.
  Examples demonstrate faults in slicing, rounding, key truncation, reset,
  monad composition, and endpoint inclusion.

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
