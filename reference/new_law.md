# Define and check a generative law

A law is a named, universally quantified claim sampled over explicitly
supplied generators. `check_law()` is test-framework neutral and returns
a structured S7 result. `expect_law()` adapts that result to one
tinytest expectation, regardless of how many generated cases were
checked.

## Usage

``` r
new_law(
  name,
  generators,
  holds,
  classify = function(...) character(),
  min_coverage = numeric()
)

assume(condition)

check_law(
  law,
  tests = getOption("s7contract.tests", 100L),
  seed = getOption("s7contract.seed", 1L),
  shrinks = getOption("s7contract.shrinks", 100L),
  discards = getOption("s7contract.discards", 100L),
  max_size = getOption("s7contract.max_size", 100L)
)

format_check_result(x)

expect_law(
  law,
  tests = getOption("s7contract.tests", 100L),
  seed = getOption("s7contract.seed", 1L),
  shrinks = getOption("s7contract.shrinks", 100L),
  discards = getOption("s7contract.discards", 100L),
  max_size = getOption("s7contract.max_size", 100L)
)
```

## Arguments

- name:

  Non-empty description of the law.

- generators:

  Uniquely named non-empty list of generators.

- holds:

  Function accepting the generated arguments and returning one
  non-missing logical value.

- classify:

  Function accepting the same generated arguments as `holds` and
  returning character labels, or `NULL` for none. Duplicate labels count
  once per case; names are ignored. Labels must be non-missing and
  non-empty. The classifier must be deterministic, must not draw random
  numbers, and must not mutate inputs or external state. It is never
  called on shrinks.

- min_coverage:

  Named numeric vector of minimum proportions in `[0, 1]`, with unique
  label names. For example, `c(nonempty = 0.5)` requires at least half
  of accepted generated cases to carry the label `"nonempty"`.

- condition:

  Scalar logical precondition.

- law:

  A law created by `new_law()`.

- tests:

  Number of passing cases required.

- seed:

  Deterministic local random seed. The caller's RNG kind and state are
  restored after the run.

- shrinks:

  Maximum number of candidate shrink evaluations.

- discards:

  Maximum number of discarded generated cases.

- max_size:

  Maximum size passed to generators.

- x:

  A result returned by `check_law()`.

## Value

`new_law()` returns an S7 law. `check_law()` returns an S7 check result.
`expect_law()` returns one `tinytest` result. `assume()` returns
invisibly when its condition is true and otherwise discards the case.

## Details

Runs use Mersenne-Twister, Inversion normals, and Rejection sampling,
independently of the caller's RNG kind. Box-Muller callers are rejected
before any RNG state is changed because R does not expose their cached
normal draw. Replay requires unchanged generator/law code, run
parameters, and compatible R/package versions; generators and laws must
not depend on external mutable state or change the RNG configuration.
The result's `parameters` list records all run arguments except `law`,
for use with `do.call(check_law, ...)`.

Shrinking is an ordered search, not a guarantee of a global minimum. The
counterexample's `minimal` field holds the last accepted failing
candidate on that search path; candidates have no general size ordering.
A result's `shrink_status` is `"complete"` when no immediate child
preserves the failure, `"budget"` when the evaluation limit stopped the
search (including zero), `"error"` if constructing candidates failed, or
`"not_needed"` when no counterexample was found. A shrinking error or
warning is stored separately in `shrink_condition`; the original and
last failing examples are retained. Generator warnings and errors
terminate the run with status `"error"`. Warnings or errors from `holds`
are counterexamples. Stateful laws created by
[`new_state_law()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_state_law.md)
additionally retain failure traces in the counterexample's
`original_condition` and `condition` fields. Callback defects stop their
shrink search, preserving any earlier false postcondition.

Optional `classify` labels each generated input before `holds` runs.
Labels count once per case whose outcome is a pass or a false
postcondition, including stateful postcondition failures. Discards,
errors, and shrink candidates are excluded. A classifier warning, error,
or invalid return terminates the run with status `"error"` before
evaluating that case's law.

The result's `coverage` data frame contains `label`, `count`,
`proportion`, `minimum`, and `met`; `coverage_cases` is the denominator.
Requirements for unseen labels have count zero. Unrequested minima and
their `met` values are `NA`; with no accepted cases, proportions and all
`met` values are also `NA`. After the requested passing cases, unmet
minima give status `"insufficient_coverage"` and make `expect_law()`
fail without a counterexample. Falsification, error, and exhaustion
retain their own statuses and report partial coverage. Minima describe
observed proportions within the test budget, without a statistical
confidence guarantee. Generator size and preconditions can change the
sampled distribution.

In a tinytest file, call `tinytest::using(s7contract)` before calling
`expect_law()`. This activates tinytest's supported extension capture so
the property run is recorded as one ordinary test result.

## Examples

``` r
reverse_law <- new_law(
  "reverse is involutive",
  generators = list(x = gen_vector(gen_integer(), max = 8L)),
  holds = function(x) identical(rev(rev(x)), x)
)
check_law(reverse_law, tests = 20L, seed = 1L)
#> Law 'reverse is involutive' passed 20 tests (seed 1).
```
