# Generative Laws with tinytest

``` r

library(S7)
library(s7contract)
tinytest::using(s7contract)
```

Generative laws separate three concerns: generators construct examples,
laws state behavior over those examples, and a runner searches for a
smaller counterexample. One call to
[`expect_law()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_law.md)
becomes one tinytest result even though the law is evaluated many times.

## A tinytest property

The generator below produces integer vectors and carries an integrated
shrink tree. The law checks that reversing a vector twice returns the
original value. This chunk executes while the vignette is built.

``` r

reverse_law <- new_law(
  "reverse is involutive",
  generators = list(
    x = gen_vector(gen_integer(-100L, 100L), max = 20L)
  ),
  holds = function(x) identical(rev(rev(x)), x)
)

expect_law(reverse_law, tests = 100L, seed = 20260902L)
#> ----- PASSED      : <-->
#>  call| expect_law(reverse_law, tests = 100, seed = 20260902)
#>  info| Law 'reverse is involutive' passed 100 tests (seed 20260902).
```

## Laws over an S7 interface

A law can exercise an existing `s7contract` interface. The generator
constructs valid `Circle` objects rather than attempting to invert the
class validator. Calls inside the law retain the interface’s return
contract.

``` r

Circle <- new_class(
  "CirclePropertyVignette",
  properties = list(radius = class_double),
  validator = function(self) {
    if (self@radius < 0) "`radius` must be non-negative."
  }
)
area <- new_generic(
  "area_property_vignette",
  "x",
  function(x) S7_dispatch()
)
method(area, Circle) <- function(x) pi * x@radius^2

HasArea <- new_interface(
  "HasAreaPropertyVignette",
  generics = list(
    area = interface_requirement(area, returns = class_double)
  )
)
circles <- gen_map(
  gen_integer(0L, 1000L),
  function(radius) Circle(radius = as.double(radius))
)
area_law <- new_law(
  "non-negative radii have non-negative area",
  generators = list(x = circles),
  holds = function(x) with(HasArea, area(x)) >= 0
)

expect_law(area_law, tests = 100L, seed = 20260902L)
#> ----- PASSED      : <-->
#>  call| expect_law(area_law, tests = 100, seed = 20260902)
#>  info| Law 'non-negative radii have non-negative area' passed 100 tests (seed 20260902).
```

## Inspecting and replaying a failure

Use
[`check_law()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_law.md)
when the structured result is needed independently of a test framework.
This deliberately false law starts at ten and shrinks to zero.

``` r

ten <- new_generator(
  draw = function(size) 10L,
  shrink = function(value) {
    if (value == 0L) list() else list(0L, value %/% 2L)
  },
  label = "ten",
  prototype = integer()
)
negative_law <- new_law(
  "generated values are negative",
  generators = list(x = ten),
  holds = function(x) x < 0L
)

failure <- check_law(negative_law, tests = 10L, seed = 20260902L)
failure
#> Law 'generated values are negative' was falsified after 1 attempts and 1 shrinks (seed 20260902).
#> The law returned FALSE.
#> Shrinking stopped: no child of this counterexample preserves the failure.
#> Smallest counterexample found:
#> List of 1
#>  $ x: int 0
```

The result records the seed, RNG kind, run parameters, original input,
smallest counterexample found, and shrink counts. The `minimal` field
keeps its name for compatibility; it does not promise a global minimum.
`shrink_status` distinguishes completion within the shrink tree, an
evaluation budget, a shrinking error, and a run that needed no
shrinking. If shrinking errors or warns, `shrink_condition` records that
problem while the original and last failing examples remain available.

Replay the same law and parameters with ordinary R function application:

``` r

replayed <- do.call(check_law, c(list(law = failure@law), failure@parameters))
identical(replayed@counterexample@minimal, failure@counterexample@minimal)
#> [1] TRUE
```

Runs use Mersenne-Twister, Inversion normals, and Rejection sampling, so
changing the caller’s RNG kind does not change the generated sequence.
Replay requires unchanged generator and law code, run parameters, and
compatible R/package versions. Generators and laws must not depend on
external mutable state or change the RNG configuration. Stored examples
can still be tested directly when the generator changes.

The caller’s RNG kind and state are restored on exit. The exception is
an admission restriction: callers using Box-Muller normals are rejected
before anything changes, because R does not expose their cached normal
draw for restoration. Select another normal RNG kind before running laws
in that session.

## Composition and shrinking

Mapping transforms both the generated value and visited shrinks.
Products combine independent generators and shrink one component at a
time. Vectors remove contiguous chunks and then shrink elements,
retaining their minimum length. Nesting vector generators produces lists
of vectors, including empty inner vectors:

``` r

nested <- new_law(
  "nested vectors retain their element type",
  generators = list(x = gen_vector(gen_vector(gen_integer(), max = 4L), max = 3L)),
  holds = function(x) is.list(x) && all(vapply(x, is.integer, logical(1)))
)
expect_law(nested, tests = 20L, seed = 1L)
#> ----- PASSED      : <-->
#>  call| expect_law(nested, tests = 20, seed = 1)
#>  info| Law 'nested vectors retain their element type' passed 20 tests (seed 1).
```

The runner constructs and transforms each shrink candidate only when
visited. `shrinks = 0L` performs no shrink expansion. A custom `shrink`
function still constructs its own list of candidates; the evaluation
budget cannot bound the work performed inside user functions. Shrinkers
and mapping functions must be deterministic, and mapped constructors
must accept every visited shrink.

## Dependent inputs

[`gen_bind()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_bind.md)
uses one generated value to construct the next generator. Here the
sequence length and its bases belong to one dependent input. Every
shrink still has exactly the declared length, so the law needs no
discarded preconditions.

``` r

sequences <- gen_bind(gen_integer(0L, 20L), function(n) {
  gen_product(
    length = gen_constant(n),
    bases = gen_vector(gen_element(c("A", "C", "G", "T")), min = n, max = n)
  )
})
sequence_law <- new_law(
  "sequence length matches its declaration",
  generators = list(x = sequences),
  holds = function(x) length(x$bases) == x$length
)
expect_law(sequence_law, tests = 40L, seed = 1L)
#> ----- PASSED      : <-->
#>  call| expect_law(sequence_law, tests = 40, seed = 1)
#>  info| Law 'sequence length matches its declaration' passed 40 tests (seed 1).
gen_example(sequences, size = 10L, seed = 42L)
#> $length
#> [1] 0
#> 
#> $bases
#> character(0)
```

Shrinking first tries smaller source values and rebuilds the dependent
generator, then shrinks its result. Each rebuild uses the same captured
local seed and size. Random draws inside the law therefore do not change
the regenerated candidates. The factory must depend only on its input
and the scoped RNG, and its constructors must accept every visited
source shrink.

## Choices and recursive values

[`gen_element()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_element.md)
chooses a value;
[`gen_choice()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_element.md)
chooses a generator. Entries are ordered from simpler to more complex
for shrinking. Optional `prob` weights control sampling; zero-weight
entries are excluded from generation and shrinking. All entries with
positive weight are available even at size zero.

``` r

nullable <- gen_choice(gen_constant(NA_integer_), gen_integer(), prob = c(1, 9))
gen_example(gen_vector(nullable, min = 6L, max = 6L), size = 10L, seed = 42L)
#> [1] NA  4  1 -4 -7 NA
```

[`gen_sized()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_bind.md)
builds a generator from the runner’s current size.
[`gen_resize()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_bind.md)
overrides the size for one generator while leaving sibling generators
alone.
[`gen_recursive()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_bind.md)
supplies its expansion function with a child generator that uses half
the current size, rounded down. At size zero, only the base generator
runs. This supports nested lists, expression trees, or recursive S7
objects.

``` r

trees <- gen_recursive(
  gen_element(c("A", "C", "G", "T")),
  function(child) gen_product(left = child, right = child)
)
gen_example(trees, size = 7L, seed = 42L)
#> [1] "G"

leaf_count <- function(tree) {
  if (is.list(tree)) leaf_count(tree$left) + leaf_count(tree$right) else 1L
}
tree_law <- new_law(
  "binary trees have at least one leaf",
  generators = list(tree = trees),
  holds = function(tree) leaf_count(tree) >= 1L
)
expect_law(tree_law, tests = 30L, seed = 1L, max_size = 7L)
#> ----- PASSED      : <-->
#>  call| expect_law(tree_law, tests = 30, seed = 1, max_size = 7)
#>  info| Law 'binary trees have at least one leaf' passed 30 tests (seed 1).
```

Recursive size bounds depth, not total node count; the expansion
function still controls branching. Shrinking can replace a recursive
value with a base value before shrinking within a branch.
[`gen_no_shrink()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_example.md)
removes a generator’s shrinking when a value must remain fixed during
the search.
[`gen_example()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_example.md)
draws one value without expanding shrinks and restores the caller’s RNG
state.

## Relationship to Hedgehog

[R Hedgehog](https://hedgehogqa.r-universe.dev/hedgehog) is the
reference for the broader property-testing scope. Its generators carry
lazy rose trees, with deterministic shrinking preserved through
composition. The underlying [Haskell
Hedgehog](https://hackage.haskell.org/package/hedgehog) design includes
mapping, dependent generation, size-aware ranges, and state-machine
testing.

The table describes the current development version, without claiming
API or seed compatibility with Hedgehog.

| Concept | s7contract |
|:---|:---|
| Mapping / functor composition | [`gen_map()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_constant.md) transforms values and their shrink trees. |
| Independent / applicative composition | [`gen_product()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_constant.md) and named law arguments combine independent generators. |
| Dependent / monadic composition | [`gen_bind()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_bind.md) rebuilds downstream generators when upstream inputs shrink. |
| Size-aware generation | Integer ranges and vector lengths grow with size; [`gen_sized()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_bind.md) and [`gen_resize()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_bind.md) expose size control. |
| Choice and recursive generation | [`gen_element()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_element.md), [`gen_choice()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_element.md), and [`gen_recursive()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_bind.md) retain integrated shrinking. |
| Inspection and shrink control | [`gen_example()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_example.md) draws a reproducible value; [`gen_no_shrink()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_example.md) removes shrinking. |
| State-machine testing | No command/model runner. |
| Behavioral contracts | Laws can exercise S7 interfaces and traits through ordinary calls. |
| Test-framework integration | [`check_law()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_law.md) returns structured results; [`expect_law()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_law.md) records one tinytest result. |
