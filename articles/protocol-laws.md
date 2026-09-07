# Reusable Laws for a Vector Protocol

``` r

library(S7)
library(s7contract)
```

This example extends [Behavioral Contracts on
S7](https://sounkou-bioinfo.github.io/s7contract/articles/s7-interfaces-and-traits.md)
with one law suite shared by two implementations and a faulty subclass.

## Vector-like behavior

Many algorithms need only a length, a way to slice, and access to
values. Both ordinary double vectors and `ReadDepth` objects implement
those operations. The class validator keeps positions and depths
aligned, while the interface describes the behavior consumers need.

``` r

vec_length <- new_generic("vec_length", "x")
vec_slice <- new_generic("vec_slice", "x", function(x, i) S7_dispatch())
vec_values <- new_generic("vec_values", "x")

VectorLike <- new_interface(
  "VectorLike",
  generics = list(
    length = interface_requirement(vec_length, returns = class_integer),
    slice = interface_requirement(vec_slice, args = list(i = class_integer)),
    values = interface_requirement(vec_values, returns = class_double)
  )
)

ReadDepth <- new_class(
  "ReadDepth",
  properties = list(position = class_integer, depth = class_double),
  validator = function(self) {
    if (length(self@position) != length(self@depth)) {
      "@position and @depth must have the same length"
    }
  }
)

method(vec_length, ReadDepth) <- function(x) length(x@depth)
method(vec_slice, ReadDepth) <- function(x, i) {
  ReadDepth(position = x@position[i], depth = x@depth[i])
}
method(vec_values, ReadDepth) <- function(x) x@depth

method(vec_length, class_double) <- function(x) length(x)
method(vec_slice, class_double) <- function(x, i) x[i]
method(vec_values, class_double) <- function(x) x

coverage <- ReadDepth(position = 1:5, depth = c(12, 15, 9, 20, 17))
implements(coverage, VectorLike)
#> [1] TRUE
implements(class_double, VectorLike)
#> [1] TRUE
```

A function can depend on this small protocol without knowing how the
object is represented internally.

``` r

window_mean <- function(x, i) {
  assert_implements(x, VectorLike)
  with(VectorLike, mean(vec_values(vec_slice(x, i))))
}

window_mean(coverage, 2:4)
#> [1] 14.66667
window_mean(c(12, 15, 9, 20, 17), 2:4)
#> [1] 14.66667
```

## One protocol, several implementations

A protocol author can publish a function returning a named list of laws.
Implementation authors supply a constructor; the laws compare each
result with the reference values passed to that constructor.

The domain here is unnamed double vectors containing small integers,
with positive, in-range integer indices. Empty vectors and selections
are included; indices may repeat or appear out of order. Missing values,
names, negative indices, and the rest of R’s subsetting semantics are
outside this example.

[`gen_bind()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_bind.md)
constructs the object and an index generator from the reference values.
When those values shrink, it rebuilds both, preserving object validity
and index bounds. Bounds come from the reference data rather than the
method being tested. Inside the contract mask,
[`base::length()`](https://rdrr.io/r/base/length.html) keeps the
reference calculation separate from the interface’s `length` alias.

``` r

vector_laws <- function(make) {
  values <- gen_map(gen_vector(gen_integer(-10L, 10L), max = 6L), as.double)
  cases <- gen_bind(values, function(values) {
    indices <- if (length(values) == 0L) {
      gen_constant(integer())
    } else {
      gen_vector(gen_element(seq_along(values)), max = 6L)
    }
    gen_product(
      x = gen_constant(make(values)),
      values = gen_constant(values),
      i = indices
    )
  })

  list(
    values = new_law("values preserve constructor input", list(input = cases),
      function(input) with(VectorLike, {
        identical(vec_values(input$x), input$values)
      })),
    length = new_law("length agrees with constructor input", list(input = cases),
      function(input) with(VectorLike, {
        identical(vec_length(input$x), base::length(input$values))
      })),
    slice_values = new_law("slicing preserves selected values and order", list(input = cases),
      function(input) with(VectorLike, {
        identical(vec_values(vec_slice(input$x, input$i)), input$values[input$i])
      }),
      classify = function(input) c(
        if (length(input$values) == 0L) "empty" else "nonempty",
        if (anyDuplicated(input$i) > 0L) "repeated",
        if (is.unsorted(input$i)) "reordered"
      ),
      min_coverage = c(empty = 0.05, nonempty = 0.5, repeated = 0.1, reordered = 0.1)
    ),
    slice_length = new_law("slice length matches the index count", list(input = cases),
      function(input) with(VectorLike, {
        identical(vec_length(vec_slice(input$x, input$i)), base::length(input$i))
      }))
  )
}
```

[`lapply()`](https://rdrr.io/r/base/lapply.html) runs the same four laws
against both representations.

``` r

implementations <- list(
  numeric = identity,
  read_depth = function(values) ReadDepth(position = seq_along(values), depth = values)
)
vector_results <- lapply(implementations, function(make) {
  lapply(vector_laws(make), check_law, tests = 100L, seed = 1L)
})
sapply(vector_results, function(results) {
  vapply(results, function(result) result@status, character(1))
})
#>              numeric  read_depth
#> values       "passed" "passed"  
#> length       "passed" "passed"  
#> slice_values "passed" "passed"  
#> slice_length "passed" "passed"
```

## Which cases were tested?

The slicing law classifies inputs as empty or nonempty, and labels
selections with repeated or reordered indices. Each label counts once
per accepted case. Its `min_coverage` requirements use proportions:
`reordered = 0.1` asks for reordered indices in at least 10% of cases.

``` r

vector_results$numeric$slice_values@coverage
#>       label count proportion minimum  met
#> 1     empty    20       0.20    0.05 TRUE
#> 2  nonempty    80       0.80    0.50 TRUE
#> 3  repeated    46       0.46    0.10 TRUE
#> 4 reordered    31       0.31    0.10 TRUE
```

Fixing size at zero exercises only empty vectors. The predicate passes,
but the coverage requirements prevent the run from passing:

``` r

empty_only <- check_law(vector_laws(identity)$slice_values,
                        tests = 10L, seed = 1L, max_size = 0L)
empty_only
#> Law 'slicing preserves selected values and order' passed 10 tests but missed coverage requirements (seed 1).
#> Case coverage (10 accepted cases):
#>   "nonempty": 0/10 (0%; minimum 50% unmet)
#>   "repeated": 0/10 (0%; minimum 10% unmet)
#>   "reordered": 0/10 (0%; minimum 10% unmet)
#>   "empty": 10/10 (100%; minimum 5%)
```

This follows the test-data classification discussed by [Claessen and
Hughes (2000),
§2.4](https://users.cs.northwestern.edu/~robby/courses/395-495-2009-fall/quick.pdf).
These minima describe observed proportions within the chosen test
budget; they carry no statistical confidence guarantee. Discards,
errors, and shrink evaluations contribute no counts. A falsifying
generated case does count, and a run that ends early reports partial
coverage alongside its primary failure. See
[`new_law()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_law.md)
for classifier requirements and result fields.

## Structural conformance and a behavioral failure

This subclass inherits the correct length and value methods, but its
slice method reverses the requested order. All required methods are
available, so
[`implements()`](https://sounkou-bioinfo.github.io/s7contract/reference/interface_requirements.md)
succeeds. Its slices also have valid representations and the expected
value type and length.

``` r

ReversedDepth <- new_class("ReversedDepth", parent = ReadDepth)
method(vec_slice, ReversedDepth) <- function(x, i) {
  ReadDepth(position = x@position[rev(i)], depth = x@depth[rev(i)])
}

implements(ReversedDepth, VectorLike)
#> [1] TRUE
broken_results <- lapply(
  vector_laws(function(values) ReversedDepth(position = seq_along(values), depth = values)),
  check_law, tests = 100L, seed = 1L
)
vapply(broken_results, function(result) result@status, character(1))
#>       values       length slice_values slice_length 
#>     "passed"     "passed"  "falsified"     "passed"
```

Only the law about selected values and their order fails.
[`check_law()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_law.md)
reports that failure separately from structural conformance, and shrinks
it to a smaller valid input. The checked call below still succeeds; its
result differs from the reference slice.

``` r

failure <- broken_results$slice_values
failure
#> Law 'slicing preserves selected values and order' was falsified after 7 attempts and 6 shrinks (seed 1).
#> The law returned FALSE.
#> Shrinking stopped: no child of this counterexample preserves the failure.
#> Smallest counterexample found:
#> List of 1
#>  $ input:List of 3
#>   ..$ x     : <ReversedDepth>
#>   .. ..@ position: int [1:2] 1 2
#>   .. ..@ depth   : num [1:2] 0 -1
#>   ..$ values: num [1:2] 0 -1
#>   ..$ i     : int [1:2] 2 1
#> Case coverage (7 accepted cases; partial run):
#>   "nonempty": 3/7 (42.9%; minimum 50% unmet)
#>   "empty": 4/7 (57.1%; minimum 5%)
#>   "repeated": 2/7 (28.6%; minimum 10%)
#>   "reordered": 1/7 (14.3%; minimum 10%)
example <- failure@counterexample@minimal$input
example$values
#> [1]  0 -1
example$i
#> [1] 2 1
with(VectorLike, vec_values(vec_slice(example$x, example$i)))
#> [1]  0 -1
example$values[example$i]
#> [1] -1  0
```

The result records the inputs and run parameters needed to replay the
failure:

``` r

replayed <- do.call(check_law, c(list(law = failure@law), failure@parameters))
identical(replayed@counterexample@minimal, failure@counterexample@minimal)
#> [1] TRUE
```

These example definitions and checks are installed together in
`system.file("examples", "vector-laws.R", package = "s7contract")`. The
vignette and package tests execute that same script. For generator
composition, budgets, and tinytest integration, see
[`vignette("property-laws")`](https://sounkou-bioinfo.github.io/s7contract/articles/property-laws.md).

[`implements()`](https://sounkou-bioinfo.github.io/s7contract/reference/interface_requirements.md)
continues to check method availability;
[`has_trait()`](https://sounkou-bioinfo.github.io/s7contract/reference/trait_methods.md)
checks declared implementation. Neither runs laws or changes meaning
after a law passes or fails. The protocol’s laws and its
implementation-specific generators are explicit test inputs.
