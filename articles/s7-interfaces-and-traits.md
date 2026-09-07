# Behavioral Contracts on S7

``` r

library(S7)
library(s7contract)
```

`s7contract` describes what a consumer needs from an S7 object and tests
whether implementations behave as expected. S7 owns class definitions,
method registration, and dispatch.

The package began with structural interfaces and explicit traits. The
[S7 traits discussion](https://github.com/RConsortium/S7/issues/34)
describes the underlying need: checking method contracts around existing
generics. [Go interfaces](https://go.dev/ref/spec#Interface_types)
inform the structural approach; [Rust
traits](https://doc.rust-lang.org/reference/items/traits.html) inform
explicit registrations, defaults, and associated metadata. Here these
are runtime R facilities. Checked calls and generative laws extend them
from method availability to evidence about behavior.

| Mechanism | Question |
|:---|:---|
| S7 properties and validators | Is the object’s representation valid? |
| [`implements()`](https://sounkou-bioinfo.github.io/s7contract/reference/interface_requirements.md) | Can S7 find the required methods? |
| [`has_trait()`](https://sounkou-bioinfo.github.io/s7contract/reference/trait_methods.md) | Has this implementation been declared? |
| [`with()`](https://rdrr.io/r/base/with.html) / `%::%` | Do this call’s arguments and return value satisfy their specifications? |
| [`check_law()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_law.md) | Does a behavioral claim hold over the generated cases? |

## A vector protocol

A windowing function needs length, slicing, and access to values.
`VectorLike` states those requirements. Both double vectors and
`ReadDepth` objects provide the methods; the `ReadDepth` validator keeps
positions and depths aligned.

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

The consumer uses the protocol without depending on either
representation:

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

[`assert_implements()`](https://sounkou-bioinfo.github.io/s7contract/reference/interface_requirements.md)
checks method availability. Inside `with(VectorLike, ...)`, calls also
check the argument and return specifications declared by the interface.
For example, its slice operation requires integer indices:

``` r

tryCatch(
  with(VectorLike, vec_slice(coverage, "first")),
  error = function(e) conditionMessage(e)
)
#> [1] "`i` must be <integer>, not <character>"
```

## Declaring an implementation

Use a trait when a declaration or associated metadata matters to the
consumer. Here the declaration attaches measurement units to
`ReadDepth`:

``` r

Measured <- new_trait("Measured",
  methods = list(values = trait_method(vec_values)),
  assoc_consts = "UNITS"
)
has_trait(ReadDepth, Measured)
#> [1] FALSE

impl_trait(Measured, ReadDepth,
  methods = list(values = function(x) x@depth),
  assoc_consts = list(UNITS = "reads"),
  replace = TRUE
)
#> Overwriting method vec_values(<ReadDepth>)
has_trait(ReadDepth, Measured)
#> [1] TRUE
trait_assoc_const(Measured, ReadDepth, "UNITS")
#> [1] "reads"
```

## Testing behavior

Method availability and valid return types leave semantic claims
untested. This law checks length against the values used to construct
the object:

``` r

length_law <- new_law("length matches constructor input",
  generators = list(values = gen_vector(gen_double(-10, 10), max = 6L)),
  holds = function(values) {
    x <- ReadDepth(position = seq_along(values), depth = values)
    with(VectorLike, identical(vec_length(x), base::length(values)))
  }
)
check_law(length_law, tests = 100L, seed = 1L)
#> Law 'length matches constructor input' was falsified after 1 attempts and 0 shrinks (seed 1).
#> The law returned FALSE.
#> Shrinking stopped: no child of this counterexample preserves the failure.
#> Smallest counterexample found:
#> List of 1
#>  $ values: num(0)
```

The [vector law
suite](https://sounkou-bioinfo.github.io/s7contract/articles/protocol-laws.md)
runs four laws against both representations and finds a faulty slice
method that still satisfies the interface. Its generators preserve valid
objects and indices while shrinking. Law results remain separate from
[`implements()`](https://sounkou-bioinfo.github.io/s7contract/reference/interface_requirements.md)
and
[`has_trait()`](https://sounkou-bioinfo.github.io/s7contract/reference/trait_methods.md).

For generator composition, replay, and tinytest integration, see
[Generative Laws with
tinytest](https://sounkou-bioinfo.github.io/s7contract/articles/property-laws.md).
The [Maybe
dictionary](https://sounkou-bioinfo.github.io/s7contract/articles/monad-dictionaries.md)
shows function-valued operations; [Testing Stateful S7
Protocols](https://sounkou-bioinfo.github.io/s7contract/articles/stateful-protocols.md)
covers sequences of mutations checked against a reference model.
