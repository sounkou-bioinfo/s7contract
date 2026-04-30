# s7contract

`s7contract` explores two contract styles on top of
[S7](https://rconsortium.github.io/S7/):

- Go-like interfaces: structural contracts defined by required generics.
- Rust-like traits: explicit, nominal contracts with default methods and
  associated metadata.

The package keeps actual dispatch in ordinary S7 generics. It does not
invent `x.method()` syntax or a second dispatch engine.

## Installation

Install from GitHub:

``` r

remotes::install_github("sounkou-bioinfo/s7contract")
```

Install locally from a checkout:

``` bash
R CMD INSTALL .
```

## Why Go-like interfaces fit S7

S7 is explicitly a **functional OOP** system: methods belong to generic
functions, and calls look like `generic(x, ...)`, not `x.method(...)`.
That makes a Go-style contract feel native:

- an S7 generic is an operation;
- an S7 method is an implementation for a class;
- a Go-like interface is just a named set of required operations.

In package terms, that becomes:

``` text
Drawable := { draw(x) }
Shape    := { area(x), draw(x) }
```

A class implements `Shape` when S7 can find methods for `area()` and
`draw()`.

## Go-like structural example

``` r

library(S7)
library(s7contract)

area <- new_generic("area", "x")
draw <- new_generic("draw", "x")
perimeter <- new_generic("perimeter", "x")

Circle <- new_class("Circle", properties = list(r = class_double))
Rect <- new_class("Rect", properties = list(w = class_double, h = class_double))

method(area, Circle) <- function(x) pi * x@r^2
method(draw, Circle) <- function(x) sprintf("circle(r = %s)", x@r)
method(area, Rect) <- function(x) x@w * x@h

Drawable <- new_interface("Drawable", list(draw = draw))
Shape <- new_interface("Shape", list(area = area), parents = Drawable)
```

``` r

implements(Circle, Shape)
#> [1] TRUE
implements(Rect, Shape)
#> [1] FALSE
missing_requirements(Rect, Shape)
#>      interface requirement    ok                               message
#> draw     Shape        draw FALSE Can't find method for `draw(<Rect>)`.
```

The dispatch path stays simple:

``` text
paint(x)
  -> assert_implements(x, Drawable)
       -> does S7 find draw(<class of x>)?
  -> draw(x)
       -> ordinary S7 dispatch
```

## Rust-like explicit example

The Rust-like layer adds an explicit
[`impl_trait()`](https://sounkou-bioinfo.github.io/s7contract/reference/trait_methods.md)
registry on top of the same S7 generics:

``` r

Measurable <- new_trait(
  "Measurable",
  methods = list(
    area = trait_method(area),
    perimeter = trait_method(perimeter, default = function(x) NA_real_)
  ),
  assoc_consts = c("UNITS")
)

impl_trait(
  Measurable,
  Circle,
  methods = list(
    area = function(x) pi * x@r^2
  ),
  assoc_consts = list(
    UNITS = "unitless"
  ),
  replace = TRUE
)
```

``` r

has_trait(Circle, Measurable)
#> [1] TRUE
trait_call(Measurable, "area", Circle(r = 2))
#> [1] 12.56637
trait_call(Measurable, "perimeter", Circle(r = 2))
#> [1] NA
trait_assoc_const(Measurable, Circle, "UNITS")
#> [1] "unitless"
```

The trait layer is nominal rather than structural:

``` text
trait_call(Measurable, "area", x)
  -> assert_trait(x, Measurable)
       -> did impl_trait(Measurable, ClassOfX, ...) run?
  -> area(x)
       -> ordinary S7 dispatch
```

## Claim audit

These are the claims this package is comfortable making:

- **Accurate for S7:** S7 generics define interfaces and methods belong
  to generics. That is the core reason the Go-like layer feels natural
  here.
- **Accurate for this package:**
  [`implements()`](https://sounkou-bioinfo.github.io/s7contract/reference/interface_requirements.md)
  is structural. It checks whether
  [`S7::method()`](https://rconsortium.github.io/S7/reference/method.html)
  can find a method for every required generic.
- **Accurate for this package:**
  [`has_trait()`](https://sounkou-bioinfo.github.io/s7contract/reference/trait_methods.md)
  is nominal. Existing S7 methods are not enough;
  [`impl_trait()`](https://sounkou-bioinfo.github.io/s7contract/reference/trait_methods.md)
  must have recorded the implementation.
- **Needs narrowing:** the Go analogy only covers Go’s **basic
  method-list interfaces**. This package does not model Go’s full
  type-set interface language such as `~T` or unions of concrete types.
- **Needs narrowing:** the Rust analogy is runtime-only. There is no
  compile-time trait checking, no coherence/orphan-rule enforcement, and
  associated types are stored metadata rather than Rust type-level
  items.

## Comparison

| Dimension | Go-like interface layer | Rust-like trait layer |
|----|----|----|
| Conformance | Structural | Explicit / nominal |
| Primary question | “Can S7 dispatch every required generic?” | “Was this trait implemented for this class?” |
| Extra registry | No | Yes |
| Default methods | Not part of the model | Natural to emulate |
| Associated items | Not natural | Supported as metadata |
| Compile-time checking | No | No |
| Fit with S7 functional OOP | Very high | Moderate |
| Best use | Protocol-style APIs | Explicit plugin / extension contracts |

## Recommendation

For ordinary S7 design, the Go-like layer is the better default. It
reuses the grain of the language: generic functions first, methods
second, classes as implementations.

Use the Rust-like layer only when you need explicit declarations such
as:

- package extension points;
- semantic opt-in, not accidental compatibility;
- default methods bundled with a named contract;
- associated constants or associated type metadata.

## References

- S7 basics: <https://rconsortium.github.io/S7/articles/S7.html>
- S7 method introspection:
  <https://rconsortium.github.io/S7/reference/method.html>
- Go specification, interface types: <https://go.dev/ref/spec>
- Rust Reference, traits:
  <https://doc.rust-lang.org/reference/items/traits.html>
- Rust Reference, implementations:
  <https://doc.rust-lang.org/reference/items/implementations.html>
