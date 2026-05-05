
<!-- README.md is generated from README.Rmd. Please edit that file -->

# s7contract

[![R-CMD-check](https://github.com/sounkou-bioinfo/s7contract/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sounkou-bioinfo/s7contract/actions/workflows/R-CMD-check.yaml)
[![R-universe](https://sounkou-bioinfo.r-universe.dev/badges/s7contract)](https://sounkou-bioinfo.r-universe.dev/s7contract)

`s7contract` provides small experimental contract helpers for
[S7](https://rconsortium.github.io/S7/). It keeps dispatch in ordinary
S7 generics and methods. The words “interface” and “trait” are loose
analogies, not Go or Rust compatibility claims.

## Installation

``` r
# Install 's7contract' in R:
install.packages(
  "s7contract",
  repos = c(
    "https://sounkou-bioinfo.r-universe.dev",
    "https://cloud.r-project.org"
  )
)
```

From a local checkout:

``` bash
R CMD INSTALL .
```

## Structural interfaces

An interface is a named set of required S7 generics. A class or object
satisfies it when S7 can find methods for every requirement. The
intended style follows Go: define concrete S7 classes and methods
normally, then define a small interface at the point where consuming
code needs a behavior.

``` r
library(S7)
library(s7contract)

area <- new_generic("area", "x")
draw <- new_generic("draw", "x")

Circle <- new_class("Circle", properties = list(r = class_double))
Rect <- new_class("Rect", properties = list(w = class_double, h = class_double))

method(area, Circle) <- function(x) pi * x@r^2
method(draw, Circle) <- function(x) sprintf("circle(r = %s)", x@r)
method(area, Rect) <- function(x) x@w * x@h

Drawable <- new_interface("Drawable", generics = list(draw = draw))
Shape <- new_interface("Shape", generics = list(area = area), parents = Drawable)

implements(Circle, Shape)
#> [1] TRUE
implements(Rect, Shape)
#> [1] FALSE
missing_requirements(Rect, Shape)
#>      interface requirement    ok                               message
#> draw     Shape        draw FALSE Can't find method for `draw(<Rect>)`.

render <- function(x) {
  assert_implements(x, Drawable)
  draw(x)
}

render(Circle(r = 2))
#> [1] "circle(r = 2)"
```

## Explicit traits

A trait requires an explicit `impl_trait()` call. It can also provide
default methods and associated metadata.

``` r
label <- new_generic("label", "x")
size <- new_generic("size", "x")

Labelled <- new_trait(
  "Labelled",
  methods = list(
    label = trait_method(label),
    size = trait_method(size, default = function(x) NA_real_)
  ),
  assoc_consts = c("KIND")
)

impl_trait(
  Labelled,
  Circle,
  methods = list(label = function(x) sprintf("circle:%s", x@r)),
  assoc_consts = list(KIND = "shape")
)

has_trait(Circle, Labelled)
#> [1] TRUE
trait_call(Labelled, "label", Circle(r = 2))
#> [1] "circle:2"
trait_call(Labelled, "size", Circle(r = 2))
#> [1] NA
trait_assoc_const(Labelled, Circle, "KIND")
#> [1] "shape"
```

## Progressive argument and return checks

Argument and return specifications are optional. When supplied,
expressions can be evaluated in a contract mask with `with()` or the
lambda.r-style `%::%` operator. Calls to required generics inside that
expression are checked.

``` r
Canvas <- new_class("Canvas")

draw_on <- new_generic(
  "draw_on",
  c("x", "canvas"),
  function(x, canvas, position, ...) S7_dispatch()
)

method(draw_on, list(Circle, Canvas)) <- function(x, canvas, position, ...) {
  sprintf("circle(r = %s) at %s", x@r, position)
}

DrawableOnCanvas <- new_interface(
  "DrawableOnCanvas",
  generics = list(
    draw_on = interface_requirement(
      draw_on,
      args = list(canvas = Canvas, position = class_integer),
      returns = class_character
    )
  )
)

canvas <- Canvas()
with(DrawableOnCanvas, draw_on(Circle(r = 2), canvas, position = 1L))
#> [1] "circle(r = 2) at 1"
draw_on(Circle(r = 2), canvas, position = 1L) %::% DrawableOnCanvas
#> [1] "circle(r = 2) at 1"

checked_draw <- with(DrawableOnCanvas, {
  function(x) draw_on(x, canvas, position = 1L)
})
checked_draw(Circle(r = 2))
#> [1] "circle(r = 2) at 1"

BadCircle <- new_class("BadCircle", properties = list(r = class_double))
method(draw_on, list(BadCircle, Canvas)) <- function(x, canvas, position, ...) {
  x@r
}

tryCatch(
  with(DrawableOnCanvas, draw_on(BadCircle(r = 2), canvas, position = 1L)),
  error = function(e) conditionMessage(e)
)
#> [1] "Return value must be <character>, not <double>"
```

## Limits

- All checks happen at runtime.
- Interfaces check S7 method availability by default; optional argument
  and return checks are progressive runtime checks.
- Traits are a package-level registry on top of S7 dispatch.
- This package does not model Go type sets or Rust compile-time trait
  rules.

## References

- The S7 package documentation: <https://rconsortium.github.io/S7/>.
- S7 issue \#34, “Traits”:
  <https://github.com/RConsortium/S7/issues/34>.
- The Go specification, especially interface types:
  <https://go.dev/ref/spec#Interface_types>.
- Chewxy, “How To Use Go Interfaces”:
  <https://blog.chewxy.com/2018/03/18/golang-interfaces/>.
- The Rust book chapter on traits:
  <https://doc.rust-lang.org/book/ch10-02-traits.html>.
- The Rust reference chapter on traits:
  <https://doc.rust-lang.org/reference/items/traits.html>.
- The `lambda.r` package on CRAN:
  <https://cran.r-project.org/package=lambda.r>.
