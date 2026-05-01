
<!-- README.md is generated from README.Rmd. Please edit that file -->

# s7contract

`s7contract` provides small experimental contract helpers for
[S7](https://rconsortium.github.io/S7/). It keeps dispatch in ordinary
S7 generics and methods. The words “interface” and “trait” are loose
analogies, not Go or Rust compatibility claims.

## Installation

``` r
remotes::install_github("sounkou-bioinfo/s7contract")
```

From a local checkout:

``` bash
R CMD INSTALL .
```

## Structural interfaces

An interface is a named set of required S7 generics. A class or object
satisfies it when S7 can find methods for every requirement.

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

Drawable <- new_interface("Drawable", list(draw = draw))
Shape <- new_interface("Shape", list(area = area), parents = Drawable)

implements(Circle, Shape)
#> [1] TRUE
implements(Rect, Shape)
#> [1] FALSE
missing_requirements(Rect, Shape)
#>      interface requirement    ok                               message
#> draw     Shape        draw FALSE Can't find method for `draw(<Rect>)`.
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

## Limits

- All checks happen at runtime.
- Interfaces only check S7 method availability.
- Traits are a package-level registry on top of S7 dispatch.
- This package does not model Go type sets or Rust compile-time trait
  rules.
