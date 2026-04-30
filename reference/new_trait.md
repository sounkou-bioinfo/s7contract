# Build a Rust-like explicit trait on top of S7

`new_trait()` adds a nominal contract registry on top of S7 dispatch. A
class only has the trait after
[`impl_trait()`](https://sounkou-bioinfo.github.io/s7contract/reference/trait_methods.md)
records the implementation, even if compatible S7 methods already exist.

## Usage

``` r
new_trait(
  name,
  methods = list(),
  parents = list(),
  assoc_types = character(),
  assoc_consts = list(),
  package = NULL
)

trait_method(generic, default = NULL, name = NULL)
```

## Arguments

- name:

  For `new_trait()`, the trait name. For `trait_method()`, the method
  name; it defaults to the generic name when omitted.

- methods:

  For `new_trait()`, a named list of S7 generics or `trait_method()`
  objects.

- parents:

  Optional trait or list of supertraits.

- assoc_types:

  Required associated type names, or a named list of default associated
  type values.

- assoc_consts:

  Required associated constant names, or a named list of default
  constant values.

- package:

  Optional package name used only for display.

- generic:

  An S7 generic function.

- default:

  Optional default implementation. If supplied,
  [`impl_trait()`](https://sounkou-bioinfo.github.io/s7contract/reference/trait_methods.md)
  uses it when a class does not provide an override for that method.

## Value

`new_trait()` returns an object of class `s7_trait`. `trait_method()`
returns an object of class `s7_trait_method`.

## Details

This makes default methods and associated metadata practical, but the
result remains a runtime R abstraction. It does not emulate Rust's
compile-time trait bounds, coherence, orphan rules, or type-checked
associated types.

## Examples

``` r
local({
  area <- S7::new_generic("area", "x")
  perimeter <- S7::new_generic("perimeter", "x")

  Circle <- S7::new_class(
    "Circle",
    properties = list(r = S7::class_double)
  )

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
    methods = list(area = function(x) pi * x@r^2),
    assoc_consts = list(UNITS = "unitless")
  )

  has_trait(Circle, Measurable)
  trait_call(Measurable, "area", Circle(r = 2))
  trait_assoc_const(Measurable, Circle, "UNITS")
})
#> [1] "unitless"
```
