# Build a Go-like structural interface on top of S7

`new_interface()` models the method-list part of Go interfaces as a list
of required S7 generics. An interface is just a named set of required
generics, and a class or object satisfies it when S7 can find a method
for every required generic.

## Usage

``` r
new_interface(
  name,
  generics = list(),
  parents = list(),
  package = NULL,
  methods = NULL
)

interface_requirement(
  generic,
  name = NULL,
  args = list(),
  returns = S7::class_any
)
```

## Arguments

- name:

  For `new_interface()`, the interface name. For
  `interface_requirement()`, the requirement name; it defaults to the
  generic name when omitted.

- generics:

  For `new_interface()`, a named list of S7 generics or
  `interface_requirement()` objects. These are generic functions because
  S7 methods are registered separately on generics.

- parents:

  Optional interface or list of interfaces to embed.

- package:

  Optional package name used only for display.

- methods:

  Compatibility alias for `generics`.

- generic:

  An S7 generic function.

- args:

  Optional named list of S7 classes, interfaces, or traits for runtime
  argument checking with [`with()`](https://rdrr.io/r/base/with.html) or
  `%::%`. Arguments named in `args` are also checked against generic and
  method formals during conformance checks. Dispatch arguments other
  than the first can use S7 classes or unions to refine
  multiple-dispatch requirements. Every concrete combination in those
  unions must have a compatible method.

- returns:

  Optional S7 class, interface, or trait for runtime return checking
  with [`with()`](https://rdrr.io/r/base/with.html) or `%::%`; defaults
  to
  [`S7::class_any`](https://rconsortium.github.io/S7/reference/class_any.html).

## Value

`new_interface()` returns an S7 object of class `s7_interface`.
`interface_requirement()` returns an S7 object of class
`s7_interface_requirement`.

## Details

This mirrors Go's basic interfaces defined only by methods. Define small
interfaces at the point where consuming code needs a behavior. Define S7
classes, generics, and methods normally; then let consumers name the
protocol they accept. Up-front interfaces are also useful for package
protocols, abstract data types, or recursive protocols.

Go's full post-1.18 type-set language is outside this model, including
tilde type terms, unions of concrete types, or pointer/value receiver
rules.

## Examples

``` r
local({
  area <- S7::new_generic("area", "x")
  draw <- S7::new_generic("draw", "x")

  Circle <- S7::new_class(
    "Circle",
    properties = list(r = S7::class_double)
  )
  Rect <- S7::new_class(
    "Rect",
    properties = list(w = S7::class_double, h = S7::class_double)
  )

  S7::method(area, Circle) <- function(x) pi * x@r^2
  S7::method(draw, Circle) <- function(x) sprintf("circle(r = %s)", x@r)
  S7::method(area, Rect) <- function(x) x@w * x@h

  Drawable <- new_interface("Drawable", generics = list(draw = draw))
  Shape <- new_interface("Shape", generics = list(area = area), parents = Drawable)

  implements(Circle, Shape)
  missing_requirements(Rect, Shape)
})
#>      interface requirement    ok                               message
#> draw     Shape        draw FALSE Can't find method for `draw(<Rect>)`.
```
