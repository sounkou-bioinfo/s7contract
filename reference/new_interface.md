# Build a Go-like structural interface on top of S7

`new_interface()` models the method-list part of Go interfaces using S7
generics. An interface is just a named set of required generics, and a
class or object satisfies it when S7 can find a method for every
required generic.

## Usage

``` r
new_interface(name, methods = list(), parents = list(), package = NULL)

interface_requirement(generic, name = NULL)
```

## Arguments

- name:

  For `new_interface()`, the interface name. For
  `interface_requirement()`, the requirement name; it defaults to the
  generic name when omitted.

- methods:

  For `new_interface()`, a named list of S7 generics or
  `interface_requirement()` objects.

- parents:

  Optional interface or list of interfaces to embed.

- package:

  Optional package name used only for display.

- generic:

  An S7 generic function.

## Value

`new_interface()` returns an object of class `s7_go_interface`.
`interface_requirement()` returns an object of class
`s7_interface_requirement`.

## Details

This deliberately mirrors Go's basic interfaces defined only by methods.
It does not attempt to emulate Go's full post-1.18 type-set language
such as `~T`, unions of concrete types, or pointer/value receiver rules.

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

  Drawable <- new_interface("Drawable", methods = list(draw = draw))
  Shape <- new_interface("Shape", methods = list(area = area), parents = Drawable)

  implements(Circle, Shape)
  missing_requirements(Rect, Shape)
})
#>      interface requirement    ok                               message
#> draw     Shape        draw FALSE Can't find method for `draw(<Rect>)`.
```
