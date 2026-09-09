# Evaluate an S7 call under an interface or trait contract

`with(contract, expr)` and `expr %::% contract` evaluate `expr` in a
contract mask. Required generics are shadowed by checking wrappers, so
calls to those generics use normal S7 dispatch while checking the
optional argument and return specifications stored in an interface
requirement or trait method. Only names resolved through the mask are
checked; namespace-qualified calls and calls inside separately defined
helpers are not instrumented.

## Usage

``` r
expr %::% contract
```

## Arguments

- expr:

  An expression evaluated in a contract mask. Calls to generics named in
  the contract are checked.

- contract:

  An interface created by
  [`new_interface()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_interface.md)
  or a trait created by
  [`new_trait()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_trait.md).

## Value

The value of `expr`, after any optional return check.

## Details

Checks force dispatch and typed arguments before the generic body runs.
Generic defaults retain their lexical scope and share ordinary R
promises. If a typed argument has only a method default, that default is
evaluated before dispatch and supplied to the generic. Such defaults
should be pure expressions of arguments and lexical bindings, without
relying on method-body locals or
[`missing()`](https://rdrr.io/r/base/missing.html) for that argument.

## Examples

``` r
local({
  draw <- S7::new_generic("draw", "x", function(x, color) {
    S7::S7_dispatch()
  })
  Circle <- S7::new_class("TypedCircle", properties = list(r = S7::class_double))
  S7::method(draw, Circle) <- function(x, color) paste(color, x@r)
  Drawable <- new_interface(
    "TypedDrawable",
    generics = list(draw = interface_requirement(
      draw,
      args = list(color = S7::class_character),
      returns = S7::class_character
    ))
  )
  with(Drawable, draw(Circle(r = 2), color = "red"))
  checked_draw <- with(Drawable, function(x) draw(x, color = "red"))
  checked_draw(Circle(r = 2))
  draw(Circle(r = 2), color = "red") %::% Drawable
})
#> [1] "red 2"
```
