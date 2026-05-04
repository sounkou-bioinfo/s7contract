# Evaluate an S7 call under an interface or trait contract

`with(contract, expr)` and `expr %::% contract` evaluate an ordinary S7
call while checking the optional argument and return specifications
stored in an interface requirement or trait method. The call itself
still uses normal S7 dispatch.

## Usage

``` r
expr %::% contract
```

## Arguments

- expr:

  An expression, usually a call to an S7 generic named in the contract.

- contract:

  An interface created by
  [`new_interface()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_interface.md)
  or a trait created by
  [`new_trait()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_trait.md).

## Value

The value of `expr`, after any optional return check.

## Examples

``` r
local({
  draw <- S7::new_generic("typed_draw", "x", function(x, color) {
    S7::S7_dispatch()
  })
  Circle <- S7::new_class("TypedCircle", properties = list(r = S7::class_double))
  S7::method(draw, Circle) <- function(x, color) paste(color, x@r)
  Drawable <- new_interface(
    "TypedDrawable",
    list(draw = interface_requirement(
      draw,
      args = list(color = S7::class_character),
      returns = S7::class_character
    ))
  )
  with(Drawable, draw(Circle(r = 2), color = "red"))
  draw(Circle(r = 2), color = "red") %::% Drawable
})
#> [1] "red 2"
```
