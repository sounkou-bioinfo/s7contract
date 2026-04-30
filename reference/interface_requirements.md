# Inspect or check a Go-like structural interface

Inspect or check a Go-like structural interface

## Usage

``` r
interface_requirements(interface, inherited = TRUE)

interface_report(x, interface)

missing_requirements(x, interface)

implements(x, interface)

assert_implements(x, interface, arg = deparse(substitute(x)))

as_interface(x, interface)
```

## Arguments

- interface:

  An interface created by
  [`new_interface()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_interface.md).

- inherited:

  Include inherited requirements from parent interfaces?

- x:

  An object, or an S7 class/base class wrapper.

- arg:

  Name to use in error messages.

## Value

`interface_requirements()` returns a named list of
[`interface_requirement()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_interface.md)
objects. `interface_report()` and `missing_requirements()` return data
frames. `implements()` returns a single logical value.
`assert_implements()` and `as_interface()` return `x`, unchanged.
