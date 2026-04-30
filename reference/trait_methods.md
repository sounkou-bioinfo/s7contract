# Inspect or use a Rust-like explicit trait

Inspect or use a Rust-like explicit trait

## Usage

``` r
trait_methods(trait, inherited = TRUE)

impl_trait(
  trait,
  class,
  methods = list(),
  assoc_types = list(),
  assoc_consts = list(),
  replace = FALSE
)

trait_report(x, trait)

has_trait(x, trait)

assert_trait(x, trait, arg = deparse(substitute(x)))

trait_call(trait, method, x, ...)

trait_assoc_type(trait, x, name)

trait_assoc_const(trait, x, name)
```

## Arguments

- trait:

  A trait created by
  [`new_trait()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_trait.md).

- inherited:

  Include inherited methods from supertraits?

- class:

  An S7 class or base class wrapper.

- methods:

  Named list of method implementations. Omitted trait methods use their
  default implementation when one is available.

- assoc_types:

  Named list of associated type values.

- assoc_consts:

  Named list of associated constant values.

- replace:

  Replace an existing implementation record and silence warnings about
  visible S7 methods?

- x:

  An object or class.

- arg:

  Name to use in error messages.

- method:

  Method name within the trait.

- ...:

  Additional arguments passed to the S7 generic.

- name:

  Associated item name.

## Value

`trait_methods()` returns a named list of
[`trait_method()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_trait.md)
objects. `impl_trait()` returns the stored implementation record,
invisibly. `trait_report()` returns a one-row data frame. `has_trait()`
returns a single logical value. `assert_trait()` returns `x`, unchanged.
`trait_call()` returns the result of the underlying S7 generic.
`trait_assoc_type()` and `trait_assoc_const()` return the stored
associated item value.
