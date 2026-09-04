# Construct a property-based test generator

A generator draws a value together with an integrated tree of smaller
values. `new_generator()` is the extension point for custom generators.
Its `draw` function receives a non-negative integer size and returns one
value. Its deterministic `shrink` function returns a list of strictly
smaller values. The custom shrinker constructs that list itself; the
framework constructs and transforms the corresponding tree nodes only as
they are visited.

## Usage

``` r
new_generator(
  draw,
  shrink = function(value) list(),
  label = "custom",
  prototype = list()
)
```

## Arguments

- draw:

  Function of one `size` argument that returns a value.

- shrink:

  Function of one generated value that returns a list of smaller values.

- label:

  Short description used in diagnostics.

- prototype:

  Zero-length prototype used by
  [`gen_vector()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_constant.md).

## Value

An S7 generator object.
