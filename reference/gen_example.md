# Inspect a generator or disable its shrinking

`gen_example()` draws one value at a fixed size with a local seed,
restoring the caller's RNG kind and state on exit, as
[`check_law()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_law.md)
does. It does not expand the shrink tree. `gen_no_shrink()` keeps
generation unchanged but removes all shrink candidates, including those
carried by composed generators.

## Usage

``` r
gen_example(generator, size = 10L, seed = 1L)

gen_no_shrink(generator)
```

## Arguments

- generator:

  A generator.

- size:

  Non-negative integer size.

- seed:

  Integer seed for reproducible generation.

## Value

`gen_example()` returns one generated value. `gen_no_shrink()` returns
an S7 generator object with the original element prototype.

## Examples

``` r
gen_example(gen_vector(gen_integer()), size = 5L, seed = 42L)
#> integer(0)
```
