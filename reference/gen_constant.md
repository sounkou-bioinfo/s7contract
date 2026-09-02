# Basic property-based test generators

These generators carry their own deterministic shrink trees. Integer
ranges and vector lengths expand with the runner's size. Integer values
shrink toward zero when zero is within bounds, or toward the nearest
bound. Product generators shrink one component at a time in argument
order. Vector generators return an atomic vector only when the element
prototype is atomic; those element draws must be scalar and match the
prototype's storage type. Otherwise, vector generators return a list
with one entry per element draw.

## Usage

``` r
gen_constant(value)

gen_integer(min = -100L, max = 100L)

gen_map(generator, transform, prototype = list())

gen_product(...)

gen_vector(element, min = 0L, max = 10L)
```

## Arguments

- value:

  Constant value to generate.

- min, max:

  Inclusive integer bounds. For `gen_vector()`, bounds on vector length.

- generator, element:

  A generator.

- transform:

  Function applied to generated values.

- prototype:

  Zero-length prototype of mapped values.

- ...:

  Uniquely named generators.

## Value

An S7 generator object.

## Examples

``` r
pairs <- gen_product(x = gen_integer(), y = gen_integer())
vectors <- gen_vector(gen_integer(), min = 0L, max = 8L)
```
