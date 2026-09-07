# Generate finite double values

At size zero, `gen_double()` draws only `origin`. The bounds expand
linearly from the origin to `min` and `max`, reaching the full interval
at size 100. Larger sizes use that same interval. Use
[`gen_resize()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_bind.md)
to sample the full range at every runner size. Within the current
bounds, generation interpolates one uniform draw from
[`stats::runif()`](https://rdrr.io/r/stats/Uniform.html). This samples a
finite-precision approximation to a continuous uniform distribution, not
all representable doubles uniformly. Bounds are inclusive constraints;
endpoints are not guaranteed to be drawn. Use
[`gen_choice()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_element.md)
with constants to target them.

## Usage

``` r
gen_double(min = -100, max = 100, origin = NULL)
```

## Arguments

- min, max:

  Finite scalar numeric bounds, with `min <= max`.

- origin:

  Finite scalar numeric shrink target within the bounds. `NULL` chooses
  zero when it is in range, otherwise the nearest bound.

## Value

An S7 generator with a double element prototype.

## Details

Shrinking tries the origin, then the midpoint between the origin and the
generated value, then successive midpoints approaching that value. For
example, 8 with origin 0 has children 0, 4, 6, 7, 7.5, and so on. Each
child follows the same rule. Candidates remain between the origin and
their parent; iteration stops when rounding prevents further progress.
Children are built only when visited. Values close to zero can require
many steps to shrink through subnormal doubles, so the runner's
evaluation budget still applies.

`NA`, `NaN`, and infinities are excluded. Add them explicitly with
[`gen_choice()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_element.md)
or
[`gen_element()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_element.md).
Branch weights control generation and branch order controls shrinking;
zero-weight branches are excluded from both.

## References

Haskell Hedgehog separates shrink origins from [size-dependent
bounds](https://github.com/hedgehogqa/haskell-hedgehog/blob/master/hedgehog/src/Hedgehog/Internal/Range.hs)
and uses [fractional shrinking toward an
origin](https://github.com/hedgehogqa/haskell-hedgehog/blob/master/hedgehog/src/Hedgehog/Internal/Shrink.hs).
The [R Hedgehog
manual](https://hedgehogqa.r-universe.dev/hedgehog/doc/manual.html)
documents `gen.unif()` and mixtures with exceptional numeric values.

## Examples

``` r
measurements <- gen_double(-10, 10)
gen_example(measurements, size = 100L)
#> [1] -4.689827

# 80% finite draws; 5% each for NA, NaN, -Inf, and Inf.
numeric_values <- gen_choice(
  measurements, gen_element(c(NA_real_, NaN, -Inf, Inf)),
  prob = c(4, 1)
)
gen_example(gen_vector(numeric_values, max = 5L))
#> numeric(0)
```
