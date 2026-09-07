# Sample source positions without replacement

`gen_sample()` draws a uniform ordered sample of exactly `size` source
positions, without replacement. Its cardinality and sampling range do
not depend on the runner's size. The default draws permutations of the
source. Shrinking moves positions toward the beginning of the source,
from left to right. Each position tries the earliest position unused by
its prefix, then integer bisections toward its current position. Targets
already in the prefix are skipped; targets used later in the sample are
swapped. Every child is lexicographically smaller, with the same
cardinality and distinct positions. The terminal sample consists of the
first `size` source entries in order.

## Usage

``` r
gen_sample(values, size = length(values))

gen_subsequence(values, min = 0L, max = length(values))
```

## Arguments

- values:

  An atomic vector or list, optionally named. `NULL` is also accepted as
  an empty source. Classed vectors such as dates and factors must
  support [`length()`](https://rdrr.io/r/base/length.html) and integer
  `[` subsetting that preserves their class and returns one entry per
  position. Arrays and data frames are not supported. Length must be at
  most `.Machine$integer.max`.

- size:

  Fixed non-negative sample cardinality, at most `length(values)`.

- min, max:

  Inclusive non-negative subsequence length bounds, with
  `min <= max <= length(values)`.

## Value

An S7 generator.

## Details

`gen_subsequence()` chooses a length uniformly between `min` and
`min(max, min + runner_size)`, then samples that many positions and
sorts them. It shrinks length toward `min` first, rebuilding a sample
with a captured seed as in
[`gen_bind()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_bind.md),
then shrinks positions. Source order and length bounds are preserved.
Different position shrinks can yield the same sorted subsequence.
Setting `min = max = length(values)` yields a constant.

Uniqueness concerns positions, not values: duplicated source entries can
appear together. Subsetting with `[` preserves names and supported
classes; values themselves are not shrunk. Empty sources allow only
empty selections. Both generators have a list prototype, so
[`gen_vector()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_constant.md)
nests their results. For sampling with replacement, compose
[`gen_element()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_element.md)
and
[`gen_vector()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_constant.md).

## References

[R's sampling
documentation](https://stat.ethz.ch/R-manual/R-devel/library/base/html/sample.html)
describes positional sampling and the hash algorithm used for small
samples from large populations. These generators use
[`sample.int()`](https://rdrr.io/r/base/sample.html) without
constructing a vector of every source position. The [R Hedgehog
manual](https://hedgehogqa.r-universe.dev/hedgehog/doc/manual.html)
documents subsequences and sampling as distinct generator domains.

## Examples

``` r
gen_example(gen_sample(letters, size = 3L))
#> [1] "y" "d" "g"
gen_example(gen_subsequence(letters, max = 5L))
#> character(0)
gen_example(gen_sample(seq_len(100000000L), size = 3L))
#> [1] 66608964 44492929 60941821
```
