# Choose values or generators

`gen_element()` samples one element of a vector or list. `gen_choice()`
samples a generator and draws from it at the current size. All choices
are available at size zero. Both shrink toward earlier entries, then
`gen_choice()` shrinks within the selected generator. Zero-weight
entries are excluded from both generation and shrinking.

## Usage

``` r
gen_element(values, prob = NULL)

gen_choice(..., prob = NULL)
```

## Arguments

- values:

  Non-empty atomic vector or list of values.

- prob:

  Optional finite non-negative sampling weights, one per entry, with at
  least one positive weight.

- ...:

  One or more generators, ordered from simpler to more complex.

## Value

An S7 generator object. `gen_element()` preserves an atomic input's
element prototype; list inputs use a list prototype. `gen_choice()`
retains a common prototype when all branches agree, otherwise it uses
list elements.

## Examples

``` r
bases <- gen_element(c("A", "C", "G", "T"))
nullable <- gen_choice(gen_constant(NA_integer_), gen_integer(), prob = c(1, 9))
gen_example(gen_vector(bases, min = 4L, max = 4L))
#> [1] "T" "G" "A" "C"
```
