# Compose dependent, sized, and recursive generators

`gen_bind()` passes a generated value to `bind`, which constructs the
next generator. Shrinking first rebuilds that generator for smaller
source values, then shrinks its output. Each rebuild uses the same
locally captured seed and size, so random downstream draws remain
reproducible regardless of shrink traversal or random draws made by a
law. The surrounding RNG state is restored after each rebuild. Callbacks
must not depend on external mutable state or change the RNG
configuration.

## Usage

``` r
gen_bind(generator, bind, prototype = list())

gen_sized(factory, prototype = list())

gen_resize(generator, size)

gen_recursive(base, expand, prototype = list())
```

## Arguments

- generator, base:

  A generator. `base` produces non-recursive values.

- bind:

  Function of one generated value returning a generator.

- prototype:

  Zero-length prototype of generated values, used when this generator is
  an element of
  [`gen_vector()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_constant.md).
  Defaults to list elements.

- factory:

  Function of one non-negative integer size returning a generator.

- size:

  Non-negative integer size to use for every draw.

- expand:

  Function accepting a child generator and returning a generator for one
  recursive layer.

## Value

An S7 generator object.

## Details

`gen_sized()` passes the current size to a generator factory.
`gen_resize()` fixes the size used by one generator without changing its
siblings.

`gen_recursive()` chooses between `base` and the generator returned by
`expand(child)`. The supplied child recursively uses half the current
size, rounded down; size zero draws only from `base`. Shrinking tries
the base branch before shrinking the expanded value. Recursion through
`child` therefore terminates, provided callbacks themselves terminate
and do not introduce other recursion. Size bounds recursion depth, not
total node count.

## Examples

``` r
sized_vectors <- gen_bind(gen_integer(1L, 8L), function(n) {
  gen_product(n = gen_constant(n), x = gen_vector(gen_integer(), n, n))
})
gen_example(sized_vectors)
#> $n
#> [1] 1
#> 
#> $x
#> [1] -10
#> 

trees <- gen_recursive(gen_constant(0L), function(child) {
  gen_product(left = child, right = child)
})
gen_example(trees, size = 4L)
#> [1] 0
```
