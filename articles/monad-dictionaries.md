# A Maybe Dictionary on S7

``` r

library(S7)
library(s7contract)
```

This S7 dictionary stores `pure` and `bind` as functions. `Just(value)`
carries a value; `Nothing()` represents absence. An interface checks
operation availability, while laws state the required behavior.

## The dictionary

`pure` wraps a value in `Just`. `bind` passes a `Just` payload to a
function returning Maybe, and propagates `Nothing` without calling that
function.

``` r

Maybe <- new_class("Maybe", abstract = TRUE)
Nothing <- new_class("Nothing", parent = Maybe)
Just <- new_class("Just", parent = Maybe, properties = list(value = class_any))

MonadDict <- new_class("MonadDict", properties = list(
  name = class_character, pure = class_function, bind = class_function
))
dict_pure <- new_generic("dict_pure", "x", function(x, value) S7_dispatch())
dict_bind <- new_generic("dict_bind", "x", function(x, mx, f) S7_dispatch())
MonadDictionary <- new_interface("MonadDictionary", generics = list(
  pure = dict_pure, bind = dict_bind
))
method(dict_pure, MonadDict) <- function(x, value) (x@pure)(value)
method(dict_bind, MonadDict) <- function(x, mx, f) (x@bind)(mx, f)

MaybeMonad <- MonadDict(
  name = "Maybe",
  pure = function(value) Just(value = value),
  bind = function(mx, f) {
    if (S7_inherits(mx, Nothing)) Nothing() else f(mx@value)
  }
)
implements(MaybeMonad, MonadDictionary)
#> [1] TRUE
```

Binding `Nothing` leaves the second callback unevaluated:

``` r

dict_bind(MaybeMonad, Just(value = 2L), function(x) Just(value = x + 1L))
#> <Just>
#>  @ value: int 3
dict_bind(MaybeMonad, Nothing(), function(x) stop("must not be called"))
#> <Nothing>
```

## Generated values and functions

The input domain is `Nothing` or `Just` containing one integer from -10
to 10, with equal constructor probabilities. Functions come from three
equally likely families: always return `Nothing`, add an integer from -5
to 5, or retain values at least a generated threshold in that range.
Integer ranges expand with size. These functions are total on the tested
inputs and intermediate values, which stay between -20 and 20.

``` r

maybe_values <- gen_choice(
  gen_constant(Nothing()),
  gen_map(gen_integer(-10L, 10L), function(value) Just(value = value))
)
maybe_functions <- gen_choice(
  gen_constant(list(op = "nothing")),
  gen_map(gen_integer(-5L, 5L), function(amount) list(op = "add", amount = amount)),
  gen_map(gen_integer(-5L, 5L), function(minimum) list(op = "at_least", minimum = minimum))
)
maybe_function <- function(spec) {
  force(spec)
  function(x) switch(spec$op,
    nothing = Nothing(),
    add = Just(value = x + spec$amount),
    at_least = if (x >= spec$minimum) Just(value = x) else Nothing(),
    stop("Unknown Maybe function")
  )
}
maybe_equal <- function(x, y) {
  if (S7_inherits(x, Nothing) && S7_inherits(y, Nothing)) return(TRUE)
  if (S7_inherits(x, Just) && S7_inherits(y, Just)) return(identical(x@value, y@value))
  FALSE
}
```

Function descriptions remain data in counterexamples. Shrinking tries
earlier constructors and function families, then moves integer
parameters toward zero. The interpreter constructs each function
independently of the dictionary under test. Equality compares the
observable constructor and uses
[`identical()`](https://rdrr.io/r/base/identical.html) for payloads, so
integer and double payloads differ.

## Three laws, one suite

The [Haskell 2010 Report,
§6.3.6](https://www.haskell.org/onlinereport/haskell2010/haskellch6.html)
gives the three monad equations. Writing `pure` for the unit operation
and `>>=` for bind:

``` text
pure(a) >>= f              = f(a)
m >>= pure                 = m
(m >>= f) >>= g            = m >>= (x -> f(x) >>= g)
```

`maybe_laws(dictionary)` returns three ordinary laws. Each equation uses
the same interpreted functions on both sides. Coverage labels describe
reference outcomes of those functions, including absence at the input or
after either function in a composition.

``` r

maybe_laws <- function(dictionary) {
  assert_implements(dictionary, MonadDictionary)
  list(
    left_identity = new_law("Maybe left identity",
      list(value = gen_integer(-10L, 10L), fn = maybe_functions),
      function(value, fn) {
        f <- maybe_function(fn)
        maybe_equal(dict_bind(dictionary, dict_pure(dictionary, value), f), f(value))
      },
      classify = function(value, fn) {
        result <- maybe_function(fn)(value)
        c(fn$op, if (S7_inherits(result, Nothing)) "Nothing" else "Just")
      },
      min_coverage = c(nothing = 0.1, add = 0.1, at_least = 0.1, Nothing = 0.2, Just = 0.2)
    ),
    right_identity = new_law("Maybe right identity", list(mx = maybe_values),
      function(mx) {
        maybe_equal(dict_bind(dictionary, mx, function(x) dict_pure(dictionary, x)), mx)
      },
      classify = function(mx) if (S7_inherits(mx, Nothing)) "Nothing" else "Just",
      min_coverage = c(Nothing = 0.2, Just = 0.2)
    ),
    associativity = new_law("Maybe associativity",
      list(mx = maybe_values, first = maybe_functions, second = maybe_functions),
      function(mx, first, second) {
        f <- maybe_function(first)
        g <- maybe_function(second)
        maybe_equal(
          dict_bind(dictionary, dict_bind(dictionary, mx, f), g),
          dict_bind(dictionary, mx, function(x) dict_bind(dictionary, f(x), g))
        )
      },
      classify = function(mx, first, second) {
        if (S7_inherits(mx, Nothing)) return("Nothing")
        fx <- maybe_function(first)(mx@value)
        if (S7_inherits(fx, Nothing)) return(c("Just", "first_Nothing"))
        gx <- maybe_function(second)(fx@value)
        c("Just", if (S7_inherits(gx, Nothing)) "second_Nothing" else "both_Just")
      },
      min_coverage = c(Nothing = 0.2, Just = 0.2, first_Nothing = 0.1,
                       second_Nothing = 0.05, both_Just = 0.05)
    )
  )
}
```

``` r

maybe_results <- lapply(maybe_laws(MaybeMonad), check_law, tests = 200L, seed = 1L)
vapply(maybe_results, function(result) result@status, character(1))
#>  left_identity right_identity  associativity 
#>       "passed"       "passed"       "passed"
maybe_results$associativity@coverage
#>            label count proportion minimum  met
#> 1        Nothing   102      0.510    0.20 TRUE
#> 2           Just    98      0.490    0.20 TRUE
#> 3  first_Nothing    56      0.280    0.10 TRUE
#> 4 second_Nothing    21      0.105    0.05 TRUE
#> 5      both_Just    21      0.105    0.05 TRUE
```

These laws test the generated families of functions and integer
payloads. `dict_bind()` calls the dictionary under test;
[`gen_bind()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_bind.md)
composes generators and their shrink trees.

## A default that breaks the laws

This dictionary replaces every `Nothing` result with `Just(0L)`. Its
operations still satisfy the structural interface:

``` r

DefaultingMaybe <- MonadDict(
  name = "Defaulting Maybe",
  pure = MaybeMonad@pure,
  bind = function(mx, f) {
    result <- dict_bind(MaybeMonad, mx, f)
    if (S7_inherits(result, Nothing)) Just(value = 0L) else result
  }
)
implements(DefaultingMaybe, MonadDictionary)
#> [1] TRUE
defaulting_results <- lapply(maybe_laws(DefaultingMaybe), check_law, tests = 200L, seed = 1L)
vapply(defaulting_results, function(result) result@status, character(1))
#>  left_identity right_identity  associativity 
#>    "falsified"    "falsified"    "falsified"
```

Associativity fails because the default allows a later function to run
on one side of the equation. The reduced input keeps both function
descriptions:

``` r

maybe_failure <- defaulting_results$associativity
maybe_failure
#> Law 'Maybe associativity' was falsified after 5 attempts and 3 shrinks (seed 1).
#> The law returned FALSE.
#> Shrinking stopped: no child of this counterexample preserves the failure.
#> Smallest counterexample found:
#> List of 3
#>  $ mx    : <Nothing>
#>  $ first :List of 1
#>   ..$ op: chr "nothing"
#>  $ second:List of 2
#>   ..$ op    : chr "add"
#>   ..$ amount: int -1
#> Case coverage (5 accepted cases; partial run):
#>   "second_Nothing": 0/5 (0%; minimum 5% unmet)
#>   "both_Just": 0/5 (0%; minimum 5% unmet)
#>   "Nothing": 3/5 (60%; minimum 20%)
#>   "Just": 2/5 (40%; minimum 20%)
#>   "first_Nothing": 2/5 (40%; minimum 10%)
example <- maybe_failure@counterexample@minimal
f <- maybe_function(example$first)
g <- maybe_function(example$second)
dict_bind(DefaultingMaybe, dict_bind(DefaultingMaybe, example$mx, f), g)
#> <Just>
#>  @ value: int -1
dict_bind(DefaultingMaybe, example$mx, function(x) dict_bind(DefaultingMaybe, f(x), g))
#> <Just>
#>  @ value: int 0
```

The same law and recorded parameters reproduce the failure:

``` r

maybe_replayed <- do.call(check_law, c(list(law = maybe_failure@law), maybe_failure@parameters))
identical(maybe_replayed@counterexample@minimal, maybe_failure@counterexample@minimal)
#> [1] TRUE
```

The [Haskell monad
tutorial](https://www.haskell.org/tutorial/monads.html) develops the
distinction between type-class operations and their laws.
