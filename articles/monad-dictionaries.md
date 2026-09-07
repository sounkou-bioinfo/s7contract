# A Maybe Dictionary on S7

``` r

library(S7)
library(s7contract)
```

Haskell type classes are often explained as dictionaries: a `Monad m`
constraint is operationally evidence that `m` has `pure` and `bind`. R
can model that idea directly because functions are first-class values
and S7 can validate function-valued properties.

The example below defines a tiny `Maybe` algebraic data type, then
stores its monad operations in an S7 dictionary object. The `s7contract`
interface checks that the dictionary exposes the operations a consumer
expects.

``` r

Maybe <- new_class("Maybe", abstract = TRUE)
Nothing <- new_class("Nothing", parent = Maybe)
Just <- new_class("Just", parent = Maybe, properties = list(value = class_any))

MonadDict <- new_class(
  "MonadDict",
  properties = list(
    name = class_character,
    pure = class_function,
    bind = class_function
  )
)

dict_pure <- new_generic("dict_pure", "x")
dict_bind <- new_generic("dict_bind", "x")

MonadDictionary <- new_interface(
  "MonadDictionary",
  generics = list(
    pure = dict_pure,
    bind = dict_bind
  )
)

method(dict_pure, MonadDict) <- function(x, value) {
  (x@pure)(value)
}
method(dict_bind, MonadDict) <- function(x, mx, f) {
  (x@bind)(mx, f)
}

MaybeMonad <- MonadDict(
  name = "Maybe",
  pure = function(value) Just(value = value),
  bind = function(mx, f) {
    if (S7_inherits(mx, Nothing)) {
      Nothing()
    } else {
      f(mx@value)
    }
  }
)

implements(MaybeMonad, MonadDictionary)
#> [1] TRUE
```

Now the dictionary can be passed around as an ordinary R object.

``` r

dict_bind(
  MaybeMonad,
  Just(value = 2),
  function(x) dict_pure(MaybeMonad, x + 1)
)
#> <Just>
#>  @ value: num 3

dict_bind(
  MaybeMonad,
  Nothing(),
  function(x) dict_pure(MaybeMonad, x + 1)
)
#> <Nothing>
```

The interface checks operation availability. The monad laws are semantic
properties. These three concrete checks illustrate their meaning; the
[vector law
suite](https://sounkou-bioinfo.github.io/s7contract/articles/protocol-laws.md)
shows how to assemble generative laws for multiple implementations.

``` r

maybe_equal <- function(x, y) {
  if (S7_inherits(x, Nothing) && S7_inherits(y, Nothing)) {
    return(TRUE)
  }
  if (S7_inherits(x, Just) && S7_inherits(y, Just)) {
    return(identical(x@value, y@value))
  }
  FALSE
}

f <- function(x) dict_pure(MaybeMonad, x + 1)
g <- function(x) dict_pure(MaybeMonad, x * 2)
mx <- Just(value = 10)

c(
  left_identity = maybe_equal(
    dict_bind(MaybeMonad, dict_pure(MaybeMonad, 10), f),
    f(10)
  ),
  right_identity = maybe_equal(
    dict_bind(MaybeMonad, mx, function(x) dict_pure(MaybeMonad, x)),
    mx
  ),
  associativity = maybe_equal(
    dict_bind(MaybeMonad, dict_bind(MaybeMonad, mx, f), g),
    dict_bind(MaybeMonad, mx, function(x) dict_bind(MaybeMonad, f(x), g))
  )
)
#>  left_identity right_identity  associativity 
#>           TRUE           TRUE           TRUE
```

Here the dictionary is an ordinary R object with function-valued
properties. See the [Haskell
report](https://www.haskell.org/onlinereport/haskell2010/haskellch6.html)
for the monad laws and their type-class setting.
