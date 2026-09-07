## ---- maybe-setup
library(S7)
library(s7contract)

## ---- maybe-dictionary
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

## ---- maybe-use
dict_bind(MaybeMonad, Just(value = 2L), function(x) Just(value = x + 1L))
dict_bind(MaybeMonad, Nothing(), function(x) stop("must not be called"))

## ---- maybe-domain
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

## ---- maybe-law-suite
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

## ---- maybe-checks
maybe_results <- lapply(maybe_laws(MaybeMonad), check_law, tests = 200L, seed = 1L)
vapply(maybe_results, function(result) result@status, character(1))
maybe_results$associativity@coverage

## ---- maybe-broken
DefaultingMaybe <- MonadDict(
  name = "Defaulting Maybe",
  pure = MaybeMonad@pure,
  bind = function(mx, f) {
    result <- dict_bind(MaybeMonad, mx, f)
    if (S7_inherits(result, Nothing)) Just(value = 0L) else result
  }
)
implements(DefaultingMaybe, MonadDictionary)
defaulting_results <- lapply(maybe_laws(DefaultingMaybe), check_law, tests = 200L, seed = 1L)
vapply(defaulting_results, function(result) result@status, character(1))

## ---- maybe-counterexample
maybe_failure <- defaulting_results$associativity
maybe_failure
example <- maybe_failure@counterexample@minimal
f <- maybe_function(example$first)
g <- maybe_function(example$second)
dict_bind(DefaultingMaybe, dict_bind(DefaultingMaybe, example$mx, f), g)
dict_bind(DefaultingMaybe, example$mx, function(x) dict_bind(DefaultingMaybe, f(x), g))

## ---- maybe-replay
maybe_replayed <- do.call(check_law, c(list(law = maybe_failure@law), maybe_failure@parameters))
identical(maybe_replayed@counterexample@minimal, maybe_failure@counterexample@minimal)
