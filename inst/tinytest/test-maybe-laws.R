sys.source(system.file("examples", "maybe-laws.R", package = "s7contract"),
           envir = environment())

expect_true(implements(MaybeMonad, MonadDictionary))
expect_identical(vapply(maybe_results, function(x) x@status, character(1)),
                  c(left_identity = "passed", right_identity = "passed", associativity = "passed"))
for (result in maybe_results) {
  expect_identical(result@tests, 200L)
  expect_identical(result@discards, 0L)
  expect_identical(result@coverage_cases, 200L)
  expect_true(all(result@coverage$met))
}
expect_identical(maybe_results$left_identity@coverage$label,
                  c("nothing", "add", "at_least", "Nothing", "Just"))
expect_identical(sum(maybe_results$left_identity@coverage$count[1:3]), 200L)
expect_identical(maybe_results$associativity@coverage$label,
                  c("Nothing", "Just", "first_Nothing", "second_Nothing", "both_Just"))
expect_identical(sum(maybe_results$associativity@coverage$count[3:5]),
                  maybe_results$associativity@coverage$count[[2L]])

# Equality observes the constructor and retains the payload's type.
expect_true(maybe_equal(Nothing(), Nothing()))
expect_true(maybe_equal(Just(value = 1L), Just(value = 1L)))
expect_false(maybe_equal(Just(value = 1L), Just(value = 1)))
expect_false(maybe_equal(Just(value = 1L), Just(value = 2L)))
expect_false(maybe_equal(Nothing(), Just(value = 0L)))
expect_false(maybe_equal(Just(value = NULL), Nothing()))
expect_false(maybe_equal(NULL, NULL))

# Function descriptions have fixed interpretations, including the guard boundary.
expect_true(maybe_equal(maybe_function(list(op = "nothing"))(10L), Nothing()))
expect_true(maybe_equal(maybe_function(list(op = "add", amount = -2L))(3L), Just(value = 1L)))
guard <- maybe_function(list(op = "at_least", minimum = 2L))
expect_true(maybe_equal(guard(1L), Nothing()))
expect_true(maybe_equal(guard(2L), Just(value = 2L)))
expect_error(maybe_function(list(op = "unknown"))(0L), pattern = "Unknown Maybe function")
local({
  spec <- list(op = "add", amount = 1L)
  f <- maybe_function(spec)
  spec$amount <- 2L
  expect_true(maybe_equal(f(0L), Just(value = 1L)))
})

# Callback traces independently check the operational short-circuit behavior.
for (scenario in c("input_absent", "first_absent", "both_present")) local({
  trace <- character()
  input <- if (scenario == "input_absent") Nothing() else Just(value = 1L)
  f <- function(x) {
    trace <<- c(trace, "f")
    if (scenario == "first_absent") Nothing() else Just(value = x + 1L)
  }
  g <- function(x) {
    trace <<- c(trace, "g")
    Just(value = x * 2L)
  }
  result <- dict_bind(MaybeMonad, dict_bind(MaybeMonad, input, f), g)
  expect_identical(trace, switch(scenario,
    input_absent = character(), first_absent = "f", both_present = c("f", "g")))
  expected <- if (scenario == "both_present") Just(value = 4L) else Nothing()
  expect_true(maybe_equal(result, expected))
})

# Structural conformance does not prevent failures of all three equations.
expect_true(implements(DefaultingMaybe, MonadDictionary))
expect_identical(vapply(defaulting_results, function(x) x@status, character(1)),
                  c(left_identity = "falsified", right_identity = "falsified", associativity = "falsified"))
expect_identical(defaulting_results$left_identity@counterexample@minimal,
                  list(value = 0L, fn = list(op = "nothing")))
expect_true(S7_inherits(defaulting_results$right_identity@counterexample@minimal$mx, Nothing))
expect_true(S7_inherits(example$mx, Nothing))
expect_identical(example$first, list(op = "nothing"))
expect_identical(example$second, list(op = "add", amount = -1L))
expect_identical(maybe_failure@counterexample@outcome, "fail")
expect_identical(maybe_failure@shrink_status, "complete")
expect_true(maybe_failure@shrinks > 0L)
expect_identical(maybe_replayed@counterexample@minimal, maybe_failure@counterexample@minimal)
expect_identical(maybe_replayed@shrink_attempts, maybe_failure@shrink_attempts)
expect_identical(maybe_replayed@coverage, maybe_failure@coverage)
left <- dict_bind(DefaultingMaybe, dict_bind(DefaultingMaybe, example$mx, f), g)
right <- dict_bind(DefaultingMaybe, example$mx, function(x) dict_bind(DefaultingMaybe, f(x), g))
expect_true(maybe_equal(left, Just(value = -1L)))
expect_true(maybe_equal(right, Just(value = 0L)))

# Changing pure alone invalidates the identities without changing associativity.
CoercingMaybe <- MonadDict(name = "Coercing Maybe",
  pure = function(value) Just(value = as.double(value)), bind = MaybeMonad@bind)
coercing_results <- lapply(maybe_laws(CoercingMaybe), check_law, tests = 200L, seed = 1L)
expect_identical(vapply(coercing_results, function(x) x@status, character(1)),
                  c(left_identity = "falsified", right_identity = "falsified", associativity = "passed"))

# Every generated and shrunk argument stays in the declared finite domain.
local({
  visits <- 0L
  original_law <- maybe_failure@law
  audited <- new_law(original_law@name, original_law@generators, function(mx, first, second) {
    stopifnot(S7_inherits(mx, Maybe))
    if (S7_inherits(mx, Just)) {
      stopifnot(is.integer(mx@value), length(mx@value) == 1L,
                mx@value >= -10L, mx@value <= 10L)
    }
    for (spec in list(first, second)) {
      stopifnot(spec$op %in% c("nothing", "add", "at_least"))
      if (spec$op != "nothing") {
        parameter <- spec[[2L]]
        stopifnot(is.integer(parameter), length(parameter) == 1L,
                  parameter >= -5L, parameter <= 5L)
      }
    }
    visits <<- visits + 1L
    (original_law@holds)(mx, first, second)
  })
  result <- do.call(check_law, c(list(law = audited), maybe_failure@parameters))
  expect_identical(result@counterexample@minimal, maybe_failure@counterexample@minimal)
  expect_identical(result@shrink_status, "complete")
  expect_identical(visits, result@attempts + result@shrink_attempts)
})

expect_true(isTRUE(expect_law(maybe_laws(MaybeMonad)$associativity, tests = 200L, seed = 1L)))
adapted <- expect_law(maybe_failure@law, tests = 200L, seed = 1L)
expect_false(isTRUE(adapted))
expect_identical(length(adapted), 1L)
expect_true(grepl("amount", attr(adapted, "info"), fixed = TRUE))
expect_error(maybe_laws(list()), pattern = "MonadDictionary")
