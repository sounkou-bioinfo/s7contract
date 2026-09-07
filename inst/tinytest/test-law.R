library(S7)

# Passing laws are deterministic and do not alter caller RNG state.
integer_gen <- gen_integer(-20L, 20L)
commutative <- new_law(
  "integer addition commutes",
  generators = list(x = integer_gen, y = integer_gen),
  holds = function(x, y) x + y == y + x
)

entry_kind <- RNGkind()
entry_had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
if (entry_had_seed) {
  entry_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
}
set.seed(90210)
seed_before <- .Random.seed
kind_before <- RNGkind()
passed <- check_law(commutative, tests = 40L, seed = 123L)
expect_identical(passed@status, "passed")
expect_identical(passed@tests, 40L)
expect_identical(passed@attempts, 40L)
expect_identical(.Random.seed, seed_before)
expect_identical(RNGkind(), kind_before)
expect_identical(
  format_check_result(passed),
  "Law 'integer addition commutes' passed 40 tests (seed 123)."
)
expect_identical(
  capture.output(print(passed)),
  "Law 'integer addition commutes' passed 40 tests (seed 123)."
)

passed_again <- check_law(commutative, tests = 40L, seed = 123L)
expect_identical(passed_again@status, passed@status)
expect_identical(passed_again@attempts, passed@attempts)

rm(".Random.seed", envir = .GlobalEnv)
check_law(commutative, tests = 1L, seed = 123L)
expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
do.call(RNGkind, as.list(entry_kind))
if (entry_had_seed) {
  assign(".Random.seed", entry_seed, envir = .GlobalEnv)
} else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
  rm(".Random.seed", envir = .GlobalEnv)
}

# A custom generator exposes deterministic shrinking without exposing trees.
shrinking_gen <- new_generator(
  draw = function(size) 10L,
  shrink = function(value) {
    if (value == 0L) list() else list(0L, value %/% 2L)
  },
  label = "ten",
  prototype = integer()
)
falsifiable <- new_law(
  "generated integers are negative",
  generators = list(x = shrinking_gen),
  holds = function(x) x < 0L
)
falsified <- check_law(falsifiable, tests = 5L, seed = 7L)
expect_identical(falsified@status, "falsified")
expect_identical(falsified@counterexample@original$x, 10L)
expect_identical(falsified@counterexample@minimal$x, 0L)
expect_identical(falsified@shrinks, 1L)
expect_true(grepl("Smallest counterexample found", format_check_result(falsified)))
expect_identical(falsified@shrink_status, "complete")

# An accepted final-budget shrink remains the reported counterexample.
one_shrink <- check_law(
  falsifiable,
  tests = 1L,
  seed = 7L,
  shrinks = 1L
)
expect_identical(one_shrink@counterexample@minimal$x, 0L)
expect_identical(one_shrink@shrinks, 1L)
expect_identical(one_shrink@shrink_attempts, 1L)
expect_identical(one_shrink@shrink_status, "budget")

# A zero shrink budget does not expand the lazy shrink tree.
unexpanded_gen <- new_generator(
  draw = function(size) 1L,
  shrink = function(value) stop("shrink tree was expanded"),
  prototype = integer()
)
unexpanded_law <- new_law(
  "zero means no shrinking",
  generators = list(x = unexpanded_gen),
  holds = function(x) FALSE
)
unexpanded <- check_law(
  unexpanded_law,
  tests = 1L,
  seed = 1L,
  shrinks = 0L
)
expect_identical(unexpanded@status, "falsified")
expect_identical(unexpanded@counterexample@minimal$x, 1L)
expect_identical(unexpanded@shrink_attempts, 0L)
expect_identical(unexpanded@shrink_status, "budget")
expect_true(grepl("evaluation budget (0)", format_check_result(unexpanded), fixed = TRUE))

# Mapping and products preserve the underlying shrink tree.
mapped <- gen_map(
  shrinking_gen,
  function(x) x * 2L,
  prototype = integer()
)
mapped_law <- new_law(
  "mapped values are negative",
  generators = list(x = mapped),
  holds = function(x) x < 0L
)
mapped_result <- check_law(mapped_law, tests = 5L, seed = 7L)
expect_identical(mapped_result@counterexample@minimal$x, 0L)

product_law <- new_law(
  "a deliberately false product law",
  generators = list(x = shrinking_gen, y = shrinking_gen),
  holds = function(x, y) x + y < 0L
)
product_result <- check_law(product_law, tests = 5L, seed = 7L)
expect_identical(product_result@counterexample@minimal, list(x = 0L, y = 0L))

# Atomic generator prototypes give empty and non-empty vectors the same type.
integer_vectors <- gen_vector(gen_integer(1L, 5L), min = 0L, max = 5L)
vector_law <- new_law(
  "integer vectors retain their type",
  generators = list(x = integer_vectors),
  holds = function(x) is.integer(x)
)
expect_identical(
  check_law(vector_law, tests = 40L, seed = 9L)@status,
  "passed"
)

# Nonscalar draws are list elements, so bounds count generated elements.
constant_vector_law <- new_law(
  "vector bounds count nonscalar elements",
  generators = list(
    x = gen_vector(gen_constant(1:2), min = 1L, max = 1L)
  ),
  holds = function(x) length(x) == 1L && identical(x[[1L]], 1:2)
)
expect_identical(
  check_law(constant_vector_law, tests = 1L, seed = 1L)@status,
  "passed"
)

bad_atomic_element <- new_generator(
  draw = function(size) 1:2,
  prototype = integer()
)
bad_atomic_vector <- new_law(
  "atomic vector elements must be scalar",
  generators = list(
    x = gen_vector(bad_atomic_element, min = 1L, max = 1L)
  ),
  holds = function(x) TRUE
)
bad_atomic_result <- check_law(bad_atomic_vector, tests = 1L, seed = 1L)
expect_identical(bad_atomic_result@status, "error")
expect_true(grepl(
  "must draw scalar values",
  conditionMessage(bad_atomic_result@condition),
  fixed = TRUE
))

# Preconditions are bounded and cannot produce a vacuous pass.
never_applicable <- new_law(
  "never applicable",
  generators = list(x = gen_constant(1L)),
  holds = function(x) {
    assume(FALSE)
    TRUE
  }
)
exhausted <- check_law(
  never_applicable,
  tests = 1L,
  seed = 1L,
  discards = 2L
)
expect_identical(exhausted@status, "exhausted")
expect_identical(exhausted@tests, 0L)
expect_identical(exhausted@discards, 3L)

# Discarded cases advance generator size, allowing later values to qualify.
size_gen <- new_generator(
  draw = function(size) as.integer(size),
  prototype = integer()
)
eventually_applicable <- new_law(
  "positive sizes are applicable",
  generators = list(x = size_gen),
  holds = function(x) {
    assume(x > 0L)
    TRUE
  }
)
applicable <- check_law(
  eventually_applicable,
  tests = 1L,
  seed = 1L,
  discards = 1L,
  max_size = 1L
)
expect_identical(applicable@status, "passed")
expect_identical(applicable@attempts, 2L)
expect_identical(applicable@discards, 1L)

# Errors and warnings are failures and are shrunk without changing outcome type.
error_law <- new_law(
  "errors are counterexamples",
  generators = list(x = shrinking_gen),
  holds = function(x) stop("boom")
)
error_result <- check_law(error_law, tests = 1L, seed = 1L)
expect_identical(error_result@status, "error")
expect_identical(error_result@counterexample@minimal$x, 0L)
expect_identical(conditionMessage(error_result@condition), "boom")

warning_law <- new_law(
  "warnings are counterexamples",
  generators = list(x = gen_constant(1L)),
  holds = function(x) {
    warning("unexpected warning")
    TRUE
  }
)
warning_result <- check_law(warning_law, tests = 1L, seed = 1L)
expect_identical(warning_result@status, "error")
expect_identical(conditionMessage(warning_result@condition), "unexpected warning")

invalid_result_law <- new_law(
  "invalid results fail",
  generators = list(x = gen_constant(1L)),
  holds = function(x) NA
)
invalid_result <- check_law(invalid_result_law, tests = 1L, seed = 1L)
expect_identical(invalid_result@status, "error")
expect_true(grepl("one non-missing logical", conditionMessage(invalid_result@condition)))

# Generator defects are structured runner errors, not uncaught failures.
bad_shrinker <- new_generator(
  draw = function(size) 1L,
  shrink = function(value) 0L,
  prototype = integer()
)
bad_generator_law <- new_law(
  "bad shrinker",
  generators = list(x = bad_shrinker),
  holds = function(x) FALSE
)
bad_generator_result <- check_law(
  bad_generator_law,
  tests = 1L,
  seed = 1L
)
expect_identical(bad_generator_result@status, "falsified")
expect_identical(bad_generator_result@counterexample@original, list(x = 1L))
expect_identical(bad_generator_result@counterexample@minimal, list(x = 1L))
expect_identical(bad_generator_result@shrink_status, "error")
expect_identical(bad_generator_result@condition, NULL)
expect_true(grepl("must return a list", conditionMessage(bad_generator_result@shrink_condition)))

# The tinytest adapter aggregates the whole run into one tinytest result.
tinytest_result <- expect_law(commutative, tests = 10L, seed = 4L)
expect_true(inherits(tinytest_result, "tinytest"))
expect_identical(length(tinytest_result), 1L)
expect_true(isTRUE(tinytest_result))
failed_tinytest_result <- expect_law(falsifiable, tests = 1L, seed = 4L)
expect_false(isTRUE(failed_tinytest_result))
expect_true(grepl(
  "expect_law",
  paste(deparse(attr(failed_tinytest_result, "call")), collapse = " "),
  fixed = TRUE
))
expect_true(grepl(
  "Smallest counterexample found",
  attr(failed_tinytest_result, "info"),
  fixed = TRUE
))

expect_error(
  gen_integer(2L, 1L),
  pattern = "ordered integer bounds",
  fixed = TRUE
)
expect_error(
  gen_integer(-2147483648, 0L),
  pattern = "`min` must be one integer",
  fixed = TRUE
)
expect_error(
  check_law(commutative, tests = 1L, seed = -2147483648),
  pattern = "`seed` must be one integer",
  fixed = TRUE
)
expect_error(
  new_generator(function(size) 1L, prototype = 1L),
  pattern = "must have length zero",
  fixed = TRUE
)
expect_error(
  new_law("bad", list(x = 1L), function(x) TRUE),
  pattern = "must be a generator",
  fixed = TRUE
)
expect_error(
  assume(NA),
  pattern = "one non-missing logical",
  fixed = TRUE
)

# Nested generators retain one list entry per vector, including empty vectors.
for (inner_length in 0:2) {
  nested_law <- new_law(
    "nested integer vectors",
    list(x = gen_vector(
      gen_vector(gen_integer(), min = inner_length, max = inner_length),
      min = 1L, max = 3L
    )),
    function(x) {
      is.list(x) && all(vapply(x, is.integer, logical(1))) &&
        all(lengths(x) == inner_length)
    }
  )
  expect_identical(check_law(nested_law, tests = 20L)@status, "passed")
}

# A later shrink failure retains both the original and last accepted example.
late_error <- new_generator(
  function(size) 10L,
  function(x) {
    if (x == 10L) return(list(5L))
    stop("cannot shrink five")
  }
)
late_law <- new_law("late shrink failure", list(x = late_error), function(x) FALSE)
late_result <- check_law(late_law, tests = 1L)
expect_identical(late_result@status, "falsified")
expect_identical(late_result@counterexample@original, list(x = 10L))
expect_identical(late_result@counterexample@minimal, list(x = 5L))
expect_identical(late_result@shrinks, 1L)
expect_identical(late_result@shrink_attempts, 1L)
expect_identical(late_result@shrink_status, "error")
expect_true(grepl("cannot shrink five", format_check_result(late_result), fixed = TRUE))

# Law errors retain their own condition when shrinking also fails.
late_error_law <- new_law("two errors", list(x = late_error), function(x) stop("law error"))
two_errors <- check_law(late_error_law, tests = 1L)
expect_identical(conditionMessage(two_errors@condition), "law error")
expect_identical(conditionMessage(two_errors@shrink_condition), "cannot shrink five")
expect_identical(two_errors@counterexample@minimal, list(x = 5L))

# One evaluation visits one mapped candidate, without touching later siblings.
local({
  transforms <- 0L
  wide <- gen_map(
    new_generator(function(size) 1000L, function(x) as.list(0:999)),
    function(x) { transforms <<- transforms + 1L; x }
  )
  wide_law <- new_law("lazy mapping", list(x = wide), function(x) FALSE)
  result <- check_law(wide_law, tests = 1L, shrinks = 1L)
  expect_identical(transforms, 2L)
  expect_identical(result@counterexample@minimal, list(x = 0L))
  expect_identical(result@shrink_attempts, 1L)
})
unvisited <- gen_map(
  new_generator(function(size) 10L, function(x) list(0L, 5L)),
  function(x) { if (x == 5L) stop("unvisited sibling"); x }
)
unvisited_law <- new_law(
  "unused siblings and product components stay lazy",
  list(x = unvisited, y = unexpanded_gen),
  function(x, y) FALSE
)
unvisited_result <- check_law(unvisited_law, tests = 1L, shrinks = 1L)
expect_identical(unvisited_result@status, "falsified")
expect_identical(unvisited_result@counterexample@minimal, list(x = 0L, y = 1L))
expect_identical(unvisited_result@shrink_condition, NULL)

# NULL is a valid generated value, distinct from an exhausted child iterator.
nullable <- new_generator(
  function(size) 1L,
  function(x) if (is.null(x)) list() else list(NULL)
)
null_result <- check_law(new_law("null shrink", list(x = nullable), function(x) FALSE))
expect_identical(null_result@counterexample@minimal, list(x = NULL))
expect_identical(null_result@shrink_status, "complete")

# Vector shrinking can remove a leading element and preserves length bounds.
local({
  position <- 0L
  element <- new_generator(function(size) {
    position <<- position + 1L
    if (position %% 3L == 0L) 10L else 0L
  }, prototype = integer())
  suffix_law <- new_law("no tens", list(x = gen_vector(element, max = 3L)),
                        function(x) !any(x == 10L))
  result <- check_law(suffix_law, tests = 20L, seed = 1L)
  expect_identical(result@counterexample@original, list(x = c(0L, 10L)))
  expect_identical(result@counterexample@minimal, list(x = 10L))
})
bounded_nested <- new_law(
  "nested shrink bounds",
  list(x = gen_vector(gen_vector(shrinking_gen, min = 2L, max = 4L),
                      min = 1L, max = 3L)),
  function(x) FALSE
)
bounded_result <- check_law(bounded_nested, tests = 1L)
expect_identical(bounded_result@counterexample@minimal, list(x = list(c(0L, 0L))))
expect_identical(bounded_result@shrink_status, "complete")

# Discarded shrinks cannot replace a failing example.
preconditioned <- new_law("valid shrinks", list(x = shrinking_gen), function(x) {
  assume(x >= 5L)
  FALSE
})
expect_identical(check_law(preconditioned, tests = 1L)@counterexample@minimal, list(x = 5L))

# Generator and shrinker warnings are captured without escaping the runner.
warning_generator <- new_generator(function(size) { warning("draw warning"); 1L })
draw_warning <- check_law(new_law("draw warning", list(x = warning_generator), function(x) TRUE))
expect_identical(draw_warning@status, "error")
expect_identical(conditionMessage(draw_warning@condition), "draw warning")
warning_shrinker <- new_generator(function(size) 1L, function(x) {
  warning("shrink warning")
  list(0L)
})
shrink_warning <- check_law(new_law("shrink warning", list(x = warning_shrinker), function(x) FALSE))
expect_identical(shrink_warning@status, "falsified")
expect_identical(shrink_warning@counterexample@minimal, list(x = 1L))
expect_identical(conditionMessage(shrink_warning@shrink_condition), "shrink warning")

# Replay uses the recorded parameters and is independent of ambient RNG kind.
local({
  old_kind <- RNGkind()
  old_options <- options(warn = 2)
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- .Random.seed
  on.exit({
    options(old_options)
    suppressWarnings(do.call(RNGkind, as.list(old_kind)))
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  })
  random_law <- new_law("replay", list(x = new_generator(function(size) runif(1))),
                        function(x) FALSE)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  first <- check_law(random_law, tests = 3L, seed = 17L, shrinks = 0L,
                     discards = 2L, max_size = 7L)
  RNGkind("L'Ecuyer-CMRG", "Kinderman-Ramage", "Rejection")
  caller_seed <- .Random.seed
  caller_kind <- RNGkind()
  replay <- do.call(check_law, c(list(law = first@law), first@parameters))
  expect_identical(replay@counterexample@original, first@counterexample@original)
  expect_identical(replay@parameters, first@parameters)
  expect_identical(replay@rng_kind, c("Mersenne-Twister", "Inversion", "Rejection"))
  expect_identical(.Random.seed, caller_seed)
  expect_identical(RNGkind(), caller_kind)

  # Restoring R's legacy sampler must also work when warnings become errors.
  suppressWarnings(RNGkind(sample.kind = "Rounding"))
  caller_seed <- .Random.seed
  caller_kind <- RNGkind()
  legacy <- do.call(check_law, c(list(law = first@law), first@parameters))
  expect_identical(legacy@counterexample@original, first@counterexample@original)
  expect_identical(.Random.seed, caller_seed)
  expect_identical(RNGkind(), caller_kind)

  # Failed admission must not reset the unexported Box-Muller cached draw.
  RNGkind(normal.kind = "Box-Muller", sample.kind = "Rejection")
  set.seed(123L)
  first_normal <- rnorm(1)
  expected_next <- rnorm(1)
  set.seed(123L)
  expect_identical(rnorm(1), first_normal)
  caller_seed <- .Random.seed
  expect_error(check_law(random_law), pattern = "cached Box-Muller", fixed = TRUE)
  expect_identical(.Random.seed, caller_seed)
  expect_identical(rnorm(1), expected_next)
})

# Integer bounds validate each scalar independently, before combining them.
for (bad_bound in list(NULL, integer(), 1:2, NA_integer_, NA_real_, NaN,
                      -Inf, Inf, 1.5, -2147483648, 2147483648, "1", TRUE, 1+1i)) {
  expect_error(gen_integer(min = bad_bound), pattern = "`min` must be one integer", fixed = TRUE)
  expect_error(gen_vector(gen_integer(), max = bad_bound), pattern = "`max` must be one", fixed = TRUE)
}
local({
  old_options <- options(warn = 2)
  on.exit(options(old_options))
  for (bound in c(-.Machine$integer.max, .Machine$integer.max)) {
    expect_identical(gen_example(gen_integer(bound, bound)), as.integer(bound))
  }
  expect_identical(gen_example(gen_integer(0, 0)), 0L)
  expect_error(gen_integer(-.Machine$integer.max - 1, 0),
               pattern = "`min` must be one integer", fixed = TRUE)
  expect_error(gen_integer(0, .Machine$integer.max + 1),
               pattern = "`max` must be one integer", fixed = TRUE)
})
expect_error(gen_product(x = gen_integer(), x = gen_integer()), pattern = "unique", fixed = TRUE)
expect_error(new_law("duplicate", list(x = integer_gen, x = integer_gen), function(x) TRUE),
             pattern = "unique", fixed = TRUE)
