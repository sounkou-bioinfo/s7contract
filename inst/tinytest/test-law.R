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
expect_true(grepl("Minimal counterexample", format_check_result(falsified)))

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

# Discards still advance generator size, allowing later values to qualify.
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
expect_identical(bad_generator_result@status, "error")
expect_true(grepl("must return a list", conditionMessage(bad_generator_result@condition)))

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
  "Minimal counterexample",
  attr(failed_tinytest_result, "info"),
  fixed = TRUE
))

expect_error(
  gen_integer(2L, 1L),
  pattern = "ordered integer bounds",
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
