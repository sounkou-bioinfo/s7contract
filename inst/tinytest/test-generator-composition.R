library(S7)

# Dependent lengths remain valid in every evaluated counterexample.
lengths <- new_generator(
  function(size) 8L,
  function(n) as.list(seq_len(n - 1L)),
  prototype = integer()
)
dependent <- gen_bind(lengths, function(n) {
  gen_product(n = gen_constant(n), values = gen_vector(gen_resize(gen_integer(), 10L), n, n))
})
seen <- list()
length_law <- new_law("vectors have fewer than three elements", list(x = dependent), function(x) {
  seen[[length(seen) + 1L]] <<- x
  stopifnot(length(x$values) == x$n)
  length(x$values) < 3L
})
result <- check_law(length_law, tests = 1L, seed = 9L)
expect_identical(result@status, "falsified")
expect_identical(result@counterexample@original$x$n, 8L)
expect_identical(result@counterexample@minimal$x, list(n = 3L, values = integer(3L)))
expect_true(all(vapply(seen, function(x) length(x$values) == x$n, logical(1))))
expect_identical(result@shrink_status, "complete")

# A law's random draws cannot change the regenerated downstream values.
random_pairs <- gen_bind(lengths, function(n) {
  gen_product(n = gen_constant(n), random = new_generator(function(size) runif(1L)))
})
plain <- new_law("plain", list(x = random_pairs), function(x) FALSE)
noisy <- new_law("uses RNG", list(x = random_pairs), function(x) {
  runif(30L)
  FALSE
})
plain_result <- check_law(plain, seed = 73L)
noisy_result <- check_law(noisy, seed = 73L)
expect_identical(plain_result@counterexample@minimal, noisy_result@counterexample@minimal)
expect_identical(plain_result@counterexample@minimal$x$n, 1L)
expect_identical(plain_result@counterexample@minimal$x$random,
                 plain_result@counterexample@original$x$random)
replayed <- do.call(check_law, c(list(law = noisy), noisy_result@parameters))
expect_identical(replayed@counterexample@minimal, noisy_result@counterexample@minimal)

# Downstream shrinking remains available after exhausting source shrinks.
downstream <- gen_bind(gen_constant(NULL), function(x) {
  stopifnot(is.null(x))
  new_generator(function(size) 10L, function(x) if (x > 0L) list(0L) else list())
})
inner_result <- check_law(new_law("inner shrink", list(x = downstream), function(x) FALSE))
expect_identical(inner_result@counterexample@minimal$x, 0L)

# Unvisited source siblings and dependent factories remain lazy.
calls <- 0L
lazy <- gen_bind(lengths, function(n) {
  calls <<- calls + 1L
  if (n == 2L) stop("unvisited factory")
  gen_constant(n)
})
lazy_law <- new_law("lazy bind", list(x = lazy), function(x) FALSE)
zero <- check_law(lazy_law, shrinks = 0L)
expect_identical(calls, 1L)
expect_identical(zero@counterexample@minimal$x, 8L)
calls <- 0L
one <- check_law(lazy_law, shrinks = 1L)
expect_identical(calls, 2L)
expect_identical(one@counterexample@minimal$x, 1L)

broken <- gen_bind(lengths, function(n) {
  if (n == 1L) stop("dependent factory failed")
  gen_constant(n)
})
broken_result <- check_law(new_law("broken shrink", list(x = broken), function(x) FALSE))
expect_identical(broken_result@status, "falsified")
expect_identical(broken_result@shrink_status, "error")
expect_identical(broken_result@counterexample@minimal$x, 8L)
expect_identical(conditionMessage(broken_result@shrink_condition), "dependent factory failed")

# Choice shrinks across branches and then within the selected branch.
ten <- new_generator(function(size) 10L,
                     function(x) if (x > 0L) list(0L) else list(), prototype = integer())
choices <- gen_choice(gen_constant(-1L), ten, prob = c(1, 1e12))
cross <- check_law(new_law("cross branch", list(x = choices), function(x) FALSE))
expect_identical(cross@counterexample@original$x, 10L)
expect_identical(cross@counterexample@minimal$x, -1L)
within <- check_law(new_law("within branch", list(x = choices), function(x) x < 0L))
expect_identical(within@counterexample@minimal$x, 0L)
expect_identical(within@shrink_status, "complete")

excluded <- gen_choice(new_generator(function(size) stop("excluded")), ten, prob = c(0, 1))
excluded_result <- check_law(new_law("excluded branch", list(x = excluded), function(x) FALSE))
expect_identical(excluded_result@counterexample@minimal$x, 0L)
expect_identical(excluded_result@shrink_status, "complete")
expect_identical(gen_example(gen_vector(choices, 2L, 2L)) |> typeof(), "integer")
mixed <- gen_choice(gen_constant(NULL), gen_constant(1:2), prob = c(1, 0))
expect_identical(gen_example(gen_vector(mixed, 2L, 2L)), list(NULL, NULL))

# Element choices handle singleton numbers, NULL, NA, and extreme weights.
expect_identical(gen_example(gen_element(100L)), 100L)
expect_identical(gen_example(gen_element(list(NULL))), NULL)
expect_identical(gen_example(gen_element(c(NA_integer_, 1L), prob = c(1, 0))), NA_integer_)
expect_identical(gen_example(gen_element(c("excluded", "chosen"), prob = c(0, 1))), "chosen")
expect_true(gen_example(gen_element(1:2, prob = c(1e308, 1e308))) %in% 1:2)
expect_identical(gen_example(gen_element(as.Date("2026-01-01"))), as.Date("2026-01-01"))
letters_result <- check_law(new_law("letters", list(x = gen_element(letters)), function(x) FALSE))
expect_identical(letters_result@counterexample@minimal$x, "a")

# Size overrides affect only the selected generator, including nested overrides.
size_value <- gen_sized(function(size) gen_constant(size), prototype = integer())
expect_identical(gen_example(size_value, size = 13L), 13L)
expect_identical(gen_example(gen_resize(size_value, 7L), size = 99L), 7L)
expect_identical(gen_example(gen_resize(gen_resize(size_value, 7L), 4L)), 7L)
expect_identical(gen_example(gen_product(fixed = gen_resize(size_value, 7L), normal = size_value),
                            size = 13L), list(fixed = 7L, normal = 13L))
expect_identical(gen_example(gen_vector(size_value, 2L, 2L), size = 13L), c(13L, 13L))

# Recursion never expands size zero; each supplied child decreases the size.
expect_identical(gen_example(gen_recursive(gen_constant(0L), function(child) stop("expanded")),
                            size = 0L), 0L)
tree_gen <- gen_recursive(gen_constant(0L), function(child) {
  gen_product(left = child, right = child)
})
depth <- function(x) if (is.list(x)) 1L + max(vapply(x, depth, integer(1))) else 0L
tree_values <- lapply(seq_len(30L), function(seed) gen_example(tree_gen, size = 15L, seed = seed))
depths <- vapply(tree_values, depth, integer(1))
expect_true(all(depths <= 4L))
expect_true(any(depths > 1L))
expect_identical(lapply(tree_values, function(x) unlist(x, use.names = FALSE)) |>
                   unlist(use.names = FALSE) |> unique(), 0L)
tree_result <- check_law(new_law("tree", list(x = gen_resize(tree_gen, 15L)), function(x) FALSE))
expect_identical(tree_result@counterexample@minimal$x, 0L)
expect_identical(tree_result@shrink_status, "complete")

# Disabling shrinking preserves values/prototypes and never calls the shrinker.
no_shrink <- gen_no_shrink(lazy)
calls <- 0L
no_shrink_result <- check_law(new_law("no shrink", list(x = no_shrink), function(x) FALSE))
expect_identical(calls, 1L)
expect_identical(no_shrink_result@counterexample@minimal$x, 8L)
expect_identical(no_shrink_result@shrink_attempts, 0L)
expect_identical(no_shrink_result@shrink_status, "complete")
expect_identical(gen_example(gen_no_shrink(choices), seed = 2L), gen_example(choices, seed = 2L))
expect_identical(gen_example(gen_vector(gen_no_shrink(ten), 2L, 2L)), c(10L, 10L))

# Factory errors, warnings, and invalid results use the existing runner errors.
for (bad in list(gen_bind(ten, function(x) 1L), gen_sized(function(size) 1L))) {
  expect_error(gen_example(bad), pattern = "factory must return a generator", fixed = TRUE)
  failure <- check_law(new_law("invalid factory", list(x = bad), function(x) TRUE))
  expect_identical(failure@status, "error")
  expect_identical(failure@counterexample, NULL)
}
warns <- gen_bind(ten, function(x) {
  warning("factory warning")
  gen_constant(x)
})
warning_result <- check_law(new_law("warning", list(x = warns), function(x) TRUE))
expect_identical(warning_result@status, "error")
expect_identical(conditionMessage(warning_result@condition), "factory warning")

# Example generation restores RNG state on success and error, including no seed.
local({
  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- .Random.seed
  on.exit({
    suppressWarnings(do.call(RNGkind, as.list(old_kind)))
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  })
  set.seed(42L, kind = "L'Ecuyer-CMRG", normal.kind = "Inversion")
  before <- .Random.seed
  kind_before <- RNGkind()
  gen_example(random_pairs)
  expect_identical(.Random.seed, before)
  expect_identical(RNGkind(), kind_before)
  expect_error(gen_example(gen_bind(ten, function(x) stop("failure"))), pattern = "failure")
  expect_identical(.Random.seed, before)
  expect_identical(RNGkind(), kind_before)
  rm(".Random.seed", envir = .GlobalEnv)
  gen_example(random_pairs)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})

expect_error(gen_bind(1L, identity), pattern = "generator")
expect_error(gen_bind(ten, 1L), pattern = "function")
expect_error(gen_sized(1L), pattern = "function")
expect_error(gen_recursive(1L, identity), pattern = "generator")
expect_error(gen_recursive(ten, 1L), pattern = "function")
expect_error(gen_choice(), pattern = "one or more generators")
expect_error(gen_choice(ten, 1L), pattern = "one or more generators")
expect_error(gen_element(integer()), pattern = "empty")
expect_error(gen_element(globalenv()), pattern = "atomic vector or list")
for (weights in list(c(1, NA), c(1, Inf), c(1, -1), c(0, 0), 1, "a")) {
  expect_error(gen_element(1:2, weights), pattern = "`prob`", fixed = TRUE)
}
for (bad_size in list(-1L, NA_integer_, 1.5, c(1L, 2L))) {
  expect_error(gen_resize(ten, bad_size), pattern = "`size`", fixed = TRUE)
  expect_error(gen_example(ten, size = bad_size), pattern = "`size`", fixed = TRUE)
}
expect_error(gen_bind(ten, identity, prototype = 1L), pattern = "length zero")
