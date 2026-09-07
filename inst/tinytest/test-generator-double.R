library(S7)

# Size zero and singleton intervals preserve the declared origin exactly.
for (bounds in list(c(-5, 12), c(2, 8), c(-8, -2))) {
  generator <- gen_double(bounds[[1L]], bounds[[2L]])
  target <- median(c(bounds, 0))
  expect_identical(gen_example(generator, size = 0L), target)
  result <- check_law(new_law("origin", list(x = gen_resize(generator, 100L)),
                              function(x) FALSE))
  expect_identical(result@counterexample@minimal$x, target)
  expect_identical(result@shrink_status, "complete")
}
for (value in c(-.Machine$double.xmax, -2^-1074, 0, 2^-1074, .Machine$double.xmax)) {
  generator <- gen_double(value, value)
  expect_identical(gen_example(generator, size = 100L), value)
  result <- check_law(new_law("constant", list(x = generator), function(x) FALSE))
  expect_identical(result@shrink_attempts, 0L)
  expect_identical(result@shrink_status, "complete")
}
expect_identical(gen_example(gen_double(-4, 12, origin = 8), size = 0L), 8)
expect_identical(typeof(gen_example(gen_double(1L, 2L), size = 0L)), "double")

# Bounds expand around the origin, and stop expanding at size 100.
generator <- gen_double(-4, 12, origin = 8)
for (seed in 1:12) {
  full <- gen_example(generator, size = 100L, seed = seed)
  half <- gen_example(generator, size = 50L, seed = seed)
  expect_true(full >= -4 && full <= 12)
  expect_true(half >= 2 && half <= 10)
  expect_equal(half, (full + 8) / 2)
  expect_identical(gen_example(generator, size = .Machine$integer.max, seed = seed), full)
}

# At full size the sampling interval is independent of the shrink origin.
# Interpolation toward a small bound must not lose that bound to cancellation.
for (bounds in list(c(1e292, 1e308), c(-1e308, -1e292))) {
  from_lower <- gen_double(bounds[[1L]], bounds[[2L]], origin = bounds[[1L]])
  from_upper <- gen_double(bounds[[1L]], bounds[[2L]], origin = bounds[[2L]])
  for (seed in 1:20) {
    expect_identical(gen_example(from_lower, size = 100L, seed = seed),
                      gen_example(from_upper, size = 100L, seed = seed))
  }
}

# Observe every root child by accepting only the original value as a failure.
# Wide intervals exercise subtraction overflow; narrow ones exercise rounding.
huge <- .Machine$double.xmax
tiny <- 2^-1074
domains <- list(
  c(0, 8, 0), c(-8, 0, 0), c(-4, 12, 8),
  c(-huge, huge, 0), c(-huge, huge, huge), c(-huge, huge, -huge),
  c(huge / 2, huge, huge / 2), c(-huge, -huge / 2, -huge / 2),
  c(0, tiny, 0), c(-tiny, 0, 0),
  c(1, 1 + .Machine$double.eps, 1),
  c(1, 1 + .Machine$double.eps, 1 + .Machine$double.eps),
  c(.Machine$double.xmin, 2 * .Machine$double.xmin, .Machine$double.xmin)
)
for (domain in domains) local({
  generator <- gen_resize(gen_double(domain[[1L]], domain[[2L]], domain[[3L]]), 100L)
  original <- gen_example(generator, seed = 4L)
  visited <- double()
  law <- new_law("visit children", list(x = generator), function(x) {
    visited <<- c(visited, x)
    !identical(x, original)
  })
  result <- check_law(law, tests = 1L, seed = 4L, shrinks = 1200L)
  children <- visited[-1L]
  expect_identical(result@status, "falsified")
  expect_identical(result@shrink_status, "complete")
  expect_identical(result@shrinks, 0L)
  expect_identical(length(children), result@shrink_attempts)
  expect_true(all(is.finite(visited)))
  expect_true(all(visited >= domain[[1L]] & visited <= domain[[2L]]))
  expect_identical(anyDuplicated(visited), 0L)
  if (original != domain[[3L]]) {
    expect_identical(children[[1L]], domain[[3L]])
    ordered <- if (original > domain[[3L]]) diff(children) > 0 else diff(children) < 0
    expect_true(all(ordered))
    expect_true(all(children >= min(original, domain[[3L]]) &
                    children <= max(original, domain[[3L]])))
  }
})

# A dyadic threshold is found exactly through nested midpoint shrinking.
local({
  visited <- double()
  generator <- gen_resize(gen_double(0, 8), 100L)
  law <- new_law("below a half", list(x = generator), function(x) {
    visited <<- c(visited, x)
    x < 0.5
  })
  result <- check_law(law, tests = 1L, seed = 1L, shrinks = 3000L)
  expect_identical(result@counterexample@minimal$x, 0.5)
  expect_identical(result@shrink_status, "complete")
  expect_identical(visited[2:4], c(0, visited[[1L]] / 2, 0))
  expect_true(all(visited >= 0 & visited <= 8))
  replayed <- do.call(check_law, c(list(law = law), result@parameters))
  expect_identical(replayed@counterexample@minimal, result@counterexample@minimal)
  expect_identical(replayed@shrink_attempts, result@shrink_attempts)
})

# Subnormal leaves terminate; a longer route toward zero obeys the budget.
leaf <- gen_resize(gen_double(0, tiny), 100L)
result <- check_law(new_law("zero", list(x = leaf), function(x) x == 0), tests = 1L, seed = 4L)
expect_identical(result@counterexample@minimal$x, tiny)
expect_identical(result@shrink_attempts, 1L)
expect_identical(result@shrink_status, "complete")
local({
  calls <- 0L
  generator <- gen_map(gen_resize(gen_double(0, 8), 100L), function(x) {
    calls <<- calls + 1L
    x
  }, prototype = double())
  law <- new_law("zero", list(x = generator), function(x) x == 0)
  zero <- check_law(law, tests = 1L, shrinks = 0L)
  expect_identical(calls, 1L)
  expect_identical(zero@shrink_status, "budget")
  calls <- 0L
  bounded <- check_law(law, tests = 1L, shrinks = 20L)
  expect_identical(calls, 21L)
  expect_identical(bounded@shrink_attempts, 20L)
  expect_identical(bounded@shrink_status, "budget")
  expect_true(bounded@counterexample@minimal$x > 0)
  expect_true(bounded@counterexample@minimal$x < bounded@counterexample@original$x)
})

# Dependent intervals and nested vectors retain bounds and atomic prototypes.
local({
  source <- new_generator(function(size) 4L,
    function(n) if (n > 1L) list(1L) else list(), prototype = integer())
  generator <- gen_bind(source, function(n) {
    gen_product(n = gen_constant(n), x = gen_vector(
      gen_vector(gen_double(n, n + 0.5), min = 1L, max = 3L), min = 1L, max = 3L))
  })
  law <- new_law("nested doubles", list(input = generator), function(input) {
    stopifnot(is.list(input$x), length(input$x) >= 1L,
      all(vapply(input$x, is.double, logical(1))))
    for (x in input$x) stopifnot(length(x) >= 1L, all(x >= input$n), all(x <= input$n + 0.5))
    FALSE
  })
  result <- check_law(law, tests = 1L)
  expect_identical(result@status, "falsified")
  expect_identical(result@shrink_status, "complete")
  expect_identical(result@counterexample@minimal$input, list(n = 1L, x = list(1)))
  expect_identical(gen_example(gen_vector(gen_double(), 0L, 0L)), double())
})

# Exceptional doubles are explicit choices; zero weights remove both draw and shrink.
for (exception in c(NA_real_, NaN, -Inf, Inf)) {
  only_exception <- gen_choice(gen_double(), gen_constant(exception), prob = c(0, 1))
  expect_identical(gen_example(gen_vector(only_exception, 2L, 2L)), rep(exception, 2L))
  result <- check_law(new_law("exception", list(x = only_exception), function(x) FALSE))
  expect_identical(result@counterexample@minimal$x, exception)
  expect_identical(result@shrink_status, "complete")
}
only_finite <- gen_choice(gen_double(), gen_element(c(NA_real_, NaN, -Inf, Inf)), prob = c(1, 0))
expect_true(all(is.finite(gen_example(gen_vector(only_finite, 20L, 20L), size = 100L))))

# New draws and shrink traversal leave the caller's RNG configuration intact.
local({
  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- .Random.seed
  on.exit({
    suppressWarnings(do.call(RNGkind, as.list(old_kind)))
    if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  })
  set.seed(38L, kind = "L'Ecuyer-CMRG")
  before <- .Random.seed
  kind <- RNGkind()
  generator <- gen_resize(gen_double(-4, 8), 100L)
  first <- gen_example(generator, seed = 91L)
  expect_identical(gen_example(generator, seed = 91L), first)
  check_law(new_law("shrinks", list(x = generator), function(x) FALSE))
  expect_identical(.Random.seed, before)
  expect_identical(RNGkind(), kind)
  rm(".Random.seed", envir = .GlobalEnv)
  gen_example(generator)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})

for (bad in list(NULL, double(), c(1, 2), NA_real_, NaN, -Inf, Inf, "1", TRUE, 1+1i)) {
  expect_error(gen_double(min = bad))
  expect_error(gen_double(max = bad))
  if (!is.null(bad)) expect_error(gen_double(origin = bad))
}
expect_error(gen_double(2, 1), pattern = "ordered bounds")
expect_error(gen_double(0, 1, origin = -1), pattern = "within the bounds")
expect_error(gen_double(0, 1, origin = 2), pattern = "within the bounds")
