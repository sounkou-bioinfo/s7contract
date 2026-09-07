library(S7)

# Check every edge over all ordered samples of four source positions.
# Strict lexicographic descent and closure of this finite domain rule out cycles.
checked <- 0L
for (k in 0:4) {
  roots <- if (k == 0L) list(integer()) else {
    grid <- expand.grid(rep(list(1:4), k))
    Filter(function(x) !anyDuplicated(x), lapply(seq_len(nrow(grid)), function(i) {
      as.integer(grid[i, ])
    }))
  }
  for (root in roots) {
    next_child <- s7contract:::.rose_children(s7contract:::.sample_rose(root))
    repeat {
      child <- next_child()
      if (is.null(child)) break
      value <- child$value
      changed <- which(value != root)
      stopifnot(length(value) == k, !anyDuplicated(value), all(value %in% 1:4),
                length(changed) > 0L, value[changed[1L]] < root[changed[1L]])
      checked <- checked + 1L
    }
  }
}
expect_true(checked > 100L)

# Fixed cardinality ignores runner size; every ordered pair is reachable.
draws <- vapply(1:100, function(seed) {
  paste(gen_example(gen_sample(1:3, 2L), size = 0L, seed = seed), collapse = "")
}, character(1))
expect_identical(sort(unique(draws)), c("12", "13", "21", "23", "31", "32"))
expect_identical(gen_example(gen_sample(1:20, 4L), size = 0L),
                  gen_example(gen_sample(1:20, 4L), size = 100L))
expect_identical(gen_example(gen_sample(99L)), 99L)
expect_identical(gen_example(gen_subsequence(99L, min = 1L)), 99L)

# Names, duplicate values, list entries and vector classes follow `[` semantics.
sources <- list(integer(), NULL, c(a = 9L, b = 9L, c = 2L),
                as.Date(c("2024-02-28", "2024-02-29", "2024-03-01")),
                factor(c("b", "a", "b")), as.raw(1:3), c(1i, 2i),
                list(a = NULL, b = 1:2, c = "x"))
for (source in sources) {
  for (k in unique(c(0L, length(source)))) {
    generator <- gen_sample(source, k)
    result <- check_law(new_law("sample", list(x = generator), function(x) FALSE))
    expect_identical(result@status, "falsified")
    expect_identical(result@shrink_status, "complete")
    expect_identical(result@counterexample@minimal$x, source[seq_len(k)])
    expect_identical(gen_example(gen_subsequence(source, k, k)), source[seq_len(k)])
  }
}
expect_identical(gen_example(gen_sample(c("x", "x"))), c("x", "x"))

# Every visited subsequence preserves source order and the declared length range.
for (minimum in 0:3) local({
  visits <- 0L
  law <- new_law("ordered positions",
    list(x = gen_resize(gen_subsequence(1:8, minimum, 6L), 20L)), function(x) {
      stopifnot(length(x) >= minimum, length(x) <= 6L,
                all(x %in% 1:8), !is.unsorted(x, strictly = TRUE))
      visits <<- visits + 1L
      FALSE
    })
  result <- check_law(law, tests = 1L, seed = 4L)
  expect_identical(result@counterexample@minimal$x, seq_len(minimum))
  expect_identical(result@shrink_status, "complete")
  expect_identical(visits, 1L + result@shrink_attempts)
})
for (size in 0:5) {
  lengths <- vapply(1:30, function(seed) {
    length(gen_example(gen_subsequence(1:10, 2L, 6L), size = size, seed = seed))
  }, integer(1))
  expect_true(all(lengths >= 2L & lengths <= min(6L, 2L + size)))
}

# Dependent samples remain valid after the population shrinks.
cases <- gen_bind(gen_vector(gen_integer(), min = 1L, max = 6L), function(values) {
  gen_product(values = gen_constant(values), i = gen_sample(seq_along(values)))
})
law <- new_law("dependent permutation", list(input = gen_resize(cases, 10L)), function(input) {
  stopifnot(identical(sort(input$i), seq_along(input$values)))
  FALSE
})
result <- check_law(law, tests = 1L, seed = 4L)
expect_identical(result@counterexample@minimal$input, list(values = 0L, i = 1L))
expect_identical(result@shrink_status, "complete")
replayed <- do.call(check_law, c(list(law = law), result@parameters))
expect_identical(replayed@counterexample@minimal, result@counterexample@minimal)
expect_identical(replayed@shrink_attempts, result@shrink_attempts)
expect_identical(gen_example(gen_vector(gen_sample(1:3, 0L), 2L, 2L)),
                  list(integer(), integer()))

# A small sample from an ALTREP population does not enumerate all positions.
large <- gen_sample(seq_len(.Machine$integer.max), 3L)
sampled <- gen_example(large)
expect_identical(length(sampled), 3L)
expect_identical(anyDuplicated(sampled), 0L)
expect_true(all(sampled > 0L))
result <- check_law(new_law("large", list(x = large), function(x) FALSE))
expect_identical(result@counterexample@minimal$x, 1:3)
expect_identical(result@shrink_status, "complete")

# Transformations happen only for evaluated children, including a zero budget.
local({
  calls <- 0L
  generator <- gen_map(large, function(x) { calls <<- calls + 1L; x })
  law <- new_law("budget", list(x = generator), function(x) FALSE)
  for (budget in c(0L, 2L)) {
    calls <- 0L
    result <- check_law(law, tests = 1L, shrinks = budget)
    expect_identical(calls, 1L + budget)
    expect_identical(result@shrink_attempts, budget)
    expect_identical(result@shrink_status, "budget")
  }
})
local({
  set.seed(917L)
  before <- .Random.seed
  sample <- gen_example(large, seed = 8L)
  expect_identical(gen_example(large, seed = 8L), sample)
  check_law(new_law("rng", list(x = gen_subsequence(1:20)), function(x) FALSE))
  expect_identical(.Random.seed, before)
})

for (bad in list(NA, Inf, -1L, 0.5, "1", c(1L, 2L), NULL)) {
  expect_error(gen_sample(1:3, size = bad))
  expect_error(gen_subsequence(1:3, min = bad))
  expect_error(gen_subsequence(1:3, max = bad))
}
for (bad in list(matrix(1:4, 2L), data.frame(x = 1:3), environment(), mean)) {
  expect_error(gen_sample(bad))
  expect_error(gen_subsequence(bad))
}
expect_error(gen_sample(integer(), 1L))
expect_error(gen_sample(1:3, 4L))
expect_error(gen_subsequence(1:3, 2L, 1L))
expect_error(gen_subsequence(1:3, max = 4L))
