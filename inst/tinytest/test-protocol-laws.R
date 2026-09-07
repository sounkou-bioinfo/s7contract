# Vector protocol conformance, behavioral laws, and shrinking.
sys.source(system.file("examples", "vector-laws.R", package = "s7contract"),
           envir = environment())

for (results in vector_results) {
  expect_identical(vapply(results, function(result) result@status, character(1)),
                   c(values = "passed", length = "passed",
                     slice_values = "passed", slice_length = "passed"))
  expect_true(all(vapply(results, function(result) result@discards == 0L, logical(1))))
}

# ReversedDepth has the required methods but fails the slicing law.
expect_true(implements(ReversedDepth, VectorLike))
expect_identical(vapply(broken_results, function(result) result@status, character(1)),
                 c(values = "passed", length = "passed",
                   slice_values = "falsified", slice_length = "passed"))
expect_identical(failure@counterexample@outcome, "fail")
expect_identical(failure@condition, NULL)
expect_identical(failure@shrink_status, "complete")
expect_true(failure@shrinks > 0L)
expect_identical(replayed@counterexample@minimal, failure@counterexample@minimal)
expect_identical(replayed@shrink_attempts, failure@shrink_attempts)

# Shrunk examples have valid objects and indices; slice values violate the law.
for (arguments in list(failure@counterexample@original, failure@counterexample@minimal)) {
  input <- arguments$input
  expect_true(all(input$i %in% seq_along(input$values)))
  expect_identical(input$x@depth, input$values)
  expect_identical(length(input$x@position), length(input$x@depth))
  selected <- with(VectorLike, vec_values(vec_slice(input$x, input$i)))
  expect_identical(typeof(selected), "double")
  expect_false(identical(selected, input$values[input$i]))
}

# The domain includes empty vectors, empty selections, duplicates, and reordering.
for (make in implementations) {
  laws <- vector_laws(make)
  for (values in list(double(), c(-1, 0, 2), c(-0.25, 0, 0.5))) {
    indices <- list(integer())
    if (length(values) > 0L) indices <- c(indices, list(c(3L, 1L, 3L)))
    for (i in indices) {
      input <- list(x = make(values), values = values, i = i)
      expect_true(all(vapply(laws, function(law) (law@holds)(input), logical(1))))
    }
  }
}

# Every visited shrink in the broken run obeys the generation domain.
visited <- 0L
audited <- new_law(failure@law@name, failure@law@generators, function(input) {
  stopifnot(all(input$i %in% seq_along(input$values)),
            identical(input$x@depth, input$values))
  visited <<- visited + 1L
  (failure@law@holds)(input)
})
audit_result <- do.call(check_law, c(list(law = audited), failure@parameters))
expect_identical(audit_result@counterexample@minimal, failure@counterexample@minimal)
expect_identical(visited, audit_result@attempts + audit_result@shrink_attempts)

# Coverage counts generated cases and enforces the requested minimum proportions.
for (results in vector_results) {
  coverage <- results$slice_values@coverage
  expect_identical(coverage$label, c("empty", "nonempty", "repeated", "reordered",
                                    "subsequence", "permutation", "fractional"))
  expect_true(all(coverage$met))
  expect_identical(sum(coverage$count[1:2]), 100L)
  expect_identical(results$slice_values@coverage_cases, 100L)
}
expect_identical(empty_only@status, "insufficient_coverage")
expect_identical(empty_only@tests, 10L)
expect_identical(empty_only@counterexample, NULL)
expect_identical(empty_only@coverage$count, c(10L, 0L, 0L, 0L, 10L, 10L, 0L))
expect_identical(empty_only@coverage$met, c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE))
expect_identical(failure@coverage_cases, failure@attempts)
expect_identical(replayed@coverage, failure@coverage)
expect_identical(failure@status, "falsified")

# Rounding preserves integer inputs but changes fractional values.
expect_true(implements(RoundedDepth, VectorLike))
expect_identical(integer_check@status, "passed")
expect_identical(fractional_check@status, "falsified")
expect_identical(fractional_check@counterexample@outcome, "fail")
for (input in list(fractional_check@counterexample@original$input,
                   fractional_check@counterexample@minimal$input)) {
  expect_true(all(is.finite(input$values)))
  expect_true(all(input$values >= -10 & input$values <= 10))
  expect_true(any(input$values != round(input$values)))
  expect_false(identical(vec_values(input$x), input$values))
}
