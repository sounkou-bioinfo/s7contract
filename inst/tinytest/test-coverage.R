library(S7)

# Fixed sizes give exact counts, independent of random luck.
sizes <- new_generator(function(size) size)
counted <- new_law("count generated cases", list(x = sizes), function(x) TRUE,
  classify = function(x) c(
    ignored_name = "all", "all",
    if (x %% 2L == 0L) "even" else "odd",
    if (x == 9L) "last"
  ),
  min_coverage = c(even = 0.5, odd = 0.5, missing = 0)
)
result <- check_law(counted, tests = 10L)
expect_identical(result@status, "passed")
expect_identical(result@coverage_cases, 10L)
expect_identical(result@coverage, data.frame(
  label = c("even", "odd", "missing", "all", "last"),
  count = c(5L, 5L, 0L, 10L, 1L),
  proportion = c(0.5, 0.5, 0, 1, 0.1),
  minimum = c(0.5, 0.5, 0, NA_real_, NA_real_),
  met = c(TRUE, TRUE, TRUE, NA, NA)
))

# Coverage thresholds are inclusive; unmet coverage fails without a counterexample.
too_high <- counted
too_high@min_coverage <- c(even = 0.5001, never = 1)
insufficient <- check_law(too_high, tests = 10L)
expect_identical(insufficient@status, "insufficient_coverage")
expect_identical(insufficient@tests, 10L)
expect_identical(insufficient@counterexample, NULL)
expect_identical(insufficient@condition, NULL)
expect_identical(insufficient@shrink_status, "not_needed")
expect_identical(insufficient@coverage$count[1:2], c(5L, 0L))
expect_identical(insufficient@coverage$met[1:2], c(FALSE, FALSE))
expect_true(grepl('"never": 0/10 (0%; minimum 100% unmet)',
                  format_check_result(insufficient), fixed = TRUE))
expect_true(grepl("missed coverage requirements", format_check_result(insufficient), fixed = TRUE))
adapted <- expect_law(too_high, tests = 10L)
expect_false(isTRUE(adapted))
expect_identical(length(adapted), 1L)
expect_true(grepl("minimum 100% unmet", attr(adapted, "info"), fixed = TRUE))

# Requirements can name labels that the classifier never returns.
unlabelled <- new_law("unlabelled", list(x = sizes), function(x) TRUE,
                      min_coverage = c(required = 0.1))
expect_identical(check_law(unlabelled, tests = 3L)@status, "insufficient_coverage")
unlabelled@min_coverage <- numeric()
plain <- check_law(unlabelled, tests = 3L)
expect_identical(plain@coverage_cases, 3L)
expect_identical(nrow(plain@coverage), 0L)
expect_identical(names(plain@coverage), c("label", "count", "proportion", "minimum", "met"))
expect_identical(format_check_result(plain), "Law 'unlabelled' passed 3 tests (seed 1).")
unlabelled@classify <- function(x) NULL
expect_identical(check_law(unlabelled, tests = 3L)@coverage, plain@coverage)

# Discarded cases advance size but contribute neither labels nor denominator.
local({
  classified <- 0L
  law <- new_law("odd sizes", list(x = sizes), function(x) {
    assume(x %% 2L == 1L)
    TRUE
  }, classify = function(x) {
    classified <<- classified + 1L
    c(if (x %% 2L == 0L) "discarded" else "accepted", if (x >= 4L) "large")
  }, min_coverage = c(accepted = 1, large = 0.6))
  result <- check_law(law, tests = 5L)
  expect_identical(result@status, "passed")
  expect_identical(result@attempts, 10L)
  expect_identical(result@discards, 5L)
  expect_identical(result@coverage_cases, 5L)
  expect_identical(result@coverage$count, c(5L, 3L))
  expect_false("discarded" %in% result@coverage$label)
  expect_identical(classified, result@attempts)
})

# Count a false generated case, never its shrinks, and preserve falsification.
local({
  classified <- 0L
  generator <- new_generator(function(size) 10L,
    function(x) if (x == 10L) list(5L) else if (x == 5L) list(0L) else list())
  law <- new_law("false with coverage", list(x = generator), function(x) FALSE,
    classify = function(x) {
      stopifnot(x == 10L)
      classified <<- classified + 1L
      "original"
    }, min_coverage = c(unseen = 0.5))
  result <- check_law(law, tests = 10L)
  expect_identical(result@status, "falsified")
  expect_identical(result@coverage_cases, 1L)
  expect_identical(result@coverage$count, c(0L, 1L))
  expect_identical(result@coverage$met, c(FALSE, NA))
  expect_identical(result@counterexample@minimal$x, 0L)
  expect_identical(result@shrinks, 2L)
  expect_identical(classified, 1L)
  expect_true(grepl("1 accepted cases; partial run", format_check_result(result), fixed = TRUE))
})

# Exhaustion with no accepted cases leaves coverage unassessed.
never <- new_law("discard all", list(x = sizes), function(x) {
  assume(FALSE)
  TRUE
}, classify = function(x) "discarded", min_coverage = c(required = 0, absent = 1))
exhausted <- check_law(never, tests = 2L, discards = 2L)
expect_identical(exhausted@status, "exhausted")
expect_identical(exhausted@coverage_cases, 0L)
expect_identical(exhausted@coverage$count, c(0L, 0L))
expect_true(all(is.na(exhausted@coverage$proportion)))
expect_true(all(is.na(exhausted@coverage$met)))
expect_true(grepl("unassessed", format_check_result(exhausted), fixed = TRUE))

# Unexpected errors retain earlier counts, including classifier failures.
for (phase in c("draw", "classify", "holds")) {
  for (fault in c("error", "warning", "invalid")) local({
    evaluated <- 0L
    defect <- function() {
      switch(fault, error = stop("case failed"), warning = warning("case warned"), invalid = NA)
    }
    law <- new_law("failed case", list(x = new_generator(function(size) {
      if (phase == "draw" && size == 2L) {
        defect()
        # A custom generator can draw NA; there is no invalid scalar contract.
        if (fault == "invalid") stop("invalid draw")
      }
      size
    })), function(x) {
      evaluated <<- evaluated + 1L
      if (phase == "holds" && x == 2L) return(defect())
      TRUE
    }, classify = function(x) {
      if (phase == "classify" && x == 2L) return(defect())
      if (x == 2L) "third" else "accepted"
    }, min_coverage = c(accepted = 1))
    result <- check_law(law, tests = 5L, shrinks = 0L)
    expect_identical(result@status, "error")
    expect_identical(result@coverage_cases, 2L)
    expect_identical(result@coverage$label, "accepted")
    expect_identical(result@coverage$count, 2L)
    expect_identical(evaluated, if (phase == "holds") 3L else 2L)
    expect_identical(is.null(result@counterexample), phase != "holds")
  })
}

# Coverage is local even when a law runs another law.
local({
  inner_results <- list()
  inner <- new_law("inner", list(x = sizes), function(x) TRUE,
                   classify = function(x) "inner", min_coverage = c(inner = 1))
  outer <- new_law("outer", list(x = sizes), function(x) {
    result <- check_law(inner, tests = 2L)
    inner_results[[length(inner_results) + 1L]] <<- result
    identical(result@status, "passed")
  }, classify = function(x) c("outer", "outer"), min_coverage = c(outer = 1))
  result <- check_law(outer, tests = 4L)
  expect_identical(result@coverage$label, "outer")
  expect_identical(result@coverage$count, 4L)
  expect_identical(result@coverage_cases, 4L)
  expect_true(all(vapply(inner_results, function(x) identical(x@coverage$count, 2L), logical(1))))
})

# Stateful laws classify whole generated sequences, once before fixture setup.
local({
  classified <- 0L
  made <- 0L
  command <- new_command("false", function(state) gen_constant(NULL),
    function(fixture, input) NULL, function(state, input, output) FALSE)
  law <- new_state_law("state coverage", NULL, list(command),
    setup = function() { made <<- made + 1L; NULL }, max_commands = 1L,
    classify = function(sequence) {
      classified <<- classified + 1L
      if (length(sequence) == 0L) "empty" else "nonempty"
    }, min_coverage = c(nonempty = 1))
  result <- check_law(law, tests = 20L)
  expect_identical(result@status, "falsified")
  expect_identical(result@coverage_cases, result@attempts)
  expect_identical(sum(result@coverage$count), result@coverage_cases)
  expect_identical(result@coverage$count[[1L]], 1L)
  expect_identical(classified, result@attempts)
  expect_identical(made, result@attempts + result@shrink_attempts)
  made <- 0L
  law@classify <- function(sequence) stop("classifier failed")
  expect_identical(check_law(law)@status, "error")
  expect_identical(made, 0L)
})

# Adding deterministic labels changes neither generated draws nor caller RNG.
local({
  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- .Random.seed
  on.exit({
    suppressWarnings(do.call(RNGkind, as.list(old_kind)))
    if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  })
  seen <- numeric()
  law <- new_law("draws", list(x = new_generator(function(size) runif(1L))), function(x) {
    seen <<- c(seen, x)
    runif(1L)
    TRUE
  })
  set.seed(418L)
  caller_seed <- .Random.seed
  plain <- check_law(law, tests = 20L, seed = 7L)
  expected <- seen
  seen <- numeric()
  law@classify <- function(x) if (x < 0.5) "lower" else "upper"
  labelled <- do.call(check_law, c(list(law = law), plain@parameters))
  expect_identical(seen, expected)
  expect_identical(.Random.seed, caller_seed)
  expect_identical(RNGkind(), old_kind)
  expect_identical(labelled@parameters, plain@parameters)
  expect_identical(labelled@attempts, plain@attempts)
  replayed <- do.call(check_law, c(list(law = law), labelled@parameters))
  expect_identical(replayed@coverage, labelled@coverage)
  rm(".Random.seed", envir = .GlobalEnv)
  check_law(law, tests = 1L)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})

# Invalid requirements and labels fail at their public admission boundaries.
for (minimum in list(c(x = -0.1), c(x = 1.1), c(x = Inf), c(x = NA_real_),
                     c(x = NaN), 0.5, c(x = 0.1, x = 0.2),
                     structure(0.1, names = ""), structure(0.1, names = NA_character_),
                     list(x = 0.1), matrix(0.1, 1L, 1L))) {
  expect_error(new_law("invalid minimum", list(x = sizes), function(x) TRUE,
                       min_coverage = minimum))
}
expect_error(new_law("invalid classifier", list(x = sizes), function(x) TRUE, classify = 1L))
expect_error(new_law("wrong formals", list(x = sizes), function(x) TRUE, classify = function(y) "label"),
              pattern = "`classify` must accept", fixed = TRUE)
for (labels in list(1L, TRUE, NA_character_, "", c("ok", NA_character_), list("label"))) {
  result <- check_law(new_law("invalid label", list(x = sizes), function(x) TRUE,
                               classify = function(x) labels), tests = 1L)
  expect_identical(result@status, "error")
  expect_identical(result@coverage_cases, 0L)
}

# Print unmet requirements first and limit output while retaining all data.
minimum <- rep(0, 25L)
names(minimum) <- paste0("label", seq_along(minimum))
minimum[[25L]] <- 1
result <- check_law(new_law("many labels", list(x = sizes), function(x) TRUE,
                            min_coverage = minimum), tests = 1L)
printed <- strsplit(format_check_result(result), "\n", fixed = TRUE)[[1L]]
expect_true(grepl('"label25": 0/1', printed[[3L]], fixed = TRUE))
expect_identical(length(printed), 23L)
expect_identical(nrow(result@coverage), 25L)
expect_true(grepl("5 more labels in @coverage", printed[[23L]], fixed = TRUE))
