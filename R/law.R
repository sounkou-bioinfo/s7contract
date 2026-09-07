# Property-based law execution.

.count_arg <- function(x, arg, positive = FALSE, signed = FALSE) {
  lower <- if (signed) {
    -.Machine$integer.max
  } else if (positive) {
    1L
  } else {
    0L
  }
  if (!is.numeric(x) || length(x) != 1L) {
    .abort("`%s` must be one integer.", arg)
  }
  # R's integer conversion owns representability, including the NA sentinel.
  value <- suppressWarnings(as.integer(x))
  if (is.na(value) || value != x || value < lower) {
    qualifier <- if (signed) "" else if (positive) "positive " else "non-negative "
    .abort("`%s` must be one %sinteger.", arg, qualifier)
  }
  value
}

# Owns the save/set/restore transaction so every exit path restores global RNG.
.with_seed <- function(seed, rng_kind, code) {
  old_kind <- RNGkind()
  if (identical(old_kind[[2L]], "Box-Muller")) {
    .abort(paste(
      "Generator runs cannot restore the cached Box-Muller normal draw.",
      "Select another normal RNG kind before running laws."
    ))
  }
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    # Re-selecting the caller's legacy Rounding sampler emits a warning in R.
    suppressWarnings(do.call(RNGkind, as.list(old_kind)))
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  })
  set.seed(seed, kind = rng_kind[[1L]], normal.kind = rng_kind[[2L]],
           sample.kind = rng_kind[[3L]])
  force(code)
}

.evaluate_law <- function(law, arguments) {
  caught <- NULL
  warnings <- list()
  value <- tryCatch(
    withCallingHandlers(
      do.call(law@holds, arguments),
      warning = function(w) {
        warnings[[length(warnings) + 1L]] <<- w
        tryInvokeRestart("muffleWarning")
      }
    ),
    s7contract_discard = function(e) {
      caught <<- e
      NULL
    },
    error = function(e) {
      caught <<- e
      NULL
    }
  )

  if (inherits(caught, "s7contract_discard")) {
    return(list(outcome = "discard", condition = caught))
  }
  if (inherits(caught, "s7contract_state_failure")) {
    return(list(outcome = "fail", condition = caught))
  }
  if (!is.null(caught)) {
    return(list(outcome = "error", condition = caught))
  }
  if (length(warnings) > 0L) {
    return(list(outcome = "error", condition = warnings[[1L]]))
  }
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    return(list(
      outcome = "error",
      condition = simpleError("A law must return one non-missing logical value.")
    ))
  }
  list(
    outcome = if (value) "pass" else "fail",
    condition = NULL
  )
}

# Owns ordered, budgeted search over one integrated shrink tree.
.shrink_law <- function(law, tree, evaluation, limit) {
  current <- tree
  current_evaluation <- evaluation
  attempts <- 0L
  accepted <- 0L

  complete <- FALSE
  next_child <- NULL

  problem <- tryCatch({
    state_problem <- .state_shrink_problem(evaluation)
    if (!is.null(state_problem)) stop(state_problem)
    while (attempts < limit) {
      if (is.null(next_child)) {
        next_child <- .rose_children(current)
      }
      child <- next_child()
      if (is.null(child)) {
        complete <- TRUE
        break
      }
      attempts <- attempts + 1L
      candidate <- .evaluate_law(law, child$value)
      state_problem <- .state_shrink_problem(candidate)
      if (!is.null(state_problem)) stop(state_problem)
      if (!identical(current_evaluation$outcome, candidate$outcome)) next
      if (inherits(current_evaluation$condition, "s7contract_state_failure") &&
          !identical(current_evaluation$condition$command, candidate$condition$command)) {
        next
      }
      if (identical(candidate$outcome, "error") &&
          !identical(class(current_evaluation$condition)[[1L]], class(candidate$condition)[[1L]])) {
        next
      }
      current <- child
      current_evaluation <- candidate
      accepted <- accepted + 1L
      next_child <- NULL
    }
    NULL
  }, error = identity, warning = identity)

  list(
    tree = current,
    evaluation = current_evaluation,
    shrinks = accepted,
    attempts = attempts,
    status = if (!is.null(problem)) "error" else if (complete) "complete" else "budget",
    condition = problem
  )
}

.new_check_result <- function(
  law,
  status,
  tests,
  attempts,
  discards,
  seed,
  rng_kind,
  parameters,
  counterexample = NULL,
  condition = NULL,
  shrinks = 0L,
  shrink_attempts = 0L,
  shrink_status = "not_needed",
  shrink_condition = NULL
) {
  s7_check_result(
    law = law,
    status = status,
    tests = as.integer(tests),
    attempts = as.integer(attempts),
    discards = as.integer(discards),
    shrinks = as.integer(shrinks),
    shrink_attempts = as.integer(shrink_attempts),
    seed = seed,
    rng_kind = rng_kind,
    parameters = parameters,
    counterexample = counterexample,
    condition = condition,
    shrink_status = shrink_status,
    shrink_condition = shrink_condition
  )
}

#' Define and check a generative law
#'
#' A law is a named, universally quantified claim sampled over explicitly
#' supplied generators. `check_law()` is test-framework neutral and returns a
#' structured S7 result. `expect_law()` adapts that result to one tinytest
#' expectation, regardless of how many generated cases were checked.
#'
#' Runs use Mersenne-Twister, Inversion normals, and Rejection sampling,
#' independently of the caller's RNG kind. Box-Muller callers are rejected before
#' any RNG state is changed because R does not expose their cached normal draw.
#' Replay requires unchanged generator/law code, run parameters, and compatible
#' R/package versions; generators and laws must not depend on external mutable
#' state or change the RNG configuration. The result's `parameters` list records
#' all run arguments except `law`, for use with `do.call(check_law, ...)`.
#'
#' Shrinking is an ordered search, not a guarantee of a global minimum. The
#' counterexample's `minimal` field holds the smallest example found. A result's
#' `shrink_status` is `"complete"` when no immediate child preserves the
#' failure, `"budget"` when the evaluation limit stopped the search (including
#' zero), `"error"` if constructing candidates failed, or `"not_needed"` when no
#' counterexample was found. A shrinking error or warning is stored separately
#' in `shrink_condition`; the original and last failing examples are retained.
#' Generator warnings and errors terminate the run with status `"error"`.
#' Warnings or errors from `holds` are counterexamples.
#' Stateful laws created by [new_state_law()] additionally retain failure traces
#' in the counterexample's `original_condition` and `condition` fields. Callback
#' defects stop their shrink search, preserving any earlier false postcondition.
#'
#' In a tinytest file, call `tinytest::using(s7contract)` before calling
#' `expect_law()`. This activates tinytest's supported extension capture so the
#' property run is recorded as one ordinary test result.
#'
#' @param name Non-empty description of the law.
#' @param generators Uniquely named non-empty list of generators.
#' @param holds Function accepting the generated arguments and returning one
#'   non-missing logical value.
#' @param law A law created by `new_law()`.
#' @param tests Number of passing cases required.
#' @param seed Deterministic local random seed. The caller's RNG kind and state
#'   are restored after the run.
#' @param shrinks Maximum number of candidate shrink evaluations.
#' @param discards Maximum number of discarded generated cases.
#' @param max_size Maximum size passed to generators.
#' @param condition Scalar logical precondition.
#' @param x A result returned by `check_law()`.
#' @return `new_law()` returns an S7 law. `check_law()` returns an S7 check
#'   result. `expect_law()` returns one `tinytest` result. `assume()` returns
#'   invisibly when its condition is true and otherwise discards the case.
#' @examples
#' reverse_law <- new_law(
#'   "reverse is involutive",
#'   generators = list(x = gen_vector(gen_integer(), max = 8L)),
#'   holds = function(x) identical(rev(rev(x)), x)
#' )
#' check_law(reverse_law, tests = 20L, seed = 1L)
#' @export
new_law <- function(name, generators, holds) {
  s7_law(name = name, generators = generators, holds = holds)
}

#' @rdname new_law
#' @export
assume <- function(condition) {
  if (!is.logical(condition) || length(condition) != 1L || is.na(condition)) {
    .abort("`condition` must be one non-missing logical value.")
  }
  if (!condition) {
    stop(structure(
      list(
        message = "Law precondition was not satisfied.",
        call = NULL
      ),
      class = c("s7contract_discard", "error", "condition")
    ))
  }
  invisible(NULL)
}

#' @rdname new_law
#' @export
check_law <- function(
  law,
  tests = getOption("s7contract.tests", 100L),
  seed = getOption("s7contract.seed", 1L),
  shrinks = getOption("s7contract.shrinks", 100L),
  discards = getOption("s7contract.discards", 100L),
  max_size = getOption("s7contract.max_size", 100L)
) {
  if (!S7::S7_inherits(law, s7_law)) {
    .abort("`law` must be created by new_law().")
  }
  tests <- .count_arg(tests, "tests", positive = TRUE)
  seed <- .count_arg(seed, "seed", signed = TRUE)
  shrinks <- .count_arg(shrinks, "shrinks")
  discards <- .count_arg(discards, "discards")
  max_size <- .count_arg(max_size, "max_size")
  rng_kind <- c("Mersenne-Twister", "Inversion", "Rejection")
  parameters <- list(
    tests = tests, seed = seed, shrinks = shrinks,
    discards = discards, max_size = max_size
  )

  .with_seed(seed, rng_kind, {
    passed <- 0L
    attempts <- 0L
    discarded <- 0L

    while (passed < tests) {
      attempts <- attempts + 1L
      size <- min(attempts - 1L, max_size)
      tree <- tryCatch(
        {
          trees <- lapply(
            law@generators,
            function(generator) generator@draw(size)
          )
          names(trees) <- names(law@generators)
          .product_rose(trees)
        },
        error = identity,
        warning = identity
      )
      if (inherits(tree, "condition")) {
        return(.new_check_result(
          law,
          "error",
          passed,
          attempts,
          discarded,
          seed,
          rng_kind,
          parameters,
          condition = tree
        ))
      }

      evaluation <- .evaluate_law(law, tree$value)
      if (identical(evaluation$outcome, "pass")) {
        passed <- passed + 1L
        next
      }
      if (identical(evaluation$outcome, "discard")) {
        discarded <- discarded + 1L
        if (discarded > discards) {
          return(.new_check_result(
            law,
            "exhausted",
            passed,
            attempts,
            discarded,
            seed,
            rng_kind,
            parameters,
            condition = evaluation$condition
          ))
        }
        next
      }

      reduced <- .shrink_law(law, tree, evaluation, shrinks)
      counterexample <- s7_counterexample(
        original = tree$value,
        minimal = reduced$tree$value,
        outcome = reduced$evaluation$outcome,
        condition = reduced$evaluation$condition,
        original_condition = evaluation$condition
      )
      status <- if (identical(evaluation$outcome, "fail")) {
        "falsified"
      } else {
        "error"
      }
      return(.new_check_result(
        law,
        status,
        passed,
        attempts,
        discarded,
        seed,
        rng_kind,
        parameters,
        counterexample = counterexample,
        condition = reduced$evaluation$condition,
        shrinks = reduced$shrinks,
        shrink_attempts = reduced$attempts,
        shrink_status = reduced$status,
        shrink_condition = reduced$condition
      ))
    }

    .new_check_result(
      law,
      "passed",
      passed,
      attempts,
      discarded,
      seed,
      rng_kind,
      parameters
    )
  })
}

#' @rdname new_law
#' @export
format_check_result <- function(x) {
  if (!S7::S7_inherits(x, s7_check_result)) {
    .abort("`x` must be returned by check_law().")
  }
  header <- switch(
    x@status,
    passed = sprintf(
      "Law '%s' passed %d tests (seed %d).",
      x@law@name,
      x@tests,
      x@seed
    ),
    exhausted = sprintf(
      "Law '%s' exhausted after %d discards and %d passes (seed %d).",
      x@law@name,
      x@discards,
      x@tests,
      x@seed
    ),
    falsified = sprintf(
      "Law '%s' was falsified after %d attempts and %d shrinks (seed %d).",
      x@law@name,
      x@attempts,
      x@shrinks,
      x@seed
    ),
    error = sprintf(
      "Law '%s' errored after %d attempts and %d shrinks (seed %d).",
      x@law@name,
      x@attempts,
      x@shrinks,
      x@seed
    )
  )
  if (is.null(x@counterexample)) {
    detail <- if (is.null(x@condition)) character() else conditionMessage(x@condition)
    return(paste(c(header, detail), collapse = "\n"))
  }
  detail <- if (is.null(x@counterexample@condition)) {
    "The law returned FALSE."
  } else {
    conditionMessage(x@counterexample@condition)
  }
  if (inherits(x@counterexample@condition, "s7contract_state_condition")) {
    condition <- x@counterexample@condition
    detail <- paste(detail, paste(utils::capture.output(utils::str(
      list(model = condition$model, input = condition$input, output = condition$output),
      max.level = 3L, list.len = 20L, vec.len = 20L, give.attr = FALSE
    )), collapse = "\n"), sep = "\n")
  }
  arguments <- paste(
    utils::capture.output(utils::str(
      x@counterexample@minimal,
      max.level = 3L,
      list.len = 20L,
      vec.len = 20L,
      give.attr = FALSE
    )),
    collapse = "\n"
  )
  shrinking <- switch(
    x@shrink_status,
    complete = "Shrinking stopped: no child of this counterexample preserves the failure.",
    budget = sprintf("Shrinking stopped at the evaluation budget (%d).", x@shrink_attempts),
    error = paste("Shrinking stopped:", conditionMessage(x@shrink_condition))
  )
  paste(
    header,
    detail,
    shrinking,
    "Smallest counterexample found:",
    arguments,
    sep = "\n"
  )
}

.print_s7_check_result <- function(x, ...) {
  cat(format_check_result(x), "\n", sep = "")
  invisible(x)
}

#' @rdname new_law
#' @export
expect_law <- function(
  law,
  tests = getOption("s7contract.tests", 100L),
  seed = getOption("s7contract.seed", 1L),
  shrinks = getOption("s7contract.shrinks", 100L),
  discards = getOption("s7contract.discards", 100L),
  max_size = getOption("s7contract.max_size", 100L)
) {
  if (!requireNamespace("tinytest", quietly = TRUE)) {
    .abort("expect_law() requires the suggested package `tinytest`.")
  }
  result <- check_law(
    law,
    tests = tests,
    seed = seed,
    shrinks = shrinks,
    discards = discards,
    max_size = max_size
  )
  tinytest::expect_true(
    identical(result@status, "passed"),
    info = format_check_result(result)
  )
}
