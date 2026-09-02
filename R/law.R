# Property-based law execution.

.count_arg <- function(x, arg, positive = FALSE, signed = FALSE) {
  lower <- if (signed) {
    -.Machine$integer.max - 1
  } else if (positive) {
    1L
  } else {
    0L
  }
  valid <- is.numeric(x) && length(x) == 1L && !is.na(x) &&
    is.finite(x) && x == trunc(x) && x >= lower &&
    x <= .Machine$integer.max
  if (!valid) {
    qualifier <- if (signed) "" else if (positive) "positive " else "non-negative "
    .abort(sprintf("`%s` must be one %sinteger.", arg, qualifier))
  }
  as.integer(x)
}

# Owns the save/set/restore transaction so every exit path restores global RNG.
.with_seed <- function(seed, code) {
  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    do.call(RNGkind, as.list(old_kind))
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  })
  set.seed(seed)
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

  repeat {
    replacement <- NULL
    for (child in .rose_children(current)) {
      if (attempts >= limit) {
        break
      }
      attempts <- attempts + 1L
      candidate <- .evaluate_law(law, child$value)
      same_outcome <- identical(
        current_evaluation$outcome,
        candidate$outcome
      )
      same_condition <- TRUE
      if (same_outcome && identical(candidate$outcome, "error")) {
        same_condition <- identical(
          class(current_evaluation$condition)[[1L]],
          class(candidate$condition)[[1L]]
        )
      }
      if (same_outcome && same_condition) {
        replacement <- child
        current_evaluation <- candidate
        accepted <- accepted + 1L
        break
      }
    }
    if (is.null(replacement)) {
      break
    }
    current <- replacement
    if (attempts >= limit) {
      break
    }
  }

  list(
    tree = current,
    evaluation = current_evaluation,
    shrinks = accepted,
    attempts = attempts
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
  counterexample = NULL,
  condition = NULL,
  shrinks = 0L,
  shrink_attempts = 0L
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
    counterexample = counterexample,
    condition = condition
  )
}

#' Define and check a generative law
#'
#' A law is a named, universally quantified claim sampled over explicitly
#' supplied generators. `check_law()` is test-framework neutral and returns a
#' structured S7 result. `expect_law()` adapts that result to one tinytest
#' expectation, regardless of how many generated cases were checked.
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
  rng_kind <- RNGkind()

  .with_seed(seed, {
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
        error = identity
      )
      if (inherits(tree, "error")) {
        return(.new_check_result(
          law,
          "error",
          passed,
          attempts,
          discarded,
          seed,
          rng_kind,
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
            condition = evaluation$condition
          ))
        }
        next
      }

      reduced <- tryCatch(
        .shrink_law(law, tree, evaluation, shrinks),
        error = identity
      )
      if (inherits(reduced, "error")) {
        return(.new_check_result(
          law,
          "error",
          passed,
          attempts,
          discarded,
          seed,
          rng_kind,
          condition = reduced
        ))
      }
      counterexample <- s7_counterexample(
        original = tree$value,
        minimal = reduced$tree$value,
        outcome = reduced$evaluation$outcome,
        condition = reduced$evaluation$condition
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
        counterexample = counterexample,
        condition = reduced$evaluation$condition,
        shrinks = reduced$shrinks,
        shrink_attempts = reduced$attempts
      ))
    }

    .new_check_result(
      law,
      "passed",
      passed,
      attempts,
      discarded,
      seed,
      rng_kind
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
  paste(
    header,
    detail,
    "Minimal counterexample:",
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
