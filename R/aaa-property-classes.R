# Internal S7 classes for generative laws and their results.

s7_generator <- S7::new_class(
  "s7_generator",
  package = "s7contract",
  properties = list(
    draw = S7::class_function,
    label = S7::class_character,
    prototype = S7::class_any
  ),
  validator = function(self) {
    if (length(self@label) != 1L || is.na(self@label) || !nzchar(self@label)) {
      return("`label` must be one non-empty string.")
    }
    if (length(self@prototype) != 0L) {
      return("`prototype` must have length zero.")
    }
  }
)

s7_law <- S7::new_class(
  "s7_law",
  package = "s7contract",
  properties = list(
    name = S7::class_character,
    generators = S7::class_list,
    holds = S7::class_function
  ),
  validator = function(self) {
    if (length(self@name) != 1L || is.na(self@name) || !nzchar(self@name)) {
      return("`name` must be one non-empty string.")
    }
    generator_names <- names(self@generators)
    valid_names <- length(self@generators) > 0L &&
      !is.null(generator_names) && !anyNA(generator_names) &&
      all(nzchar(generator_names)) && !anyDuplicated(generator_names)
    if (!valid_names) {
      return("`generators` must be a non-empty list with unique names.")
    }
    if (!all(vapply(self@generators, .is_generator, logical(1)))) {
      return("Every element of `generators` must be a generator.")
    }
    law_formals <- names(formals(self@holds))
    if (!"..." %in% law_formals && !all(generator_names %in% law_formals)) {
      return("`holds` must accept every named generator argument or `...`.")
    }
    NULL
  }
)

s7_counterexample <- S7::new_class(
  "s7_counterexample",
  package = "s7contract",
  properties = list(
    original = S7::class_list,
    minimal = S7::class_list,
    outcome = S7::class_character,
    condition = S7::class_any
  )
)

s7_check_result <- S7::new_class(
  "s7_check_result",
  package = "s7contract",
  properties = list(
    law = s7_law,
    status = S7::class_character,
    tests = S7::class_integer,
    attempts = S7::class_integer,
    discards = S7::class_integer,
    shrinks = S7::class_integer,
    shrink_attempts = S7::class_integer,
    seed = S7::class_integer,
    rng_kind = S7::class_character,
    counterexample = S7::class_any,
    condition = S7::class_any
  ),
  validator = function(self) {
    if (
      length(self@status) != 1L ||
        !self@status %in% c("passed", "falsified", "error", "exhausted")
    ) {
      "`status` must be passed, falsified, error, or exhausted."
    }
  }
)

.is_generator <- function(x) {
  S7::S7_inherits(x, s7_generator)
}

