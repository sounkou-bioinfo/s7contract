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
    holds = S7::class_function,
    classify = S7::class_function,
    min_coverage = S7::new_property(
      S7::class_numeric,
      default = numeric(),
      validator = function(value) {
        if (anyNA(value) || any(value < 0 | value > 1)) {
          return("Minimum coverage must contain finite proportions between zero and one.")
        }
        if (length(value) == 0L) return(NULL)
        nms <- names(value)
        if (is.null(nms) || !is.null(dim(value))) {
          return("Minimum coverage must be a named numeric vector.")
        }
        if (anyNA(nms) || any(!nzchar(nms)) || anyDuplicated(nms)) {
          return("Coverage labels must be non-missing, non-empty and unique.")
        }
      }
    )
  ),
  validator = function(self) {
    if (length(self@name) != 1L || is.na(self@name) || !nzchar(self@name)) {
      return("`name` must be one non-empty string.")
    }
    problem <- .generator_list_error(self@generators)
    if (!is.null(problem)) {
      return(paste("`generators`:", problem))
    }
    generator_names <- names(self@generators)
    for (callback in c("holds", "classify")) {
      callback_formals <- names(formals(S7::prop(self, callback)))
      if (!"..." %in% callback_formals && !all(generator_names %in% callback_formals)) {
        return(sprintf("`%s` must accept every named generator argument or `...`.", callback))
      }
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
    condition = S7::class_any,
    original_condition = S7::class_any
  )
)

s7_command <- S7::new_class(
  "s7_command",
  package = "s7contract",
  properties = list(
    name = S7::class_character,
    generate = S7::class_function,
    execute = S7::class_function,
    update = S7::class_function,
    ensure = S7::class_function,
    require = S7::class_function
  ),
  validator = function(self) {
    if (length(self@name) != 1L || is.na(self@name) || !nzchar(self@name)) {
      "`name` must be one non-empty string."
    }
  }
)

# References keep producer identities when earlier commands are removed.
s7_command_ref <- S7::new_class(
  "s7_command_ref",
  package = "s7contract",
  properties = list(id = S7::class_integer)
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
    parameters = S7::class_list,
    counterexample = S7::class_any,
    condition = S7::class_any,
    shrink_status = S7::class_character,
    shrink_condition = S7::class_any,
    coverage = S7::class_data.frame,
    coverage_cases = S7::class_integer
  ),
  validator = function(self) {
    if (
      length(self@status) != 1L ||
        !self@status %in% c("passed", "falsified", "error", "exhausted", "insufficient_coverage")
    ) {
      "`status` must be passed, falsified, error, exhausted, or insufficient_coverage."
    }
  }
)

.is_generator <- function(x) {
  S7::S7_inherits(x, s7_generator)
}

# Shared admission invariant for laws and product generators.
.generator_list_error <- function(generators) {
  nms <- names(generators)
  if (length(generators) == 0L || is.null(nms)) {
    return("A non-empty list of named generators is required.")
  }
  if (anyNA(nms) || any(!nzchar(nms)) || anyDuplicated(nms)) {
    return("Generator names must be non-missing, non-empty and unique.")
  }
  if (!all(vapply(generators, .is_generator, logical(1)))) {
    return("Every element must be a generator.")
  }
  NULL
}
