# Small internal helpers. Kept dependency-free on purpose.

.abort <- function(..., call. = FALSE) {
  stop(sprintf(...), call. = call.)
}

.warn <- function(..., call. = FALSE) {
  warning(sprintf(...), call. = call.)
}

.compact <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

.as_class_or_null <- function(x, arg = "x") {
  tryCatch(S7::as_class(x, arg = arg), error = function(e) NULL)
}

.is_class_spec <- function(x) {
  !is.null(.as_class_or_null(x))
}

.class_key <- function(class) {
  cls <- .as_class_or_null(class, arg = "class")
  if (is.null(cls)) {
    return(NA_character_)
  }

  nm <- tryCatch(nameOfClass(cls), error = function(e) NULL)
  if (!is.null(nm) && length(nm) == 1 && !is.na(nm) && nzchar(nm)) {
    return(nm)
  }

  line <- paste(utils::capture.output(print(cls)), collapse = " ")
  line <- gsub("\\s+", " ", line)
  trimws(line)
}

.class_equal <- function(a, b) {
  identical(a, b) || identical(.class_key(a), .class_key(b))
}

.class_label <- function(class) {
  key <- .class_key(class)
  if (is.na(key) || key == "") "<unknown class>" else sprintf("<%s>", key)
}

.base_class_of <- function(x) {
  switch(typeof(x),
    logical = S7::class_logical,
    integer = S7::class_integer,
    double = S7::class_double,
    complex = S7::class_complex,
    character = S7::class_character,
    raw = S7::class_raw,
    list = S7::class_list,
    expression = S7::class_expression,
    symbol = S7::class_name,
    language = S7::class_call,
    closure = S7::class_function,
    NULL
  )
}

.target_class_or_null <- function(x, arg = "x") {
  cls <- .as_class_or_null(x, arg = arg)
  if (!is.null(cls)) {
    return(cls)
  }

  cls <- tryCatch(S7::S7_class(x), error = function(e) NULL)
  if (!is.null(cls)) {
    return(cls)
  }

  .base_class_of(x)
}

.lookup_s7_method <- function(generic, target) {
  cls <- .as_class_or_null(target, arg = "target")

  tryCatch({
    method <- if (is.null(cls)) {
      S7::method(generic, object = target)
    } else {
      S7::method(generic, class = cls)
    }
    list(ok = TRUE, method = method, error = NULL)
  }, error = function(e) {
    list(ok = FALSE, method = NULL, error = e)
  })
}

.generic_label <- function(generic, fallback = "method") {
  if (inherits(generic, "S7_generic")) {
    return(generic@name)
  }

  line <- paste(utils::capture.output(print(generic))[1], collapse = "")
  name <- sub("^.*<S7_generic>\\s*", "", line)
  name <- sub("\\(.*$", "", name)
  name <- trimws(name)
  if (nzchar(name) && !identical(name, line)) {
    return(name)
  }

  fallback
}

.register_s7_method <- function(generic, class, fun, replace = FALSE) {
  if (!is.function(fun)) {
    .abort("S7 method implementation must be a function")
  }

  if (!replace) {
    existing <- tryCatch(S7::method(generic, class = class), error = function(e) NULL)
    if (!is.null(existing)) {
      .warn(
        "An S7 method for %s and %s is already visible; registering anyway. Pass replace = TRUE to silence this warning.",
        .generic_label(generic), .class_label(class)
      )
    }
  }

  S7::`method<-`(generic, class, value = fun)
  invisible(fun)
}
