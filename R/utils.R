.abort <- function(..., call. = FALSE) {
  stop(sprintf(...), call. = call.)
}

.check_name <- function(name, arg = "name") {
  if (
    !is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name)
  ) {
    .abort("`%s` must be a non-empty, non-missing string.", arg)
  }
  invisible(NULL)
}

.check_names <- function(names, arg) {
  if (
    is.null(names) ||
      anyNA(names) ||
      !all(nzchar(names)) ||
      anyDuplicated(names)
  ) {
    .abort("`%s` names must be non-missing, non-empty and unique.", arg)
  }
  invisible(NULL)
}

.as_class_or_null <- function(x, arg = "x") {
  tryCatch(S7::as_class(x, arg = arg), error = function(e) NULL)
}

.check_s7_generic <- function(x, arg = "generic") {
  if (!is.function(x) || !inherits(x, "S7_generic")) {
    .abort("`%s` must be an S7 generic created with S7::new_generic().", arg)
  }
  invisible(x)
}

.class_key <- function(class) {
  if (inherits(class, "S7_S3_class")) {
    return(list(kind = "S3", name = class$class[[1L]]))
  }
  if (isS4(class)) {
    return(list(kind = "S4", package = class@package, name = class@className))
  }
  if (inherits(class, "S7_union")) {
    return(list(kind = "union", classes = lapply(class$classes, .class_key)))
  }
  name <- if (is.null(class)) {
    "NULL"
  } else if (identical(class, S7::class_any)) {
    "ANY"
  } else if (identical(class, S7::class_missing)) {
    "MISSING"
  } else {
    nameOfClass(class)
  }
  list(kind = base::class(class)[[1L]], name = name)
}

.class_label <- function(class) {
  if (inherits(class, "S7_union")) {
    return(paste(
      vapply(class$classes, .class_label, character(1)),
      collapse = " | "
    ))
  }
  key <- .class_key(class)
  name <- switch(
    key$kind,
    S3 = paste0("S3/", key$name),
    S4 = paste0("S4/", key$package, "::", key$name),
    key$name
  )
  sprintf("<%s>", name)
}

.target_class_or_null <- function(x, arg = "x") {
  cls <- .as_class_or_null(x, arg = arg)
  if (!is.null(cls)) {
    return(cls)
  }
  cls <- S7::S7_class(x)
  if (!is.null(cls)) {
    return(cls)
  }
  if (isS4(x)) {
    return(S7::as_class(methods::getClass(class(x))))
  }
  if (is.object(x)) {
    return(S7::new_S3_class(class(x)))
  }

  switch(
    typeof(x),
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
    closure = ,
    special = ,
    builtin = S7::class_function,
    environment = S7::class_environment,
    NULL
  )
}

.normalise_requirements <- function(x, descriptor, constructor, arg) {
  if (is.null(x)) {
    x <- list()
  }
  if (is.function(x) || S7::S7_inherits(x, descriptor)) {
    x <- list(x)
  }
  if (!is.list(x)) {
    .abort("`%s` must be a list of S7 generics or requirements.", arg)
  }
  nms <- names(x)
  if (is.null(nms)) {
    nms <- rep("", length(x))
  }
  if (anyNA(nms)) {
    .abort("`%s` names must be non-missing.", arg)
  }
  for (i in seq_along(x)) {
    req <- x[[i]]
    if (!S7::S7_inherits(req, descriptor)) {
      req <- constructor(req)
    }
    if (nzchar(nms[[i]])) {
      req@name <- nms[[i]]
    }
    x[[i]] <- req
    nms[[i]] <- req@name
  }
  names(x) <- nms
  .merge_requirements(x)
}

.normalise_parents <- function(parents, descriptor, constructor) {
  if (is.null(parents)) {
    return(list())
  }
  if (S7::S7_inherits(parents, descriptor)) {
    parents <- list(parents)
  }
  if (
    !is.list(parents) ||
      !all(vapply(parents, S7::S7_inherits, logical(1), descriptor))
  ) {
    .abort("`parents` must contain descriptors created with %s().", constructor)
  }
  parents
}

.merge_requirements <- function(requirements) {
  bindings <- out <- list()
  for (req in requirements) {
    nms <- unique(c(req@name, req@generic@name))
    .check_names(nms, "requirement")
    for (name in nms) {
      other <- bindings[[name]]
      if (!is.null(other)) {
        comparable <- req
        comparable@name <- other@name
        if (!identical(comparable, other)) {
          .abort("Conflicting requirements for name `%s`.", name)
        }
      }
      bindings[[name]] <- req
    }
    out[[req@name]] <- req
  }
  out
}
