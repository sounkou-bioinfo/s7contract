.s7contract_registry <- new.env(parent = emptyenv())
.s7contract_registry$next_trait_id <- 0L
.s7contract_registry$impls <- list()

.next_trait_id <- function() {
  .s7contract_registry$next_trait_id <- .s7contract_registry$next_trait_id + 1L
  sprintf("trait:%d", .s7contract_registry$next_trait_id)
}

#' Build a Rust-like explicit trait on top of S7
#'
#' `new_trait()` adds a nominal contract registry on top of S7 dispatch. A class
#' only has the trait after `impl_trait()` records the implementation, even if
#' compatible S7 methods already exist.
#'
#' This makes default methods and associated metadata practical, but the result
#' remains a runtime R abstraction. It does not emulate Rust's compile-time
#' trait bounds, coherence, orphan rules, or type-checked associated types.
#'
#' @param name For `new_trait()`, the trait name. For `trait_method()`, the
#'   method name; it defaults to the generic name when omitted.
#' @param methods For `new_trait()`, a named list of S7 generics or
#'   `trait_method()` objects.
#' @param parents Optional trait or list of supertraits.
#' @param assoc_types Required associated type names, or a named list of default
#'   associated type values.
#' @param assoc_consts Required associated constant names, or a named list of
#'   default constant values.
#' @param package Optional package name used only for display.
#' @return `new_trait()` returns an object of class `s7_trait`.
#'   `trait_method()` returns an object of class `s7_trait_method`.
#' @examples
#' local({
#'   area <- S7::new_generic("area", "x")
#'   perimeter <- S7::new_generic("perimeter", "x")
#'
#'   Circle <- S7::new_class(
#'     "Circle",
#'     properties = list(r = S7::class_double)
#'   )
#'
#'   Measurable <- new_trait(
#'     "Measurable",
#'     methods = list(
#'       area = trait_method(area),
#'       perimeter = trait_method(perimeter, default = function(x) NA_real_)
#'     ),
#'     assoc_consts = c("UNITS")
#'   )
#'
#'   impl_trait(
#'     Measurable,
#'     Circle,
#'     methods = list(area = function(x) pi * x@r^2),
#'     assoc_consts = list(UNITS = "unitless")
#'   )
#'
#'   has_trait(Circle, Measurable)
#'   trait_call(Measurable, "area", Circle(r = 2))
#'   trait_assoc_const(Measurable, Circle, "UNITS")
#' })
#' @export
new_trait <- function(
  name,
  methods = list(),
  parents = list(),
  assoc_types = character(),
  assoc_consts = list(),
  package = NULL
) {
  if (!is.character(name) || length(name) != 1 || !nzchar(name)) {
    .abort("`name` must be a non-empty string.")
  }
  if (!is.null(package) && (!is.character(package) || length(package) != 1)) {
    .abort("`package` must be NULL or a single string.")
  }

  structure(
    list(
      id = .next_trait_id(),
      name = name,
      package = package,
      parents = .normalise_trait_parents(parents),
      methods = .normalise_trait_methods(methods),
      assoc_types = .normalise_assoc(assoc_types, "assoc_types"),
      assoc_consts = .normalise_assoc(assoc_consts, "assoc_consts")
    ),
    class = "s7_trait"
  )
}

#' @param generic An S7 generic function.
#' @param default Optional default implementation. If supplied, `impl_trait()`
#'   uses it when a class does not provide an override for that method.
#' @rdname new_trait
#' @export
trait_method <- function(generic, default = NULL, name = NULL) {
  if (!is.function(generic)) {
    .abort(
      "`generic` must be a function, usually an S7 generic created with S7::new_generic()."
    )
  }
  if (!is.null(default) && !is.function(default)) {
    .abort("`default` must be NULL or a function.")
  }
  if (is.null(name)) {
    name <- .generic_label(generic)
  }
  if (!is.character(name) || length(name) != 1 || !nzchar(name)) {
    .abort("`name` must be a non-empty string.")
  }

  structure(
    list(name = name, generic = generic, default = default),
    class = "s7_trait_method"
  )
}

.as_trait_method <- function(x, name = NULL) {
  if (inherits(x, "s7_trait_method")) {
    if (!is.null(name)) {
      x$name <- name
    }
    return(x)
  }
  if (is.function(x)) {
    return(trait_method(x, name = name))
  }
  .abort("Trait methods must be S7 generics or trait_method() objects.")
}

.normalise_trait_methods <- function(methods) {
  if (is.null(methods)) {
    methods <- list()
  }
  if (is.function(methods) || inherits(methods, "s7_trait_method")) {
    methods <- list(methods)
  }
  if (!is.list(methods)) {
    .abort("`methods` must be a list of S7 generics or trait_method() objects.")
  }

  nms <- names(methods)
  if (is.null(nms)) {
    nms <- rep("", length(methods))
  }

  out <- vector("list", length(methods))
  for (i in seq_along(methods)) {
    nm <- if (nzchar(nms[[i]])) nms[[i]] else NULL
    method <- .as_trait_method(methods[[i]], name = nm)
    out[[i]] <- method
    nms[[i]] <- method$name
  }
  names(out) <- nms
  out
}

.normalise_trait_parents <- function(parents) {
  if (is.null(parents)) {
    return(list())
  }
  if (inherits(parents, "s7_trait")) {
    parents <- list(parents)
  }
  if (!is.list(parents)) {
    .abort("`parents` must be a trait or a list of traits.")
  }
  for (parent in parents) {
    if (!inherits(parent, "s7_trait")) {
      .abort("Every parent must be created with new_trait().")
    }
  }
  parents
}

.normalise_assoc <- function(x, what) {
  if (is.null(x)) {
    return(list())
  }
  if (is.character(x)) {
    out <- lapply(x, function(name) {
      list(required = TRUE, default = NULL)
    })
    names(out) <- x
    return(out)
  }
  if (is.list(x)) {
    if (length(x) > 0 && (is.null(names(x)) || any(names(x) == ""))) {
      .abort("`%s` must be a named list or a character vector.", what)
    }
    out <- lapply(x, function(value) {
      list(required = FALSE, default = value)
    })
    return(out)
  }
  .abort("`%s` must be NULL, a character vector, or a named list.", what)
}

.trait_label <- function(trait) {
  if (!is.null(trait$package)) {
    sprintf("%s::%s", trait$package, trait$name)
  } else {
    trait$name
  }
}

#' Inspect or use a Rust-like explicit trait
#'
#' @param trait A trait created by `new_trait()`.
#' @param inherited Include inherited methods from supertraits?
#' @return `trait_methods()` returns a named list of `trait_method()` objects.
#'   `impl_trait()` returns the stored implementation record, invisibly.
#'   `trait_report()` returns a one-row data frame. `has_trait()` returns a
#'   single logical value. `assert_trait()` returns `x`, unchanged.
#'   `trait_call()` returns the result of the underlying S7 generic.
#'   `trait_assoc_type()` and `trait_assoc_const()` return the stored associated
#'   item value.
#' @rdname trait_methods
#' @export
trait_methods <- function(trait, inherited = TRUE) {
  if (!inherits(trait, "s7_trait")) {
    .abort("`trait` must be created with new_trait().")
  }

  out <- list()
  if (isTRUE(inherited)) {
    for (parent in trait$parents) {
      out <- c(out, trait_methods(parent, inherited = TRUE))
    }
  }
  out <- c(out, trait$methods)

  if (length(out) > 0) {
    out <- out[!duplicated(names(out), fromLast = TRUE)]
  }
  out
}

.trait_assoc_types <- function(trait, inherited = TRUE) {
  out <- list()
  if (isTRUE(inherited)) {
    for (parent in trait$parents) {
      out <- c(out, .trait_assoc_types(parent, inherited = TRUE))
    }
  }
  out <- c(out, trait$assoc_types)
  if (length(out) > 0) out[!duplicated(names(out), fromLast = TRUE)] else out
}

.trait_assoc_consts <- function(trait, inherited = TRUE) {
  out <- list()
  if (isTRUE(inherited)) {
    for (parent in trait$parents) {
      out <- c(out, .trait_assoc_consts(parent, inherited = TRUE))
    }
  }
  out <- c(out, trait$assoc_consts)
  if (length(out) > 0) out[!duplicated(names(out), fromLast = TRUE)] else out
}

.find_trait_impl <- function(trait, class) {
  impls <- .s7contract_registry$impls
  for (impl in impls) {
    if (identical(impl$trait_id, trait$id) && .class_equal(impl$class, class)) {
      return(impl)
    }
  }
  NULL
}

.store_trait_impl <- function(impl, replace = FALSE) {
  impls <- .s7contract_registry$impls
  keep <- rep(TRUE, length(impls))

  for (i in seq_along(impls)) {
    if (identical(impls[[i]]$trait_id, impl$trait_id) && .class_equal(impls[[i]]$class, impl$class)) {
      if (!replace) {
        .abort(
          "%s is already implemented for %s. Pass replace = TRUE to replace it.",
          impl$trait_label,
          .class_label(impl$class)
        )
      }
      keep[[i]] <- FALSE
    }
  }

  .s7contract_registry$impls <- c(impls[keep], list(impl))
  invisible(impl)
}

.normalise_impl_methods <- function(methods) {
  if (is.null(methods)) {
    return(list())
  }
  if (!is.list(methods)) {
    .abort("`methods` must be a named list of functions.")
  }
  if (length(methods) > 0 && (is.null(names(methods)) || any(names(methods) == ""))) {
    .abort("`methods` must be a named list of functions.")
  }
  for (name in names(methods)) {
    if (!is.function(methods[[name]])) {
      .abort("Implementation for `%s` must be a function.", name)
    }
  }
  methods
}

.resolve_assoc_impl <- function(required, provided, what) {
  if (is.null(provided)) {
    provided <- list()
  }
  if (!is.list(provided)) {
    .abort("`%s` must be a named list.", what)
  }
  if (length(provided) > 0 && (is.null(names(provided)) || any(names(provided) == ""))) {
    .abort("`%s` must be a named list.", what)
  }

  out <- list()
  for (name in names(required)) {
    spec <- required[[name]]
    if (name %in% names(provided)) {
      out[[name]] <- provided[[name]]
    } else if (isTRUE(spec$required)) {
      .abort("Missing required associated item `%s` in `%s`.", name, what)
    } else {
      out[[name]] <- spec$default
    }
  }

  extra <- setdiff(names(provided), names(required))
  if (length(extra) > 0) {
    .abort("Unknown associated item(s) for `%s`: %s", what, paste(extra, collapse = ", "))
  }
  out
}

#' @param class An S7 class or base class wrapper.
#' @param methods Named list of method implementations. Omitted trait methods
#'   use their default implementation when one is available.
#' @param assoc_types Named list of associated type values.
#' @param assoc_consts Named list of associated constant values.
#' @param replace Replace an existing implementation record and silence warnings
#'   about visible S7 methods?
#' @rdname trait_methods
#' @export
impl_trait <- function(
  trait,
  class,
  methods = list(),
  assoc_types = list(),
  assoc_consts = list(),
  replace = FALSE
) {
  if (!inherits(trait, "s7_trait")) {
    .abort("`trait` must be created with new_trait().")
  }

  cls <- .as_class_or_null(class, arg = "class")
  if (is.null(cls)) {
    .abort("`class` must be an S7 class, S7 union, S3 class wrapper, S4 class, or base class wrapper.")
  }

  for (parent in trait$parents) {
    if (is.null(.find_trait_impl(parent, cls))) {
      .abort(
        "Cannot implement %s for %s until its supertrait %s is implemented.",
        .trait_label(trait),
        .class_label(cls),
        .trait_label(parent)
      )
    }
  }

  trait_reqs <- trait_methods(trait, inherited = FALSE)
  provided_methods <- .normalise_impl_methods(methods)
  resolved_methods <- list()

  for (name in names(trait_reqs)) {
    req <- trait_reqs[[name]]
    fun <- provided_methods[[name]]
    if (is.null(fun)) {
      fun <- req$default
    }
    if (is.null(fun)) {
      .abort("Missing required trait method `%s` for %s.", name, .trait_label(trait))
    }
    resolved_methods[[name]] <- fun
  }

  extra_methods <- setdiff(names(provided_methods), names(trait_reqs))
  if (length(extra_methods) > 0) {
    .abort("Unknown trait method(s): %s", paste(extra_methods, collapse = ", "))
  }

  resolved_assoc_types <- .resolve_assoc_impl(
    .trait_assoc_types(trait, inherited = FALSE),
    assoc_types,
    "assoc_types"
  )
  resolved_assoc_consts <- .resolve_assoc_impl(
    .trait_assoc_consts(trait, inherited = FALSE),
    assoc_consts,
    "assoc_consts"
  )

  impl <- list(
    trait = trait,
    trait_id = trait$id,
    trait_label = .trait_label(trait),
    class = cls,
    methods = resolved_methods,
    assoc_types = resolved_assoc_types,
    assoc_consts = resolved_assoc_consts
  )

  .store_trait_impl(impl, replace = replace)

  for (name in names(trait_reqs)) {
    .register_s7_method(
      generic = trait_reqs[[name]]$generic,
      class = cls,
      fun = resolved_methods[[name]],
      replace = replace
    )
  }

  invisible(impl)
}

#' @param x An object or class.
#' @rdname trait_methods
#' @export
trait_report <- function(x, trait) {
  if (!inherits(trait, "s7_trait")) {
    .abort("`trait` must be created with new_trait().")
  }

  cls <- .target_class_or_null(x, arg = "x")
  if (is.null(cls)) {
    return(data.frame(
      trait = .trait_label(trait),
      class = "<unknown>",
      ok = FALSE,
      message = "Could not determine an S7 class for this value.",
      stringsAsFactors = FALSE
    ))
  }

  impl <- .find_trait_impl(trait, cls)
  data.frame(
    trait = .trait_label(trait),
    class = .class_label(cls),
    ok = !is.null(impl),
    message = if (is.null(impl)) "No explicit impl_trait() record." else "",
    stringsAsFactors = FALSE
  )
}

#' @rdname trait_methods
#' @export
has_trait <- function(x, trait) {
  trait_report(x, trait)$ok[[1]]
}

#' @param arg Name to use in error messages.
#' @rdname trait_methods
#' @export
assert_trait <- function(x, trait, arg = deparse(substitute(x))) {
  report <- trait_report(x, trait)
  if (!report$ok[[1]]) {
    .abort(
      "%s does not explicitly implement %s for %s: %s",
      arg,
      report$trait[[1]],
      report$class[[1]],
      report$message[[1]]
    )
  }
  x
}

#' @param method Method name within the trait.
#' @param ... Additional arguments passed to the S7 generic.
#' @rdname trait_methods
#' @export
trait_call <- function(trait, method, x, ...) {
  if (!inherits(trait, "s7_trait")) {
    .abort("`trait` must be created with new_trait().")
  }
  if (!is.character(method) || length(method) != 1 || !nzchar(method)) {
    .abort("`method` must be a non-empty string.")
  }

  assert_trait(x, trait)
  reqs <- trait_methods(trait, inherited = TRUE)
  if (!method %in% names(reqs)) {
    .abort("Trait %s has no method `%s`.", .trait_label(trait), method)
  }
  reqs[[method]]$generic(x, ...)
}

.assoc_from_impl <- function(trait, x, field, name) {
  cls <- .target_class_or_null(x, arg = "x")
  if (is.null(cls)) {
    .abort("Could not determine an S7 class for this value.")
  }
  impl <- .find_trait_impl(trait, cls)
  if (is.null(impl)) {
    .abort("%s does not explicitly implement %s.", .class_label(cls), .trait_label(trait))
  }
  values <- impl[[field]]
  if (!name %in% names(values)) {
    .abort("Trait %s has no associated item `%s`.", .trait_label(trait), name)
  }
  values[[name]]
}

#' @param name Associated item name.
#' @rdname trait_methods
#' @export
trait_assoc_type <- function(trait, x, name) {
  .assoc_from_impl(trait, x, "assoc_types", name)
}

#' @rdname trait_methods
#' @export
trait_assoc_const <- function(trait, x, name) {
  .assoc_from_impl(trait, x, "assoc_consts", name)
}

#' @method print s7_trait
#' @export
#' @noRd
print.s7_trait <- function(x, ...) {
  reqs <- trait_methods(x, inherited = TRUE)
  assoc_types <- .trait_assoc_types(x, inherited = TRUE)
  assoc_consts <- .trait_assoc_consts(x, inherited = TRUE)

  cat(sprintf("<S7 Rust-like trait> %s\n", .trait_label(x)))
  if (length(x$parents) > 0) {
    cat(
      "  supertraits:",
      paste(vapply(x$parents, .trait_label, character(1)), collapse = ", "),
      "\n"
    )
  }
  cat("  methods:")
  if (length(reqs) == 0) {
    cat(" <none>\n")
  } else {
    cat("\n")
    for (req in reqs) {
      suffix <- if (is.null(req$default)) "" else " [default]"
      cat(sprintf("    - %s()%s\n", req$name, suffix))
    }
  }
  if (length(assoc_types) > 0) {
    cat("  associated types:", paste(names(assoc_types), collapse = ", "), "\n")
  }
  if (length(assoc_consts) > 0) {
    cat("  associated consts:", paste(names(assoc_consts), collapse = ", "), "\n")
  }
  invisible(x)
}
