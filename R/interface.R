#' Build a Go-like structural interface on top of S7
#'
#' `new_interface()` models the method-list part of Go interfaces using S7
#' generics. An interface is just a named set of required generics, and a class
#' or object satisfies it when S7 can find a method for every required generic.
#'
#' This deliberately mirrors Go's basic interfaces defined only by methods. It
#' does not attempt to emulate Go's full post-1.18 type-set language such as
#' `~T`, unions of concrete types, or pointer/value receiver rules.
#'
#' @param name For `new_interface()`, the interface name. For
#'   `interface_requirement()`, the requirement name; it defaults to the generic
#'   name when omitted.
#' @param methods For `new_interface()`, a named list of S7 generics or
#'   `interface_requirement()` objects.
#' @param parents Optional interface or list of interfaces to embed.
#' @param package Optional package name used only for display.
#' @return `new_interface()` returns an object of class `s7_go_interface`.
#'   `interface_requirement()` returns an object of class
#'   `s7_interface_requirement`.
#' @examples
#' local({
#'   area <- S7::new_generic("area", "x")
#'   draw <- S7::new_generic("draw", "x")
#'
#'   Circle <- S7::new_class(
#'     "Circle",
#'     properties = list(r = S7::class_double)
#'   )
#'   Rect <- S7::new_class(
#'     "Rect",
#'     properties = list(w = S7::class_double, h = S7::class_double)
#'   )
#'
#'   S7::method(area, Circle) <- function(x) pi * x@r^2
#'   S7::method(draw, Circle) <- function(x) sprintf("circle(r = %s)", x@r)
#'   S7::method(area, Rect) <- function(x) x@w * x@h
#'
#'   Drawable <- new_interface("Drawable", methods = list(draw = draw))
#'   Shape <- new_interface("Shape", methods = list(area = area), parents = Drawable)
#'
#'   implements(Circle, Shape)
#'   missing_requirements(Rect, Shape)
#' })
#' @export
new_interface <- function(name, methods = list(), parents = list(), package = NULL) {
  if (!is.character(name) || length(name) != 1 || !nzchar(name)) {
    .abort("`name` must be a non-empty string.")
  }
  if (!is.null(package) && (!is.character(package) || length(package) != 1)) {
    .abort("`package` must be NULL or a single string.")
  }

  structure(
    list(
      name = name,
      package = package,
      parents = .normalise_interface_parents(parents),
      methods = .normalise_interface_methods(methods)
    ),
    class = "s7_go_interface"
  )
}

#' @param generic An S7 generic function.
#' @rdname new_interface
#' @export
interface_requirement <- function(generic, name = NULL) {
  if (!is.function(generic)) {
    .abort(
      "`generic` must be a function, usually an S7 generic created with S7::new_generic()."
    )
  }
  if (is.null(name)) {
    name <- .generic_label(generic)
  }
  if (!is.character(name) || length(name) != 1 || !nzchar(name)) {
    .abort("`name` must be a non-empty string.")
  }

  structure(
    list(name = name, generic = generic),
    class = "s7_interface_requirement"
  )
}

.as_interface_requirement <- function(x, name = NULL) {
  if (inherits(x, "s7_interface_requirement")) {
    if (!is.null(name)) {
      x$name <- name
    }
    return(x)
  }
  if (is.function(x)) {
    return(interface_requirement(x, name = name))
  }
  .abort("Interface requirements must be S7 generics or interface_requirement() objects.")
}

.normalise_interface_methods <- function(methods) {
  if (is.null(methods)) {
    methods <- list()
  }
  if (is.function(methods) || inherits(methods, "s7_interface_requirement")) {
    methods <- list(methods)
  }
  if (!is.list(methods)) {
    .abort("`methods` must be a list of S7 generics or interface_requirement() objects.")
  }

  nms <- names(methods)
  if (is.null(nms)) {
    nms <- rep("", length(methods))
  }

  out <- vector("list", length(methods))
  for (i in seq_along(methods)) {
    nm <- if (nzchar(nms[[i]])) nms[[i]] else NULL
    req <- .as_interface_requirement(methods[[i]], name = nm)
    out[[i]] <- req
    nms[[i]] <- req$name
  }
  names(out) <- nms
  out
}

.normalise_interface_parents <- function(parents) {
  if (is.null(parents)) {
    return(list())
  }
  if (inherits(parents, "s7_go_interface")) {
    parents <- list(parents)
  }
  if (!is.list(parents)) {
    .abort("`parents` must be an interface or a list of interfaces.")
  }
  for (parent in parents) {
    if (!inherits(parent, "s7_go_interface")) {
      .abort("Every parent must be created with new_interface().")
    }
  }
  parents
}

.interface_label <- function(interface) {
  if (!is.null(interface$package)) {
    sprintf("%s::%s", interface$package, interface$name)
  } else {
    interface$name
  }
}

#' Inspect or check a Go-like structural interface
#'
#' @param interface An interface created by `new_interface()`.
#' @param inherited Include inherited requirements from parent interfaces?
#' @return `interface_requirements()` returns a named list of
#'   `interface_requirement()` objects. `interface_report()` and
#'   `missing_requirements()` return data frames. `implements()` returns a single
#'   logical value. `assert_implements()` and `as_interface()` return `x`,
#'   unchanged.
#' @rdname interface_requirements
#' @export
interface_requirements <- function(interface, inherited = TRUE) {
  if (!inherits(interface, "s7_go_interface")) {
    .abort("`interface` must be created with new_interface().")
  }

  out <- list()
  if (isTRUE(inherited)) {
    for (parent in interface$parents) {
      out <- c(out, interface_requirements(parent, inherited = TRUE))
    }
  }
  out <- c(out, interface$methods)

  if (length(out) > 0) {
    out <- out[!duplicated(names(out), fromLast = TRUE)]
  }
  out
}

#' @param x An object, or an S7 class/base class wrapper.
#' @rdname interface_requirements
#' @export
interface_report <- function(x, interface) {
  reqs <- interface_requirements(interface, inherited = TRUE)

  rows <- lapply(reqs, function(req) {
    found <- .lookup_s7_method(req$generic, x)
    data.frame(
      interface = .interface_label(interface),
      requirement = req$name,
      ok = found$ok,
      message = if (found$ok) "" else conditionMessage(found$error),
      stringsAsFactors = FALSE
    )
  })

  if (length(rows) == 0) {
    return(data.frame(
      interface = character(),
      requirement = character(),
      ok = logical(),
      message = character(),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, rows)
}

#' @rdname interface_requirements
#' @export
missing_requirements <- function(x, interface) {
  report <- interface_report(x, interface)
  report[!report$ok, , drop = FALSE]
}

#' @rdname interface_requirements
#' @export
implements <- function(x, interface) {
  nrow(missing_requirements(x, interface)) == 0L
}

#' @param arg Name to use in error messages.
#' @rdname interface_requirements
#' @export
assert_implements <- function(x, interface, arg = deparse(substitute(x))) {
  miss <- missing_requirements(x, interface)
  if (nrow(miss) > 0) {
    .abort(
      "%s does not implement %s; missing: %s",
      arg,
      .interface_label(interface),
      paste0(miss$requirement, "()", collapse = ", ")
    )
  }
  x
}

#' @rdname interface_requirements
#' @export
as_interface <- function(x, interface) {
  assert_implements(x, interface)
}

#' @method print s7_go_interface
#' @export
#' @noRd
print.s7_go_interface <- function(x, ...) {
  reqs <- interface_requirements(x, inherited = TRUE)
  cat(sprintf("<S7 Go-like interface> %s\n", .interface_label(x)))
  if (length(x$parents) > 0) {
    cat(
      "  embeds:",
      paste(vapply(x$parents, .interface_label, character(1)), collapse = ", "),
      "\n"
    )
  }
  cat("  requirements:")
  if (length(reqs) == 0) {
    cat(" <none>\n")
  } else {
    cat("\n")
    for (req in reqs) {
      cat(sprintf("    - %s()\n", req$name))
    }
  }
  invisible(x)
}
