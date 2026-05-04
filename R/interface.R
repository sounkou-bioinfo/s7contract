#' Build a Go-like structural interface on top of S7
#'
#' `new_interface()` models the method-list part of Go interfaces as a list of
#' required S7 generics. An interface is just a named set of required generics,
#' and a class or object satisfies it when S7 can find a method for every
#' required generic.
#'
#' This deliberately mirrors Go's basic interfaces defined only by methods. The
#' intended style is to define small interfaces at the point where consuming code
#' needs a behavior, not beside a single concrete implementation. Define S7
#' classes, generics, and methods normally; then let consumers name the protocol
#' they accept. Up-front interfaces can still be useful for deliberate package
#' protocols, abstract data types, or recursive protocols.
#'
#' It does not attempt to emulate Go's full post-1.18 type-set language such as
#' tilde type terms, unions of concrete types, or pointer/value receiver rules.
#'
#' @param name For `new_interface()`, the interface name. For
#'   `interface_requirement()`, the requirement name; it defaults to the generic
#'   name when omitted.
#' @param generics For `new_interface()`, a named list of S7 generics or
#'   `interface_requirement()` objects. These are generic functions because S7
#'   methods are registered separately on generics.
#' @param methods Compatibility alias for `generics`.
#' @param parents Optional interface or list of interfaces to embed.
#' @param package Optional package name used only for display.
#' @return `new_interface()` returns an S7 object of class
#'   `s7_interface`. `interface_requirement()` returns an S7 object of class
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
#'   Drawable <- new_interface("Drawable", generics = list(draw = draw))
#'   Shape <- new_interface("Shape", generics = list(area = area), parents = Drawable)
#'
#'   implements(Circle, Shape)
#'   missing_requirements(Rect, Shape)
#' })
#' @export
new_interface <- function(
  name,
  generics = list(),
  parents = list(),
  package = NULL,
  methods = NULL
) {
  if (!is.null(methods)) {
    if (!missing(generics)) {
      .abort("Use either `generics` or `methods`, not both.")
    }
    generics <- methods
  }

  if (!is.character(name) || length(name) != 1 || !nzchar(name)) {
    .abort("`name` must be a non-empty string.")
  }
  if (!is.null(package) && (!is.character(package) || length(package) != 1)) {
    .abort("`package` must be NULL or a single string.")
  }

  s7_interface(
    name = name,
    package = package,
    parents = .normalise_interface_parents(parents),
    requirements = .normalise_interface_generics(generics)
  )
}

#' @param generic An S7 generic function.
#' @param args Optional named list of S7 classes, interfaces, or traits for
#'   runtime argument checking with `with()` or `%::%`. Arguments named in
#'   `args` are also checked against generic and method formals during
#'   conformance checks. Dispatch arguments other than the first can use S7
#'   classes or unions to refine multiple-dispatch requirements.
#' @param returns Optional S7 class, interface, or trait for runtime return
#'   checking with `with()` or `%::%`; defaults to `S7::class_any`.
#' @rdname new_interface
#' @export
interface_requirement <- function(
  generic,
  name = NULL,
  args = list(),
  returns = S7::class_any
) {
  .check_s7_generic(generic, "generic")
  if (is.null(name)) {
    name <- .generic_label(generic)
  }
  if (!is.character(name) || length(name) != 1 || !nzchar(name)) {
    .abort("`name` must be a non-empty string.")
  }

  s7_interface_requirement(
    name = name,
    generic = generic,
    args = .normalise_type_specs(args, "args"),
    returns = .normalise_return_spec(returns)
  )
}

.as_interface_requirement <- function(x, name = NULL) {
  if (.is_interface_requirement(x)) {
    if (!is.null(name)) {
      x@name <- name
    }
    return(x)
  }
  if (is.function(x)) {
    return(interface_requirement(x, name = name))
  }
  .abort(
    "Interface requirements must be S7 generics or interface_requirement() objects."
  )
}

.normalise_interface_generics <- function(generics) {
  if (is.null(generics)) {
    generics <- list()
  }
  if (is.function(generics) || .is_interface_requirement(generics)) {
    generics <- list(generics)
  }
  if (!is.list(generics)) {
    .abort(
      "`generics` must be a list of S7 generics or interface_requirement() objects."
    )
  }

  nms <- names(generics)
  if (is.null(nms)) {
    nms <- rep("", length(generics))
  }

  out <- vector("list", length(generics))
  for (i in seq_along(generics)) {
    nm <- if (nzchar(nms[[i]])) nms[[i]] else NULL
    req <- .as_interface_requirement(generics[[i]], name = nm)
    out[[i]] <- req
    nms[[i]] <- req@name
  }
  names(out) <- nms
  out
}

.normalise_interface_parents <- function(parents) {
  if (is.null(parents)) {
    return(list())
  }
  if (.is_interface(parents)) {
    parents <- list(parents)
  }
  if (!is.list(parents)) {
    .abort("`parents` must be an interface or a list of interfaces.")
  }
  for (parent in parents) {
    if (!.is_interface(parent)) {
      .abort("Every parent must be created with new_interface().")
    }
  }
  parents
}

.interface_label <- function(interface) {
  if (!is.null(interface@package)) {
    sprintf("%s::%s", interface@package, interface@name)
  } else {
    interface@name
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
  if (!.is_interface(interface)) {
    .abort("`interface` must be created with new_interface().")
  }

  out <- list()
  if (isTRUE(inherited)) {
    for (parent in interface@parents) {
      out <- c(out, interface_requirements(parent, inherited = TRUE))
    }
  }
  out <- c(out, interface@requirements)

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
    found <- .lookup_requirement_method(req, x)
    data.frame(
      interface = .interface_label(interface),
      requirement = req@name,
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

.print_s7_interface <- function(x, ...) {
  reqs <- interface_requirements(x, inherited = TRUE)
  cat(sprintf("<S7 Go-like interface> %s\n", .interface_label(x)))
  if (length(x@parents) > 0) {
    cat(
      "  embeds:",
      paste(vapply(x@parents, .interface_label, character(1)), collapse = ", "),
      "\n"
    )
  }
  cat("  requirements:")
  if (length(reqs) == 0) {
    cat(" <none>\n")
  } else {
    cat("\n")
    for (req in reqs) {
      typed_args <- names(req@args)
      typed_args <- if (length(typed_args) == 0) {
        ""
      } else {
        sprintf(" args: %s", paste(typed_args, collapse = ", "))
      }
      cat(sprintf("    - %s()%s\n", req@name, typed_args))
    }
  }
  invisible(x)
}
