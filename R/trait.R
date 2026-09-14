.s7contract_registry <- new.env(parent = emptyenv())
.s7contract_registry$impls <- list()

#' Build a Rust-like explicit trait on top of S7
#'
#' `new_trait()` adds a nominal contract registry on top of S7 dispatch. A class
#' only has the trait after `impl_trait()` records the implementation, even if
#' compatible S7 methods already exist. Registrations belong to the current
#' R session and the particular trait descriptor, not its display name. Reuse
#' the same descriptor for registration and checks; deserializing a descriptor
#' does not transfer its registrations. S7 subclasses and S3 subclasses need
#' their own registration; method inheritance alone does not confer a trait.
#'
#' Method aliases and generic names share the scoped call namespace. Overlapping
#' requirements must have identical generics, defaults and type specifications.
#' Associated items must have one declaring trait: a diamond can share the same
#' declaration, but independent supertraits cannot declare the same item name
#' within `assoc_types` or within `assoc_consts`. Inherited values are read from
#' the declaring supertrait's implementation.
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
#' @return `new_trait()` returns an S7 object of class `s7_trait`.
#'   `trait_method()` returns an S7 object of class `s7_trait_method`.
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
  .check_name(name)
  if (!is.null(package)) {
    .check_name(package, "package")
  }

  trait <- s7_trait(
    id = new.env(parent = emptyenv()),
    name = name,
    package = package,
    parents = .normalise_parents(parents, s7_trait, "new_trait"),
    methods = .normalise_requirements(
      methods,
      s7_trait_method,
      trait_method,
      "methods"
    ),
    assoc_types = .normalise_assoc(assoc_types, "assoc_types"),
    assoc_consts = .normalise_assoc(assoc_consts, "assoc_consts")
  )
  trait_methods(trait)
  .trait_assoc_owners(trait, "assoc_types")
  .trait_assoc_owners(trait, "assoc_consts")
  trait
}

#' @param generic An S7 generic function.
#' @param default Optional default implementation. If supplied, `impl_trait()`
#'   uses it when a class does not provide an override for that method.
#' @param args Optional named list of S7 classes, interfaces, or traits for
#'   runtime argument checking with `with()` or `%::%`. Dispatch arguments other
#'   than the first can use S7 classes or unions to refine multiple-dispatch
#'   requirements.
#' @param returns Optional S7 class, interface, or trait for runtime return
#'   checking with `with()` or `%::%`; defaults to `S7::class_any`.
#' @rdname new_trait
#' @export
trait_method <- function(
  generic,
  default = NULL,
  name = NULL,
  args = list(),
  returns = S7::class_any
) {
  .check_s7_generic(generic, "generic")
  if (!is.null(default) && !is.function(default)) {
    .abort("`default` must be NULL or a function.")
  }
  if (is.null(name)) {
    name <- generic@name
  }
  .check_name(name)

  s7_trait_method(
    name = name,
    generic = generic,
    default = default,
    args = .normalise_type_specs(args, "args"),
    returns = .normalise_return_spec(returns)
  )
}

.normalise_assoc <- function(x, what) {
  if (is.null(x)) {
    return(list())
  }
  if (is.character(x)) {
    .check_names(x, what)
    out <- lapply(x, function(name) {
      s7_assoc_item(required = TRUE, default = NULL)
    })
    names(out) <- x
    return(out)
  }
  if (is.list(x)) {
    if (length(x) > 0L) {
      .check_names(names(x), what)
    }
    out <- lapply(x, function(value) {
      s7_assoc_item(required = FALSE, default = value)
    })
    return(out)
  }
  .abort("`%s` must be NULL, a character vector, or a named list.", what)
}

.trait_label <- function(trait) {
  if (!is.null(trait@package)) {
    sprintf("%s::%s", trait@package, trait@name)
  } else {
    trait@name
  }
}

#' Inspect or use a Rust-like explicit trait
#'
#' Methods are validated by S7 on isolated tables before registration is
#' published. Method bindings and the implementation record are published
#' together; a publication error restores the touched bindings. Aliases for the
#' same generic must supply identical implementation functions.
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
  if (!.is_trait(trait)) {
    .abort("`trait` must be created with new_trait().")
  }

  out <- list()
  if (isTRUE(inherited)) {
    for (parent in trait@parents) {
      out <- c(out, trait_methods(parent, inherited = TRUE))
    }
  }
  out <- c(out, trait@methods)

  .merge_requirements(out)
}

.trait_assoc_owners <- function(trait, field) {
  owners <- list()
  for (parent in trait@parents) {
    owners <- c(owners, .trait_assoc_owners(parent, field))
  }
  local_names <- names(S7::prop(trait, field))
  local <- rep(list(trait), length(local_names))
  names(local) <- local_names
  owners <- c(owners, local)
  for (name in names(owners)[duplicated(names(owners))]) {
    declarations <- owners[names(owners) == name]
    if (
      !all(vapply(
        declarations,
        function(owner) {
          identical(owner@id, declarations[[1L]]@id)
        },
        logical(1)
      ))
    ) {
      .abort(
        "Conflicting declarations for associated item `%s` in `%s`.",
        name,
        field
      )
    }
  }
  owners[!duplicated(names(owners))]
}

.find_trait_impl <- function(trait, class) {
  key <- .class_key(class)
  impls <- .s7contract_registry$impls
  for (impl in impls) {
    if (
      identical(impl@trait@id, trait@id) &&
        identical(.class_key(impl@target_class), key)
    ) {
      return(impl)
    }
  }
  NULL
}

.check_trait_impl_admissible <- function(trait, class, replace) {
  for (parent in trait@parents) {
    if (is.null(.find_trait_impl(parent, class))) {
      .abort(
        "Cannot implement %s for %s until its supertrait %s is implemented.",
        .trait_label(trait),
        .class_label(class),
        .trait_label(parent)
      )
    }
  }
  if (!replace && !is.null(.find_trait_impl(trait, class))) {
    .abort(
      "%s is already implemented for %s. Pass replace = TRUE to replace it.",
      .trait_label(trait),
      .class_label(class)
    )
  }
  invisible(NULL)
}

.resolve_method_impl <- function(required, provided) {
  if (is.null(provided)) {
    provided <- list()
  }
  if (!is.list(provided)) {
    .abort("`methods` must be a named list of functions.")
  }
  if (length(provided) > 0L) {
    .check_names(names(provided), "methods")
  }
  extra <- setdiff(names(provided), names(required))
  if (length(extra) > 0L) {
    .abort("Unknown trait method(s): %s", paste(extra, collapse = ", "))
  }
  out <- list()
  for (name in names(required)) {
    if (name %in% names(provided)) {
      fun <- provided[[name]]
      if (!is.function(fun)) {
        .abort("Implementation for `%s` must be a function.", name)
      }
    } else {
      fun <- required[[name]]@default
      if (is.null(fun)) .abort("Missing required trait method `%s`.", name)
    }
    out[[name]] <- fun
  }
  out
}

.resolve_assoc_impl <- function(required, provided, what) {
  if (is.null(provided)) {
    provided <- list()
  }
  if (!is.list(provided)) {
    .abort("`%s` must be a named list.", what)
  }
  if (length(provided) > 0L) {
    .check_names(names(provided), what)
  }

  out <- list()
  for (name in names(required)) {
    spec <- required[[name]]
    if (name %in% names(provided)) {
      out[name] <- list(provided[[name]])
    } else if (isTRUE(spec@required)) {
      .abort("Missing required associated item `%s` in `%s`.", name, what)
    } else {
      out[name] <- list(spec@default)
    }
  }

  extra <- setdiff(names(provided), names(required))
  if (length(extra) > 0) {
    .abort(
      "Unknown associated item(s) for `%s`: %s",
      what,
      paste(extra, collapse = ", ")
    )
  }
  out
}

.register_trait_impl <- function(impl, requirements, replace) {
  staged <- list()
  for (name in names(requirements)) {
    req <- requirements[[name]]
    generic <- req@generic
    fun <- impl@methods[[name]]
    aliases <- which(vapply(
      staged,
      function(entry) {
        identical(entry$generic@methods, generic@methods)
      },
      logical(1)
    ))
    if (length(aliases) > 0L) {
      if (!identical(fun, staged[[aliases[[1L]]]]$fun)) {
        .abort("Conflicting implementations for generic `%s`.", generic@name)
      }
      next
    }
    signature <- .requirement_signature(req, impl@target_class)
    if (
      !replace &&
        !is.null(tryCatch(
          S7::method(generic, class = signature),
          error = function(e) NULL
        ))
    ) {
      warning(
        sprintf(
          "An S7 method for `%s` is already visible; registering anyway. Pass replace = TRUE to silence this warning.",
          generic@name
        ),
        call. = FALSE
      )
    }
    copy <- generic
    copy@methods <- new.env(parent = emptyenv())
    # Session-local registration must not append package reload hooks.
    do.call(
      S7::`method<-`,
      list(copy, signature, value = fun),
      envir = baseenv()
    )
    staged[[length(staged) + 1L]] <- list(
      generic = generic,
      table = copy@methods,
      fun = fun
    )
  }

  # Validation condition handlers may register implementations themselves.
  .check_trait_impl_admissible(impl@trait, impl@target_class, replace)
  changes <- list()
  complete <- FALSE
  on.exit(
    if (!complete) {
      for (change in rev(changes)) {
        if (identical(change$table[[change$name]], change$value)) {
          next
        }
        # S7 tables contain methods and sub-tables; NULL denotes an absent binding.
        if (is.null(change$value)) {
          rm(list = change$name, envir = change$table)
        } else {
          change$table[[change$name]] <- change$value
        }
      }
    }
  )
  publish <- function(source, table, generic) {
    for (name in ls(source, all.names = TRUE)) {
      value <- source[[name]]
      old <- table[[name]]
      if (is.environment(value) && !is.null(old)) {
        publish(value, old, generic)
        next
      }
      changes[[length(changes) + 1L]] <<- list(
        table = table,
        name = name,
        value = old
      )
      if (is.environment(value)) {
        table[[name]] <- new.env(parent = emptyenv())
        publish(value, table[[name]], generic)
      } else {
        value@generic <- generic
        table[[name]] <- value
      }
    }
  }
  impls <- .s7contract_registry$impls
  key <- .class_key(impl@target_class)
  keep <- !vapply(
    impls,
    function(existing) {
      identical(existing@trait@id, impl@trait@id) &&
        identical(.class_key(existing@target_class), key)
    },
    logical(1)
  )
  updated <- c(impls[keep], list(impl))
  suspendInterrupts({
    for (entry in staged) {
      publish(entry$table, entry$generic@methods, entry$generic)
    }
    .s7contract_registry$impls <- updated
    complete <- TRUE
  })
  invisible(impl)
}

#' @param class A concrete S7 class, S3 class wrapper, S4 class, or base class
#'   wrapper. Register union members separately. Dispatch wildcards
#'   (`class_any` and `class_missing`) are not concrete targets.
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
  if (!.is_trait(trait)) {
    .abort("`trait` must be created with new_trait().")
  }

  cls <- .as_class_or_null(class, arg = "class")
  if (is.null(cls)) {
    .abort(
      "`class` must be an S7 class, S3 class wrapper, S4 class, or base class wrapper."
    )
  }
  if (inherits(cls, c("S7_union", "S7_any", "S7_missing"))) {
    .abort(
      "Trait implementations require a concrete class; register union members separately and use concrete classes instead of dispatch wildcards."
    )
  }

  if (!is.logical(replace) || length(replace) != 1L || is.na(replace)) {
    .abort("`replace` must be TRUE or FALSE.")
  }
  .check_trait_impl_admissible(trait, cls, replace)

  trait_reqs <- trait_methods(trait, inherited = FALSE)
  resolved_methods <- .resolve_method_impl(trait_reqs, methods)

  resolved_assoc_types <- .resolve_assoc_impl(
    trait@assoc_types,
    assoc_types,
    "assoc_types"
  )
  resolved_assoc_consts <- .resolve_assoc_impl(
    trait@assoc_consts,
    assoc_consts,
    "assoc_consts"
  )

  impl <- s7_trait_impl(
    trait = trait,
    target_class = cls,
    methods = resolved_methods,
    assoc_types = resolved_assoc_types,
    assoc_consts = resolved_assoc_consts
  )

  .register_trait_impl(impl, trait_reqs, replace)
}

#' @param x An S7, S3, S4 or supported base-class object, or its class descriptor.
#' @rdname trait_methods
#' @export
trait_report <- function(x, trait) {
  if (!.is_trait(trait)) {
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
  if (!.is_trait(trait)) {
    .abort("`trait` must be created with new_trait().")
  }
  .check_name(method, "method")

  assert_trait(x, trait)
  reqs <- trait_methods(trait, inherited = TRUE)
  if (!method %in% names(reqs)) {
    .abort("Trait %s has no method `%s`.", .trait_label(trait), method)
  }
  reqs[[method]]@generic(x, ...)
}

.assoc_from_impl <- function(trait, x, field, name) {
  if (!.is_trait(trait)) {
    .abort("`trait` must be created with new_trait().")
  }
  .check_name(name)
  cls <- .target_class_or_null(x, arg = "x")
  if (is.null(cls)) {
    .abort("Could not determine an S7 class for this value.")
  }
  impl <- .find_trait_impl(trait, cls)
  if (is.null(impl)) {
    .abort(
      "%s does not explicitly implement %s.",
      .class_label(cls),
      .trait_label(trait)
    )
  }

  owner <- .trait_assoc_owners(trait, field)[[name]]
  if (is.null(owner)) {
    .abort("Trait %s has no associated item `%s`.", .trait_label(trait), name)
  }
  owner_impl <- .find_trait_impl(owner, cls)
  S7::prop(owner_impl, field)[[name]]
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

.print_s7_trait <- function(x, ...) {
  reqs <- trait_methods(x, inherited = TRUE)
  assoc_types <- .trait_assoc_owners(x, "assoc_types")
  assoc_consts <- .trait_assoc_owners(x, "assoc_consts")

  cat(sprintf("<S7 Rust-like trait> %s\n", .trait_label(x)))
  if (length(x@parents) > 0) {
    cat(
      "  supertraits:",
      paste(vapply(x@parents, .trait_label, character(1)), collapse = ", "),
      "\n"
    )
  }
  cat("  methods:")
  if (length(reqs) == 0) {
    cat(" <none>\n")
  } else {
    cat("\n")
    for (req in reqs) {
      suffix <- if (is.null(req@default)) "" else " [default]"
      typed_args <- names(req@args)
      typed_args <- if (length(typed_args) == 0) {
        ""
      } else {
        sprintf(" args: %s", paste(typed_args, collapse = ", "))
      }
      cat(sprintf("    - %s()%s%s\n", req@name, suffix, typed_args))
    }
  }
  if (length(assoc_types) > 0) {
    cat("  associated types:", paste(names(assoc_types), collapse = ", "), "\n")
  }
  if (length(assoc_consts) > 0) {
    cat(
      "  associated consts:",
      paste(names(assoc_consts), collapse = ", "),
      "\n"
    )
  }
  invisible(x)
}
