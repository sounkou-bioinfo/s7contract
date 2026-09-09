# Helpers for optional, progressive runtime typing of contract requirements.

.normalise_type_specs <- function(args, what = "args") {
  if (is.null(args)) {
    return(list())
  }
  if (!is.list(args)) {
    .abort("`%s` must be a named list of type specifications.", what)
  }
  if (length(args) > 0 && (is.null(names(args)) || any(names(args) == ""))) {
    .abort("`%s` must be a named list of type specifications.", what)
  }
  for (name in names(args)) {
    .check_type_spec(args[[name]], sprintf("%s$%s", what, name))
  }
  args
}

.normalise_return_spec <- function(returns) {
  if (is.null(returns)) {
    return(S7::class_any)
  }
  .check_type_spec(returns, "returns")
  returns
}

.check_type_spec <- function(spec, arg) {
  if (.is_interface(spec) || .is_trait(spec)) {
    return(invisible(spec))
  }
  if (is.null(.as_class_or_null(spec, arg = arg))) {
    .abort("`%s` must be an S7 class, S7 union, interface, or trait.", arg)
  }
  invisible(spec)
}

.spec_as_dispatch_class <- function(spec, arg) {
  if (.is_interface(spec) || .is_trait(spec)) {
    .abort(
      "`%s` is a dispatch argument and must be an S7 class or S7 union, not an interface or trait.",
      arg
    )
  }
  cls <- .as_class_or_null(spec, arg = arg)
  if (is.null(cls)) {
    .abort("`%s` must be an S7 class or S7 union for S7 dispatch.", arg)
  }
  cls
}

.value_error_label <- function(arg) {
  if (identical(arg, ".return")) "Return value" else sprintf("`%s`", arg)
}

.check_value_conforms <- function(value, spec, arg) {
  if (.is_interface(spec)) {
    assert_implements(value, spec, arg = .value_error_label(arg))
    return(invisible(value))
  }
  if (.is_trait(spec)) {
    assert_trait(value, spec, arg = .value_error_label(arg))
    return(invisible(value))
  }

  checker <- S7::new_class(
    "s7contract_value_checker",
    package = "s7contract",
    properties = list(value = spec)
  )
  tryCatch(
    checker(value = value),
    error = function(e) {
      msg <- conditionMessage(e)
      lines <- strsplit(msg, "\n", fixed = TRUE)[[1L]]
      value_line <- grep("^- @value ", lines, value = TRUE)
      if (length(value_line) > 0) {
        msg <- sub("^- @value ", "", value_line[[1L]])
      }
      .abort("%s %s", .value_error_label(arg), msg)
    }
  )
  invisible(value)
}

.check_required_formals <- function(fun, arg_names, what) {
  if (length(arg_names) == 0) {
    return(invisible(TRUE))
  }
  formals_names <- names(formals(fun))
  missing <- setdiff(arg_names, formals_names)
  if (length(missing) > 0) {
    .abort(
      "%s is missing required argument(s): %s",
      what,
      paste(missing, collapse = ", ")
    )
  }
  invisible(TRUE)
}

.requirement_signature <- function(req, target) {
  generic <- req@generic
  dispatch_args <- generic@dispatch_args
  cls <- .target_class_or_null(target, arg = "x")
  if (is.null(cls)) {
    .abort("Could not determine the class of the first dispatch argument.")
  }
  if (length(dispatch_args) <= 1L) {
    return(cls)
  }

  signature <- vector("list", length(dispatch_args))
  names(signature) <- dispatch_args
  signature[[1L]] <- cls

  for (arg in dispatch_args[-1L]) {
    if (!arg %in% names(req@args)) {
      .abort("Missing type specification for dispatch argument `%s`.", arg)
    }
    signature[[arg]] <- .spec_as_dispatch_class(req@args[[arg]], arg)
  }

  signature
}

.lookup_requirement_method <- function(req, target) {
  generic <- req@generic

  tryCatch(
    {
      signature <- .requirement_signature(req, target)
      if (length(generic@dispatch_args) == 1L) signature <- list(signature)
      .check_required_formals(
        generic,
        names(req@args),
        sprintf("Generic `%s()`", req@name)
      )

      # Union registration expands signatures; lookup requires concrete classes.
      check_signature <- function(signature, position = 1L) {
        if (position > length(signature)) {
          method <- S7::method(generic, class = if (length(signature) == 1L) {
            signature[[1L]]
          } else {
            signature
          })
          .check_required_formals(
            method, names(req@args), sprintf("Method `%s()`", req@name)
          )
          return(invisible(NULL))
        }
        cls <- signature[[position]]
        classes <- if (inherits(cls, "S7_union")) cls$classes else list(cls)
        for (cls in classes) {
          signature[position] <- list(cls)
          check_signature(signature, position + 1L)
        }
      }
      check_signature(signature)
      list(ok = TRUE, error = NULL)
    },
    error = function(e) {
      list(ok = FALSE, error = e)
    }
  )
}

.checked_generic_call <- function(contract, req, call, env, trait = FALSE) {
  generic <- req@generic
  matched <- tryCatch(
    match.call(definition = generic, call = call, expand.dots = TRUE),
    error = function(e) {
      .abort(
        "Could not match call to `%s()`: %s",
        req@name,
        conditionMessage(e)
      )
    }
  )

  check_arguments <- function(frame) {
    dispatch_args <- generic@dispatch_args
    dispatch_values <- lapply(dispatch_args, function(arg) {
      value <- get(arg, envir = frame, inherits = FALSE)
      if (arg %in% names(req@args)) {
        .check_value_conforms(value, req@args[[arg]], arg)
      }
      value
    })
    if (trait) {
      assert_trait(dispatch_values[[1L]], contract, arg = dispatch_args[[1L]])
      .check_required_formals(generic, names(req@args), sprintf("Generic `%s()`", req@name))
    } else {
      assert_implements(dispatch_values[[1L]], contract, arg = dispatch_args[[1L]])
    }
    method <- S7::method(generic, object = if (length(dispatch_values) == 1L) {
      dispatch_values[[1L]]
    } else {
      dispatch_values
    })
    .check_required_formals(method, names(req@args), sprintf("Method `%s()`", req@name))

    generic_formals <- formals(generic)
    method_frame <- NULL
    for (arg in setdiff(names(req@args), dispatch_args)) {
      absent <- eval(call("missing", as.name(arg)), envir = frame) &&
        identical(generic_formals[[arg]], quote(expr = ))
      if (absent) {
        if (identical(formals(method)[[arg]], quote(expr = ))) {
          .abort("Call is missing typed argument `%s` and has no default.", arg)
        }
        if (is.null(method_frame)) {
          # Method-only defaults use method scope and share generic promises.
          actuals <- lapply(names(generic_formals), as.name)
          names(actuals) <- names(generic_formals)
          for (name in setdiff(names(actuals), "...")) {
            if (eval(call("missing", as.name(name)), envir = frame) &&
                identical(generic_formals[[name]], quote(expr = ))) {
              actuals[name] <- NULL
            }
          }
          names(actuals)[names(actuals) == "..."] <- ""
          body(method) <- quote(base::environment())
          method_frame <- eval(as.call(c(list(method), actuals)), envir = frame)
        }
        assign(arg, get(arg, envir = method_frame, inherits = FALSE), envir = frame)
      }
      .check_value_conforms(get(arg, envir = frame, inherits = FALSE), req@args[[arg]], arg)
    }
  }

  # A per-call copy keeps S7 dispatch and R's argument promises in the real frame.
  checked <- S7::S7_data(generic)
  body(checked) <- substitute({ CHECK(base::environment()); BODY },
                             list(CHECK = check_arguments, BODY = body(generic)))
  checked_generic <- generic
  S7::S7_data(checked_generic) <- checked
  matched[[1L]] <- checked_generic
  out <- withVisible(eval(matched, envir = env))
  .check_value_conforms(out$value, req@returns, ".return")
  if (out$visible) out$value else invisible(out$value)
}

.make_checked_generic <- function(contract, req, trait = FALSE) {
  force(contract)
  force(req)
  force(trait)
  function(...) {
    .checked_generic_call(
      contract,
      req,
      sys.call(),
      parent.frame(),
      trait = trait
    )
  }
}

.contract_mask <- function(contract, env, trait = FALSE) {
  reqs <- if (trait) {
    trait_methods(contract, inherited = TRUE)
  } else {
    interface_requirements(contract, inherited = TRUE)
  }
  mask <- new.env(parent = env)

  for (req in reqs) {
    wrapper <- .make_checked_generic(contract, req, trait = trait)
    bind_names <- unique(c(req@name, req@generic@name))
    for (name in bind_names) {
      assign(name, wrapper, envir = mask)
    }
  }

  mask
}

.with_contract <- function(contract, expr, env, trait = FALSE) {
  mask <- .contract_mask(contract, env, trait = trait)
  out <- withVisible(eval(expr, envir = mask))
  value <- out$value
  if (identical(typeof(value), "closure") && !identical(environment(value), mask)) {
    environment(value) <- .contract_mask(contract, environment(value), trait = trait)
  }
  if (out$visible) value else invisible(value)
}

.with_s7_interface <- function(data, expr, ...) {
  .with_contract(data, substitute(expr), parent.frame(), trait = FALSE)
}

.with_s7_trait <- function(data, expr, ...) {
  .with_contract(data, substitute(expr), parent.frame(), trait = TRUE)
}

#' Evaluate an S7 call under an interface or trait contract
#'
#' `with(contract, expr)` and `expr %::% contract` evaluate `expr` in a
#' contract mask. Required generics are shadowed by checking wrappers, so calls
#' to those generics use normal S7 dispatch while checking the optional argument
#' and return specifications stored in an interface requirement or trait method.
#' Only names resolved through the mask are checked; namespace-qualified calls
#' and calls inside separately defined helpers are not instrumented.
#'
#' Checks force dispatch and typed arguments before the generic body runs.
#' Generic defaults retain their lexical scope and share ordinary R promises.
#' If a typed argument has only a method default, that default is evaluated
#' before dispatch and supplied to the generic. Such defaults should be pure
#' expressions of arguments and lexical bindings, without relying on method-body
#' locals or `missing()` for that argument.
#'
#' @param expr An expression evaluated in a contract mask. Calls to generics
#'   named in the contract are checked.
#' @param contract An interface created by [new_interface()] or a trait created
#'   by [new_trait()].
#' @return The value of `expr`, after any optional return check.
#' @aliases contract_syntax
#' @examples
#' local({
#'   draw <- S7::new_generic("draw", "x", function(x, color) {
#'     S7::S7_dispatch()
#'   })
#'   Circle <- S7::new_class("TypedCircle", properties = list(r = S7::class_double))
#'   S7::method(draw, Circle) <- function(x, color) paste(color, x@r)
#'   Drawable <- new_interface(
#'     "TypedDrawable",
#'     generics = list(draw = interface_requirement(
#'       draw,
#'       args = list(color = S7::class_character),
#'       returns = S7::class_character
#'     ))
#'   )
#'   with(Drawable, draw(Circle(r = 2), color = "red"))
#'   checked_draw <- with(Drawable, function(x) draw(x, color = "red"))
#'   checked_draw(Circle(r = 2))
#'   draw(Circle(r = 2), color = "red") %::% Drawable
#' })
#' @export
`%::%` <- function(expr, contract) {
  contract <- eval(substitute(contract), envir = parent.frame())
  if (.is_interface(contract)) {
    return(.with_contract(
      contract,
      substitute(expr),
      parent.frame(),
      trait = FALSE
    ))
  }
  if (.is_trait(contract)) {
    return(.with_contract(
      contract,
      substitute(expr),
      parent.frame(),
      trait = TRUE
    ))
  }
  .abort("Right-hand side of `%::%` must be an interface or trait.")
}
