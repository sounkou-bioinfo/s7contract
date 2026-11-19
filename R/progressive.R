# Helpers for optional, progressive runtime typing of contract requirements.

.normalise_type_specs <- function(args, what = "args") {
  if (is.null(args)) {
    return(list())
  }
  if (!is.list(args)) {
    .abort("`%s` must be a named list of type specifications.", what)
  }
  if (length(args) > 0L) {
    .check_names(names(args), what)
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

.value_error_label <- function(arg) {
  if (identical(arg, ".return")) "Return value" else sprintf("`%s`", arg)
}

# Returns a function that checks one value against `spec`. The S7 property
# class is built once per checker, so each check is one validated construction.
.value_checker <- function(spec, arg) {
  label <- .value_error_label(arg)
  if (.is_interface(spec)) {
    plans <- .requirement_plans(spec)
    return(function(value) {
      .assert_conforms(.value_class_or_null(value), spec, plans, label)
    })
  }
  if (.is_trait(spec)) {
    return(function(value) {
      .assert_trait_status(.value_class_or_null(value), spec, label)
    })
  }
  if (identical(spec, S7::class_any)) {
    return(function(value) invisible(value))
  }

  checker <- S7::new_class(
    "s7contract_value_checker",
    package = "s7contract",
    properties = list(value = spec)
  )
  function(value) {
    tryCatch(
      checker(value = value),
      error = function(e) {
        msg <- conditionMessage(e)
        lines <- strsplit(msg, "\n", fixed = TRUE)[[1L]]
        value_line <- grep("^- @value ", lines, value = TRUE)
        if (length(value_line) > 0) {
          msg <- sub("^- @value ", "", value_line[[1L]])
        }
        .abort("%s %s", label, msg)
      }
    )
    invisible(value)
  }
}

.check_required_formals <- function(fun, arg_names, what) {
  if (length(arg_names) == 0) {
    return(invisible(TRUE))
  }
  missing <- arg_names[!arg_names %in% names(formals(fun))]
  if (length(missing) > 0) {
    .abort(
      "%s is missing required argument(s): %s",
      what,
      paste(missing, collapse = ", ")
    )
  }
  invisible(TRUE)
}

# Classes for the dispatch arguments after the receiver, from the requirement's
# type specifications.
.requirement_dispatch_tail <- function(req) {
  dispatch_args <- req@generic@dispatch_args[-1L]
  tail <- lapply(dispatch_args, function(arg) {
    if (!arg %in% names(req@args)) {
      .abort("Missing type specification for dispatch argument `%s`.", arg)
    }
    spec <- req@args[[arg]]
    if (.is_interface(spec) || .is_trait(spec)) {
      .abort(
        "`%s` is a dispatch argument and must be an S7 class or S7 union, not an interface or trait.",
        arg
      )
    }
    S7::as_class(spec, arg = arg)
  })
  names(tail) <- dispatch_args
  tail
}

.requirement_signature <- function(req, target) {
  cls <- .target_class_or_null(target, arg = "x")
  if (is.null(cls)) {
    .abort("Could not determine the class of the first dispatch argument.")
  }
  if (length(req@generic@dispatch_args) <= 1L) {
    return(cls)
  }
  signature <- c(list(cls), .requirement_dispatch_tail(req))
  names(signature)[[1L]] <- req@generic@dispatch_args[[1L]]
  signature
}

# The parts of a conformance check that depend only on the requirement.
# Validation failures are kept as conditions and reported per receiver, after
# the receiver's class is known.
.requirement_plan <- function(req) {
  tail <- NULL
  problem <- tryCatch(
    {
      tail <- unname(.requirement_dispatch_tail(req))
      .check_required_formals(
        req@generic,
        names(req@args),
        sprintf("Generic `%s()`", req@name)
      )
      NULL
    },
    error = identity
  )
  list(req = req, tail = tail, problem = problem)
}

.requirement_plans <- function(interface) {
  lapply(interface_requirements(interface, inherited = TRUE), .requirement_plan)
}

# NULL when the receiver class `cls` has a method for every concrete signature
# of the requirement, otherwise the condition that explains the gap.
.requirement_problem <- function(plan, cls) {
  req <- plan$req
  tryCatch(
    {
      if (is.null(cls)) {
        .abort("Could not determine the class of the first dispatch argument.")
      }
      if (!is.null(plan$problem)) {
        stop(plan$problem)
      }
      method_what <- sprintf("Method `%s()`", req@name)
      # Union registration expands signatures; lookup requires concrete classes.
      check_signature <- function(signature, position = 1L) {
        if (position > length(signature)) {
          method <- S7::method(
            req@generic,
            class = if (length(signature) == 1L) {
              signature[[1L]]
            } else {
              signature
            }
          )
          .check_required_formals(method, names(req@args), method_what)
          return(invisible(NULL))
        }
        classes <- signature[[position]]
        classes <- if (inherits(classes, "S7_union")) {
          classes$classes
        } else {
          list(classes)
        }
        for (cls in classes) {
          signature[position] <- list(cls)
          check_signature(signature, position + 1L)
        }
      }
      check_signature(c(list(cls), plan$tail))
      NULL
    },
    error = identity
  )
}

.requirement_problems <- function(cls, plans) {
  lapply(plans, .requirement_problem, cls = cls)
}

# `cls` is the receiver class, or NULL when it could not be determined.
.assert_conforms <- function(cls, interface, plans, arg) {
  problems <- .requirement_problems(cls, plans)
  missing <- !vapply(problems, is.null, logical(1))
  if (any(missing)) {
    missing_names <- vapply(
      plans[missing],
      function(plan) plan$req@name,
      character(1)
    )
    .abort(
      "%s does not implement %s; missing: %s",
      arg,
      .interface_label(interface),
      paste0(missing_names, "()", collapse = ", ")
    )
  }
  invisible(NULL)
}

# Builds the instrumented copy of a requirement's generic. Its body runs the
# argument checks in the generic's own frame, so S7 dispatch and R's argument
# promises behave as in an uninstrumented call.
.instrument_generic <- function(contract, req, plans, trait) {
  generic <- req@generic
  dispatch_args <- generic@dispatch_args
  typed_args <- names(req@args)
  checkers <- Map(.value_checker, req@args, typed_args)
  generic_formals <- formals(generic)
  method_what <- sprintf("Method `%s()`", req@name)
  generic_problem <- if (trait) {
    tryCatch(
      .check_required_formals(
        generic,
        typed_args,
        sprintf("Generic `%s()`", req@name)
      ),
      error = identity
    )
  }

  check_arguments <- function(frame) {
    dispatch_values <- lapply(dispatch_args, function(arg) {
      value <- get(arg, envir = frame, inherits = FALSE)
      if (arg %in% typed_args) {
        checkers[[arg]](value)
      }
      value
    })
    # The receiver is a value, so a class object is classed as an object.
    receiver_class <- .value_class_or_null(dispatch_values[[1L]])
    if (trait) {
      .assert_trait_status(receiver_class, contract, dispatch_args[[1L]])
      if (inherits(generic_problem, "condition")) {
        stop(generic_problem)
      }
    } else {
      .assert_conforms(receiver_class, contract, plans, dispatch_args[[1L]])
    }
    method <- S7::method(
      generic,
      object = if (length(dispatch_values) == 1L) {
        dispatch_values[[1L]]
      } else {
        dispatch_values
      }
    )
    .check_required_formals(method, typed_args, method_what)

    method_frame <- NULL
    for (arg in setdiff(typed_args, dispatch_args)) {
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
            if (
              eval(call("missing", as.name(name)), envir = frame) &&
                identical(generic_formals[[name]], quote(expr = ))
            ) {
              actuals[name] <- NULL
            }
          }
          names(actuals)[names(actuals) == "..."] <- ""
          body(method) <- quote(base::environment())
          method_frame <- eval(as.call(c(list(method), actuals)), envir = frame)
        }
        assign(
          arg,
          get(arg, envir = method_frame, inherits = FALSE),
          envir = frame
        )
      }
      checkers[[arg]](get(arg, envir = frame, inherits = FALSE))
    }
  }

  checked <- S7::S7_data(generic)
  body(checked) <- substitute(
    {
      CHECK(base::environment())
      BODY
    },
    list(CHECK = check_arguments, BODY = body(generic))
  )
  checked_generic <- generic
  S7::S7_data(checked_generic) <- checked
  checked_generic
}

# The wrapper bound in a contract mask. The instrumented generic and its value
# checkers depend only on the contract, so they are built by a promise on the
# first call through this wrapper and shared by later calls.
.make_checked_generic <- function(contract, req, plans, trait = FALSE) {
  force(contract)
  force(req)
  force(plans)
  force(trait)
  delayedAssign(
    "checked_generic",
    .instrument_generic(contract, req, plans, trait)
  )
  delayedAssign("check_return", .value_checker(req@returns, ".return"))
  function(...) {
    env <- parent.frame()
    matched <- tryCatch(
      # `envir` expands `...` from callers such as lapply(xs, generic, arg = value).
      match.call(
        definition = req@generic,
        call = sys.call(),
        expand.dots = TRUE,
        envir = env
      ),
      error = function(e) {
        .abort(
          "Could not match call to `%s()`: %s",
          req@name,
          conditionMessage(e)
        )
      }
    )
    matched[[1L]] <- checked_generic
    out <- withVisible(eval(matched, envir = env))
    check_return(out$value)
    if (out$visible) out$value else invisible(out$value)
  }
}

# Wrappers for every requirement of a contract, keyed by the names they bind.
.contract_wrappers <- function(contract, trait = FALSE) {
  reqs <- if (trait) {
    trait_methods(contract, inherited = TRUE)
  } else {
    interface_requirements(contract, inherited = TRUE)
  }
  plans <- if (!trait) lapply(reqs, .requirement_plan)

  wrappers <- list()
  for (req in reqs) {
    wrapper <- .make_checked_generic(contract, req, plans, trait = trait)
    for (name in unique(c(req@name, req@generic@name))) {
      wrappers[[name]] <- wrapper
    }
  }
  wrappers
}

.contract_mask <- function(wrappers, env) {
  list2env(wrappers, envir = new.env(parent = env))
}

.with_contract <- function(contract, expr, env, trait = FALSE) {
  wrappers <- .contract_wrappers(contract, trait = trait)
  mask <- .contract_mask(wrappers, env)
  out <- withVisible(eval(expr, envir = mask))
  value <- out$value
  if (
    identical(typeof(value), "closure") && !identical(environment(value), mask)
  ) {
    environment(value) <- .contract_mask(wrappers, environment(value))
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
