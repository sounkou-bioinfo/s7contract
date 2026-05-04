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
  dispatch_args <- if (inherits(generic, "S7_generic")) {
    generic@dispatch_args
  } else {
    character()
  }
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
      method <- S7::method(generic, class = signature)

      .check_required_formals(
        generic,
        names(req@args),
        sprintf("Generic `%s()`", req@name)
      )
      .check_required_formals(
        method,
        names(req@args),
        sprintf("Method `%s()`", req@name)
      )

      list(ok = TRUE, method = method, error = NULL)
    },
    error = function(e) {
      list(ok = FALSE, method = NULL, error = e)
    }
  )
}


.bind_checked_arg <- function(call, arg, value, eval_env, prefix) {
  nm <- sprintf(".%s_%s", prefix, arg)
  assign(nm, value, envir = eval_env)
  assign(arg, value, envir = eval_env)
  call[[arg]] <- as.name(nm)
  call
}

.bind_delayed_arg <- function(call, arg, expr, eval_env, source_env, prefix) {
  nm <- sprintf(".%s_%s", prefix, arg)
  force(expr)
  force(eval_env)
  force(source_env)
  force(nm)

  delayedAssign(nm, eval(expr, envir = source_env), assign.env = eval_env)
  delayedAssign(arg, get(nm, envir = eval_env), assign.env = eval_env)
  call[[arg]] <- as.name(nm)
  call
}

.is_missing_default <- function(x) {
  identical(x, quote(expr = ))
}

.default_typed_arg <- function(generic, method, arg, eval_env) {
  for (fun in list(method, generic)) {
    fml <- formals(fun)
    if (arg %in% names(fml) && !.is_missing_default(fml[[arg]])) {
      return(eval(fml[[arg]], envir = eval_env))
    }
  }
  .abort(
    "Call is missing typed argument `%s` and no default could be evaluated.",
    arg
  )
}

.generic_bind_names <- function(req) {
  unique(c(req@name, .generic_label(req@generic)))
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

  eval_env <- new.env(parent = env)
  assign(".s7contract_generic", generic, envir = eval_env)
  matched[[1L]] <- as.name(".s7contract_generic")

  formal_args <- names(formals(generic))
  supplied_args <- intersect(names(matched)[-1L], formal_args)
  supplied_args <- setdiff(supplied_args[nzchar(supplied_args)], "...")
  for (arg in supplied_args) {
    matched <- .bind_delayed_arg(
      matched,
      arg,
      matched[[arg]],
      eval_env,
      env,
      "arg"
    )
  }

  dispatch_args <- if (inherits(generic, "S7_generic")) {
    generic@dispatch_args
  } else {
    names(formals(generic))[1L]
  }
  first_arg <- dispatch_args[[1L]]
  if (!first_arg %in% names(matched)) {
    .abort("Call is missing first dispatch argument `%s`.", first_arg)
  }
  first_value <- eval(matched[[first_arg]], envir = eval_env)
  matched <- .bind_checked_arg(matched, first_arg, first_value, eval_env, "arg")

  if (trait) {
    assert_trait(first_value, contract, arg = first_arg)
  } else {
    assert_implements(first_value, contract, arg = first_arg)
  }

  found <- .lookup_requirement_method(req, first_value)
  if (!found$ok) {
    .abort("%s", conditionMessage(found$error))
  }

  for (arg in names(req@args)) {
    value <- if (arg %in% names(matched)) {
      eval(matched[[arg]], envir = eval_env)
    } else {
      .default_typed_arg(generic, found$method, arg, eval_env)
    }
    .check_value_conforms(value, req@args[[arg]], arg)
    matched <- .bind_checked_arg(matched, arg, value, eval_env, "arg")
  }

  out <- eval(matched, envir = eval_env)
  .check_value_conforms(out, req@returns, ".return")
  out
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
    for (name in .generic_bind_names(req)) {
      assign(name, wrapper, envir = mask)
    }
  }

  mask
}

.rebind_contract_function <- function(fun, contract, mask, trait = FALSE) {
  if (!identical(typeof(fun), "closure") || identical(environment(fun), mask)) {
    return(fun)
  }

  function_mask <- .contract_mask(contract, environment(fun), trait = trait)
  environment(fun) <- function_mask
  fun
}

.with_contract <- function(contract, expr, env, trait = FALSE) {
  mask <- .contract_mask(contract, env, trait = trait)
  out <- eval(expr, envir = mask)
  .rebind_contract_function(out, contract, mask, trait = trait)
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
