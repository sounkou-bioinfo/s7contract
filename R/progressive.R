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
    .abort("`%s` is a dispatch argument and must be an S7 class or S7 union, not an interface or trait.", arg)
  }
  cls <- .as_class_or_null(spec, arg = arg)
  if (is.null(cls)) {
    .abort("`%s` must be an S7 class or S7 union for S7 dispatch.", arg)
  }
  cls
}

.spec_label <- function(spec) {
  if (.is_interface(spec)) {
    return(.interface_label(spec))
  }
  if (.is_trait(spec)) {
    return(.trait_label(spec))
  }
  .class_label(spec)
}

.check_value_conforms <- function(value, spec, arg) {
  if (.is_interface(spec)) {
    assert_implements(value, spec, arg = arg)
    return(invisible(value))
  }
  if (.is_trait(spec)) {
    assert_trait(value, spec, arg = arg)
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
      .abort("`%s` must satisfy %s: %s", arg, .spec_label(spec), msg)
    }
  )
  invisible(value)
}

.has_dots <- function(fun) {
  "..." %in% names(formals(fun))
}

.check_required_formals <- function(fun, arg_names, what) {
  if (length(arg_names) == 0) {
    return(invisible(TRUE))
  }
  formals_names <- names(formals(fun))
  missing <- setdiff(arg_names, formals_names)
  if (length(missing) > 0) {
    .abort("%s is missing required argument(s): %s", what, paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}

.requirement_signature <- function(req, target) {
  generic <- req@generic
  dispatch_args <- if (inherits(generic, "S7_generic")) generic@dispatch_args else character()
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

  tryCatch({
    signature <- .requirement_signature(req, target)
    method <- S7::method(generic, class = signature)

    .check_required_formals(generic, names(req@args), sprintf("Generic `%s()`", req@name))
    .check_required_formals(method, names(req@args), sprintf("Method `%s()`", req@name))

    list(ok = TRUE, method = method, error = NULL)
  }, error = function(e) {
    list(ok = FALSE, method = NULL, error = e)
  })
}

.call_head_name <- function(head) {
  if (is.symbol(head)) {
    return(as.character(head))
  }
  if (is.call(head) && as.character(head[[1L]]) %in% c("::", ":::")) {
    return(as.character(head[[3L]]))
  }
  "<call>"
}

.find_contract_requirement <- function(contract, expr, env, trait = FALSE) {
  if (!is.call(expr)) {
    .abort("Contract expressions must be calls.")
  }

  generic <- tryCatch(eval(expr[[1L]], envir = env), error = function(e) NULL)
  reqs <- if (trait) trait_methods(contract, inherited = TRUE) else interface_requirements(contract, inherited = TRUE)
  call_name <- .call_head_name(expr[[1L]])

  for (req in reqs) {
    if (!is.null(generic) && identical(req@generic, generic)) {
      return(req)
    }
  }

  .abort("%s has no requirement for the generic used by call `%s()`.", if (trait) .trait_label(contract) else .interface_label(contract), call_name)
}

.bind_checked_arg <- function(call, arg, value, eval_env, prefix) {
  nm <- sprintf(".%s_%s", prefix, arg)
  assign(nm, value, envir = eval_env)
  assign(arg, value, envir = eval_env)
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
  .abort("Call is missing typed argument `%s` and no default could be evaluated.", arg)
}

.with_contract <- function(contract, expr, env, trait = FALSE) {
  req <- .find_contract_requirement(contract, expr, env, trait = trait)
  generic <- req@generic
  matched <- tryCatch(
    match.call(definition = generic, call = expr, expand.dots = FALSE),
    error = function(e) .abort("Could not match call to `%s()`: %s", req@name, conditionMessage(e))
  )

  eval_env <- new.env(parent = env)
  assign(".s7contract_generic", generic, envir = eval_env)
  matched[[1L]] <- as.name(".s7contract_generic")

  dispatch_args <- if (inherits(generic, "S7_generic")) generic@dispatch_args else names(formals(generic))[1L]
  first_arg <- dispatch_args[[1L]]
  if (!first_arg %in% names(matched)) {
    .abort("Call is missing first dispatch argument `%s`.", first_arg)
  }
  first_value <- eval(matched[[first_arg]], envir = env)
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

.with_s7_interface <- function(data, expr, ...) {
  .with_contract(data, substitute(expr), parent.frame(), trait = FALSE)
}

.with_s7_trait <- function(data, expr, ...) {
  .with_contract(data, substitute(expr), parent.frame(), trait = TRUE)
}

#' Evaluate an S7 call under an interface or trait contract
#'
#' `with(contract, expr)` and `expr %::% contract` evaluate an ordinary S7 call
#' while checking the optional argument and return specifications stored in an
#' interface requirement or trait method. The call itself still uses normal S7
#' dispatch.
#'
#' @param expr An expression, usually a call to an S7 generic named in the
#'   contract.
#' @param contract An interface created by [new_interface()] or a trait created
#'   by [new_trait()].
#' @return The value of `expr`, after any optional return check.
#' @aliases contract_syntax
#' @examples
#' local({
#'   draw <- S7::new_generic("typed_draw", "x", function(x, color) {
#'     S7::S7_dispatch()
#'   })
#'   Circle <- S7::new_class("TypedCircle", properties = list(r = S7::class_double))
#'   S7::method(draw, Circle) <- function(x, color) paste(color, x@r)
#'   Drawable <- new_interface(
#'     "TypedDrawable",
#'     list(draw = interface_requirement(
#'       draw,
#'       args = list(color = S7::class_character),
#'       returns = S7::class_character
#'     ))
#'   )
#'   with(Drawable, draw(Circle(r = 2), color = "red"))
#'   draw(Circle(r = 2), color = "red") %::% Drawable
#' })
#' @export
`%::%` <- function(expr, contract) {
  contract <- eval(substitute(contract), envir = parent.frame())
  if (.is_interface(contract)) {
    return(.with_contract(contract, substitute(expr), parent.frame(), trait = FALSE))
  }
  if (.is_trait(contract)) {
    return(.with_contract(contract, substitute(expr), parent.frame(), trait = TRUE))
  }
  .abort("Right-hand side of `%::%` must be an interface or trait.")
}
