library(S7)

# A union requirement covers every concrete dispatch combination.
local({
  C <- new_class("UnionContractTest")
  combine <- new_generic("combine_union_contract_test", c("x", "y", "z"))
  method(combine, list(C, class_integer, class_numeric)) <- function(x, y, z) TRUE
  Protocol <- new_interface("UnionProtocolTest", list(
    combine = interface_requirement(combine, args = list(y = class_numeric, z = class_numeric))
  ))
  expect_false(implements(C, Protocol))
  method(combine, list(C, class_double, class_numeric)) <- function(x, y, z) TRUE
  expect_true(implements(C, Protocol))
  expect_true(with(Protocol, combine(C(), 1L, 2)))
  expect_true(with(Protocol, combine(C(), 1, 2L)))
  expect_error(with(Protocol, combine(C(), "bad", 2L)), "y")

  measure <- new_generic("measure_union_contract_test", "x")
  method(measure, class_numeric) <- function(x) length(x)
  Measured <- new_interface("MeasuredUnionTest", list(measure = measure))
  expect_true(implements(class_numeric, Measured))
})

# Checked calls select defaults from the concrete method, not a union signature.
local({
  C <- new_class("UnionDefaultContractTest")
  choose <- new_generic("choose_union_default_test", c("x", "y"),
                        function(x, y, n, ...) S7_dispatch())
  expect_warning(method(choose, list(C, class_integer)) <- function(x, y, n = 1L, ...) n,
                 "default value")
  expect_warning(method(choose, list(C, class_double)) <- function(x, y, n = 2L, ...) n,
                 "default value")
  Protocol <- new_interface("UnionDefaultProtocolTest", list(
    choose = interface_requirement(choose, args = list(y = class_numeric, n = class_integer))
  ))
  expect_identical(with(Protocol, choose(C(), 0L)), choose(C(), 0L))
  expect_identical(with(Protocol, choose(C(), 0)), choose(C(), 0))

  Trait <- new_trait("UnionDefaultTraitTest", list(
    choose = trait_method(choose, args = list(y = class_numeric, n = class_integer))
  ))
  expect_warning(
    impl_trait(Trait, C, methods = list(choose = function(x, y, n = 3L, ...) n), replace = TRUE),
    "default value"
  )
  expect_identical(with(Trait, choose(C(), 0L)), 3L)
  expect_identical(with(Trait, choose(C(), 0)), 3L)
})

# Generic defaults retain lexical scope and take precedence over method defaults.
local({
  C <- new_class("LexicalDefaultContractTest")
  value <- local({
    default <- 7L
    new_generic("value_lexical_default_test", "x", function(x, n = default) S7_dispatch())
  })
  method(value, C) <- local({
    default <- 9L
    function(x, n = default) n
  })
  Protocol <- new_interface("LexicalDefaultProtocolTest", list(
    value = interface_requirement(value, args = list(n = class_integer))
  ))
  default <- "caller binding"
  expect_identical(value(C()), 7L)
  expect_identical(with(Protocol, value(C())), value(C()))
  expect_identical(value(C()) %::% Protocol, value(C()))
})

# Typed defaults share promises for dependencies, irrespective of specification order.
local({
  C <- new_class("DependentDefaultContractTest")
  calls <- 0L
  next_value <- function() {
    calls <<- calls + 1L
    4L
  }
  value <- new_generic("value_dependent_default_test", "x",
                       function(x, n = m, m = next_value()) S7_dispatch())
  method(value, C) <- function(x, n = m, m = next_value()) c(n, m)
  Protocol <- new_interface("DependentDefaultProtocolTest", list(
    value = interface_requirement(value, args = list(n = class_integer, m = class_integer))
  ))
  expect_identical(with(Protocol, value(C())), c(4L, 4L))
  expect_identical(calls, 1L)

  # Untyped defaults used by a typed default retain their cached value.
  OnlyN <- new_interface("OneDefaultProtocolTest", list(
    value = interface_requirement(value, args = list(n = class_integer))
  ))
  calls <- 0L
  expect_identical(with(OnlyN, value(C())), c(4L, 4L))
  expect_identical(calls, 1L)
})

# Checked dispatch preserves generic execution and return visibility.
local({
  C <- new_class("GenericBodyContractTest")
  calls <- 0L
  value <- new_generic("value_generic_body_test", "x", function(x, n = 1L) {
    calls <<- calls + 1L
    S7_dispatch()
  })
  method(value, C) <- function(x, n = 1L) invisible(n)
  original_body <- body(value)
  original_method <- method(value, C)
  Protocol <- new_interface("GenericBodyProtocolTest", list(
    value = interface_requirement(value, args = list(n = class_integer))
  ))
  expect_identical(withVisible(with(Protocol, value(C()))), list(value = 1L, visible = FALSE))
  expect_identical(calls, 1L)
  expect_error(with(Protocol, value(C(), n = "bad")), "n")
  expect_identical(calls, 1L)
  expect_identical(withVisible(value(C()) %::% Protocol), list(value = 1L, visible = FALSE))
  expect_identical(calls, 2L)
  expect_identical(body(value), original_body)
  expect_identical(method(value, C), original_method)
})

# Explicit registration does not make an absent generic formal available.
local({
  C <- new_class("MissingFormalContractTest")
  value <- new_generic("value_missing_formal_test", "x")
  Trait <- new_trait("MissingFormalTraitTest", list(
    value = trait_method(value, args = list(n = class_integer))
  ))
  impl_trait(Trait, C, methods = list(value = function(x, ...) TRUE))
  expect_true(has_trait(C, Trait))
  expect_error(with(Trait, value(C())), "Generic .*missing required argument")
})

# Trait registrations use descriptor identity rather than a display label.
local({
  C <- new_class("NominalIdentityContractTest")
  Trait <- new_trait("NominalIdentityTest")
  expect_error(impl_trait(Trait, class_numeric), "concrete class")
  Other <- new_trait("NominalIdentityTest")
  restored <- unserialize(serialize(Trait, NULL))
  impl_trait(Trait, C)
  expect_true(has_trait(C, Trait))
  expect_false(has_trait(C, Other))
  expect_false(has_trait(C, restored))
  impl_trait(restored, C)
  expect_true(has_trait(C, restored))
  expect_true(has_trait(C, Trait))
  expect_error(impl_trait(Trait, C), "already implemented")
  impl_trait(Trait, C, replace = TRUE)
  expect_true(has_trait(C, Trait))
  expect_true(has_trait(C, restored))
})
