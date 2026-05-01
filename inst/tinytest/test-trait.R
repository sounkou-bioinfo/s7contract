library(S7)

local({
  area <- new_generic("area_trait_test", "x")
  perimeter <- new_generic("perimeter_trait_test", "x")
  draw <- new_generic("draw_trait_test", "x")

  Circle <- new_class(
    "CircleTraitTest",
    properties = list(r = class_double)
  )

  method(area, Circle) <- function(x) pi * x@r^2
  method(draw, Circle) <- function(x) sprintf("circle(%s)", x@r)

  Measurable <- new_trait(
    "MeasurableTraitTest",
    methods = list(
      area = trait_method(area),
      perimeter = trait_method(perimeter, default = function(x) NA_real_)
    ),
    assoc_consts = c("UNITS")
  )

  expect_false(has_trait(Circle, Measurable))

  impl_trait(
    Measurable,
    Circle,
    methods = list(area = function(x) pi * x@r^2),
    assoc_consts = list(UNITS = "unitless"),
    replace = TRUE
  )

  expect_true(has_trait(Circle, Measurable))
  expect_equal(trait_call(Measurable, "area", Circle(r = 2)), pi * 4)
  expect_true(is.na(trait_call(Measurable, "perimeter", Circle(r = 2))))
  expect_equal(trait_assoc_const(Measurable, Circle, "UNITS"), "unitless")

  Drawable <- new_trait(
    "DrawableTraitTest",
    methods = list(draw = trait_method(draw))
  )
  Renderable <- new_trait(
    "RenderableTraitTest",
    parents = list(Drawable)
  )

  expect_error(
    impl_trait(Renderable, Circle, replace = TRUE),
    "supertrait"
  )

  impl_trait(
    Drawable,
    Circle,
    methods = list(draw = function(x) sprintf("circle(%s)", x@r)),
    replace = TRUE
  )
  impl_trait(Renderable, Circle, replace = TRUE)
  expect_true(has_trait(Circle, Renderable))

  bad <- new_generic("bad_trait_test", "x")
  BadTrait <- new_trait(
    "BadTraitTest",
    methods = list(bad = trait_method(bad))
  )

  expect_error(
    impl_trait(BadTrait, Circle, methods = list(bad = function(y) y)),
    "dispatches on"
  )
  expect_false(has_trait(Circle, BadTrait))
  impl_trait(BadTrait, Circle, methods = list(bad = function(x) "ok"))
  expect_true(has_trait(Circle, BadTrait))
  expect_equal(trait_call(BadTrait, "bad", Circle(r = 1)), "ok")

  ParentAssoc <- new_trait(
    "ParentAssocTraitTest",
    assoc_consts = c("PARENT")
  )
  ChildAssoc <- new_trait(
    "ChildAssocTraitTest",
    parents = list(ParentAssoc),
    assoc_consts = list(LOCAL_NULL = NULL)
  )
  RequiredNull <- new_trait(
    "RequiredNullTraitTest",
    assoc_consts = c("VALUE")
  )

  impl_trait(
    ParentAssoc,
    Circle,
    assoc_consts = list(PARENT = "parent"),
    replace = TRUE
  )
  impl_trait(ChildAssoc, Circle, replace = TRUE)
  impl_trait(
    RequiredNull,
    Circle,
    assoc_consts = list(VALUE = NULL),
    replace = TRUE
  )

  expect_equal(trait_assoc_const(ChildAssoc, Circle, "PARENT"), "parent")
  expect_null(trait_assoc_const(ChildAssoc, Circle, "LOCAL_NULL"))
  expect_null(trait_assoc_const(RequiredNull, Circle, "VALUE"))
})
