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
})
