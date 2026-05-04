library(S7)

local({
  area <- new_generic("area_interface_test", "x")
  draw <- new_generic("draw_interface_test", "x")

  Circle <- new_class(
    "CircleInterfaceTest",
    properties = list(r = class_double)
  )
  Rect <- new_class(
    "RectInterfaceTest",
    properties = list(w = class_double, h = class_double)
  )

  method(area, Circle) <- function(x) pi * x@r^2
  method(draw, Circle) <- function(x) sprintf("circle(%s)", x@r)
  method(area, Rect) <- function(x) x@w * x@h

  Drawable <- new_interface(
    "DrawableInterfaceTest",
    generics = list(draw = draw)
  )
  Shape <- new_interface(
    "ShapeInterfaceTest",
    generics = list(area = area),
    parents = Drawable
  )
  DrawableAlias <- new_interface(
    "DrawableAliasInterfaceTest",
    methods = list(draw = draw)
  )

  expect_equal(names(interface_requirements(Drawable)), "draw")
  expect_equal(names(interface_requirements(DrawableAlias)), "draw")
  expect_error(
    new_interface("BadInterfaceTest", generics = list(identity)),
    "S7 generic"
  )
  expect_error(
    new_interface(
      "ConflictInterfaceTest",
      generics = list(draw = draw),
      methods = list(area = area)
    ),
    "either"
  )
  expect_true(any(grepl(
    "<S7 Go-like interface> DrawableInterfaceTest",
    capture.output(print(Drawable)),
    fixed = TRUE
  )))
  expect_true(implements(Circle, Shape))
  expect_true(implements(Circle(r = 2), Shape))
  expect_false(implements(Rect, Shape))
  expect_equal(missing_requirements(Rect, Shape)$requirement, "draw")
  expect_identical(as_interface(Circle(r = 2), Drawable)@r, 2)
  expect_error(assert_implements(Rect, Shape), "missing: draw\\(\\)")
})
