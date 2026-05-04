library(S7)

local({
  Canvas <- new_class("CanvasProgressiveTest")
  Circle <- new_class(
    "CircleProgressiveTest",
    properties = list(r = class_double)
  )
  Square <- new_class(
    "SquareProgressiveTest",
    properties = list(side = class_double)
  )

  draw_on <- new_generic(
    "draw_on_progressive_test",
    c("x", "canvas"),
    function(x, canvas, position, ...) S7_dispatch()
  )

  method(draw_on, list(Circle, Canvas)) <- function(x, canvas, position, ...) {
    sprintf("circle(%s)@%s", x@r, position)
  }
  method(draw_on, list(Square, Canvas)) <- function(x, canvas, position, ...) {
    x@side
  }

  DrawableOnCanvas <- new_interface(
    "DrawableOnCanvasProgressiveTest",
    methods = list(
      draw_on = interface_requirement(
        draw_on,
        args = list(canvas = Canvas, position = class_integer),
        returns = class_character
      )
    )
  )

  circle <- Circle(r = 2)
  square <- Square(side = 3)
  canvas <- Canvas()

  expect_true(implements(Circle, DrawableOnCanvas))
  expect_true(implements(circle, DrawableOnCanvas))
  expect_equal(with(DrawableOnCanvas, draw_on(circle, canvas, position = 1L)), "circle(2)@1")
  expect_equal(draw_on(circle, canvas, position = 1L) %::% DrawableOnCanvas, "circle(2)@1")

  n_eval <- 0L
  make_circle <- function() {
    n_eval <<- n_eval + 1L
    circle
  }
  make_position <- function() {
    n_eval <<- n_eval + 1L
    1L
  }
  expect_equal(with(DrawableOnCanvas, draw_on(make_circle(), canvas, position = make_position())), "circle(2)@1")
  expect_equal(n_eval, 2L)

  local({
    draw_on <- function(...) "not the S7 generic"
    expect_equal(with(DrawableOnCanvas, draw_on(circle, canvas, position = 1L)), "circle(2)@1")
  })

  expect_equal(
    with(DrawableOnCanvas, {
      render <- function(x) draw_on(x, canvas, position = 1L)
      render(circle)
    }),
    "circle(2)@1"
  )

  render_checked <- with(DrawableOnCanvas, function(x) draw_on(x, canvas, position = 1L))
  expect_equal(render_checked(circle), "circle(2)@1")
  expect_error(render_checked(square), "Return value")

  render_checked2 <- (function(x) draw_on(x, canvas, position = 1L)) %::% DrawableOnCanvas
  expect_equal(render_checked2(circle), "circle(2)@1")
  expect_error(render_checked2(square), "Return value")

  expect_error(with(DrawableOnCanvas, draw_on(circle, canvas, position = "bad")), "position")
  expect_error(with(DrawableOnCanvas, draw_on(square, canvas, position = 1L)), "Return value")

  draw_default <- new_generic(
    "draw_default_progressive_test",
    c("x", "canvas"),
    function(x, canvas, position = 1L, ...) S7_dispatch()
  )
  method(draw_default, list(Circle, Canvas)) <- function(x, canvas, position = 1L, ...) {
    sprintf("default-circle(%s)@%s", x@r, position)
  }
  DrawableDefault <- new_interface(
    "DrawableDefaultProgressiveTest",
    methods = list(
      draw_default = interface_requirement(
        draw_default,
        args = list(canvas = Canvas, position = class_integer),
        returns = class_character
      )
    )
  )
  expect_equal(with(DrawableDefault, draw_default(circle, canvas)), "default-circle(2)@1")

  draw_bad_default <- new_generic(
    "draw_bad_default_progressive_test",
    c("x", "canvas"),
    function(x, canvas, position = "bad", ...) S7_dispatch()
  )
  method(draw_bad_default, list(Circle, Canvas)) <- function(x, canvas, position = "bad", ...) {
    sprintf("bad-default-circle(%s)@%s", x@r, position)
  }
  DrawableBadDefault <- new_interface(
    "DrawableBadDefaultProgressiveTest",
    methods = list(
      draw_bad_default = interface_requirement(
        draw_bad_default,
        args = list(canvas = Canvas, position = class_integer),
        returns = class_character
      )
    )
  )
  expect_error(with(DrawableBadDefault, draw_bad_default(circle, canvas)), "position")
  expect_error(draw_bad_default(circle, canvas) %::% DrawableBadDefault, "position")

  no_position <- new_generic(
    "no_position_progressive_test",
    c("x", "canvas"),
    function(x, canvas, ...) S7_dispatch()
  )
  method(no_position, list(Circle, Canvas)) <- function(x, canvas, ...) "ok"
  BadShape <- new_interface(
    "BadShapeProgressiveTest",
    methods = list(
      no_position = interface_requirement(
        no_position,
        args = list(canvas = Canvas, position = class_integer),
        returns = class_character
      )
    )
  )
  expect_false(implements(Circle, BadShape))
  expect_equal(missing_requirements(Circle, BadShape)$requirement, "no_position")

  trait_draw_on <- new_generic(
    "trait_draw_on_progressive_test",
    c("x", "canvas"),
    function(x, canvas, position, ...) S7_dispatch()
  )

  DrawableTrait <- new_trait(
    "DrawableTraitProgressiveTest",
    methods = list(
      trait_draw_on = trait_method(
        trait_draw_on,
        args = list(canvas = Canvas, position = class_integer),
        returns = class_character
      )
    )
  )

  impl_trait(
    DrawableTrait,
    Circle,
    methods = list(trait_draw_on = function(x, canvas, position, ...) sprintf("trait-circle(%s)@%s", x@r, position))
  )

  expect_equal(with(DrawableTrait, trait_draw_on(circle, canvas, position = 2L)), "trait-circle(2)@2")
  expect_equal(trait_draw_on(circle, canvas, position = 2L) %::% DrawableTrait, "trait-circle(2)@2")
  expect_error(with(DrawableTrait, trait_draw_on(circle, canvas, position = "bad")), "position")
})
