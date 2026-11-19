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
    generics = list(
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
  expect_equal(
    with(DrawableOnCanvas, draw_on(circle, canvas, position = 1L)),
    "circle(2)@1"
  )
  expect_equal(
    draw_on(circle, canvas, position = 1L) %::% DrawableOnCanvas,
    "circle(2)@1"
  )

  n_eval <- 0L
  make_circle <- function() {
    n_eval <<- n_eval + 1L
    circle
  }
  make_position <- function() {
    n_eval <<- n_eval + 1L
    1L
  }
  expect_equal(
    with(
      DrawableOnCanvas,
      draw_on(make_circle(), canvas, position = make_position())
    ),
    "circle(2)@1"
  )
  expect_equal(n_eval, 2L)

  local({
    draw_on <- function(...) "not the S7 generic"
    expect_equal(
      with(DrawableOnCanvas, draw_on(circle, canvas, position = 1L)),
      "circle(2)@1"
    )
  })

  expect_equal(
    with(DrawableOnCanvas, {
      render <- function(x) draw_on(x, canvas, position = 1L)
      render(circle)
    }),
    "circle(2)@1"
  )

  render_checked <- with(DrawableOnCanvas, function(x) {
    draw_on(x, canvas, position = 1L)
  })
  expect_equal(render_checked(circle), "circle(2)@1")
  expect_error(render_checked(square), "Return value")

  render_checked2 <- (function(x) draw_on(x, canvas, position = 1L)) %::%
    DrawableOnCanvas
  expect_equal(render_checked2(circle), "circle(2)@1")
  expect_error(render_checked2(square), "Return value")

  render_plain <- function(x) draw_on(x, canvas, position = 1L)
  render_checked3 <- render_plain %::% DrawableOnCanvas
  expect_equal(render_checked3(circle), "circle(2)@1")
  expect_error(render_checked3(square), "Return value")

  render_checked4 <- with(DrawableOnCanvas, render_plain)
  expect_equal(render_checked4(circle), "circle(2)@1")
  expect_error(render_checked4(square), "Return value")

  render_capturing <- local({
    pos <- 1L
    function(x) draw_on(x, canvas, position = pos)
  })
  render_checked5 <- render_capturing %::% DrawableOnCanvas
  expect_equal(render_checked5(circle), "circle(2)@1")
  expect_error(render_checked5(square), "Return value")

  expect_error(
    with(DrawableOnCanvas, draw_on(circle, canvas, position = "bad")),
    "position"
  )
  expect_error(
    with(DrawableOnCanvas, draw_on(square, canvas, position = 1L)),
    "Return value"
  )

  draw_default <- new_generic(
    "draw_default_progressive_test",
    c("x", "canvas"),
    function(x, canvas, position = 1L, ...) S7_dispatch()
  )
  method(draw_default, list(Circle, Canvas)) <- function(
    x,
    canvas,
    position = 1L,
    ...
  ) {
    sprintf("default-circle(%s)@%s", x@r, position)
  }
  DrawableDefault <- new_interface(
    "DrawableDefaultProgressiveTest",
    generics = list(
      draw_default = interface_requirement(
        draw_default,
        args = list(canvas = Canvas, position = class_integer),
        returns = class_character
      )
    )
  )
  expect_equal(
    with(DrawableDefault, draw_default(circle, canvas)),
    "default-circle(2)@1"
  )

  draw_bad_default <- new_generic(
    "draw_bad_default_progressive_test",
    c("x", "canvas"),
    function(x, canvas, position = "bad", ...) S7_dispatch()
  )
  method(draw_bad_default, list(Circle, Canvas)) <- function(
    x,
    canvas,
    position = "bad",
    ...
  ) {
    sprintf("bad-default-circle(%s)@%s", x@r, position)
  }
  DrawableBadDefault <- new_interface(
    "DrawableBadDefaultProgressiveTest",
    generics = list(
      draw_bad_default = interface_requirement(
        draw_bad_default,
        args = list(canvas = Canvas, position = class_integer),
        returns = class_character
      )
    )
  )
  expect_error(
    with(DrawableBadDefault, draw_bad_default(circle, canvas)),
    "position"
  )
  expect_error(
    draw_bad_default(circle, canvas) %::% DrawableBadDefault,
    "position"
  )

  draw_dots <- new_generic(
    "draw_dots_progressive_test",
    "x",
    function(x, ..., flag = TRUE) S7_dispatch()
  )
  method(draw_dots, Circle) <- function(x, ..., flag = TRUE) {
    list(flag = flag, dots = list(...))
  }
  DrawableDots <- new_interface(
    "DrawableDotsProgressiveTest",
    generics = list(
      draw_dots = interface_requirement(draw_dots, returns = class_list)
    )
  )
  dots_out <- with(DrawableDots, draw_dots(circle, a = 1, b = 2, flag = FALSE))
  expect_equal(dots_out$flag, FALSE)
  expect_equal(dots_out$dots, list(a = 1, b = 2))

  default_from_arg <- new_generic(
    "default_from_arg_progressive_test",
    "x",
    function(x, values, n = length(values)) S7_dispatch()
  )
  method(default_from_arg, Circle) <- function(x, values, n = length(values)) {
    n + length(values)
  }
  DefaultFromArg <- new_interface(
    "DefaultFromArgProgressiveTest",
    generics = list(
      default_from_arg = interface_requirement(
        default_from_arg,
        args = list(n = class_integer),
        returns = class_integer
      )
    )
  )
  n_values_eval <- 0L
  make_values <- function() {
    n_values_eval <<- n_values_eval + 1L
    1:3
  }
  expect_equal(
    with(DefaultFromArg, default_from_arg(circle, make_values())),
    6L
  )
  expect_equal(n_values_eval, 1L)

  Plain <- new_class("PlainProgressiveTest")
  make_bad <- new_generic("make_bad_progressive_test", "x")
  method(make_bad, Circle) <- function(x) Plain()
  MakerDrawable <- new_interface(
    "MakerDrawableProgressiveTest",
    generics = list(
      make_bad = interface_requirement(make_bad, returns = DrawableOnCanvas)
    )
  )
  expect_error(
    with(MakerDrawable, make_bad(circle)),
    "Return value does not implement"
  )

  no_position <- new_generic(
    "no_position_progressive_test",
    c("x", "canvas"),
    function(x, canvas, ...) S7_dispatch()
  )
  method(no_position, list(Circle, Canvas)) <- function(x, canvas, ...) "ok"
  BadShape <- new_interface(
    "BadShapeProgressiveTest",
    generics = list(
      no_position = interface_requirement(
        no_position,
        args = list(canvas = Canvas, position = class_integer),
        returns = class_character
      )
    )
  )
  expect_false(implements(Circle, BadShape))
  expect_equal(
    missing_requirements(Circle, BadShape)$requirement,
    "no_position"
  )

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
    methods = list(trait_draw_on = function(x, canvas, position, ...) {
      sprintf("trait-circle(%s)@%s", x@r, position)
    })
  )

  expect_equal(
    with(DrawableTrait, trait_draw_on(circle, canvas, position = 2L)),
    "trait-circle(2)@2"
  )
  expect_equal(
    trait_draw_on(circle, canvas, position = 2L) %::% DrawableTrait,
    "trait-circle(2)@2"
  )
  expect_error(
    with(DrawableTrait, trait_draw_on(circle, canvas, position = "bad")),
    "position"
  )
})

local({
  Pen <- new_class("PenForwardingTest", properties = list(width = class_double))
  stroke <- new_generic(
    "stroke_forwarding_test",
    "x",
    function(x, color) S7_dispatch()
  )
  method(stroke, Pen) <- function(x, color) sprintf("%s:%s", color, x@width)
  Stroking <- new_interface(
    "StrokingForwardingTest",
    generics = list(
      stroke = interface_requirement(
        stroke,
        args = list(color = class_character),
        returns = class_character
      )
    )
  )

  pens <- list(Pen(width = 1), Pen(width = 2))
  X <- "red"
  evaluations <- 0L
  next_color <- function() {
    evaluations <<- evaluations + 1L
    "blue"
  }

  expect_equal(
    with(Stroking, lapply(pens, stroke, color = X)),
    list("red:1", "red:2")
  )
  expect_equal(
    with(Stroking, vapply(pens, stroke, character(1), "green")),
    c("green:1", "green:2")
  )
  expect_equal(
    with(Stroking, lapply(pens, stroke, color = next_color())),
    list("blue:1", "blue:2")
  )
  expect_identical(evaluations, 1L)
  expect_error(
    with(Stroking, lapply(pens, stroke, color = 1)),
    "`color` must be <character>"
  )

  trait_stroke <- new_generic(
    "trait_stroke_forwarding_test",
    "x",
    function(x, color) S7_dispatch()
  )
  Strokes <- new_trait(
    "StrokesForwardingTest",
    methods = list(
      trait_stroke = trait_method(
        trait_stroke,
        args = list(color = class_character)
      )
    )
  )
  impl_trait(
    Strokes,
    Pen,
    methods = list(
      trait_stroke = function(x, color) sprintf("%s:%s", color, x@width)
    )
  )
  expect_equal(
    with(Strokes, lapply(pens, trait_stroke, color = X)),
    list("red:1", "red:2")
  )
  expect_error(
    with(Strokes, lapply(pens, trait_stroke, color = 1)),
    "`color` must be <character>"
  )
})

# A contract scope checks method tables when each call runs, so a scoped closure
# observes registrations made after the scope was created.
local({
  Tile <- new_class("TileScopeTest")
  paint <- new_generic("paint_scope_test", "x")
  measure <- new_generic("measure_scope_test", "x")
  method(paint, Tile) <- function(x, ...) "painted"
  Surface <- new_interface(
    "SurfaceScopeTest",
    generics = list(paint = paint, measure = measure)
  )
  tile <- Tile()
  scoped_paint <- with(Surface, function() paint(tile))

  expect_error(
    scoped_paint(),
    "x does not implement SurfaceScopeTest; missing: measure()",
    fixed = TRUE
  )
  method(measure, Tile) <- function(x, ...) 1
  expect_identical(scoped_paint(), "painted")
  expect_true(implements(tile, Surface))

  rm(list = Tile@name, envir = measure@methods)
  expect_error(scoped_paint(), "missing: measure()", fixed = TRUE)
  expect_false(implements(tile, Surface))
  expect_identical(
    interface_report(tile, Surface)$ok,
    c(TRUE, FALSE)
  )
})

# A checked call treats its receiver and typed values as values: a class object
# is classed as the object it is. The conformance functions keep reading a
# class argument as the class to check.
local({
  Sheet <- new_class("SheetValueTest")
  label <- new_generic("label_value_test", "x")
  method(label, new_S3_class("S7_class")) <- function(x, ...) x@name
  Labelled <- new_interface("LabelledValueTest", generics = list(label = label))

  expect_identical(label(Sheet), "SheetValueTest")
  expect_identical(with(Labelled, label(Sheet)), "SheetValueTest")
  expect_error(
    with(Labelled, label(Sheet())),
    "x does not implement LabelledValueTest; missing: label()",
    fixed = TRUE
  )
  expect_false(implements(Sheet, Labelled))

  size <- new_generic("size_value_test", "x")
  method(size, Sheet) <- function(x, ...) 1
  Sized <- new_interface("SizedValueTest", generics = list(size = size))
  expect_true(implements(Sheet, Sized))
  expect_identical(with(Sized, size(Sheet())), 1)
  expect_error(
    with(Sized, size(Sheet)),
    "x does not implement SizedValueTest; missing: size()",
    fixed = TRUE
  )

  stamp <- new_generic(
    "stamp_value_test",
    "x",
    function(x, mark) S7_dispatch()
  )
  method(stamp, Sheet) <- function(x, mark) "stamped"
  Stamping <- new_interface(
    "StampingValueTest",
    generics = list(
      stamp = interface_requirement(stamp, args = list(mark = Labelled))
    )
  )
  expect_identical(with(Stamping, stamp(Sheet(), mark = Sheet)), "stamped")
  expect_error(
    with(Stamping, stamp(Sheet(), mark = Sheet())),
    "`mark` does not implement LabelledValueTest; missing: label()",
    fixed = TRUE
  )

  weigh <- new_generic("weigh_value_test", "x")
  Weighable <- new_trait(
    "WeighableValueTest",
    methods = list(weigh = trait_method(weigh))
  )
  impl_trait(
    Weighable,
    Sheet,
    methods = list(weigh = function(x, ...) 2)
  )
  expect_true(has_trait(Sheet, Weighable))
  expect_identical(with(Weighable, weigh(Sheet())), 2)
  expect_error(
    with(Weighable, weigh(Sheet)),
    "x does not explicitly implement WeighableValueTest"
  )
})
