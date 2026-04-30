library(S7)
library(s7contract)

# Shared S7 generics
area <- new_generic("area", "x")
draw <- new_generic("draw", "x")
perimeter <- new_generic("perimeter", "x")

# Domain classes
Circle <- new_class("Circle", properties = list(r = class_double))
Rect <- new_class("Rect", properties = list(w = class_double, h = class_double))

# Some methods
method(area, Circle) <- function(x) pi * x@r^2
method(draw, Circle) <- function(x) sprintf("circle(r = %s)", x@r)
method(area, Rect) <- function(x) x@w * x@h

# Go-like structural interface
Drawable <- new_interface("Drawable", list(draw = draw))
Shape <- new_interface("Shape", list(area = area), parents = Drawable)

stopifnot(implements(Circle, Shape))
stopifnot(!implements(Rect, Shape))
print(missing_requirements(Rect, Shape))

# Rust-like explicit traits
Measurable <- new_trait(
  "Measurable",
  methods = list(
    area = trait_method(area),
    perimeter = trait_method(perimeter, default = function(x) NA_real_)
  ),
  assoc_consts = c("UNITS")
)

impl_trait(
  Measurable,
  Circle,
  methods = list(area = function(x) pi * x@r^2),
  assoc_consts = list(UNITS = "unitless"),
  replace = TRUE
)

stopifnot(has_trait(Circle, Measurable))
stopifnot(!has_trait(Rect, Measurable))
stopifnot(is.na(trait_call(Measurable, "perimeter", Circle(r = 2))))
print(trait_assoc_const(Measurable, Circle, "UNITS"))
