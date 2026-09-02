library(S7)
using(s7contract)

CirclePropertyIntegrationTest <- new_class(
  "CirclePropertyIntegrationTest",
  properties = list(radius = class_double),
  validator = function(self) {
    if (self@radius < 0) "`radius` must be non-negative."
  }
)
area_property_integration_test <- new_generic(
  "area_property_integration_test",
  "x",
  function(x) S7_dispatch()
)
method(area_property_integration_test, CirclePropertyIntegrationTest) <-
  function(x) pi * x@radius^2

HasAreaPropertyIntegrationTest <- new_interface(
  "HasAreaPropertyIntegrationTest",
  generics = list(
    area = interface_requirement(
      area_property_integration_test,
      returns = class_double
    )
  )
)
circle_property_generator <- gen_map(
  gen_integer(0L, 1000L),
  function(radius) {
    CirclePropertyIntegrationTest(radius = as.double(radius))
  }
)
non_negative_area_law <- new_law(
  "non-negative radii have non-negative area",
  generators = list(x = circle_property_generator),
  holds = function(x) {
    with(
      HasAreaPropertyIntegrationTest,
      area_property_integration_test(x)
    ) >= 0
  }
)

expect_law(non_negative_area_law, tests = 100L, seed = 20260902L)

combine_property_integration_test <- new_generic(
  "combine_property_integration_test",
  "x",
  function(x, y) S7_dispatch()
)
AdditivePropertyIntegrationTest <- new_trait(
  "AdditivePropertyIntegrationTest",
  methods = list(
    combine = trait_method(
      combine_property_integration_test,
      args = list(y = class_integer),
      returns = class_integer
    )
  )
)
impl_trait(
  AdditivePropertyIntegrationTest,
  class_integer,
  methods = list(combine = function(x, y) x + y)
)

small_integer_property_generator <- gen_integer(-100L, 100L)
associative_addition_law <- new_law(
  "integer addition is associative through the Additive trait",
  generators = list(
    x = small_integer_property_generator,
    y = small_integer_property_generator,
    z = small_integer_property_generator
  ),
  holds = function(x, y, z) {
    left <- trait_call(
      AdditivePropertyIntegrationTest,
      "combine",
      trait_call(AdditivePropertyIntegrationTest, "combine", x, y),
      z
    )
    right <- trait_call(
      AdditivePropertyIntegrationTest,
      "combine",
      x,
      trait_call(AdditivePropertyIntegrationTest, "combine", y, z)
    )
    identical(left, right)
  }
)

expect_law(associative_addition_law, tests = 100L, seed = 20260902L)
