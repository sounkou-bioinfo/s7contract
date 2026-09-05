## ---- vector-law-setup
library(S7)
library(s7contract)

## ---- vector-interface
vec_length <- new_generic("vec_length", "x")
vec_slice <- new_generic("vec_slice", "x", function(x, i) S7_dispatch())
vec_values <- new_generic("vec_values", "x")

VectorLike <- new_interface(
  "VectorLike",
  generics = list(
    length = interface_requirement(vec_length, returns = class_integer),
    slice = interface_requirement(vec_slice, args = list(i = class_integer)),
    values = interface_requirement(vec_values, returns = class_double)
  )
)

ReadDepth <- new_class(
  "ReadDepth",
  properties = list(position = class_integer, depth = class_double),
  validator = function(self) {
    if (length(self@position) != length(self@depth)) {
      "@position and @depth must have the same length"
    }
  }
)

method(vec_length, ReadDepth) <- function(x) length(x@depth)
method(vec_slice, ReadDepth) <- function(x, i) {
  ReadDepth(position = x@position[i], depth = x@depth[i])
}
method(vec_values, ReadDepth) <- function(x) x@depth

method(vec_length, class_double) <- function(x) length(x)
method(vec_slice, class_double) <- function(x, i) x[i]
method(vec_values, class_double) <- function(x) x

coverage <- ReadDepth(position = 1:5, depth = c(12, 15, 9, 20, 17))
implements(coverage, VectorLike)
implements(class_double, VectorLike)

## ---- vector-consumer
window_mean <- function(x, i) {
  assert_implements(x, VectorLike)
  with(VectorLike, mean(vec_values(vec_slice(x, i))))
}

window_mean(coverage, 2:4)
window_mean(c(12, 15, 9, 20, 17), 2:4)

## ---- vector-law-suite
vector_laws <- function(make) {
  values <- gen_map(gen_vector(gen_integer(-10L, 10L), max = 6L), as.double)
  cases <- gen_bind(values, function(values) {
    indices <- if (length(values) == 0L) {
      gen_constant(integer())
    } else {
      gen_vector(gen_element(seq_along(values)), max = 6L)
    }
    gen_product(
      x = gen_constant(make(values)),
      values = gen_constant(values),
      i = indices
    )
  })

  list(
    values = new_law("values preserve constructor input", list(input = cases),
      function(input) with(VectorLike, {
        identical(vec_values(input$x), input$values)
      })),
    length = new_law("length agrees with constructor input", list(input = cases),
      function(input) with(VectorLike, {
        identical(vec_length(input$x), base::length(input$values))
      })),
    slice_values = new_law("slicing preserves selected values and order", list(input = cases),
      function(input) with(VectorLike, {
        identical(vec_values(vec_slice(input$x, input$i)), input$values[input$i])
      })),
    slice_length = new_law("slice length matches the index count", list(input = cases),
      function(input) with(VectorLike, {
        identical(vec_length(vec_slice(input$x, input$i)), base::length(input$i))
      }))
  )
}

## ---- vector-law-implementations
implementations <- list(
  numeric = identity,
  read_depth = function(values) ReadDepth(position = seq_along(values), depth = values)
)
vector_results <- lapply(implementations, function(make) {
  lapply(vector_laws(make), check_law, tests = 100L, seed = 1L)
})
sapply(vector_results, function(results) {
  vapply(results, function(result) result@status, character(1))
})

## ---- vector-law-broken
ReversedDepth <- new_class("ReversedDepth", parent = ReadDepth)
method(vec_slice, ReversedDepth) <- function(x, i) {
  ReadDepth(position = x@position[rev(i)], depth = x@depth[rev(i)])
}

implements(ReversedDepth, VectorLike)
broken_results <- lapply(
  vector_laws(function(values) ReversedDepth(position = seq_along(values), depth = values)),
  check_law, tests = 100L, seed = 1L
)
vapply(broken_results, function(result) result@status, character(1))

## ---- vector-law-counterexample
failure <- broken_results$slice_values
failure
example <- failure@counterexample@minimal$input
example$values
example$i
with(VectorLike, vec_values(vec_slice(example$x, example$i)))
example$values[example$i]

## ---- vector-law-replay
replayed <- do.call(check_law, c(list(law = failure@law), failure@parameters))
identical(replayed@counterexample@minimal, failure@counterexample@minimal)
