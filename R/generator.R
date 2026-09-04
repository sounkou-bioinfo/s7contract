# Integrated generation and shrinking.

.new_rose <- function(value, children = function() function() NULL) {
  structure(
    list(value = value, children = children),
    class = "s7contract_rose"
  )
}

.rose_children <- function(tree) {
  if (!inherits(tree, "s7contract_rose")) {
    .abort("A generator returned an invalid shrink tree.")
  }
  next_child <- tree$children()
  if (!is.function(next_child)) {
    .abort("A generator returned an invalid shrink iterator.")
  }
  function() {
    child <- next_child()
    if (!is.null(child) && !inherits(child, "s7contract_rose")) {
      .abort("A generator returned an invalid shrink child.")
    }
    child
  }
}

.unfold_rose <- function(value, shrink) {
  .new_rose(
    value,
    function() {
      candidates <- shrink(value)
      if (!is.list(candidates)) {
        .abort("A custom `shrink` function must return a list of values.")
      }
      i <- 0L
      function() {
        if (i >= length(candidates)) {
          return(NULL)
        }
        i <<- i + 1L
        .unfold_rose(candidates[[i]], shrink)
      }
    }
  )
}

.map_rose <- function(tree, transform) {
  .new_rose(
    transform(tree$value),
    function() {
      next_child <- .rose_children(tree)
      function() {
        child <- next_child()
        if (is.null(child)) {
          return(NULL)
        }
        .map_rose(child, transform)
      }
    }
  )
}

# Products and vectors share ordered, lazy replacement of one component.
.component_children <- function(trees, assemble) {
  i <- 1L
  next_child <- NULL
  function() {
    while (i <= length(trees)) {
      if (is.null(next_child)) {
        next_child <<- .rose_children(trees[[i]])
      }
      child <- next_child()
      if (!is.null(child)) {
        candidate <- trees
        candidate[[i]] <- child
        return(assemble(candidate))
      }
      i <<- i + 1L
      next_child <<- NULL
    }
    NULL
  }
}

.product_rose <- function(trees) {
  .new_rose(
    lapply(trees, `[[`, "value"),
    function() .component_children(trees, .product_rose)
  )
}

.shrink_integer_values <- function(value, target) {
  distance <- as.double(value) - as.double(target)
  if (distance == 0) {
    return(integer())
  }
  offsets <- floor(abs(distance) / 2^(0:floor(log2(abs(distance)))))
  candidates <- as.double(value) - sign(distance) * unique(offsets)
  as.integer(unique(candidates[candidates != value]))
}

.vector_rose <- function(trees, prototype, min_length) {
  values <- lapply(trees, `[[`, "value")
  value <- if (is.atomic(prototype)) {
    valid_elements <- vapply(
      values,
      function(value) {
        is.atomic(value) && length(value) == 1L &&
          identical(typeof(value), typeof(prototype))
      },
      logical(1)
    )
    if (!all(valid_elements)) {
      .abort(paste(
        "An element generator with an atomic prototype must draw",
        "scalar values of the prototype's type."
      ))
    }
    do.call(c, c(list(prototype), values))
  } else {
    values
  }
  .new_rose(
    value,
    function() {
      width <- length(trees) - min_length
      start <- 1L
      elements <- NULL
      function() {
        while (width > 0L) {
          if (start + width - 1L <= length(trees)) {
            kept <- trees[-seq.int(start, length.out = width)]
            start <<- start + width
            return(.vector_rose(kept, prototype, min_length))
          }
          width <<- width %/% 2L
          start <<- 1L
        }
        if (is.null(elements)) {
          elements <<- .component_children(
            trees,
            function(candidate) .vector_rose(candidate, prototype, min_length)
          )
        }
        elements()
      }
    }
  )
}

#' Construct a property-based test generator
#'
#' A generator draws a value together with an integrated tree of smaller values.
#' `new_generator()` is the extension point for custom generators. Its `draw`
#' function receives a non-negative integer size and returns one value. Its
#' deterministic `shrink` function returns a list of strictly smaller values.
#' The custom shrinker constructs that list itself; the framework constructs
#' and transforms the corresponding tree nodes only as they are visited.
#'
#' @param draw Function of one `size` argument that returns a value.
#' @param shrink Function of one generated value that returns a list of smaller
#'   values.
#' @param label Short description used in diagnostics.
#' @param prototype Zero-length prototype used by [gen_vector()].
#' @return An S7 generator object.
#' @export
new_generator <- function(
  draw,
  shrink = function(value) list(),
  label = "custom",
  prototype = list()
) {
  if (!is.function(draw)) {
    .abort("`draw` must be a function.")
  }
  if (!is.function(shrink)) {
    .abort("`shrink` must be a function.")
  }
  s7_generator(
    draw = function(size) .unfold_rose(draw(size), shrink),
    label = label,
    prototype = prototype
  )
}

#' Basic property-based test generators
#'
#' These generators carry their own deterministic shrink trees. Integer ranges
#' and vector lengths expand with the runner's size. Integer values shrink
#' toward zero when zero is within bounds, or toward the nearest bound. Product
#' generators shrink one component at a time in argument order. Vector
#' generators return an atomic vector only when the element prototype is atomic;
#' those element draws must be scalar and match the prototype's storage type.
#' Otherwise, vector generators return a list with one entry per element draw.
#' Nested vector generators therefore return lists of vectors. Vector shrinking
#' removes contiguous chunks, then shrinks individual elements, preserving the
#' minimum length.
#'
#' @param value Constant value to generate.
#' @param min,max Inclusive integer bounds. For `gen_vector()`, bounds on vector
#'   length.
#' @param generator,element A generator.
#' @param transform Function applied to generated values.
#' @param prototype Zero-length prototype of mapped values.
#' @param ... Uniquely named generators.
#' @return An S7 generator object.
#' @examples
#' pairs <- gen_product(x = gen_integer(), y = gen_integer())
#' vectors <- gen_vector(gen_integer(), min = 0L, max = 8L)
#' @export
gen_constant <- function(value) {
  prototype <- if (is.atomic(value) && length(value) == 1L) value[0] else list()
  s7_generator(
    draw = function(size) .new_rose(value),
    label = "constant",
    prototype = prototype
  )
}

#' @rdname gen_constant
#' @export
gen_integer <- function(min = -100L, max = 100L) {
  min <- .count_arg(min, "min", signed = TRUE)
  max <- .count_arg(max, "max", signed = TRUE)
  if (min > max) {
    .abort("`min` and `max` must be ordered integer bounds.")
  }
  target <- if (min > 0L) min else if (max < 0L) max else 0L

  new_generator(
    draw = function(size) {
      lower <- base::max(as.double(min), as.double(target) - size)
      upper <- base::min(as.double(max), as.double(target) + size)
      span <- upper - lower + 1
      value <- lower + sample.int(span, 1L) - 1
      as.integer(value)
    },
    shrink = function(value) as.list(.shrink_integer_values(value, target)),
    label = sprintf("integer[%d,%d]", min, max),
    prototype = integer()
  )
}

#' @rdname gen_constant
#' @export
gen_map <- function(generator, transform, prototype = list()) {
  if (!.is_generator(generator)) {
    .abort("`generator` must be a generator.")
  }
  if (!is.function(transform)) {
    .abort("`transform` must be a function.")
  }
  s7_generator(
    draw = function(size) .map_rose(generator@draw(size), transform),
    label = sprintf("map(%s)", generator@label),
    prototype = prototype
  )
}

#' @rdname gen_constant
#' @export
gen_product <- function(...) {
  generators <- list(...)
  problem <- .generator_list_error(generators)
  if (!is.null(problem)) {
    .abort("`...`: %s", problem)
  }
  generator_names <- names(generators)
  s7_generator(
    draw = function(size) {
      trees <- lapply(generators, function(generator) generator@draw(size))
      names(trees) <- generator_names
      .product_rose(trees)
    },
    label = sprintf("product(%s)", paste(generator_names, collapse = ",")),
    prototype = list()
  )
}

#' @rdname gen_constant
#' @export
gen_vector <- function(element, min = 0L, max = 10L) {
  if (!.is_generator(element)) {
    .abort("`element` must be a generator.")
  }
  min <- .count_arg(min, "min")
  max <- .count_arg(max, "max")
  if (min > max) {
    .abort("`min` and `max` must be ordered non-negative integer lengths.")
  }

  s7_generator(
    draw = function(size) {
      current_max <- base::min(as.double(max), as.double(min) + size)
      span <- current_max - as.double(min) + 1
      n <- as.integer(as.double(min) + sample.int(span, 1L) - 1)
      trees <- lapply(seq_len(n), function(i) element@draw(size))
      .vector_rose(trees, element@prototype, min)
    },
    label = sprintf("vector(%s)[%d,%d]", element@label, min, max),
    prototype = list()
  )
}
