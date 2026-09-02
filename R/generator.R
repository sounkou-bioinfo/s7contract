# Integrated generation and shrinking.

.new_rose <- function(value, children = function() list()) {
  structure(
    list(value = value, children = children),
    class = "s7contract_rose"
  )
}

.rose_children <- function(tree) {
  if (!inherits(tree, "s7contract_rose")) {
    .abort("A generator returned an invalid shrink tree.")
  }
  children <- tree$children()
  if (!is.list(children) || !all(vapply(
    children,
    inherits,
    logical(1),
    what = "s7contract_rose"
  ))) {
    .abort("A generator returned invalid shrink children.")
  }
  children
}

.unfold_rose <- function(value, shrink) {
  .new_rose(
    value,
    function() {
      candidates <- shrink(value)
      if (!is.list(candidates)) {
        .abort("A custom `shrink` function must return a list of values.")
      }
      lapply(candidates, .unfold_rose, shrink = shrink)
    }
  )
}

.map_rose <- function(tree, transform) {
  .new_rose(
    transform(tree$value),
    function() lapply(.rose_children(tree), .map_rose, transform = transform)
  )
}

.product_rose <- function(trees) {
  .new_rose(
    lapply(trees, `[[`, "value"),
    function() {
      out <- list()
      for (i in seq_along(trees)) {
        for (child in .rose_children(trees[[i]])) {
          candidate <- trees
          candidate[[i]] <- child
          out[[length(out) + 1L]] <- .product_rose(candidate)
        }
      }
      out
    }
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

.integer_rose <- function(value, target) {
  .new_rose(
    value,
    function() {
      lapply(
        .shrink_integer_values(value, target),
        .integer_rose,
        target = target
      )
    }
  )
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
      out <- list()
      smaller_lengths <- .shrink_integer_values(length(trees), min_length)
      for (n in smaller_lengths) {
        kept <- if (n == 0L) list() else trees[seq_len(n)]
        out[[length(out) + 1L]] <- .vector_rose(
          kept,
          prototype,
          min_length
        )
      }
      for (i in seq_along(trees)) {
        for (child in .rose_children(trees[[i]])) {
          candidate <- trees
          candidate[[i]] <- child
          out[[length(out) + 1L]] <- .vector_rose(
            candidate,
            prototype,
            min_length
          )
        }
      }
      out
    }
  )
}

#' Construct a property-based test generator
#'
#' A generator draws a value together with an integrated tree of smaller values.
#' `new_generator()` is the extension point for custom generators. Its `draw`
#' function receives a non-negative integer size and returns one value. Its
#' deterministic `shrink` function returns a list of strictly smaller values.
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
  bounds <- c(min, max)
  valid_bounds <- is.numeric(bounds) && length(bounds) == 2L &&
    !anyNA(bounds) && all(is.finite(bounds)) &&
    all(bounds == trunc(bounds)) &&
    min >= -.Machine$integer.max - 1 && max <= .Machine$integer.max &&
    min <= max
  if (!valid_bounds) {
    .abort("`min` and `max` must be ordered integer bounds.")
  }
  min <- as.integer(min)
  max <- as.integer(max)
  target <- if (min > 0L) min else if (max < 0L) max else 0L

  s7_generator(
    draw = function(size) {
      lower <- base::max(as.double(min), as.double(target) - size)
      upper <- base::min(as.double(max), as.double(target) + size)
      span <- upper - lower + 1
      value <- lower + sample.int(span, 1L) - 1
      .integer_rose(as.integer(value), target)
    },
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
  generator_names <- names(generators)
  valid_names <- length(generators) > 0L && !is.null(generator_names) &&
    !anyNA(generator_names) && all(nzchar(generator_names)) &&
    !anyDuplicated(generator_names)
  if (!valid_names) {
    .abort("`...` must contain one or more uniquely named generators.")
  }
  if (!all(vapply(generators, .is_generator, logical(1)))) {
    .abort("Every element of `...` must be a generator.")
  }
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
  lengths <- c(min, max)
  valid_lengths <- is.numeric(lengths) && length(lengths) == 2L &&
    !anyNA(lengths) && all(is.finite(lengths)) &&
    all(lengths == trunc(lengths)) && min >= 0 &&
    max <= .Machine$integer.max && min <= max
  if (!valid_lengths) {
    .abort("`min` and `max` must be ordered non-negative integer lengths.")
  }
  min <- as.integer(min)
  max <- as.integer(max)

  s7_generator(
    draw = function(size) {
      current_max <- base::min(as.double(max), as.double(min) + size)
      span <- current_max - as.double(min) + 1
      n <- as.integer(as.double(min) + sample.int(span, 1L) - 1)
      trees <- lapply(seq_len(n), function(i) element@draw(size))
      .vector_rose(trees, element@prototype, min)
    },
    label = sprintf("vector(%s)[%d,%d]", element@label, min, max),
    prototype = element@prototype
  )
}
