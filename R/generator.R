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

# Shrink the source first, rebuilding the dependent tree, then its result.
.bind_rose <- function(tree, bind) {
  result <- bind(tree$value)
  .new_rose(
    result$value,
    function() {
      source <- .rose_children(tree)
      target <- NULL
      function() {
        if (!is.null(source)) {
          child <- source()
          if (!is.null(child)) {
            return(.bind_rose(child, bind))
          }
          source <<- NULL
          target <<- .rose_children(result)
        }
        target()
      }
    }
  )
}

# Generator-producing callbacks share this result contract.
.factory_tree <- function(generator, size) {
  if (!.is_generator(generator)) {
    .abort("A generator factory must return a generator.")
  }
  generator@draw(size)
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
    function() .sequence_children(trees, min_length, function(candidate) {
      .vector_rose(candidate, prototype, min_length)
    })
  )
}

# Vectors and command sequences remove chunks before shrinking elements.
.sequence_children <- function(trees, min_length, assemble) {
  width <- length(trees) - min_length
  start <- 1L
  elements <- NULL
  function() {
    while (width > 0L) {
      if (start + width - 1L <= length(trees)) {
        kept <- trees[-seq.int(start, length.out = width)]
        start <<- start + width
        return(assemble(kept))
      }
      width <<- width %/% 2L
      start <<- 1L
    }
    if (is.null(elements)) {
      elements <<- .component_children(trees, assemble)
    }
    elements()
  }
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

#' Compose dependent, sized, and recursive generators
#'
#' `gen_bind()` passes a generated value to `bind`, which constructs the next
#' generator. Shrinking first rebuilds that generator for smaller source values,
#' then shrinks its output. Each rebuild uses the same locally captured seed and
#' size, so random downstream draws remain reproducible regardless of shrink
#' traversal or random draws made by a law. The surrounding RNG state is restored
#' after each rebuild. Callbacks must not depend on external mutable state or
#' change the RNG configuration.
#'
#' `gen_sized()` passes the current size to a generator factory. `gen_resize()`
#' fixes the size used by one generator without changing its siblings.
#'
#' `gen_recursive()` chooses between `base` and the generator returned by
#' `expand(child)`. The supplied child recursively uses half the current size,
#' rounded down; size zero draws only from `base`. Shrinking tries the base
#' branch before shrinking the expanded value. Recursion through `child`
#' therefore terminates, provided callbacks themselves terminate and do not
#' introduce other recursion. Size bounds recursion depth, not total node count.
#'
#' @param generator,base A generator. `base` produces non-recursive values.
#' @param bind Function of one generated value returning a generator.
#' @param factory Function of one non-negative integer size returning a generator.
#' @param expand Function accepting a child generator and returning a generator
#'   for one recursive layer.
#' @param size Non-negative integer size to use for every draw.
#' @param prototype Zero-length prototype of generated values, used when this
#'   generator is an element of [gen_vector()]. Defaults to list elements.
#' @return An S7 generator object.
#' @examples
#' sized_vectors <- gen_bind(gen_integer(1L, 8L), function(n) {
#'   gen_product(n = gen_constant(n), x = gen_vector(gen_integer(), n, n))
#' })
#' gen_example(sized_vectors)
#'
#' trees <- gen_recursive(gen_constant(0L), function(child) {
#'   gen_product(left = child, right = child)
#' })
#' gen_example(trees, size = 4L)
#' @export
gen_bind <- function(generator, bind, prototype = list()) {
  if (!.is_generator(generator)) {
    .abort("`generator` must be a generator.")
  }
  if (!is.function(bind)) {
    .abort("`bind` must be a function.")
  }
  s7_generator(
    draw = function(size) {
      tree <- generator@draw(size)
      seed <- sample.int(.Machine$integer.max, 1L)
      rng_kind <- RNGkind()
      .bind_rose(tree, function(value) {
        .with_seed(seed, rng_kind, .factory_tree(bind(value), size))
      })
    },
    label = sprintf("bind(%s)", generator@label),
    prototype = prototype
  )
}

#' @rdname gen_bind
#' @export
gen_sized <- function(factory, prototype = list()) {
  if (!is.function(factory)) {
    .abort("`factory` must be a function.")
  }
  s7_generator(
    draw = function(size) .factory_tree(factory(size), size),
    label = "sized",
    prototype = prototype
  )
}

#' @rdname gen_bind
#' @export
gen_resize <- function(generator, size) {
  if (!.is_generator(generator)) {
    .abort("`generator` must be a generator.")
  }
  size <- .count_arg(size, "size")
  s7_generator(
    draw = function(ignored_size) generator@draw(size),
    label = sprintf("resize(%s,%d)", generator@label, size),
    prototype = generator@prototype
  )
}

#' @rdname gen_bind
#' @export
gen_recursive <- function(base, expand, prototype = list()) {
  if (!.is_generator(base)) {
    .abort("`base` must be a generator.")
  }
  if (!is.function(expand)) {
    .abort("`expand` must be a function.")
  }
  generator <- gen_sized(function(size) {
    if (size == 0L) {
      return(base)
    }
    child <- gen_resize(generator, size %/% 2L)
    gen_choice(base, gen_sized(function(size) expand(child), prototype))
  }, prototype)
  generator
}

#' Choose values or generators
#'
#' `gen_element()` samples one element of a vector or list. `gen_choice()`
#' samples a generator and draws from it at the current size. All choices are
#' available at size zero. Both shrink toward earlier entries, then
#' `gen_choice()` shrinks within the selected generator. Zero-weight entries
#' are excluded from both generation and shrinking.
#'
#' @param values Non-empty atomic vector or list of values.
#' @param ... One or more generators, ordered from simpler to more complex.
#' @param prob Optional finite non-negative sampling weights, one per entry,
#'   with at least one positive weight.
#' @return An S7 generator object. `gen_element()` preserves an atomic input's
#'   element prototype; list inputs use a list prototype. `gen_choice()` retains
#'   a common prototype when all branches agree, otherwise it uses list elements.
#' @examples
#' bases <- gen_element(c("A", "C", "G", "T"))
#' nullable <- gen_choice(gen_constant(NA_integer_), gen_integer(), prob = c(1, 9))
#' gen_example(gen_vector(bases, min = 4L, max = 4L))
#' @export
gen_element <- function(values, prob = NULL) {
  if (!is.atomic(values) && !is.list(values)) {
    .abort("`values` must be an atomic vector or list.")
  }
  if (length(values) == 0L) {
    .abort("`values` must not be empty.")
  }
  if (!is.null(prob)) {
    if (!is.numeric(prob) || length(prob) != length(values)) {
      .abort("`prob` must have one numeric weight per entry.")
    }
    if (anyNA(prob) || any(!is.finite(prob)) || any(prob < 0)) {
      .abort("`prob` must contain finite non-negative weights.")
    }
    if (!any(prob > 0)) {
      .abort("`prob` must contain a positive weight.")
    }
    values <- values[prob > 0]
    prob <- prob[prob > 0]
    prob <- prob / max(prob)
  }
  indices <- new_generator(
    draw = function(size) sample.int(length(values), 1L, prob = prob),
    shrink = function(index) as.list(.shrink_integer_values(index, 1L)),
    label = "element index",
    prototype = integer()
  )
  gen_map(indices, function(index) values[[index]],
          prototype = if (is.atomic(values)) values[0] else list())
}

#' @rdname gen_element
#' @export
gen_choice <- function(..., prob = NULL) {
  generators <- list(...)
  if (length(generators) == 0L ||
      !all(vapply(generators, .is_generator, logical(1)))) {
    .abort("`...` must contain one or more generators.")
  }
  prototype <- generators[[1L]]@prototype
  shared <- vapply(generators, function(g) identical(g@prototype, prototype), logical(1))
  if (!all(shared)) {
    prototype <- list()
  }
  gen_bind(gen_element(seq_along(generators), prob),
           function(index) generators[[index]], prototype)
}

#' Inspect a generator or disable its shrinking
#'
#' `gen_example()` draws one value at a fixed size with a local seed, restoring
#' the caller's RNG kind and state on exit, as [check_law()] does. It does not
#' expand the shrink tree. `gen_no_shrink()` keeps generation unchanged but
#' removes all shrink candidates, including those carried by composed generators.
#'
#' @param generator A generator.
#' @param size Non-negative integer size.
#' @param seed Integer seed for reproducible generation.
#' @return `gen_example()` returns one generated value. `gen_no_shrink()` returns
#'   an S7 generator object with the original element prototype.
#' @examples
#' gen_example(gen_vector(gen_integer()), size = 5L, seed = 42L)
#' @export
gen_example <- function(generator, size = 10L, seed = 1L) {
  if (!.is_generator(generator)) {
    .abort("`generator` must be a generator.")
  }
  size <- .count_arg(size, "size")
  seed <- .count_arg(seed, "seed", signed = TRUE)
  .with_seed(seed, c("Mersenne-Twister", "Inversion", "Rejection"),
             generator@draw(size)$value)
}

#' @rdname gen_example
#' @export
gen_no_shrink <- function(generator) {
  if (!.is_generator(generator)) {
    .abort("`generator` must be a generator.")
  }
  s7_generator(
    draw = function(size) .new_rose(generator@draw(size)$value),
    label = sprintf("no shrink(%s)", generator@label),
    prototype = generator@prototype
  )
}
