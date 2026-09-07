# Interpolate within finite bounds without overflowing their difference.
.interpolate_double <- function(from, to, weight) {
  value <- if (sign(from) != sign(to)) {
    (1 - weight) * from + weight * to
  } else if (weight <= 0.5) {
    from + (to - from) * weight
  } else {
    to - (to - from) * (1 - weight)
  }
  min(max(from, to), max(min(from, to), value))
}

.double_rose <- function(value, origin) {
  .new_rose(value, function() {
    candidate <- origin
    finished <- value == origin
    function() {
      if (finished) return(NULL)
      child <- candidate
      candidate <<- .interpolate_double(child, value, 0.5)
      finished <<- candidate == child || candidate == value
      .double_rose(child, origin)
    }
  })
}

#' Generate finite double values
#'
#' At size zero, `gen_double()` draws only `origin`. The bounds expand linearly
#' from the origin to `min` and `max`, reaching the full interval at size 100.
#' Larger sizes use that same interval. Use [gen_resize()] to sample the full
#' range at every runner size. Within the current bounds, generation interpolates
#' one uniform draw from [stats::runif()]. This samples a finite-precision
#' approximation to a continuous uniform distribution, not all representable
#' doubles uniformly. Bounds are inclusive constraints; endpoints are not
#' guaranteed to be drawn. Use [gen_choice()] with constants to target them.
#'
#' Shrinking tries the origin, then the midpoint between the origin and the
#' generated value, then successive midpoints approaching that value. For
#' example, 8 with origin 0 has children 0, 4, 6, 7, 7.5, and so on. Each child
#' follows the same rule. Candidates remain between the origin and their parent;
#' iteration stops when rounding prevents further progress. Children are built
#' only when visited. Values close to zero can require many steps to shrink
#' through subnormal doubles, so the runner's evaluation budget still applies.
#'
#' `NA`, `NaN`, and infinities are excluded. Add them explicitly with
#' [gen_choice()] or [gen_element()]. Branch weights control generation and
#' branch order controls shrinking; zero-weight branches are excluded from both.
#'
#' @param min,max Finite scalar numeric bounds, with `min <= max`.
#' @param origin Finite scalar numeric shrink target within the bounds. `NULL`
#'   chooses zero when it is in range, otherwise the nearest bound.
#' @return An S7 generator with a double element prototype.
#' @references Haskell Hedgehog separates shrink origins from
#'   [size-dependent bounds](https://github.com/hedgehogqa/haskell-hedgehog/blob/master/hedgehog/src/Hedgehog/Internal/Range.hs)
#'   and uses [fractional shrinking toward an origin](https://github.com/hedgehogqa/haskell-hedgehog/blob/master/hedgehog/src/Hedgehog/Internal/Shrink.hs).
#'   The [R Hedgehog manual](https://hedgehogqa.r-universe.dev/hedgehog/doc/manual.html)
#'   documents `gen.unif()` and mixtures with exceptional numeric values.
#' @examples
#' measurements <- gen_double(-10, 10)
#' gen_example(measurements, size = 100L)
#'
#' # 80% finite draws; 5% each for NA, NaN, -Inf, and Inf.
#' numeric_values <- gen_choice(
#'   measurements, gen_element(c(NA_real_, NaN, -Inf, Inf)),
#'   prob = c(4, 1)
#' )
#' gen_example(gen_vector(numeric_values, max = 5L))
#' @export
gen_double <- function(min = -100, max = 100, origin = NULL) {
  bounds <- list(min = min, max = max)
  if (!is.null(origin)) bounds$origin <- origin
  for (arg in names(bounds)) {
    value <- bounds[[arg]]
    if (!is.numeric(value) || length(value) != 1L) {
      .abort("`%s` must be one number.", arg)
    }
    if (!is.finite(value) || is.complex(value)) {
      .abort("`%s` must be finite and real.", arg)
    }
  }
  min <- as.double(min)
  max <- as.double(max)
  if (min > max) .abort("`min` and `max` must be ordered bounds.")
  if (is.null(origin)) origin <- base::min(base::max(0, min), max)
  origin <- as.double(origin)
  if (origin < min || origin > max) .abort("`origin` must lie within the bounds.")

  s7_generator(
    draw = function(size) {
      scale <- base::min(size, 100L) / 100
      lower <- .interpolate_double(origin, min, scale)
      upper <- .interpolate_double(origin, max, scale)
      value <- if (lower == upper) lower else {
        .interpolate_double(lower, upper, stats::runif(1L))
      }
      .double_rose(value, origin)
    },
    label = sprintf("double[%g,%g] (origin %g)", min, max, origin),
    prototype = double()
  )
}
