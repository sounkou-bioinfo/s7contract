# Shrink positions lexicographically, swapping when a target is already selected.
.sample_rose <- function(positions) {
  .new_rose(positions, function() {
    i <- 0L
    candidates <- integer()
    cursor <- 1L
    function() {
      while (cursor > length(candidates)) {
        i <<- i + 1L
        if (i > length(positions)) return(NULL)
        prefix <- sort(positions[seq_len(i - 1L)])
        target <- which(c(prefix, 0L) != seq_len(i))[1L]
        candidates <<- .shrink_integer_values(positions[i], target)
        candidates <<- candidates[!candidates %in% prefix]
        cursor <<- 1L
      }
      candidate <- positions
      replacement <- candidates[cursor]
      cursor <<- cursor + 1L
      other <- match(replacement, positions, nomatch = 0L)
      if (other > 0L) candidate[other] <- positions[i]
      candidate[i] <- replacement
      .sample_rose(candidate)
    }
  })
}

# Samples and subsequences share source and cardinality admission.
.sample_size <- function(values, size) {
  if (!is.atomic(values) && !is.list(values) && !is.null(values)) {
    .abort("`values` must be an atomic vector or list.")
  }
  if (!is.null(dim(values)) || is.data.frame(values)) {
    .abort("`values` must be one-dimensional, not an array or data frame.")
  }
  n <- .count_arg(length(values), "length(values)")
  size <- .count_arg(size, "size")
  if (size > n) .abort("Selection length must not exceed the number of source positions.")
  size
}

#' Sample source positions without replacement
#'
#' `gen_sample()` draws a uniform ordered sample of exactly `size` source
#' positions, without replacement. Its cardinality and sampling range do not
#' depend on the runner's size. The default draws permutations of the source.
#' Shrinking moves positions toward the beginning of the source, from left to
#' right. Each position tries the earliest position unused by its prefix, then
#' integer bisections toward its current position. Targets already in the prefix
#' are skipped; targets used later in the sample are swapped. Every child is
#' lexicographically smaller, with the same cardinality and distinct positions.
#' The terminal sample consists of the first `size` source entries in order.
#'
#' `gen_subsequence()` chooses a length uniformly between `min` and
#' `min(max, min + runner_size)`, then samples that many positions and sorts
#' them. It shrinks length toward `min` first, rebuilding a sample with a captured
#' seed as in [gen_bind()], then shrinks positions. Source order and length
#' bounds are preserved. Different position shrinks can yield the same sorted
#' subsequence. Setting `min = max = length(values)` yields a constant.
#'
#' Uniqueness concerns positions, not values: duplicated source entries can
#' appear together. Subsetting with `[` preserves names and supported classes;
#' values themselves are not shrunk. Empty sources allow only empty selections.
#' Both generators have a list prototype, so [gen_vector()] nests their results.
#' For sampling with replacement, compose [gen_element()] and [gen_vector()].
#'
#' @param values An atomic vector or list, optionally named.
#'   `NULL` is also accepted as an empty source. Classed vectors such as dates
#'   and factors must support `length()` and integer `[` subsetting
#'   that preserves their class and returns one entry per position. Arrays and
#'   data frames are not supported. Length must be at most `.Machine$integer.max`.
#' @param size Fixed non-negative sample cardinality, at most `length(values)`.
#' @param min,max Inclusive non-negative subsequence length bounds, with
#'   `min <= max <= length(values)`.
#' @return An S7 generator.
#' @references [R's sampling documentation](https://stat.ethz.ch/R-manual/R-devel/library/base/html/sample.html)
#'   describes positional sampling and the hash algorithm used for small samples
#'   from large populations. These generators use `sample.int()` without
#'   constructing a vector of every source position.
#'   The [R Hedgehog manual](https://hedgehogqa.r-universe.dev/hedgehog/doc/manual.html)
#'   documents subsequences and sampling as distinct generator domains.
#' @examples
#' gen_example(gen_sample(letters, size = 3L))
#' gen_example(gen_subsequence(letters, max = 5L))
#' gen_example(gen_sample(seq_len(100000000L), size = 3L))
#' @export
gen_sample <- function(values, size = length(values)) {
  cardinality <- .sample_size(values, size)
  n <- length(values)
  s7_generator(
    draw = function(size) {
      .map_rose(.sample_rose(sample.int(n, cardinality)), function(i) values[i])
    },
    label = sprintf("sample[%d of %d]", cardinality, n),
    prototype = list()
  )
}

#' @rdname gen_sample
#' @export
gen_subsequence <- function(values, min = 0L, max = length(values)) {
  max <- .sample_size(values, max)
  min <- .count_arg(min, "min")
  if (min > max) .abort("`min` and `max` must be ordered length bounds.")
  n <- length(values)
  gen_bind(gen_integer(min, max), function(k) {
    if (k == n) return(gen_constant(values))
    gen_map(gen_sample(seq_len(n), size = k), function(i) values[sort(i)])
  })
}
