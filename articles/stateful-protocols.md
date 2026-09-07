# Testing Stateful S7 Protocols

``` r

library(S7)
library(s7contract)
```

A mutable store must behave correctly across calls: a put changes a
later get, a delete removes a key, and a reset clears earlier entries.
We can test these relationships by generating sequential commands and
comparing each operation with a reference model.

## The protocol and its implementations

This protocol stores one integer per string key and reports keys in
sorted order. The example uses bounded strings and small integer values.
Missing keys return `NULL`; generated get and delete commands use
existing keys.

``` r

store_put <- new_generic("store_put", "x", function(x, key, value) S7_dispatch())
store_get <- new_generic("store_get", "x", function(x, key) S7_dispatch())
store_delete <- new_generic("store_delete", "x", function(x, key) S7_dispatch())
store_reset <- new_generic("store_reset", "x")
store_keys <- new_generic("store_keys", "x")

KeyValue <- new_interface("KeyValue", generics = list(
  put = interface_requirement(store_put,
    args = list(key = class_character, value = class_integer)),
  get = interface_requirement(store_get, args = list(key = class_character)),
  delete = interface_requirement(store_delete, args = list(key = class_character)),
  reset = store_reset,
  keys = interface_requirement(store_keys, returns = class_character)
))
```

One implementation stores environment bindings; the other updates a list
held in an environment. Both use ordinary S7 methods.

``` r

EnvStore <- new_class("EnvStore", properties = list(data = class_environment))
method(store_put, EnvStore) <- function(x, key, value) {
  assign(key, value, envir = x@data)
  invisible(NULL)
}
method(store_get, EnvStore) <- function(x, key) {
  if (exists(key, envir = x@data, inherits = FALSE)) get(key, envir = x@data)
}
method(store_delete, EnvStore) <- function(x, key) {
  if (exists(key, envir = x@data, inherits = FALSE)) rm(list = key, envir = x@data)
  invisible(NULL)
}
method(store_reset, EnvStore) <- function(x) {
  rm(list = ls(x@data, all.names = TRUE), envir = x@data)
  invisible(NULL)
}
method(store_keys, EnvStore) <- function(x) sort(ls(x@data, all.names = TRUE))

ListStore <- new_class("ListStore", properties = list(data = class_environment))
method(store_put, ListStore) <- function(x, key, value) {
  x@data$values[key] <- list(value)
  invisible(NULL)
}
method(store_get, ListStore) <- function(x, key) x@data$values[[key]]
method(store_delete, ListStore) <- function(x, key) {
  x@data$values[key] <- NULL
  invisible(NULL)
}
method(store_reset, ListStore) <- function(x) {
  x@data$values <- list()
  invisible(NULL)
}
method(store_keys, ListStore) <- function(x) sort(as.character(names(x@data$values)))
```

## Commands and a reference model

Keys contain one to four code points from `a`, `b`, `c`, and `\u00e9`
(é). This recipe converts the alphabet to UTF-8, rejects missing and
byte strings, and counts code points rather than bytes or grapheme
clusters. The alphabet must be nonempty; each entry must contain exactly
one code point. With `min = 0`, the recipe also generates `""`, which
environment bindings cannot use as a key. Shrinking removes chunks
before moving characters toward earlier alphabet entries.

``` r

string_generator <- function(alphabet, min = 0L, max = 4L) {
  if (!is.character(alphabet) || anyNA(alphabet)) {
    stop("alphabet must contain non-missing characters")
  }
  if (any(Encoding(alphabet) == "bytes")) stop("byte strings are not supported")
  alphabet <- enc2utf8(alphabet)
  if (any(!validUTF8(alphabet))) stop("alphabet must be valid UTF-8")
  if (any(nchar(alphabet, type = "chars") != 1L)) {
    stop("each alphabet entry must be one Unicode code point")
  }
  gen_map(gen_vector(gen_element(alphabet), min, max),
          function(parts) paste0(parts, collapse = ""), prototype = character())
}
store_keys_generator <- string_generator(c("a", "b", "c", "\u00e9"), min = 1L)
```

[`paste()`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/paste.html)
preserves UTF-8 here and collapses zero entries to one empty string.
Missing values are rejected before concatenation because
[`paste()`](https://rdrr.io/r/base/paste.html) would turn them into the
literal text `"NA"`.

The model is a named list of expected values. Each command defines its
input generator, implementation call, and postcondition. A model update
derives the next state from the input. `get` and `delete` are available
only when a key exists; their preconditions also apply during shrinking.

``` r

existing_key <- function(state) {
  if (length(state) == 0L) return(NULL)
  gen_element(names(state))
}
store_commands <- list(
  new_command("put",
    generate = function(state) gen_product(
      key = store_keys_generator, value = gen_integer(-10L, 10L)),
    execute = function(fixture, input) with(KeyValue, {
      store_put(fixture, input$key, input$value)
      store_get(fixture, input$key)
    }),
    update = function(state, input, output) {
      state[input$key] <- list(input$value)
      state
    },
    ensure = function(state, input, output) identical(output, input$value)
  ),
  new_command("get",
    generate = existing_key,
    require = function(state, input) input %in% names(state),
    execute = function(fixture, input) with(KeyValue, store_get(fixture, input)),
    ensure = function(state, input, output) identical(output, state[[input]])
  ),
  new_command("delete",
    generate = existing_key,
    require = function(state, input) input %in% names(state),
    execute = function(fixture, input) with(KeyValue, {
      store_delete(fixture, input)
      store_keys(fixture)
    }),
    update = function(state, input, output) {
      state[input] <- NULL
      state
    },
    ensure = function(state, input, output) {
      identical(output, sort(setdiff(names(state), input)))
    }
  ),
  new_command("reset",
    generate = function(state) gen_constant(NULL),
    execute = function(fixture, input) with(KeyValue, {
      store_reset(fixture)
      store_keys(fixture)
    }),
    update = function(state, input, output) list(),
    ensure = function(state, input, output) identical(output, character())
  )
)
```

`ensure(state, input, output)` sees the model before the command and
returns a scalar logical. It runs after
[`update()`](https://rdrr.io/r/stats/update.html) has computed the next
model. The model’s expected values come from the inputs, independently
of the store’s answers.

## One law, two fixtures

[`new_state_law()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_state_law.md)
gives every case and evaluated shrink a fresh fixture. Teardown runs
once after each successful setup, including after a false postcondition,
warning, or error.

``` r

store_law <- function(make) {
  new_state_law("key/value operations follow the model", list(), store_commands,
    setup = function() {
      fixture <- make()
      assert_implements(fixture, KeyValue)
      fixture
    },
    teardown = function(fixture) {
      rm(list = ls(fixture@data, all.names = TRUE), envir = fixture@data)
    },
    max_commands = 12L,
    classify = function(sequence) {
      puts <- Filter(function(step) step$command == "put", sequence)
      keys <- vapply(puts, function(step) step$input$key, character(1))
      c(if (any(nchar(keys, type = "chars") > 1L)) "multi_character",
        if (any(grepl("\u00e9", keys, fixed = TRUE))) "non_ascii")
    },
    min_coverage = c(multi_character = 0.3, non_ascii = 0.2)
  )
}
stores <- list(
  environment = function() EnvStore(data = new.env(parent = emptyenv())),
  list = function() ListStore(data = list2env(list(values = list()), parent = emptyenv()))
)
store_results <- lapply(stores, function(make) check_law(store_law(make), tests = 100L, seed = 1L))
vapply(store_results, function(result) result@status, character(1))
#> environment        list 
#>    "passed"    "passed"
```

Sequence length grows with the runner’s size, up to `max_commands`.
Generation may stop sooner when no command is available. Empty sequences
are allowed.
[`expect_law()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_law.md)
can run the same law as one tinytest expectation.

The classifier records sequences containing multi-character and
non-ASCII put keys. It counts generated inputs, including any suffix
after an execution failure.

``` r

store_results$environment@coverage
#>             label count proportion minimum  met
#> 1 multi_character    78       0.78     0.3 TRUE
#> 2       non_ascii    69       0.69     0.2 TRUE
```

## A put that truncates keys

This implementation keeps only the first character when writing a key.
The law reduces its failure to `put("aa", 0L)`: a subsequent get of
`"aa"` returns `NULL`.

``` r

TruncatedStore <- new_class("TruncatedStore", parent = EnvStore)
method(store_put, TruncatedStore) <- function(x, key, value) {
  assign(substr(key, 1L, 1L), value, envir = x@data)
  invisible(NULL)
}
truncated_failure <- check_law(
  store_law(function() TruncatedStore(data = new.env(parent = emptyenv()))),
  tests = 100L, shrinks = 200L, seed = 1L
)
truncated_failure
#> Law 'key/value operations follow the model' was falsified after 3 attempts and 3 shrinks (seed 1).
#> Step 1 'put' (ensure): postcondition returned FALSE
#> List of 3
#>  $ model : list()
#>  $ input :List of 2
#>   ..$ key  : chr "aa"
#>   ..$ value: int 0
#>  $ output: NULL
#> Shrinking stopped: no child of this counterexample preserves the failure.
#> Smallest counterexample found:
#> List of 1
#>  $ sequence:List of 1
#>   ..$ :List of 3
#>   .. ..$ id     : int 1
#>   .. ..$ command: chr "put"
#>   .. ..$ input  :List of 2
#> Case coverage (3 accepted cases; partial run):
#>   "non_ascii": 0/3 (0%; minimum 20% unmet)
#>   "multi_character": 1/3 (33.3%; minimum 30%)
```

## A reset that leaves data behind

This subclass has every required method, but reset does nothing:

``` r

StickyStore <- new_class("StickyStore", parent = EnvStore)
method(store_reset, StickyStore) <- function(x) invisible(NULL)
implements(StickyStore, KeyValue)
#> [1] TRUE

store_failure <- check_law(
  store_law(function() StickyStore(data = new.env(parent = emptyenv()))),
  tests = 100L, shrinks = 200L, seed = 1L
)
store_failure
#> Law 'key/value operations follow the model' was falsified after 6 attempts and 5 shrinks (seed 1).
#> Step 2 'reset' (ensure): postcondition returned FALSE
#> List of 3
#>  $ model :List of 1
#>   ..$ a: int 0
#>  $ input : NULL
#>  $ output: chr "a"
#> Shrinking stopped: no child of this counterexample preserves the failure.
#> Smallest counterexample found:
#> List of 1
#>  $ sequence:List of 2
#>   ..$ :List of 3
#>   .. ..$ id     : int 2
#>   .. ..$ command: chr "put"
#>   .. ..$ input  :List of 2
#>   ..$ :List of 3
#>   .. ..$ id     : int 3
#>   .. ..$ command: chr "reset"
#>   .. ..$ input  : NULL
#> Case coverage (6 accepted cases; partial run):
#>   "non_ascii": 1/6 (16.7%; minimum 20% unmet)
#>   "multi_character": 2/6 (33.3%; minimum 30%)
```

The runner reduces the failure to a put followed by a reset. It first
removes chunks of commands, then shrinks their inputs. After each
change, it removes commands whose preconditions no longer hold, along
with their dependents. The search retains failures of the same command’s
postcondition; the result is minimal relative to the shrink tree and
evaluation budget.

The failure condition retains the resolved inputs, outputs, and model
states:

``` r

store_failure@counterexample@condition$trace
#> [[1]]
#> [[1]]$id
#> [1] 2
#> 
#> [[1]]$command
#> [1] "put"
#> 
#> [[1]]$input
#> [[1]]$input$key
#> [1] "a"
#> 
#> [[1]]$input$value
#> [1] 0
#> 
#> 
#> [[1]]$output
#> [1] 0
#> 
#> [[1]]$before
#> list()
#> 
#> [[1]]$after
#> [[1]]$after$a
#> [1] 0
#> 
#> 
#> 
#> [[2]]
#> [[2]]$id
#> [1] 3
#> 
#> [[2]]$command
#> [1] "reset"
#> 
#> [[2]]$input
#> NULL
#> 
#> [[2]]$output
#> [1] "a"
#> 
#> [[2]]$before
#> [[2]]$before$a
#> [1] 0
#> 
#> 
#> [[2]]$after
#> list()
```

The original trace is in
`store_failure@counterexample@original_condition$trace`. Unexpected
callback errors stop shrinking and preserve an established
counterexample. If teardown also fails, its condition is retained
separately as `cleanup_condition`. `shrink_condition` reports why the
search stopped.

``` r

store_replayed <- do.call(check_law, c(list(law = store_failure@law), store_failure@parameters))
identical(store_replayed@counterexample@minimal, store_failure@counterexample@minimal)
#> [1] TRUE
```

Replay requires unchanged commands, generators, run parameters, and
compatible R/package versions. Setup must reproduce the initial state;
shared mutable state outside the fixture would break that guarantee. The
runner restores the caller’s RNG state as described in [Generative Laws
with
tinytest](https://sounkou-bioinfo.github.io/s7contract/articles/property-laws.md).

## Commands that return handles

Some protocols allocate a handle that later commands consume. During
generation, `update(state, input, output)` receives an opaque reference
to the future output. It can append that reference to a list of live
handles, and later generators can select one with
[`gen_element()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_element.md).
During execution, the same update receives the actual handle.

References resolve when passed directly as command inputs or nested in
ordinary lists. They retain the producer’s ID when earlier commands are
removed. Removing a producer also removes consumers of its output.
References inside classed containers are not traversed; place them in a
plain list before execution. The update must work with both symbolic and
concrete outputs, without inspecting their representation. Models need
value semantics; traces containing mutable handles retain R’s reference
semantics.

The design follows [R Hedgehog’s state-machine
example](https://github.com/hedgehogqa/r-hedgehog/blob/master/vignettes/state-machines.Rmd)
and [Haskell Hedgehog’s
commands](https://github.com/hedgehogqa/haskell-hedgehog/blob/master/hedgehog/src/Hedgehog/Internal/State.hs).
For the model-based testing background, see [Hughes
(2016)](https://research.chalmers.se/publication/232550). This runner
executes sequential commands; it does not test concurrent histories.
