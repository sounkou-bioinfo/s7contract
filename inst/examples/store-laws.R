## ---- store-setup
library(S7)
library(s7contract)

## ---- store-interface
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

## ---- store-implementations
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

## ---- store-strings
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

## ---- store-commands
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

## ---- store-law
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

## ---- store-broken
StickyStore <- new_class("StickyStore", parent = EnvStore)
method(store_reset, StickyStore) <- function(x) invisible(NULL)
implements(StickyStore, KeyValue)

store_failure <- check_law(
  store_law(function() StickyStore(data = new.env(parent = emptyenv()))),
  tests = 100L, shrinks = 200L, seed = 1L
)
store_failure

## ---- store-trace
store_failure@counterexample@condition$trace

## ---- store-replay
store_replayed <- do.call(check_law, c(list(law = store_failure@law), store_failure@parameters))
identical(store_replayed@counterexample@minimal, store_failure@counterexample@minimal)

## ---- store-key-coverage
store_results$environment@coverage

## ---- store-truncated
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
