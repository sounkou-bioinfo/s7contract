sys.source(system.file("examples", "store-laws.R", package = "s7contract"),
           envir = environment())

expect_identical(vapply(store_results, function(result) result@status, character(1)),
                 c(environment = "passed", list = "passed"))
expect_true(implements(StickyStore, KeyValue))
expect_identical(store_failure@status, "falsified")
expect_identical(store_failure@shrink_status, "complete")
expect_identical(vapply(store_failure@counterexample@minimal$sequence,
                        function(step) step$command, character(1)), c("put", "reset"))
expect_identical(store_failure@counterexample@minimal$sequence[[1L]]$input,
                 list(key = "a", value = 0L))
expect_identical(store_failure@condition$step, 2L)
expect_identical(store_failure@condition$command, "reset")
expect_identical(store_failure@condition$model, list(a = 0L))
expect_identical(store_failure@condition$output, "a")
expect_identical(store_failure@condition$trace[[2L]]$after, list())
expect_identical(store_replayed@counterexample@minimal, store_failure@counterexample@minimal)
expect_identical(store_replayed@condition$trace, store_failure@condition$trace)
expect_true(length(store_failure@counterexample@original_condition$trace) >= 2L)
expect_true(grepl("Step 2 'reset' (ensure)", format_check_result(store_failure), fixed = TRUE))

# Check every visited prefix against an independent key-set oracle.
visits <- 0L
audited <- new_law(store_failure@law@name, store_failure@law@generators, function(sequence) {
  keys <- character()
  ids <- integer()
  for (step in sequence) {
    stopifnot(!step$id %in% ids)
    ids <- c(ids, step$id)
    switch(step$command,
      put = {
        stopifnot(grepl("^[abc\u00e9]{1,4}$", step$input$key), validUTF8(step$input$key))
        keys <- union(keys, step$input$key)
      },
      get = { stopifnot(step$input %in% keys) },
      delete = { stopifnot(step$input %in% keys); keys <- setdiff(keys, step$input) },
      reset = { keys <- character() }
    )
  }
  visits <<- visits + 1L
  (store_failure@law@holds)(sequence)
})
audited_result <- do.call(check_law, c(list(law = audited), store_failure@parameters))
expect_identical(audited_result@counterexample@minimal, store_failure@counterexample@minimal)
expect_identical(visits, audited_result@attempts + audited_result@shrink_attempts)

# The same protocol suite remains one tinytest expectation.
expect_true(isTRUE(expect_law(store_law(stores$environment), tests = 20L)))
failed_expectation <- expect_law(store_failure@law, tests = 100L, seed = 1L)
expect_false(isTRUE(failed_expectation))
expect_true(grepl("reset", attr(failed_expectation, "info"), fixed = TRUE))

for (result in store_results) {
  expect_true(all(result@coverage$met))
  expect_identical(result@coverage$label, c("multi_character", "non_ascii"))
}
expect_true(implements(TruncatedStore, KeyValue))
expect_identical(truncated_failure@status, "falsified")
expect_identical(truncated_failure@shrink_status, "complete")
expect_identical(length(truncated_failure@counterexample@minimal$sequence), 1L)
expect_identical(truncated_failure@condition$command, "put")
expect_identical(truncated_failure@condition$input, list(key = "aa", value = 0L))
replayed <- do.call(check_law,
  c(list(law = truncated_failure@law), truncated_failure@parameters))
expect_identical(replayed@counterexample@minimal, truncated_failure@counterexample@minimal)
expect_identical(replayed@shrink_attempts, truncated_failure@shrink_attempts)

# General strings allow empty output, but require a nonempty code-point alphabet.
expect_identical(gen_example(string_generator("x", 0L, 0L)), "")
expect_identical(gen_example(string_generator("x", 3L, 3L)), "xxx")
expect_identical(gen_example(gen_vector(string_generator("x", 0L, 0L), 2L, 2L)), c("", ""))
expect_error(string_generator(character()))
for (bad in list(NA_character_, c("a", NA_character_), "", "ab", 1L)) {
  expect_error(string_generator(bad))
}
bytes <- "\u00e9"
Encoding(bytes) <- "bytes"
expect_error(string_generator(bytes), pattern = "byte strings")
invalid <- rawToChar(as.raw(255L))
Encoding(invalid) <- "UTF-8"
expect_error(string_generator(invalid), pattern = "UTF-8")
for (bad in list(-1L, NA_integer_, Inf, 0.5, "1")) {
  expect_error(string_generator("a", min = bad))
  expect_error(string_generator("a", max = bad))
}
expect_error(string_generator("a", 3L, 2L))
for (minimum in 0:2) {
  generator <- gen_resize(string_generator(c("\u00e9", "a"), minimum, 4L), 10L)
  law <- new_law("string domain", list(key = generator), function(key) {
    stopifnot(length(key) == 1L, validUTF8(key),
              nchar(key, type = "chars") >= minimum,
              nchar(key, type = "chars") <= 4L,
              grepl("^[a\u00e9]*$", key))
    FALSE
  })
  result <- check_law(law, seed = 4L)
  expect_identical(result@counterexample@minimal$key,
                    paste0(rep("\u00e9", minimum), collapse = ""))
  expect_identical(result@shrink_status, "complete")
}
