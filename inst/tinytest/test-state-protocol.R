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
      put = { keys <- union(keys, step$input$key) },
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
