library(S7)

# Fixtures are fresh and released once for every generated and shrunk case.
local({
  fixtures <- list()
  released <- 0L
  command <- new_command("increment",
    generate = function(state) new_generator(function(size) 10L,
      function(x) if (x == 0L) list() else list(0L)),
    execute = function(fixture, input) {
      stopifnot(!fixture$closed)
      fixture$value <- fixture$value + input
      fixture$value
    },
    update = function(state, input, output) state + input,
    ensure = function(state, input, output) output < 0L
  )
  law <- new_state_law("fresh counter", 0L, list(command),
    setup = function() {
      fixture <- list2env(list(value = 0L, closed = FALSE), parent = emptyenv())
      fixtures[[length(fixtures) + 1L]] <<- fixture
      fixture
    },
    teardown = function(fixture) {
      stopifnot(!fixture$closed)
      fixture$closed <- TRUE
      released <<- released + 1L
    }
  )
  result <- check_law(law, tests = 20L, seed = 1L)
  expect_identical(result@status, "falsified")
  expect_identical(result@counterexample@minimal$sequence[[1L]]$input, 0L)
  expect_identical(length(fixtures), result@attempts + result@shrink_attempts)
  expect_identical(released, length(fixtures))
  expect_true(all(vapply(fixtures, function(fixture) fixture$closed, logical(1))))
  expect_false(anyDuplicated(fixtures) > 0L)
  expect_identical(result@counterexample@original_condition$trace[[1L]]$before, 0L)
  expect_identical(result@condition$trace[[1L]]$before, 0L)
})

# Keep IDs across removals, resolve nested references, and prune dependent chains.
local({
  commands <- list(
    new_command("padding",
      generate = function(state) if (state$stage == 0L) gen_constant(NULL),
      execute = function(fixture, input) NULL,
      update = function(state, input, output) list(stage = 1L),
      ensure = function(state, input, output) TRUE),
    new_command("allocate",
      generate = function(state) if (state$stage == 1L) gen_constant(NULL),
      execute = function(fixture, input) {
        fixture$handles <- 11L
        11L
      },
      update = function(state, input, output) list(stage = 2L, handle = output),
      ensure = function(state, input, output) TRUE),
    new_command("alias",
      generate = function(state) if (state$stage == 2L) gen_constant(state$handle),
      execute = function(fixture, input) {
        stopifnot(input %in% fixture$handles)
        fixture$handles <- c(fixture$handles, input + 1L)
        input + 1L
      },
      update = function(state, input, output) list(stage = 3L, handle = output),
      ensure = function(state, input, output) identical(output, input + 1L)),
    new_command("use",
      generate = function(state) if (state$stage == 3L) gen_product(
        nested = gen_constant(list(ref = list(state$handle))),
        value = new_generator(function(size) 10L,
          function(x) if (x == 0L) list() else list(0L))),
      execute = function(fixture, input) {
        stopifnot(identical(input$nested$ref[[1L]], 12L),
                  input$nested$ref[[1L]] %in% fixture$handles)
        input$value
      },
      update = function(state, input, output) list(stage = 4L),
      ensure = function(state, input, output) output < 0L)
  )
  law <- new_state_law("dependent handles", list(stage = 0L), commands,
    setup = function() list2env(list(handles = integer()), parent = emptyenv()),
    max_commands = 4L)
  result <- check_law(law, tests = 100L, seed = 1L, shrinks = 100L)
  sequence <- result@counterexample@minimal$sequence
  expect_identical(result@status, "falsified")
  expect_identical(result@shrink_status, "complete")
  expect_identical(vapply(sequence, function(step) step$command, character(1)),
                   c("allocate", "alias", "use"))
  expect_identical(vapply(sequence, function(step) step$id, integer(1)), 2:4)
  expect_identical(sequence[[3L]]$input$value, 0L)
  expect_identical(result@condition$input$nested, list(ref = list(12L)))
  expect_identical(result@condition$trace[[2L]]$input, 11L)
  replay <- do.call(check_law, c(list(law = law), result@parameters))
  expect_identical(replay@counterexample@minimal, result@counterexample@minimal)

  # Audit every evaluated candidate, not just the final counterexample.
  visited <- list()
  audited <- new_law(law@name, law@generators, function(sequence) {
    ids <- integer()
    for (step in sequence) {
      if (step$command == "alias") stopifnot(step$input@id %in% ids)
      if (step$command == "use") stopifnot(step$input$nested$ref[[1L]]@id %in% ids)
      ids <- c(ids, step$id)
    }
    visited[[length(visited) + 1L]] <<- sequence
    (law@holds)(sequence)
  })
  audited_result <- do.call(check_law, c(list(law = audited), result@parameters))
  expect_identical(audited_result@counterexample@minimal, result@counterexample@minimal)
  expect_true(any(lengths(visited) == 0L))
  expect_true(any(vapply(visited, function(x) length(x) == 1L && x[[1L]]$command == "allocate",
                         logical(1))))
})

# NULL outputs remain resolvable; an unavailable command ends generation.
local({
  seen <- 0L
  commands <- list(
    new_command("null",
      generate = function(state) if (state$stage == 0L) gen_constant(NULL),
      execute = function(fixture, input) NULL,
      update = function(state, input, output) list(stage = 1L, handle = output),
      ensure = function(state, input, output) is.null(output)),
    new_command("consume",
      generate = function(state) if (state$stage == 1L) gen_constant(state$handle),
      execute = function(fixture, input) {
        stopifnot(is.null(input))
        seen <<- seen + 1L
        input
      },
      update = function(state, input, output) list(stage = 2L),
      ensure = function(state, input, output) is.null(output))
  )
  law <- new_state_law("NULL handles", list(stage = 0L), commands, function() NULL)
  result <- check_law(law, tests = 20L)
  expect_identical(result@status, "passed")
  expect_true(seen > 0L)
  example <- gen_example(law@generators$sequence, size = 100L)
  expect_true(length(example) <= 2L)
  expect_identical(gen_example(gen_commands(list(stage = 0L), commands, max = 0L)), list())
})

# Unexpected callback failures retain their phase and always release the fixture.
for (phase in c("setup", "execute", "update", "ensure", "teardown")) {
  local({
    closed <- 0L
    made <- 0L
    command <- new_command("operation",
      generate = function(state) gen_constant(1L),
      execute = function(fixture, input) {
        if (phase == "execute") stop("execute failed")
        input
      },
      update = function(state, input, output) {
        if (phase == "update" && is.integer(output)) stop("update failed")
        state
      },
      ensure = function(state, input, output) {
        if (phase == "ensure") warning("ensure warned")
        TRUE
      })
    law <- new_state_law("callback failure", NULL, list(command),
      setup = function() {
        if (phase == "setup") stop("setup failed")
        made <<- made + 1L
        NULL
      },
      teardown = function(fixture) {
        closed <<- closed + 1L
        if (phase == "teardown") stop("teardown failed")
      })
    result <- check_law(law, tests = 20L)
    expect_identical(result@status, "error")
    expect_identical(result@condition$phase, phase)
    expect_identical(result@shrink_attempts, 0L)
    expect_identical(closed, made)
    expect_true(inherits(result@condition, "s7contract_state_error"))
  })
}

# Cleanup cannot overwrite a false postcondition or an earlier callback error.
for (postcondition in c("false", "error")) {
  local({
    ready <- FALSE
    closed <- 0L
    command <- new_command("broken",
      generate = function(state) gen_constant(NULL),
      execute = function(fixture, input) { ready <<- TRUE; NULL },
      ensure = function(state, input, output) {
        if (postcondition == "error") stop("primary error")
        FALSE
      })
    law <- new_state_law("two failures", NULL, list(command), function() NULL,
      teardown = function(fixture) {
        closed <<- closed + 1L
        if (ready) stop("cleanup error")
      })
    result <- check_law(law, tests = 20L)
    expect_identical(result@status, if (postcondition == "false") "falsified" else "error")
    expect_identical(result@condition$phase, "ensure")
    expect_identical(conditionMessage(result@condition$cleanup_condition), "cleanup error")
    expect_identical(result@counterexample@minimal, result@counterexample@original)
    expect_identical(result@shrink_status, "error")
    expect_identical(closed, result@attempts)
  })
}

# A callback failure during a later shrink keeps the last accepted failure.
for (failure_phase in c("execute", "setup", "teardown")) local({
  closed <- 0L
  made <- 0L
  fail_setup <- FALSE
  command <- new_command("broken",
    generate = function(state) new_generator(function(size) 10L, function(x) {
      if (x == 10L) list(5L) else if (x == 5L) list(0L) else list()
    }),
    execute = function(fixture, input) {
      fixture$input <- input
      if (input == 0L && failure_phase == "execute") stop("later failure")
      if (input == 5L && failure_phase == "setup") fail_setup <<- TRUE
      input
    },
    ensure = function(state, input, output) FALSE)
  law <- new_state_law("later error", NULL, list(command),
    setup = function() {
      if (fail_setup) stop("later failure")
      made <<- made + 1L
      list2env(list(input = NA_integer_), parent = emptyenv())
    },
    teardown = function(fixture) {
      closed <<- closed + 1L
      if (identical(fixture$input, 0L) && failure_phase == "teardown") stop("later failure")
    })
  result <- check_law(law, tests = 20L)
  expect_identical(result@status, "falsified")
  expect_identical(result@counterexample@minimal$sequence[[1L]]$input, 5L)
  expect_identical(result@counterexample@original$sequence[[1L]]$input, 10L)
  expect_identical(result@shrink_status, "error")
  expect_true(grepl("later failure", conditionMessage(result@shrink_condition), fixed = TRUE))
  expect_identical(closed, made)
})

# A smaller sequence failing a different postcondition cannot replace the failure.
local({
  first <- new_command("first",
    generate = function(state) gen_constant(10L),
    execute = function(fixture, input) input,
    ensure = function(state, input, output) output != 10L)
  second <- new_command("second", function(state) gen_constant(NULL),
    function(fixture, input) NULL, function(state, input, output) FALSE)
  law <- new_state_law("same postcondition", NULL, list(first, second), function() NULL)
  original <- list(list(id = 1L, command = "first", input = 10L),
                   list(id = 2L, command = "second", input = NULL))
  law@generators <- list(sequence = new_generator(function(size) original, function(sequence) {
    if (length(sequence) == 1L) return(list())
    changed <- sequence
    changed[[1L]]$input <- 0L
    list(sequence[2L], changed, sequence[1L])
  }))
  result <- check_law(law, tests = 1L, shrinks = 10L)
  expect_identical(result@status, "falsified")
  expect_identical(result@counterexample@minimal$sequence, original[1L])
  expect_identical(result@condition$command, "first")
  expect_identical(result@shrink_status, "complete")
})

# Zero and one-candidate budgets do not expand unused command input shrinks.
local({
  expanded <- 0L
  command <- new_command("broken",
    generate = function(state) new_generator(function(size) 1L, function(x) {
      expanded <<- expanded + 1L
      stop("input shrink expanded")
    }),
    execute = function(fixture, input) input,
    ensure = function(state, input, output) FALSE)
  law <- new_state_law("lazy commands", NULL, list(command), function() NULL,
                       max_commands = 1L)
  zero <- check_law(law, tests = 20L, shrinks = 0L)
  expect_identical(zero@shrink_attempts, 0L)
  expect_identical(zero@shrink_status, "budget")
  one <- check_law(law, tests = 20L, shrinks = 1L)
  expect_identical(one@shrink_attempts, 1L)
  expect_identical(one@shrink_status, "budget")
  expect_identical(expanded, 0L)
  more <- check_law(law, tests = 20L, shrinks = 10L)
  expect_identical(more@shrink_status, "error")
  expect_identical(expanded, 1L)
})

# Invalid descriptors, generators, and predicates fail at their own boundary.
command <- new_command("valid", function(state) gen_constant(NULL),
                        function(fixture, input) NULL, function(state, input, output) TRUE)
expect_error(new_command("", command@generate, command@execute, command@ensure),
              pattern = "one non-empty string", fixed = TRUE)
expect_error(new_command("bad", NULL, command@execute, command@ensure))
expect_error(gen_commands(NULL, list()), pattern = "non-empty list", fixed = TRUE)
expect_error(gen_commands(NULL, list(command, command)), pattern = "unique", fixed = TRUE)
expect_error(new_state_law("bad", NULL, list(command), NULL), pattern = "must be functions", fixed = TRUE)
expect_error(gen_commands(NULL, list(command), max = -1L), pattern = "non-negative", fixed = TRUE)
for (fault in c("generate", "require", "ensure")) {
  broken <- command
  if (fault == "generate") broken@generate <- function(state) 1L
  if (fault == "require") broken@require <- function(state, input) NA
  if (fault == "ensure") broken@ensure <- function(state, input, output) c(TRUE, FALSE)
  law <- new_state_law("invalid callback", NULL, list(broken), function() NULL)
  result <- check_law(law, tests = 20L)
  expect_identical(result@status, "error")
  expect_true(grepl(if (fault == "generate") "generator or NULL" else "one non-missing logical",
                    conditionMessage(result@condition), fixed = TRUE))
}
broken <- command
broken@require <- function(state, input) FALSE
result <- check_law(new_state_law("invalid generated input", NULL, list(broken), function() NULL),
                     tests = 20L)
expect_identical(result@status, "error")
expect_true(grepl("violates the precondition", conditionMessage(result@condition), fixed = TRUE))

# Random implementation draws cannot perturb an already generated shrink tree.
local({
  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- .Random.seed
  on.exit({
    suppressWarnings(do.call(RNGkind, as.list(old_kind)))
    if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  })
  make_law <- function(random) {
    command <- new_command("draw",
      generate = function(state) gen_bind(gen_integer(1L, 10L), function(n) gen_integer(0L, n)),
      execute = function(fixture, input) {
        if (random) runif(9L)
        input
      },
      ensure = function(state, input, output) FALSE)
    law <- new_state_law("random implementation", NULL, list(command), function() NULL)
    law@generators <- list(sequence = gen_resize(law@generators$sequence, 10L))
    law
  }
  set.seed(802L)
  caller_seed <- .Random.seed
  quiet <- check_law(make_law(FALSE), tests = 20L, seed = 1L)
  noisy <- check_law(make_law(TRUE), tests = 20L, seed = 1L)
  expect_identical(noisy@counterexample@original, quiet@counterexample@original)
  expect_identical(noisy@counterexample@minimal, quiet@counterexample@minimal)
  expect_identical(noisy@shrink_attempts, quiet@shrink_attempts)
  expect_identical(.Random.seed, caller_seed)
  expect_identical(RNGkind(), old_kind)
  rm(".Random.seed", envir = .GlobalEnv)
  check_law(make_law(TRUE), tests = 20L, seed = 1L)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})
