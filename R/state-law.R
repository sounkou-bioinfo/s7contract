#' Describe a command for a stateful protocol
#'
#' Commands separate the implementation's effects from a reference model.
#' `generate(state)` returns an input generator, or `NULL` when the command is
#' unavailable. `execute(fixture, input)` calls the implementation.
#' `require(state, input)` checks the symbolic precondition, and
#' `ensure(state, input, output)` checks the observed result against the model
#' before the command. Both predicates return one non-missing logical value.
#'
#' `update(state, input, output)` returns the next model. During generation and
#' shrinking, `output` is an opaque reference to this command's future result.
#' During execution it is the actual result. Updates may store and pass outputs
#' but must not inspect or compute with them. Expected values should come from
#' the model and inputs, independently of the implementation.
#'
#' References may be passed as inputs directly or inside ordinary, unclassed
#' lists. They are resolved before execution, including references to `NULL`
#' outputs. References embedded in other objects are not traversed. Model values
#' must have value semantics: callbacks must not mutate shared environments or
#' other reference objects in the model. Generation, preconditions, and updates
#' must be pure apart from generator draws; preconditions and updates must not
#' draw random numbers.
#'
#' @param name Non-empty command name, unique within a command set.
#' @param generate Function of the model state returning a generator or `NULL`.
#' @param execute Function of the fixture and resolved input returning an output.
#' @param update Function of the previous state, input, and output returning the
#'   next state. Defaults to leaving the model unchanged.
#' @param ensure Function of the previous state, resolved input, and output
#'   returning whether the postcondition holds.
#' @param require Function of the symbolic state and input returning whether the
#'   command is permitted. Defaults to `TRUE`.
#' @return An S7 command descriptor, used by [gen_commands()] and
#'   [new_state_law()].
#' @export
new_command <- function(
  name, generate, execute, ensure,
  update = function(state, input, output) state,
  require = function(state, input) TRUE
) {
  s7_command(name = name, generate = generate, execute = execute,
             update = update, ensure = ensure, require = require)
}

.command_set <- function(commands) {
  if (!is.list(commands) || length(commands) == 0L ||
      !all(vapply(commands, S7::S7_inherits, logical(1), s7_command))) {
    .abort("`commands` must be a non-empty list of command descriptors.")
  }
  names(commands) <- vapply(commands, function(command) command@name, character(1))
  if (anyDuplicated(names(commands))) {
    .abort("Command names must be unique.")
  }
  commands
}

.command_predicate <- function(value, name) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    .abort("Command `%s` must return one non-missing logical value.", name)
  }
  value
}

.resolve_command_input <- function(input, outputs) {
  if (S7::S7_inherits(input, s7_command_ref)) {
    id <- as.character(input@id)
    if (!id %in% names(outputs)) {
      stop(errorCondition("A command refers to a removed output.",
                          class = "s7contract_missing_output"))
    }
    return(outputs[[id]])
  }
  if (is.list(input) && !is.object(input)) {
    return(lapply(input, .resolve_command_input, outputs = outputs))
  }
  input
}

# Prune invalid commands and their trees together, so they cannot reappear.
.commands_rose <- function(initial, commands, trees) {
  state <- initial
  outputs <- list()
  kept <- list()
  for (tree in trees) {
    step <- tree$value
    command <- commands[[step$command]]
    valid <- tryCatch({
      .resolve_command_input(step$input, outputs)
      .command_predicate((command@require)(state, step$input), "require")
    }, s7contract_missing_output = function(e) FALSE)
    if (!valid) next
    output <- s7_command_ref(id = step$id)
    state <- (command@update)(state, step$input, output)
    outputs[as.character(step$id)] <- list(output)
    kept[[length(kept) + 1L]] <- tree
  }
  .new_rose(
    lapply(kept, `[[`, "value"),
    function() .sequence_children(kept, 0L, function(candidate) {
      .commands_rose(initial, commands, candidate)
    })
  )
}

#' Generate and shrink model-valid command sequences
#'
#' Sequence length is sampled from zero through `min(size, max)`. Generation
#' stops early if every command is unavailable. Available commands are selected
#' with equal probability. Generated inputs must satisfy their preconditions.
#'
#' Shrinking removes contiguous chunks, then shrinks command inputs. Each
#' candidate is replayed against the symbolic model: commands with unsatisfied
#' preconditions or missing output references are removed, together with any
#' commands that depend on them. Retained command IDs never change. Candidate
#' construction is lazy and executes no implementation commands or fixtures.
#'
#' @param initial Initial model with value semantics.
#' @param commands Non-empty list of descriptors made with [new_command()].
#' @param max Maximum sequence length.
#' @return An S7 generator of lists of steps. Each step contains an integer `id`,
#'   a `command` name, and its generated `input`.
#' @export
gen_commands <- function(initial, commands, max = 10L) {
  commands <- .command_set(commands)
  max <- .count_arg(max, "max")
  s7_generator(
    draw = function(size) {
      n <- sample.int(as.double(min(size, max)) + 1, 1L) - 1L
      trees <- list()
      state <- initial
      for (id in seq_len(n)) {
        generators <- lapply(commands, function(command) (command@generate)(state))
        available <- which(!vapply(generators, is.null, logical(1)))
        if (length(available) == 0L) break
        if (!all(vapply(generators[available], .is_generator, logical(1)))) {
          .abort("Command `generate` must return a generator or NULL.")
        }
        selected <- available[[sample.int(length(available), 1L)]]
        command <- commands[[selected]]
        tree <- generators[[selected]]@draw(size)
        if (!.command_predicate((command@require)(state, tree$value), "require")) {
          .abort("Generated input violates the precondition of command '%s'.", command@name)
        }
        trees[[length(trees) + 1L]] <- .map_rose(tree, local({
          step_id <- id
          name <- command@name
          function(input) list(id = step_id, command = name, input = input)
        }))
        state <- (command@update)(state, tree$value, s7_command_ref(id = id))
      }
      .commands_rose(initial, commands, trees)
    },
    label = "commands",
    prototype = list()
  )
}

#' Define a generative law for a stateful protocol
#'
#' Builds an ordinary law for [check_law()] or [expect_law()]. Each evaluation,
#' including every shrink candidate, calls `setup()` to obtain a fresh fixture
#' and `teardown(fixture)` once after successful setup. If setup itself fails,
#' it is responsible for releasing any partially acquired resources.
#'
#' A false command postcondition falsifies the law. Unexpected callback errors
#' or warnings produce an error and stop shrinking. Cleanup failures do not
#' replace an established postcondition failure: they are recorded in its
#' `cleanup_condition` and stop shrinking. User callbacks must terminate; the
#' shrink budget counts candidate evaluations, not individual commands.
#'
#' Counterexamples retain the original and reduced command sequences. Their
#' `original_condition` and `condition` describe the original and reduced
#' failures, with the failing `step`, `command`, callback `phase`, pre-command
#' `model`, resolved `input`, `output`, and execution `trace`. Trace entries
#' record the model before and after each completed command. Model and output
#' snapshots require value semantics; mutable output handles retain their usual
#' R reference semantics. Replay has the same requirements as [check_law()],
#' and setup must reproduce the same initial implementation state.
#'
#' @param name Non-empty description of the law.
#' @param initial Initial model with value semantics.
#' @param commands Non-empty list of descriptors made with [new_command()].
#' @param setup Function of no arguments returning a fresh implementation fixture.
#' @param teardown Function of that fixture releasing its resources.
#' @param max_commands Maximum generated sequence length.
#' @param classify Function of the generated `sequence` returning case labels,
#'   as in [new_law()]. Labels describe the generated sequence, including any
#'   suffix not executed after a failure.
#' @param min_coverage Named minimum case proportions, as in [new_law()].
#' @return An S7 law accepted by [check_law()] and [expect_law()].
#' @examples
#' increment <- new_command(
#'   "increment",
#'   generate = function(state) gen_integer(0L, 5L),
#'   execute = function(fixture, input) {
#'     fixture$value <- fixture$value + input
#'     fixture$value
#'   },
#'   update = function(state, input, output) state + input,
#'   ensure = function(state, input, output) identical(output, state + input)
#' )
#' counter_law <- new_state_law(
#'   "counter follows its model", 0L, list(increment),
#'   setup = function() list2env(list(value = 0L), parent = emptyenv())
#' )
#' check_law(counter_law, tests = 20L, seed = 1L)
#' @export
new_state_law <- function(
  name, initial, commands, setup, teardown = function(fixture) NULL,
  max_commands = 10L, classify = function(...) character(), min_coverage = numeric()
) {
  commands <- .command_set(commands)
  if (!is.function(setup) || !is.function(teardown)) {
    .abort("`setup` and `teardown` must be functions.")
  }
  new_law(name, list(sequence = gen_commands(initial, commands, max_commands)),
          function(sequence) .run_commands(initial, commands, sequence, setup, teardown),
          classify = classify, min_coverage = min_coverage)
}

.run_commands <- function(initial, commands, sequence, setup, teardown) {
  state <- initial
  trace <- list()
  outputs <- list()
  step <- 0L
  command_name <- NULL
  input <- output <- NULL
  phase <- "setup"
  ready <- FALSE
  on.exit(if (ready) teardown(fixture))
  result <- tryCatch({
    fixture <- setup()
    ready <- TRUE
    passed <- TRUE
    for (step in seq_along(sequence)) {
      action <- sequence[[step]]
      command_name <- action$command
      command <- commands[[command_name]]
      input <- output <- NULL
      phase <- "resolve"
      input <- .resolve_command_input(action$input, outputs)
      phase <- "execute"
      output <- (command@execute)(fixture, input)
      phase <- "update"
      next_state <- (command@update)(state, input, output)
      trace[[step]] <- list(id = action$id, command = command_name,
                            input = input, output = output,
                            before = state, after = next_state)
      phase <- "ensure"
      passed <- .command_predicate((command@ensure)(state, input, output), "ensure")
      if (!passed) break
      state <- next_state
      outputs[as.character(action$id)] <- list(output)
    }
    passed
  }, error = identity, warning = identity)

  problem <- NULL
  if (!isTRUE(result)) {
    message <- if (inherits(result, "condition")) conditionMessage(result) else "postcondition returned FALSE"
    problem <- errorCondition(
      sprintf("Step %d%s (%s): %s", step,
              if (is.null(command_name)) "" else paste0(" '", command_name, "'"), phase, message),
      class = c(if (isFALSE(result)) "s7contract_state_failure" else "s7contract_state_error",
                "s7contract_state_condition"),
      step = step, command = command_name, phase = phase, model = state,
      input = input, output = output, trace = trace,
      parent = if (inherits(result, "condition")) result else NULL
    )
  }
  if (ready) {
    ready <- FALSE
    cleanup <- tryCatch({ teardown(fixture); NULL }, error = identity, warning = identity)
    if (!is.null(cleanup)) {
      if (is.null(problem)) {
        problem <- errorCondition(
          paste("Fixture teardown:", conditionMessage(cleanup)),
          class = c("s7contract_state_error", "s7contract_state_condition"),
          step = step, command = command_name, phase = "teardown", model = state,
          input = input, output = output, trace = trace, parent = cleanup
        )
      } else {
        problem$cleanup_condition <- cleanup
        problem$message <- paste(problem$message, "Cleanup also failed:", conditionMessage(cleanup))
      }
    }
  }
  if (!is.null(problem)) stop(problem)
  TRUE
}

# Callback defects must not turn an established false postcondition into a pass.
.state_shrink_problem <- function(evaluation) {
  condition <- evaluation$condition
  if (inherits(condition, "s7contract_state_error")) return(condition)
  if (inherits(condition, "s7contract_state_failure")) return(condition$cleanup_condition)
  NULL
}
