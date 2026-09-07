# Define a generative law for a stateful protocol

Builds an ordinary law for
[`check_law()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_law.md)
or
[`expect_law()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_law.md).
Each evaluation, including every shrink candidate, calls `setup()` to
obtain a fresh fixture and `teardown(fixture)` once after successful
setup. If setup itself fails, it is responsible for releasing any
partially acquired resources.

## Usage

``` r
new_state_law(
  name,
  initial,
  commands,
  setup,
  teardown = function(fixture) NULL,
  max_commands = 10L
)
```

## Arguments

- name:

  Non-empty description of the law.

- initial:

  Initial model with value semantics.

- commands:

  Non-empty list of descriptors made with
  [`new_command()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_command.md).

- setup:

  Function of no arguments returning a fresh implementation fixture.

- teardown:

  Function of that fixture releasing its resources.

- max_commands:

  Maximum generated sequence length.

## Value

An S7 law accepted by
[`check_law()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_law.md)
and
[`expect_law()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_law.md).

## Details

A false command postcondition falsifies the law. Unexpected callback
errors or warnings produce an error and stop shrinking. Cleanup failures
do not replace an established postcondition failure: they are recorded
in its `cleanup_condition` and stop shrinking. User callbacks must
terminate; the shrink budget counts candidate evaluations, not
individual commands.

Counterexamples retain the original and reduced command sequences. Their
`original_condition` and `condition` describe the original and reduced
failures, with the failing `step`, `command`, callback `phase`,
pre-command `model`, resolved `input`, `output`, and execution `trace`.
Trace entries record the model before and after each completed command.
Model and output snapshots require value semantics; mutable output
handles retain their usual R reference semantics. Replay has the same
requirements as
[`check_law()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_law.md),
and setup must reproduce the same initial implementation state.

## Examples

``` r
increment <- new_command(
  "increment",
  generate = function(state) gen_integer(0L, 5L),
  execute = function(fixture, input) {
    fixture$value <- fixture$value + input
    fixture$value
  },
  update = function(state, input, output) state + input,
  ensure = function(state, input, output) identical(output, state + input)
)
counter_law <- new_state_law(
  "counter follows its model", 0L, list(increment),
  setup = function() list2env(list(value = 0L), parent = emptyenv())
)
check_law(counter_law, tests = 20L, seed = 1L)
#> Law 'counter follows its model' passed 20 tests (seed 1).
```
