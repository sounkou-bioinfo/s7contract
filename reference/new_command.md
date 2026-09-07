# Describe a command for a stateful protocol

Commands separate the implementation's effects from a reference model.
`generate(state)` returns an input generator, or `NULL` when the command
is unavailable. `execute(fixture, input)` calls the implementation.
[`require(state, input)`](https://rdrr.io/r/base/library.html) checks
the symbolic precondition, and `ensure(state, input, output)` checks the
observed result against the model before the command. Both predicates
return one non-missing logical value.

## Usage

``` r
new_command(
  name,
  generate,
  execute,
  ensure,
  update = function(state, input, output) state,
  require = function(state, input) TRUE
)
```

## Arguments

- name:

  Non-empty command name, unique within a command set.

- generate:

  Function of the model state returning a generator or `NULL`.

- execute:

  Function of the fixture and resolved input returning an output.

- ensure:

  Function of the previous state, resolved input, and output returning
  whether the postcondition holds.

- update:

  Function of the previous state, input, and output returning the next
  state. Defaults to leaving the model unchanged.

- require:

  Function of the symbolic state and input returning whether the command
  is permitted. Defaults to `TRUE`.

## Value

An S7 command descriptor, used by
[`gen_commands()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_commands.md)
and
[`new_state_law()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_state_law.md).

## Details

`update(state, input, output)` returns the next model. During generation
and shrinking, `output` is an opaque reference to this command's future
result. During execution it is the actual result. Updates may store and
pass outputs but must not inspect or compute with them. Expected values
should come from the model and inputs, independently of the
implementation.

References may be passed as inputs directly or inside ordinary,
unclassed lists. They are resolved before execution, including
references to `NULL` outputs. References embedded in other objects are
not traversed. Model values must have value semantics: callbacks must
not mutate shared environments or other reference objects in the model.
Generation, preconditions, and updates must be pure apart from generator
draws; preconditions and updates must not draw random numbers.
