# Generate and shrink model-valid command sequences

Sequence length is sampled from zero through `min(size, max)`.
Generation stops early if every command is unavailable. Available
commands are selected with equal probability. Generated inputs must
satisfy their preconditions.

## Usage

``` r
gen_commands(initial, commands, max = 10L)
```

## Arguments

- initial:

  Initial model with value semantics.

- commands:

  Non-empty list of descriptors made with
  [`new_command()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_command.md).

- max:

  Maximum sequence length.

## Value

An S7 generator of lists of steps. Each step contains an integer `id`, a
`command` name, and its generated `input`.

## Details

Shrinking removes contiguous chunks, then shrinks command inputs. Each
candidate is replayed against the symbolic model: commands with
unsatisfied preconditions or missing output references are removed,
together with any commands that depend on them. Retained command IDs
never change. Candidate construction is lazy and executes no
implementation commands or fixtures.
