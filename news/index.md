# Changelog

## s7contract 0.1.0

- Fixed
  [`impl_trait()`](https://sounkou-bioinfo.github.io/s7contract/reference/trait_methods.md)
  so failed S7 method registration no longer leaves a stale trait
  implementation record.
- Preserved explicit `NULL` associated item values and allowed subtraits
  to retrieve inherited associated items.
- Added regression tests for trait registration failure and associated
  metadata edge cases.
- Simplified the README to keep the Go/Rust analogies clearly scoped to
  runtime S7 helpers.
