# s7contract: Behavioral Contracts and Generative Laws for S7

`s7contract` makes behavioral protocols explicit and testable around
ordinary S7 dispatch:

## Details

- Go-like structural interfaces defined by required generics.

- Rust-like explicit traits with default methods and associated
  metadata.

- Optional argument and return specifications checked at the point of
  use.

- Property-based laws with integrated shrinking and tinytest
  expectations.

[`implements()`](https://sounkou-bioinfo.github.io/s7contract/reference/interface_requirements.md)
checks method availability and
[`has_trait()`](https://sounkou-bioinfo.github.io/s7contract/reference/trait_methods.md)
checks declared implementation.
[`check_law()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_law.md)
tests behavior over generated cases. Protocol authors can reuse laws
across implementations by writing functions that construct lists of
laws; see
[`vignette("protocol-laws")`](https://sounkou-bioinfo.github.io/s7contract/articles/protocol-laws.md).
[`new_state_law()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_state_law.md)
tests sequences of commands against a reference model with fresh
fixtures.

## See also

Useful links:

- <https://github.com/sounkou-bioinfo/s7contract>

- <https://sounkou-bioinfo.github.io/s7contract/>

- Report bugs at <https://github.com/sounkou-bioinfo/s7contract/issues>

## Author

**Maintainer**: Sounkou Mahamane Toure <sounkoutoure@gmail.com>
