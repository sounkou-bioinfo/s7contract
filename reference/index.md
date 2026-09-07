# Package index

## Package

- [`s7contract-package`](https://sounkou-bioinfo.github.io/s7contract/reference/s7contract.md)
  [`s7contract`](https://sounkou-bioinfo.github.io/s7contract/reference/s7contract.md)
  : s7contract: Behavioral Contracts and Generative Laws for S7

## Interfaces

- [`new_interface()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_interface.md)
  [`interface_requirement()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_interface.md)
  : Build a Go-like structural interface on top of S7
- [`interface_requirements()`](https://sounkou-bioinfo.github.io/s7contract/reference/interface_requirements.md)
  [`interface_report()`](https://sounkou-bioinfo.github.io/s7contract/reference/interface_requirements.md)
  [`missing_requirements()`](https://sounkou-bioinfo.github.io/s7contract/reference/interface_requirements.md)
  [`implements()`](https://sounkou-bioinfo.github.io/s7contract/reference/interface_requirements.md)
  [`assert_implements()`](https://sounkou-bioinfo.github.io/s7contract/reference/interface_requirements.md)
  [`as_interface()`](https://sounkou-bioinfo.github.io/s7contract/reference/interface_requirements.md)
  : Inspect or check a Go-like structural interface

## Traits

- [`new_trait()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_trait.md)
  [`trait_method()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_trait.md)
  : Build a Rust-like explicit trait on top of S7
- [`trait_methods()`](https://sounkou-bioinfo.github.io/s7contract/reference/trait_methods.md)
  [`impl_trait()`](https://sounkou-bioinfo.github.io/s7contract/reference/trait_methods.md)
  [`trait_report()`](https://sounkou-bioinfo.github.io/s7contract/reference/trait_methods.md)
  [`has_trait()`](https://sounkou-bioinfo.github.io/s7contract/reference/trait_methods.md)
  [`assert_trait()`](https://sounkou-bioinfo.github.io/s7contract/reference/trait_methods.md)
  [`trait_call()`](https://sounkou-bioinfo.github.io/s7contract/reference/trait_methods.md)
  [`trait_assoc_type()`](https://sounkou-bioinfo.github.io/s7contract/reference/trait_methods.md)
  [`trait_assoc_const()`](https://sounkou-bioinfo.github.io/s7contract/reference/trait_methods.md)
  : Inspect or use a Rust-like explicit trait

## Progressive checks

- [`` `%::%` ``](https://sounkou-bioinfo.github.io/s7contract/reference/grapes-colon-colon-grapes.md)
  : Evaluate an S7 call under an interface or trait contract

## Property laws

- [`new_generator()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_generator.md)
  : Construct a property-based test generator
- [`gen_constant()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_constant.md)
  [`gen_integer()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_constant.md)
  [`gen_map()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_constant.md)
  [`gen_product()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_constant.md)
  [`gen_vector()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_constant.md)
  : Basic property-based test generators
- [`gen_double()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_double.md)
  : Generate finite double values
- [`gen_bind()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_bind.md)
  [`gen_sized()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_bind.md)
  [`gen_resize()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_bind.md)
  [`gen_recursive()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_bind.md)
  : Compose dependent, sized, and recursive generators
- [`gen_element()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_element.md)
  [`gen_choice()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_element.md)
  : Choose values or generators
- [`gen_example()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_example.md)
  [`gen_no_shrink()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_example.md)
  : Inspect a generator or disable its shrinking
- [`new_law()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_law.md)
  [`assume()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_law.md)
  [`check_law()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_law.md)
  [`format_check_result()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_law.md)
  [`expect_law()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_law.md)
  : Define and check a generative law
- [`new_command()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_command.md)
  : Describe a command for a stateful protocol
- [`gen_commands()`](https://sounkou-bioinfo.github.io/s7contract/reference/gen_commands.md)
  : Generate and shrink model-valid command sequences
- [`new_state_law()`](https://sounkou-bioinfo.github.io/s7contract/reference/new_state_law.md)
  : Define a generative law for a stateful protocol
