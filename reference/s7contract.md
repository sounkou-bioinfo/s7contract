# s7contract: Contract Helpers for S7

`s7contract` provides two experimental contract layers on top of S7:

## Details

- Go-like structural interfaces defined by required generics.

- Rust-like explicit traits with default methods and associated
  metadata.

The package keeps actual method dispatch inside ordinary S7 generics and
uses runtime checks to describe or assert conformance.

## Author

**Maintainer**: Sounkou Mahamane Toure <sounkoutoure@gmail.com>
