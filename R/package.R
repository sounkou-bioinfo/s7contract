#' s7contract: Behavioral Contracts and Generative Laws for S7
#'
#' `s7contract` makes behavioral protocols explicit and testable around ordinary
#' S7 dispatch:
#'
#' - Go-like structural interfaces defined by required generics.
#' - Rust-like explicit traits with default methods and associated metadata.
#' - Optional argument and return specifications checked at the point of use.
#' - Property-based laws with integrated shrinking and tinytest expectations.
#'
#' [implements()] checks method availability and [has_trait()] checks declared
#' implementation. [check_law()] tests behavior over generated cases. Protocol
#' authors can reuse laws across implementations by writing functions that
#' construct lists of laws; see `vignette("protocol-laws")`.
#' [new_state_law()] tests sequences of commands against a reference model
#' with fresh fixtures.
#'
#' @docType package
#' @name s7contract
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  S7::method(print, s7_interface) <- .print_s7_interface
  S7::method(print, s7_trait) <- .print_s7_trait
  S7::method(print, s7_check_result) <- .print_s7_check_result
  S7::method(with, s7_interface) <- .with_s7_interface
  S7::method(with, s7_trait) <- .with_s7_trait
  S7::methods_register()

  if (requireNamespace("tinytest", quietly = TRUE)) {
    tinytest::register_tinytest_extension(
      pkg = pkgname,
      functions = "expect_law"
    )
  }
}
