#' s7contract: Contract Helpers for S7
#'
#' `s7contract` provides two experimental contract layers on top of S7:
#'
#' - Go-like structural interfaces defined by required generics.
#' - Rust-like explicit traits with default methods and associated metadata.
#'
#' The package keeps actual method dispatch inside ordinary S7 generics and uses
#' runtime checks to describe or assert conformance.
#'
#' @docType package
#' @name s7contract
"_PACKAGE"

.onLoad <- function(...) {
  S7::method(print, s7_interface) <- .print_s7_interface
  S7::method(print, s7_trait) <- .print_s7_trait
  S7::method(with, s7_interface) <- .with_s7_interface
  S7::method(with, s7_trait) <- .with_s7_trait
  S7::methods_register()
}
