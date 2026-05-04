# Internal S7 classes for contract descriptors.
#
# These are intentionally not exported. User-facing construction should go
# through new_interface(), interface_requirement(), new_trait(), trait_method(),
# and impl_trait() so validation and method registration happen in one place.

s7_interface_requirement <- S7::new_class(
  "s7_interface_requirement",
  package = "s7contract",
  properties = list(
    name = S7::class_character,
    generic = S7::class_function,
    args = S7::class_list,
    returns = S7::class_any
  )
)

s7_interface <- S7::new_class(
  "s7_interface",
  package = "s7contract",
  properties = list(
    name = S7::class_character,
    package = S7::new_union(NULL, S7::class_character),
    parents = S7::class_list,
    requirements = S7::class_list
  )
)

s7_trait_method <- S7::new_class(
  "s7_trait_method",
  package = "s7contract",
  properties = list(
    name = S7::class_character,
    generic = S7::class_function,
    default = S7::new_union(NULL, S7::class_function),
    args = S7::class_list,
    returns = S7::class_any
  )
)

s7_assoc_item <- S7::new_class(
  "s7_assoc_item",
  package = "s7contract",
  properties = list(
    required = S7::class_logical,
    default = S7::class_any
  )
)

s7_trait <- S7::new_class(
  "s7_trait",
  package = "s7contract",
  properties = list(
    id = S7::class_character,
    name = S7::class_character,
    package = S7::new_union(NULL, S7::class_character),
    parents = S7::class_list,
    methods = S7::class_list,
    assoc_types = S7::class_list,
    assoc_consts = S7::class_list
  )
)

s7_trait_impl <- S7::new_class(
  "s7_trait_impl",
  package = "s7contract",
  properties = list(
    trait = s7_trait,
    trait_id = S7::class_character,
    trait_label = S7::class_character,
    target_class = S7::class_any,
    methods = S7::class_list,
    assoc_types = S7::class_list,
    assoc_consts = S7::class_list
  )
)

.is_interface_requirement <- function(x) {
  S7::S7_inherits(x, s7_interface_requirement)
}

.is_interface <- function(x) {
  S7::S7_inherits(x, s7_interface)
}

.is_trait_method <- function(x) {
  S7::S7_inherits(x, s7_trait_method)
}

.is_trait <- function(x) {
  S7::S7_inherits(x, s7_trait)
}
