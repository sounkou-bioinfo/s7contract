library(S7)

# Rejected registrations preserve existing methods and implementation records.
local({
  first <- new_generic("hardening_atomic_first", "x")
  second <- new_generic("hardening_atomic_second", "x")
  C <- new_class("HardeningAtomicClass")
  Fresh <- new_class("HardeningFreshClass")
  Trait <- new_trait("HardeningAtomicTrait", list(first = first, second = second),
                     assoc_consts = list(VALUE = "old"))
  impl_trait(Trait, C, list(first = function(x) "first", second = function(x) "second"))
  old_first <- method(first, C)
  old_second <- method(second, C)
  first_table <- first@methods
  invalid <- list(first = function(x) "changed", second = function(y) y)
  expect_error(impl_trait(Trait, C, invalid, assoc_consts = list(VALUE = "new"), replace = TRUE),
               "dispatches on")
  expect_identical(method(first, C), old_first)
  expect_identical(method(second, C), old_second)
  expect_identical(first@methods, first_table)
  expect_identical(first(C()), "first")
  expect_true(has_trait(C(), Trait))
  expect_identical(trait_assoc_const(Trait, C(), "VALUE"), "old")
  expect_error(impl_trait(Trait, Fresh, invalid), "dispatches on")
  expect_false(has_trait(Fresh, Trait))
  expect_error(method(first, Fresh), "Can't find method")
  expect_error(method(second, Fresh), "Can't find method")

  warned <- new_generic("hardening_warning", "x", function(x, amount = 1, ...) S7_dispatch())
  W <- new_trait("HardeningWarnings", list(first = first, warned = warned))
  expect_error(withCallingHandlers(
    impl_trait(W, C, list(first = function(x) "changed", warned = function(x, amount = 2) amount),
               replace = TRUE), warning = function(w) stop(conditionMessage(w))
  ), "default value")
  expect_identical(method(first, C), old_first)
  expect_false(has_trait(C, W))
})

# Reentrant registrations retain their own effects and still obey replace.
local({
  g <- new_generic("hardening_reentrant", "x")
  C <- new_class("HardeningReentrantClass")
  method(g, C) <- function(x) "base"
  Trait <- new_trait("HardeningReentrantTrait", list(g = g), assoc_consts = "VALUE")
  expect_error(withCallingHandlers(
    impl_trait(Trait, C, list(g = function(x) "outer"), assoc_consts = list(VALUE = "outer")),
    warning = function(w) {
      impl_trait(Trait, C, list(g = function(x) "inner"),
                 assoc_consts = list(VALUE = "inner"), replace = TRUE)
      invokeRestart("muffleWarning")
    }
  ), "already implemented")
  expect_identical(g(C()), "inner")
  expect_true(has_trait(C, Trait))
  expect_identical(trait_assoc_const(Trait, C, "VALUE"), "inner")
})

# A publishing error restores union leaves and removes newly created branches.
local({
  new_branch <- new_generic("hardening_new_branch", c("x", "y"))
  existing_branch <- new_generic("hardening_existing_branch", c("x", "y"))
  locked <- new_generic("hardening_locked", "x")
  C <- new_class("HardeningJournalClass")
  method(existing_branch, list(C, class_integer)) <- function(x, y) "original"
  method(locked, C) <- function(x) "locked"
  old_method <- method(existing_branch, list(C, class_integer))
  old_branch <- existing_branch@methods[[C@name]]
  Trait <- new_trait("HardeningJournalTrait", list(
    new_branch = trait_method(new_branch, args = list(y = class_numeric)),
    existing_branch = trait_method(existing_branch, args = list(y = class_numeric)),
    locked = locked
  ))
  lockBinding(C@name, locked@methods)
  expect_error(impl_trait(Trait, C, list(
    new_branch = function(x, y) "new",
    existing_branch = function(x, y) "changed",
    locked = function(x) "changed"
  ), replace = TRUE), "locked")
  unlockBinding(C@name, locked@methods)
  expect_length(ls(new_branch@methods), 0L)
  expect_identical(existing_branch@methods[[C@name]], old_branch)
  expect_identical(method(existing_branch, list(C, class_integer)), old_method)
  expect_error(method(existing_branch, list(C, class_double)), "Can't find method")
  expect_identical(locked(C()), "locked")
  expect_false(has_trait(C, Trait))

  impl_trait(Trait, C, list(new_branch = function(x, y) "new",
                           existing_branch = function(x, y) "changed",
                           locked = function(x) "changed"), replace = TRUE)
  expect_true(has_trait(C(), Trait))
  for (value in list(1L, 1)) {
    expect_identical(new_branch(C(), value), "new")
    expect_identical(existing_branch(C(), value), "changed")
  }
  expect_identical(method(existing_branch, list(C, class_double))@generic, existing_branch)
})

# Published methods use the live generic for inherited dispatch.
local({
  g <- new_generic("hardening_super", "x")
  Parent <- new_class("HardeningSuperParent")
  Child <- new_class("HardeningSuperChild", parent = Parent)
  method(g, Parent) <- function(x) "parent"
  Trait <- new_trait("HardeningSuperTrait", list(g = g))
  impl_trait(Trait, Child, list(g = function(x) paste0("child/", g(super(x, Parent)))), replace = TRUE)
  expect_identical(g(Child()), "child/parent")
  expect_identical(method(g, Child)@generic, g)
  expect_false(has_trait(Parent, Trait))
})

# Runtime trait registration does not defer temporary generics to package reload.
local({
  g <- new_generic("hardening_foreign_namespace", "x")
  environment(g) <- asNamespace("S7")
  C <- new_class("HardeningForeignNamespace")
  Trait <- new_trait("HardeningForeignTrait", list(g = g))
  impl_trait(Trait, C, list(g = function(x) "registered"))
  expect_identical(g(C()), "registered")
  expect_silent(eval(quote(S7::methods_register()), envir = asNamespace("s7contract")))
  expect_identical(g(C()), "registered")
})

# Nominal registration recognizes supported class instances, not their storage types.
local({
  S4 <- methods::setClass("HardeningS4Value", slots = c(value = "integer"))
  cases <- list(
    list(class = class_environment, object = new.env(parent = emptyenv())),
    list(class = class_function, object = sum),
    list(class = class_function, object = get("if", envir = baseenv())),
    list(class = class_function, object = function() NULL),
    list(class = class_Date, object = as.Date("2026-01-01")),
    list(class = new_S3_class("ordered"), object = ordered("a")),
    list(class = S4, object = S4(value = 1L)),
    list(class = class_integer, object = matrix(1L, 1, 1)),
    list(class = class_logical, object = TRUE),
    list(class = class_double, object = 1),
    list(class = class_complex, object = 1 + 1i),
    list(class = class_character, object = c("numeric", "double")),
    list(class = class_raw, object = as.raw(1)),
    list(class = class_list, object = list(value = 1)),
    list(class = class_expression, object = expression(x + 1)),
    list(class = class_name, object = quote(x)),
    list(class = class_call, object = quote(f(1)))
  )
  for (i in seq_along(cases)) {
    case <- cases[[i]]
    g <- new_generic(paste0("hardening_instance_", i), "x")
    Trait <- new_trait(paste0("HardeningInstanceTrait", i), list(g = g),
                       assoc_consts = list(TAG = i))
    I <- new_interface(paste0("HardeningInstanceInterface", i), list(g = g))
    impl_trait(Trait, case$class, list(g = function(x) TRUE), replace = TRUE)
    expect_true(g(case$object))
    expect_true(implements(case$class, I))
    expect_true(implements(case$object, I))
    expect_true(has_trait(case$class, Trait))
    expect_true(has_trait(case$object, Trait))
    expect_true(trait_call(Trait, "g", case$object))
    expect_identical(trait_assoc_const(Trait, case$object, "TAG"), i)
  }
  methods::removeClass("HardeningS4Value")

  g <- new_generic("hardening_s3_parent", "x")
  P <- new_S3_class("HardeningS3Parent")
  C <- new_S3_class(c("HardeningS3Child", "HardeningS3Parent"))
  object <- structure(1, class = C$class)
  Trait <- new_trait("HardeningS3ParentTrait", list(g = g))
  impl_trait(Trait, P, list(g = function(x) TRUE))
  I <- new_interface("HardeningS3ParentInterface", list(g = g))
  expect_true(g(object))
  expect_true(implements(object, I))
  expect_true(implements(C, I))
  expect_false(has_trait(object, Trait))
  expect_false(has_trait(C, Trait))
  impl_trait(Trait, C, list(g = function(x) TRUE), replace = TRUE)
  expect_true(has_trait(object, Trait))

  Numeric <- new_trait("HardeningNumericStorage")
  impl_trait(Numeric, class_double)
  expect_false(has_trait(as.Date("2026-01-01"), Numeric))
  expect_true(has_trait(1, Numeric))
})

# S7 class identity includes the defining package.
local({
  A <- new_class("HardeningQualifiedClass", package = "firstpkg")
  B <- new_class("HardeningQualifiedClass", package = "secondpkg")
  g <- new_generic("hardening_qualified", "x")
  Trait <- new_trait("HardeningQualifiedTrait", list(g = g), assoc_consts = "TAG")
  impl_trait(Trait, A, list(g = function(x) "a"), assoc_consts = list(TAG = "a"))
  expect_true(has_trait(A(), Trait))
  expect_false(has_trait(B(), Trait))
  impl_trait(Trait, B, list(g = function(x) "b"), assoc_consts = list(TAG = "b"))
  impl_trait(Trait, A, list(g = function(x) "new a"), assoc_consts = list(TAG = "new a"), replace = TRUE)
  expect_identical(g(A()), "new a")
  expect_identical(g(B()), "b")
  expect_identical(trait_assoc_const(Trait, B(), "TAG"), "b")
})

# Printed labels do not define class identity across object systems.
local({
  S3Class <- new_S3_class("HardeningCrossKind")
  S7Class <- new_class("S3/HardeningCrossKind")
  Trait <- new_trait("HardeningCrossKindTrait", assoc_consts = "TAG")
  impl_trait(Trait, S3Class, assoc_consts = list(TAG = "s3"))
  expect_false(has_trait(S7Class(), Trait))
  impl_trait(Trait, S7Class, assoc_consts = list(TAG = "s7"))
  expect_identical(trait_assoc_const(Trait, S7Class(), "TAG"), "s7")
  object <- structure(1, class = "HardeningCrossKind")
  expect_identical(trait_assoc_const(Trait, object, "TAG"), "s3")
})

# Only omitted method implementations use defaults.
local({
  g <- new_generic("hardening_default", "x")
  C <- new_class("HardeningDefaultClass")
  Trait <- new_trait("HardeningDefaultTrait", list(g = trait_method(g, function(x) "default")))
  for (bad in list(NULL, 1, "function")) {
    expect_error(impl_trait(Trait, C, list(g = bad)), "must be a function")
    expect_false(has_trait(C, Trait))
  }
  expect_error(impl_trait(Trait, C, list(unknown = identity)), "Unknown trait method")
  expect_false(withVisible(impl_trait(Trait, C))$visible)
  expect_identical(g(C()), "default")
})

# Embedding retains parent contracts and rejects conflicting scoped names.
local({
  first <- new_generic("hardening_first", "x")
  second <- new_generic("hardening_second", "x")
  C <- new_class("HardeningCompositionClass")
  method(first, C) <- function(x) "first"
  method(second, C) <- function(x) "second"
  A <- new_interface("HardeningParentA", list(op = first))
  B <- new_interface("HardeningParentB", list(op = second))
  expect_error(new_interface("HardeningConflict", parents = list(A, B)), "Conflicting requirements")
  expect_error(new_interface("HardeningLocalConflict", list(op = second), parents = A),
               "Conflicting requirements")
  Left <- new_interface("HardeningLeft", parents = A)
  Right <- new_interface("HardeningRight", parents = A)
  Diamond <- new_interface("HardeningDiamond", parents = list(Left, Right))
  expect_length(interface_requirements(Diamond), 1L)
  expect_true(implements(C, Diamond))
  expect_true(implements(C, A))
  TraitA <- new_trait("HardeningTraitA", list(op = first))
  TraitB <- new_trait("HardeningTraitB", list(op = second))
  expect_error(new_trait("HardeningTraitConflict", parents = list(TraitA, TraitB)),
               "Conflicting requirements")
  DefaultA <- new_trait("HardeningDefaultA", list(op = trait_method(first, function(x) "a")))
  DefaultB <- new_trait("HardeningDefaultB", list(op = trait_method(first, function(x) "b")))
  expect_error(new_trait("HardeningDefaultConflict", parents = list(DefaultA, DefaultB)),
               "Conflicting requirements")

  for (constructor in list(new_interface, new_trait)) {
    expect_error(constructor("HardeningAliases", list(hardening_second = first, hardening_first = second)),
                 "Conflicting requirements")
    valid <- constructor("HardeningValidAliases", list(one = first, two = second))
    if (identical(constructor, new_trait)) {
      impl_trait(valid, C, list(one = function(x) "first", two = function(x) "second"), replace = TRUE)
    }
    expect_identical(with(valid, one(C())), "first")
    expect_identical(with(valid, hardening_first(C())), "first")
    expect_identical(with(valid, two(C())), "second")
    expect_identical(with(valid, hardening_second(C())), "second")
  }
  bad_types <- list(a = interface_requirement(first, returns = class_integer),
                    b = interface_requirement(first, returns = class_character))
  expect_error(new_interface("HardeningTypeCollision", bad_types), "Conflicting requirements")
  aliases <- new_trait("HardeningIdenticalAliases", list(a = first, b = first))
  expect_error(impl_trait(aliases, C, list(a = function(x) "a", b = function(x) "b"), replace = TRUE),
               "Conflicting implementations")
  expect_identical(first(C()), "first")
  expect_false(has_trait(C, aliases))
  fun <- function(x) "both"
  impl_trait(aliases, C, list(a = fun, b = fun), replace = TRUE)
  expect_identical(with(aliases, a(C())), "both")
  expect_identical(with(aliases, b(C())), "both")
})

# Associated items have one declaring trait, including through a diamond.
local({
  C <- new_class("HardeningAssociatedClass")
  g <- new_generic("hardening_associated", "x")
  Root <- new_trait("HardeningAssociatedRoot", list(g = trait_method(g, function(x) "root")),
                    assoc_types = "TYPE", assoc_consts = list(VALUE = NULL))
  Left <- new_trait("HardeningAssociatedLeft", parents = Root)
  Right <- new_trait("HardeningAssociatedRight", parents = Root)
  Diamond <- new_trait("HardeningAssociatedDiamond", parents = list(Left, Right))
  impl_trait(Root, C, assoc_types = list(TYPE = class_integer))
  impl_trait(Left, C)
  impl_trait(Right, C)
  impl_trait(Diamond, C)
  expect_length(trait_methods(Diamond), 1L)
  expect_identical(with(Diamond, g(C())), "root")
  expect_null(trait_assoc_const(Diamond, C(), "VALUE"))
  expect_identical(trait_assoc_type(Diamond, C(), "TYPE"), class_integer)
  impl_trait(Root, C, assoc_types = list(TYPE = class_double), assoc_consts = list(VALUE = 2), replace = TRUE)
  expect_identical(trait_assoc_const(Diamond, C, "VALUE"), 2)
  expect_identical(trait_assoc_type(Diamond, C, "TYPE"), class_double)
  Other <- new_trait("HardeningAssociatedOther", assoc_consts = list(VALUE = NULL))
  expect_error(new_trait("HardeningAssociatedConflict", parents = list(Root, Other)),
               "Conflicting declarations")
  expect_error(new_trait("HardeningAssociatedLocalConflict", parents = Root, assoc_types = "TYPE"),
               "Conflicting declarations")
  expect_error(trait_assoc_const(Diamond, C, "UNKNOWN"), "no associated item")
})

# Descriptor and map names are non-missing, non-empty and unambiguous.
local({
  g <- new_generic("hardening_names", "x")
  for (bad in list(NA_character_, "", character(), c("a", "b"), 1)) {
    expect_error(new_interface(bad), "string")
    expect_error(new_trait(bad), "string")
    expect_error(interface_requirement(g, name = bad), "string")
    expect_error(trait_method(g, name = bad), "string")
    expect_error(new_interface("HardeningName", package = bad), "string")
    expect_error(new_trait("HardeningName", package = bad), "string")
  }
  for (keys in list("", NA_character_, c("a", "a"))) {
    args <- rep(list(class_integer), length(keys))
    names(args) <- keys
    expect_error(interface_requirement(g, args = args), "names")
    expect_error(trait_method(g, args = args), "names")
    expect_error(new_trait("HardeningMap", assoc_types = keys), "names")
    expect_error(new_trait("HardeningMap", assoc_consts = args), "names")
    Trait <- new_trait("HardeningMap", list(a = g), assoc_types = "a")
    C <- new_class("HardeningMapClass")
    expect_error(impl_trait(Trait, C, methods = args), "names")
    expect_error(impl_trait(Trait, C, methods = list(a = function(x) x), assoc_types = args), "names")
  }
  generics <- list(g)
  names(generics) <- NA_character_
  expect_error(new_interface("HardeningMissingAlias", generics), "non-missing")
  expect_error(new_trait("HardeningMissingAlias", generics), "non-missing")
  Trait <- new_trait("HardeningReplace")
  C <- new_class("HardeningReplaceClass")
  for (abstract in list(class_any, class_missing, class_numeric)) {
    expect_error(impl_trait(Trait, abstract), "concrete class")
  }
  for (bad in list(NA, logical(), c(TRUE, FALSE), 1, "yes")) {
    expect_error(impl_trait(Trait, C, replace = bad), "TRUE or FALSE")
  }
})
