# Interfaces for Bioinformatics Containers

``` r

library(S7)
library(s7contract)
```

Bioinformatics packages often exchange rich containers rather than plain
matrices. Bioconductor’s `SummarizedExperiment` class, for example,
coordinates assays, feature metadata, sample metadata, names, validity
rules, and many specialized methods. This vignette does **not**
reimplement that class. It asks a narrower design question: can
interfaces describe a small slice of such a container so downstream code
can depend on behavior rather than one concrete class?

## A toy assay container

We make a tiny S7 class with assays, row metadata, and column metadata.
The validator enforces the minimum shape invariant needed by the
examples.

``` r

MiniSummarizedExperiment <- new_class(
  "MiniSummarizedExperiment",
  properties = list(
    assays = class_list,
    row_data = class_data.frame,
    col_data = class_data.frame
  ),
  validator = function(self) {
    if (length(self@assays) == 0) {
      return("@assays must contain at least one matrix")
    }

    dims <- lapply(self@assays, dim)
    if (any(vapply(dims, is.null, logical(1)))) {
      return("every assay must be matrix-like")
    }

    first_dim <- dims[[1]]
    same_dim <- vapply(dims, identical, logical(1), first_dim)
    if (!all(same_dim)) {
      return("all assays must have the same dimensions")
    }

    if (nrow(self@row_data) != first_dim[[1]]) {
      return("@row_data must have one row per assay feature")
    }
    if (nrow(self@col_data) != first_dim[[2]]) {
      return("@col_data must have one row per assay sample")
    }
  }
)

counts <- matrix(
  c(10, 0, 3, 4, 12, 8),
  nrow = 3,
  dimnames = list(c("geneA", "geneB", "geneC"), c("sample1", "sample2"))
)

mini <- MiniSummarizedExperiment(
  assays = list(counts = counts, logcounts = log1p(counts)),
  row_data = data.frame(gc = c(0.42, 0.51, 0.37), row.names = rownames(counts)),
  col_data = data.frame(condition = c("control", "treated"), row.names = colnames(counts))
)
```

## An interface for the behavior a consumer needs

A differential-expression helper might not care about the concrete
class. It might only need assay names, feature names, sample names, and
a way to retrieve an assay matrix.

``` r

assay_names <- new_generic("assay_names", "x")
feature_names <- new_generic("feature_names", "x")
sample_names <- new_generic("sample_names", "x")
assay_matrix <- new_generic("assay_matrix", "x")

AssayContainer <- new_interface(
  "AssayContainer",
  methods = list(
    assay_names = assay_names,
    feature_names = feature_names,
    sample_names = sample_names,
    assay_matrix = assay_matrix
  )
)

method(assay_names, MiniSummarizedExperiment) <- function(x) names(x@assays)
method(feature_names, MiniSummarizedExperiment) <- function(x) rownames(x@assays[[1]])
method(sample_names, MiniSummarizedExperiment) <- function(x) colnames(x@assays[[1]])
method(assay_matrix, MiniSummarizedExperiment) <- function(x, name = assay_names(x)[[1]]) {
  x@assays[[name]]
}

implements(mini, AssayContainer)
#> [1] TRUE
assay_names(mini)
#> [1] "counts"    "logcounts"
sample_names(mini)
#> [1] "sample1" "sample2"
assay_matrix(mini, "counts")[, "sample1"]
#> geneA geneB geneC 
#>    10     0     3
```

A consumer can now assert the behavior rather than a concrete class
name.

``` r

library_size <- function(x, assay = "counts") {
  assert_implements(x, AssayContainer)
  mat <- assay_matrix(x, assay)
  colSums(mat)
}

library_size(mini)
#> sample1 sample2 
#>      13      24
```

This is the useful part of the idea: an interface can describe a *small
view* of an object. Another package could satisfy the same interface
with an HDF5-backed matrix container, a remote query result, or a test
double, as long as the same runtime methods exist.

## A nominal trait for explicit adapters

The trait layer is useful when structural compatibility is not enough.
Here the implementation records extra metadata about the orientation of
the assay.

``` r

ExperimentLike <- new_trait(
  "ExperimentLike",
  methods = list(
    assay_names = trait_method(assay_names),
    feature_names = trait_method(feature_names),
    sample_names = trait_method(sample_names),
    assay_matrix = trait_method(assay_matrix)
  ),
  assoc_consts = c("ASSAY_ORIENTATION")
)

impl_trait(
  ExperimentLike,
  MiniSummarizedExperiment,
  methods = list(
    assay_names = function(x) names(x@assays),
    feature_names = function(x) rownames(x@assays[[1]]),
    sample_names = function(x) colnames(x@assays[[1]]),
    assay_matrix = function(x, name = assay_names(x)[[1]]) x@assays[[name]]
  ),
  assoc_consts = list(ASSAY_ORIENTATION = "features_by_samples"),
  replace = TRUE
)

has_trait(mini, ExperimentLike)
#> [1] TRUE
trait_assoc_const(ExperimentLike, mini, "ASSAY_ORIENTATION")
#> [1] "features_by_samples"
```

## Where the mimicry breaks

Interfaces are not a substitute for `SummarizedExperiment` itself.

- They do not reproduce Bioconductor’s S4 method ecosystem, validity
  contracts, delayed operations, genomic ranges integration, or metadata
  conventions.
- They do not enforce semantic laws such as row metadata staying
  synchronized after subsetting unless the class and its methods
  implement those laws.
- They do not make assay data fast. Performance still belongs in the
  concrete matrix/container implementation.
- They cannot prove at compile time that a downstream analysis is safe.

## Nonsense ideas and sharper alternatives

| Nonsense idea | Critique | Sharper alternative |
|----|----|----|
| “Let’s rebuild `SummarizedExperiment` with interfaces.” | The existing class is a mature interoperability standard. Rebuilding it would fragment users and lose a large method ecosystem. | Use interfaces for small adapters or tests while respecting established container classes. |
| “An interface should guarantee biological correctness.” | A method list cannot prove that batches, features, genome builds, and assay transforms are meaningful. | Keep biological checks explicit and domain-specific. |
| “One trait can cover matrices, genomic ranges, variant calls, and single-cell objects.” | A huge trait becomes vague and hard to satisfy correctly. | Split contracts by behavior: assay access, interval overlap, variant alleles, sample metadata, and so on. |
| “Runtime traits remove the need for tests.” | Runtime conformance only checks method availability or explicit registration. | Use traits as assertions plus ordinary unit tests and validation fixtures. |

A good bioinformatics interface should be boring: small, named after
behavior, well-tested on toy data, and honest about what it leaves to
the concrete class.
