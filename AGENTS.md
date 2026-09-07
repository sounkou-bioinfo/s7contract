# Code and documentation

- Do not use Python in this repository. Use R and shell tools.
- Validate numeric type, finiteness, integrality, and range before
  integer conversion. Do not use
  [`suppressWarnings()`](https://rdrr.io/r/base/warning.html) around
  coercion for validation.
- Preserve scientific background, design explanations, and theoretical
  context in documentation. Remove maintenance narration and empty
  repetition.

# Releases

- Before CRAN submission, run win-builder and mac-builder checks on the
  release archive and review both results. GitHub Actions checks do not
  replace these.
- Submit to CRAN only when the maintainer explicitly requests
  submission.
- The maintainer writes submission form comments separately from
  `cran-comments.md`. Use actual previous submission text when matching
  their style; keep comments brief and relevant to CRAN review.
