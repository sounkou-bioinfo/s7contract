# Code and documentation

- Apply `native-tool-discipline`: use native file tools for edits and R
  or shell for repository tasks. Do not use Python in this repository.
- Validate numeric type, finiteness, integrality, and range before
  integer conversion. Do not use
  [`suppressWarnings()`](https://rdrr.io/r/base/warning.html) around
  coercion for validation.
- Apply `no-ghosts` to documentation, code comments, tests, and review
  text. Write for readers of the final artifact; omit edit history.
  Preserve scientific background, design explanations, and cautions
  about real traps.

# Releases

- Before CRAN submission, run win-builder and mac-builder checks on the
  release archive and review both results. GitHub Actions checks do not
  replace these.
- Submit to CRAN only when the maintainer explicitly requests
  submission.
- The maintainer writes submission form comments separately from
  `cran-comments.md`. Use actual previous submission text when matching
  their style; keep comments brief and relevant to CRAN review.
