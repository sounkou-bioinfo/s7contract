## R CMD check results

Checked s7contract 0.2.0 locally with `R CMD check --as-cran` on
R 4.6.0, x86_64 Linux, Ubuntu 24.04.3 LTS.

0 errors | 0 warnings | 0 notes

The source-package check includes examples, tinytest tests, rebuilt vignettes,
and the PDF and HTML manuals.

## Additional validation

* Verified the framework-neutral law runner in an isolated library without the
  suggested tinytest package, including the adapter's missing-dependency error.
* Ran the Tree-sitter anti-slop audit on all tracked R sources with every native
  rule enabled. Only private-helper usage review prompts remain.
