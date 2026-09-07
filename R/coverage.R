.format_law_coverage <- function(result) {
  coverage <- result@coverage
  if (nrow(coverage) == 0L) return(character())
  partial <- if (result@tests < result@parameters$tests) "; partial run" else ""
  header <- sprintf("Case coverage (%d accepted cases%s):", result@coverage_cases, partial)
  rows <- order(coverage$met, na.last = TRUE)[seq_len(min(nrow(coverage), 20L))]
  details <- vapply(rows, function(i) {
    share <- if (is.na(coverage$proportion[[i]])) {
      "unassessed"
    } else {
      paste0(format(100 * coverage$proportion[[i]], digits = 3, trim = TRUE), "%")
    }
    required <- if (is.na(coverage$minimum[[i]])) "" else paste0(
      "; minimum ", format(100 * coverage$minimum[[i]], trim = TRUE), "%",
      if (isFALSE(coverage$met[[i]])) " unmet" else ""
    )
    sprintf("  %s: %d/%d (%s%s)", encodeString(coverage$label[[i]], quote = '"'),
            coverage$count[[i]], result@coverage_cases, share, required)
  }, character(1))
  if (nrow(coverage) > length(rows)) {
    details <- c(details, sprintf("  ... %d more labels in @coverage.", nrow(coverage) - length(rows)))
  }
  c(header, details)
}
