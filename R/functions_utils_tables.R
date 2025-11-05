#' Call `other_fn` or `docx_fn` on df based on the current document output format
dispatch_format <- function(df, other_fn, docx_fn = NULL, ...) {
  if (!is_html_or_latex() && !interactive() && !is.null(docx_fn)) {
    docx_fn(df, ...)
  } else {
    other_fn(df, ...)
  }
}

adapt_formatting <- function(table_stats, docx_ok = FALSE) {

  if (!is_html_or_latex() && !interactive() && docx_ok) { # flextable for docx
    out <- format_selection_flex(table_stats)
  } else {
    out <- format_selection_kable(table_stats)
  }

  out
}
