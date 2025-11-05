build_tbl_stats1_autocor <- function(autocor, autocor_spat) {
  autocor_table <- map(list(autocor, autocor_spat), function(a) {
    map2(a, names(a), function(x, n) {
      c(x$statistic, pval = x$p.value, response = n)
    }) %>% do.call(bind_rows, .)
  }) %>%
    do.call(bind_rows, .) %>%
    mutate(
      case = rep(c("glmmTMB", "sdmTMB"), each = 2)
    )

  autocor_table_wide <- autocor_table %>%
    mutate(
      across(c(expected, sd, observed, pval), ~ signif(as.numeric(.x), digits = 2))
    ) %>%
    pivot_wider(names_from = case, values_from = c(observed, pval)) %>%
    mutate(
      response = case_match(
        response,
        "variability" ~ "Variability",
        "trend" ~ "Trend"
      )
    )

  autocor_table_wide
}


#### Formatting functions ####

format_autocor_kable <- function(autocor_wide, format = NULL, ...) {
  if (is.null(format)) {
    format <- get_current_kable_format()
  }
  cols_bold <- autocor_wide %>%
    mutate(
      sig_glm = pval_glmmTMB < 0.05,
      sig_sdm = pval_sdmTMB < 0.05
    )

  autocor_wide %>%
    mutate(
      observed_glmmTMB =  kableExtra::cell_spec(observed_glmmTMB, bold = pval_glmmTMB < 0.05),
      observed_sdmTMB =  kableExtra::cell_spec(observed_sdmTMB, bold = pval_sdmTMB < 0.05)
    ) %>%
    select(response, observed_glmmTMB, observed_sdmTMB, expected, sd) %>%
    knitr::kable(
      col.names = c("", "(with glmmTMB)", "(with sdmTMB)", "Expected null $I$ (mean)", "(sd)"),
      align = c("l", rep("c", 4)),
      booktabs = T,
      format = format,
      escape = FALSE
    ) %>%
    kableExtra::add_header_above(
      c(" " = 1, "Observed $I$" = 2, " " = 2)
    ) %>%
    kableExtra::kable_classic() %>%
    maybe_strip_latex_env(format, function(tbl) { # kable weirdly escapes the header, remove this
      tbl %>%
        str_replace("\\\\\\$I\\\\\\$", "$I$")
    })
}
