build_stats1_selection_table <- function(out_glm) {
  anova_tables <- c(variability = "sd_r_average", trend = "abs_trend_average") %>%
    map(function(r) {
      anova_list(out_glm$models[[r]]) %>%
        as.data.frame %>%
        tibble::rownames_to_column("model") %>%
        select(model, Df, AIC, BIC) %>%
        arrange(AIC) %>%
        mutate(
          response = r
        )
    }) %>% do.call(bind_rows, .) %>%
    mutate(across(matches("IC"), ~round(.x))) %>%
    pivot_wider(names_from = response, values_from = c(AIC,BIC)) %>%
    mutate(
      model = str_replace(model, "_", " "),
      model = str_to_sentence(model),
      model = str_replace(model, "npoints$", "n. points")
    ) %>%
    mutate(
      variables = case_match(
        model,
        "Full" ~ "None",
        "No landscape" ~ "Human impact and lanscape complexity",
        "No habitat" ~ "Habitat category",
        "No ecology" ~ "All except the number of points by community",
        "No n. points" ~ "Number of points by community",
        "Null" ~ "All"
      )
    ) %>%
    select( # reorder
      model, Df, AIC_sd_r_average, BIC_sd_r_average, AIC_abs_trend_average, BIC_abs_trend_average, variables
    )

  anova_tables
}

is_html_or_latex <- function() {
  (knitr::is_html_output() || knitr::is_latex_output())
}

format_selection_kable <- function(anova_tables, format = NULL) {
  if (is.null(format)) {
    format <- get_current_kable_format()
  }

  out <- anova_tables %>%
    mutate(
      across(matches("(A|B)IC"), ~ kableExtra::cell_spec(.x, bold = (.x == min(.x))))
    ) %>%
    knitr::kable(
      col.names = c("", "df", "AIC", "BIC", "AIC", "BIC", "Left out fixed effects"),
      align = c("l", rep("c", 5), "l"),
      booktabs = T,
      format = format,
      escape = F
    ) %>%
    kableExtra::add_header_above(
      c(" " = 2, "Variability" = 2, "Trend" = 2, " " = 1)
    ) %>%
    kableExtra::column_spec(column = 7, width = "10em") %>%
    kableExtra::kable_classic(font_size = 10) %>%
    maybe_strip_latex_env(format) # important for supplementary tables

  out
}

#' (for kable output only, i.e. excluding docx)
get_current_kable_format <- function() {
  if (interactive() || knitr::is_html_output()) {
    "html"
  } else {
    "latex"
  }
}

#' Adjust latex output from kableExtra
#'
#' kableExtra will sometimes output latex code that is incompatible with
#' Quarto environments (especially custom ones). This helper function allows
#' to quickly strip a table from its containing `{table}` environments as well
#' as doing other quick substitutions with the function `f` to pass as a
#' parameter
maybe_strip_latex_env <- function(tbl, format, f = NULL) {

  if (format == "latex") {
    if (!is.null(f)) {
      tbl <- f(tbl)
    }

    tbl <- tbl %>%
      str_replace("\\\\begin\\{table\\}\\n", "") %>%
      str_replace("\\\\centering\\n", "") %>%
      str_replace("\\\\end\\{table\\}", "") %>%
      knitr::raw_latex()
  }

  tbl
}


