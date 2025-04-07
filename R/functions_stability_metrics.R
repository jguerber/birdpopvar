predictions_metrics <- function(predictions, ab_col = "AB_SUM", preferred_family = "poisson") {
  if (nrow(predictions) == 0) return(data.frame()) # pass around empty data

  predictions %>%
    # select(!c(filename)) %>%
    filter(model_name == preferred_family) %>%
    rename(raw = residuals) %>%
    pivot_longer(cols = c("raw", "pearson"), names_to = "residual_type", values_to = "residual_value") %>%
    group_by(series_id, residual_type) %>%
    summarise(
      sd_r = sd(residual_value),
      mse := mean((fit - (!!sym(ab_col))**2)),
      mean_ab := mean(!!sym(ab_col)),
      raw_cv := cv(!!sym(ab_col)),
      npars = if ("SAMPLING" %in% colnames(.)) {
        ifelse(n_distinct(SAMPLING) > 1, 2, 1) # one parameter if sampling was not used
      } else {
        1 # one parameter if sampling is not in data
      },
      theta = sum(residual_value**2)/(n_distinct(YEAR) - npars),
      .groups = "drop"
    ) %>%
    mutate(
      cv_eq = sd_r / mean_ab
    )
}

cv <- function(x, na.rm = T) {
  sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
}


join_summaries_and_predictions <- function(
    trends_summary,
    predictions_summary,
    preferred_model = "poisson",
    rtype = "poisson"
  ) {

  n_rows <- map(list(trends_summary, predictions_summary), nrow)
  if (all(n_rows == 0)) return(data.frame())

  trends_summary %>%
    filter(
      model_name == preferred_model & !is.na(convProblems) # non-errored trends
    ) %>%
    # select(!filename) %>%
    right_join(
      filter(predictions_summary, residual_type == rtype), # predictions has the most rows because it has two residual types
      by  = "series_id"
    ) %>%
    filter(!is.na(model_name)) # filter out series that were not matched in join (mostly errored trends)

}
