#' Attribute a batch_id to each community in survey
#'
#' Creates a maximum of 4 times more batches than there are available workers,
#' minimum 2
prepare_trend_batches <- function(survey, parameters) {

  n_workers <- if (is_slurm()) {
    as.integer(parameters$remote_n_tasks)
  } else {
    as.integer(parameters$local_n_cores) - 1
  }
  print(n_workers)
  survey %>%
    select(COMMUNITY_ID) %>%
    unique %>%
    mutate(
      # "stretch" the range 1:n_batches over nrow(.) indexes
      # (found in ?tarchetypes:::tar_group_count_index)
      batch_id = as.integer(
        cut(
          seq_len(n()),
          breaks = max(2, round(n_workers * 4))
        )
      )
    )
}


wrap_local_trends <- function(df, type = "species", override_arguments = NULL) {

  if (is.null(df)) return(NULL)

  if (nrow(df) == 0) return(NULL)

  if ("try_families" %in% names(override_arguments)) {
    try_families <- override_arguments$try_families
  } else {
    try_families <- c("gaussian", "nbinom2_zi", "nbinom2", "poisson_zi", "poisson")
  }

  if (type == "species") {
    df <- df %>% mutate(series_id = paste0(COMMUNITY_ID, "_", SPECIES))
  } else if (type == "community") {
    df <- df %>% mutate(series_id = COMMUNITY_ID)
  } else {
    stop(glue::glue("Unknown type argument {type}"))
  }

  # covariables for local trends models
  lm_covariables <- c("SAMPLING")
  offset <- "log(N_POINTS)"

  control_num_levels <- T

  all_single_series_names <- if (type == "species") {
    c("series_id", "COMMUNITY_ID", "SPECIES")
  } else {
    c("series_id", "COMMUNITY_ID")
  }

  response_var <- ifelse(type == "species", "AB_SUM", "AB_COMM")

  output_list <- df %>%
    mutate_center_year(T) %>%
    group_by_at(all_single_series_names) %>%
    group_map(
      ~ single_series_trends(
        .x,
        response_var = response_var,
        series_name = "series_id",
        lm_covariables = lm_covariables,
        offset = offset,
        return_value = "list", # return a list
        try_families = try_families,
        control_num_levels = control_num_levels # control for the number of factor levels
      ),
      .keep = T # keep grouping variables
    )
  # output_list is a list of length the number of time series in df
  # we "unpack" these result by binding summaries and predicitions together

  list(
    predictions = unpack_from_list(
      output_list,
      key = "predictions"
    ),
    summaries = unpack_from_list(
      output_list,
      key = "summaries"
    )
  ) # => single list by batch
}


unpack_from_list <- function(l, key) {
  do.call(
    dplyr::bind_rows,
    map(l, \(el) el[[key]])
  )
}

single_series_trends <- function(df, ...) {
  browser()
  return (
    list(
      predictions = df %>% select(series_id) %>% unique,
      summaries = df %>% summarise(n_distinct(series_id))
    )
  )
}

mutate_center_year <- function(df, cy = T) {
  if (cy) {
    df <- df %>%
      mutate(YEAR = YEAR - mean(YEAR))
  }

  df
}

single_series_trends <- function(
    df,
    response_var = "ABUNDANCE",
    try_families = c(
      "gaussian", "nbinom2_zi", "nbinom2", "poisson_zi", "poisson"
    ),
    series_name = "default",
    lm_covariables = c("SAMPLING"),
    offset = "N_POINTS",
    return_value = "nothing", # summaries, predictions or model_outputs
    control_num_levels = F
) {

  # infer series names (the value of series_id) based on a data col
  if (series_name != "default") {
    series_name <- df %>%
      pull(series_name) %>%
      unique
  }

  # check nrow(df) and n absences

  N_absences <- df %>%
    filter(!!sym(response_var) == 0) %>%
    nrow

  data_deficient = (nrow(df) <= 2 || (nrow(df) - N_absences <= 2))

  if (data_deficient) {
    return(data.frame())
  }

  browser()

  # safely fit models and return output for non-errored ones
  model_fits <- single_series_local_trend(
    df,
    response_var,
    lm_covariables,
    offset,
    try_families,
    control_num_levels
  )


  # when data is non-integer, count models will issue a warning
  is_count_data <- all(round(df[,response_var]) == df[,response_var])

  if (is_count_data) {
    threshold_poisson <- 0
  } else {
    threshold_poisson <- 1
  }

  # get a summary table of trend models
  summaries <- summarize_model_fits(
    model_fits$outputs,
    response_var = response_var,
    threshold_poisson = threshold_poisson
  ) %>%
    data.frame %>%
    dplyr::mutate(
      N = nrow(df),
      corrected = model_fits$was_corrected,
      N_ab = N_absences,
      series_id = series_name
    )

  # filter models for converging ones
  no_problems <- summaries %>%
    filter(convProblems <= threshold_warnings) %>%
    pull(model_name)

  model_outputs <- lapply(
    no_problems,
    function(x) {
      model_fits$outputs[[x]]
    })
  names(model_outputs) <- no_problems


  # get fitted values and residuals for convergent models
  predictions <- extract_predictions(
    model_outputs,
    series_name
  )

  # return_value allows several option to return the object you're interested in
  if (return_value == "model_outputs") {
    return(model_outputs)
  }

  return(list(
    predictions = predictions,
    summaries = summaries
  ))
}
