#' Summarise communities, time series and species numbers
coms_and_timeseries <- function(df) {
  df %>%
    summarise(
      ncom = n_distinct(COMMUNITY_ID),
      nseries = n_distinct(COMMUNITY_ID, SPECIES),
      nspecies = n_distinct(SPECIES),
      .groups = "drop"
    )
}
