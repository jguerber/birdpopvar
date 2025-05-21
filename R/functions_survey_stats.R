coms_and_timeseries <- function(df) {
  df %>%
    summarise(
      ncom = n_distinct(COMMUNITY_ID),
      nseries = n_distinct(COMMUNITY_ID, SPECIES),
      nspecies = n_distinct(SPECIES),
      .groups = "drop"
    )
}

fetch_survey_stats <- function(
    aggregate_survey,
    population_variability_data,
    local_trends_outputs
) {
  ## Step Data availability : correct habitat information (between years) and consistent sampling (within years)
  # cleaned raw data -> all possible communities each year in correct habitats

  step_da <- aggregate_survey %>%
    coms_and_timeseries

  ## Step Correct_habitat : habitat in farmland, woodland or built
  step_ch <- aggregate_survey %>%
    filter(str_detect(COMMUNITY_ID, "other", negate = T)) %>%
    coms_and_timeseries

  dropped_coms <- list()
  dropped_coms[["step_ch"]] <- aggregate_survey %>%
    filter(str_detect(COMMUNITY_ID, "other")) %>%
    pull(COMMUNITY_ID)

  ## Step Survey quality :
  # one year of a community is included if it's 3+ points.
  # of these communities, take only those surveyed 9+ years
  # (function R/build_communities.R => filter_survey(by = "survey quality"))
  sq_coms <- aggregate_survey %>%
    filter(str_detect(COMMUNITY_ID, "other", negate = T)) %>%
    filter_survey_coverage %>%
    pull(COMMUNITY_ID) %>% unique

  step_sq <- aggregate_survey %>%
    filter(str_detect(COMMUNITY_ID, "other", negate = T)) %>%
    filter_survey_coverage %>%
    coms_and_timeseries()

  dropped_coms[["step_sq"]] <- aggregate_survey %>%
    filter(str_detect(COMMUNITY_ID, "other", negate = T)) %>% # passing habitat cat
    filter(!(COMMUNITY_ID %in% sq_coms)) %>% # but not sq
    pull(COMMUNITY_ID) %>%
    unique

  ## Step Species presence :
  # in a given community, time series with zeroes for more than half of the community's survey are discarded
  # browser()
  sp_coms <- aggregate_survey %>%
    filter(str_detect(COMMUNITY_ID, "other", negate = T)) %>%
    filter_survey_coverage %>%
    filter_species_presence %>%
    mutate(
      series_id = paste0(COMMUNITY_ID, "_", SPECIES)
    ) %>%
    pull(series_id) %>% unique

  dropped_coms[["step_sp"]] <- aggregate_survey %>%
    mutate(
      series_id = paste0(COMMUNITY_ID, "_", SPECIES)
    ) %>%
    filter(str_detect(COMMUNITY_ID, "other", negate = T)) %>% # passing habitat cat
    filter_survey_coverage %>% # pass sq
    filter(!(series_id %in% sp_coms)) %>% # but not sp
    select(COMMUNITY_ID, SPECIES) %>%
    unique

  step_sp <- aggregate_survey %>%
    filter(str_detect(COMMUNITY_ID, "other", negate = T)) %>%
    filter_survey_coverage %>%
    filter_species_presence %>%
    coms_and_timeseries()


  # Step Trends convergence : each time series is kept if it does not have errors nor warnings nor weird trend values
  # time series with too many absences (more than half of survey years) are also discarded

  vartrend_sp <- local_trends_outputs %>%
    filter(convProblems <= threshold_warnings)

  tc_coms <- vartrend_sp %>%
    split_series_id %>%
    pull(series_id) %>% unique

  dropped_coms[["step_tc"]] <- aggregate_survey %>%
    mutate(
      series_id = paste0(COMMUNITY_ID, "_", SPECIES)
    ) %>%
    filter(str_detect(COMMUNITY_ID, "other", negate = T)) %>% # passing habitat cat
    filter(COMMUNITY_ID %in% sq_coms) %>% # pass sq
    filter(series_id %in% sp_coms) %>% # pass sp
    filter(!(series_id %in% tc_coms)) %>% # but not tc
    select(COMMUNITY_ID, SPECIES) %>%
    unique

  step_tc <- vartrend_sp %>%
    split_series_id %>%
    coms_and_timeseries(
    )

  final_data <- population_variability_data %>%
    filter(!str_detect(COMMUNITY_ID, "other"))

  step_pa <- vartrend_sp %>%
    split_series_id %>%
    filter(COMMUNITY_ID %in% final_data$COMMUNITY_ID) %>%
    coms_and_timeseries()

  percent_trends_corrected <- vartrend_sp %>%
    split_series_id %>%
    filter(COMMUNITY_ID %in% final_data$COMMUNITY_ID) %>%
    group_by(corrected) %>%
    summarise(
      n_series = n_distinct(series_id)
    ) %>%
    mutate(
      f_series = signif(n_series/sum(n_series) * 100, digits = 3)
    ) %>%
    filter(corrected) %>%
    pull(f_series)

  recap_table <- bind_rows(
    step_da,
    step_ch,
    step_sq,
    step_sp,
    step_tc,
    step_pa
  ) %>%
    mutate(
      step = c(
        "Available FBBS communities",
        "Habitats of interest",
        "Survey coverage",
        "Abundant enough species",
        "Convergent local trends",
        "Correct pressure data"
      )
    ) %>%
    pivot_longer(cols = matches("^n"), names_to = "data_type", values_to = "N", names_transform = \(s) str_replace(s, "^n_", "")) %>%
    group_by(data_type) %>%
    mutate(
      freq = N / max(N),
      clean = paste0(as.character(N), " (", round(freq*100), "%)")
    )

  return(
    list(
      table = recap_table,
      percent_correct = percent_trends_corrected,
      last_out = final_data,
      dropped_coms = dropped_coms
    )
  )
}



