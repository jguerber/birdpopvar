
#' From sampling data, group points that do not change habitat across year according to their habitat
#'
#' @param habitat_transform function to apply on HABITAT_CODE column
observer_habitats_fbbs <- function(sampling, habitat_transform = transform_observer_category) {

  # listening points that do not change habitats across years
  unique_habs <- sampling %>%
    group_by(SITE1) %>%
    summarise(number_habitats = n_distinct(HABITAT_CODE)) %>%
    filter(number_habitats == 1) %>%
    pull(SITE1)

  # listening points that do not change and where
  # habitat description is consistent between survey sessions
  sampling %>%
    filter(SITE1 %in% unique_habs) %>%
    filter(HABITAT_CODE != "DISAGREE") %>%
    select(
      c(SITE1, SITE2, HABITAT_CODE)
    ) %>%
    unique %>% # 30996 points (constant habitat descriptions)
    mutate(
      HABITAT_GROUP = habitat_transform(HABITAT_CODE)
    ) %>%
    select(
      c(SITE2, SITE1, HABITAT_GROUP)
    )
}

#' From FBBS habitat codes to community habitat categories
transform_observer_category <- function(x) {
  ifelse(
    x %in% c("A", "B"),
    "woodland",
    ifelse(
      x == "E",
      "built",
      ifelse(x == "D", "farmland", "other")
    )
  )
}


check_yearly_sampling <- function(df, group_sampling_vars, group_survey_vars, sites) {

  out <- df %>%
    select(all_of(c(sites, group_sampling_vars))) %>%
    group_by(across(all_of(group_sampling_vars[-length(group_sampling_vars)]))) %>% # "COMMUNITY_ID", "SAMPLING", "YEAR"
    mutate(
      N_POINTS = n_distinct(SITE1) # N_POINTS in the community that would be defined by this (YEAR, SAMPLING) group
    ) %>%
    group_by(across(all_of(group_survey_vars[-length(group_survey_vars)]))) %>% # "COMMUNITY_ID", "YEAR"
    slice_max(N_POINTS, n = 1, with_ties = T)   # slice the SAMPLING category that has the most points
  # a single community thus can have varying number of points, but in a year, all points have the same
  # sampling method

  # filter ties : for STOC, prioritize 2_passages
  out <- out %>%
    slice_min(factor(SAMPLING, levels = c("2_passages", "passage_2", "passage_1", "1_passage")), n = 1)

  return(out)

}

#' Aggregate species abundances when they belong to the same GROUP each sampling
aggregate_communities <- function(joined) {

  sites <- joined %>%
    colnames %>%
    str_subset("^SITE")

  sites_without_points <- sites[-length(sites)]

  # if there are sampling-specific covariates, group_by them : they get included in output
  group_sampling_vars <- c("COMMUNITY_ID", "SAMPLING", "YEAR", "SPECIES")

  group_survey_vars <- c("COMMUNITY_ID", "YEAR", "SPECIES")

  joined <- joined %>%
    unite(
      "COMMUNITY_ID",
      all_of(c(sites_without_points, "GROUP")),
      remove = F
    )

  # calculate number of points by year and sampling type so that a case with, in a single year,
  # two different sampling (ex : all points except one with two sessions)
  # for an example see SITE2 == 010935  & YEAR == 2023 : P08 has passage_1 and other points have 2_passages

  points_checked <- joined %>%
    check_yearly_sampling(group_sampling_vars, group_survey_vars, sites)

  joined %>%
    inner_join(points_checked, by = c(group_sampling_vars, sites)) %>% # add N_POINTS information to joined but keeps only POINTS with correct SAMPLING
    group_by(across(all_of(c(group_sampling_vars, "N_POINTS")))) %>% # separate each (year, sampling, species) from each other
    summarise(
      AB_SUM = sum(ABUNDANCE),
      AB_MAX = max(ABUNDANCE),
      .groups = "drop"
    ) %>%
    mutate(
      AB_REL = AB_SUM/N_POINTS
    )
}


#' Add zeros for absences
fill_absences <- function(survey, by = NULL, sampling = NULL) {

  if (is.null(by)) {
    site_colnames <- stringr::str_subset(colnames(survey), "SITE")
  } else {
    site_colnames <- by
  }

  if (is.null(sampling)) {
    sampling <- c()
  }

  surveyed_species <- survey %>% # at each site, all species that got sampled at least once
    select(
      all_of(c(
        site_colnames,
        "SPECIES"
      ))
    ) %>%
    unique

  surveyed_years <- survey %>% # at each site, all years of survey
    select(
      all_of(c(
        site_colnames,
        "YEAR",
        sampling # also take site specific info
      ))
    ) %>%
    unique

  surveyed_years %>%
    group_by(YEAR) %>%
    left_join(surveyed_species, by = site_colnames, relationship = "many-to-many") %>%
    ungroup %>%
    left_join(
      survey,
      by = c(
        site_colnames, "YEAR", "SPECIES", sampling
      ) # this list should be all locally meaningful columns, except abundance
    ) %>%
    mutate(
      across(matches("^AB"), replace_nas) # fill NAs: they are absences
    )
}

replace_nas <- function(x, by = 0) {
  ifelse(is.na(x), by, x)
}
