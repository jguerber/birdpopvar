
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
