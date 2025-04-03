#' clean function for "short" version of FBBS : with species codes + coordinates already present
clean_fbbs <- function(survey) {
  survey %>%
    rename(
      id_carre = V1,
      num_point = V2,
      annee = V3,
      code_sp = V4,
      passage = V5,
      maxabd = V6,
      habitat_p = V7,
      habitat_s = V8,
      lon = V9,
      lat = V10
    ) %>%
    filter(id_carre != "id_carre") %>% # the real header is at a weird position, filter it out
    mutate(
      id_carre = stringr::str_pad(id_carre, 6, pad = "0", side = "left"),
      id_point = paste0(id_carre, "P", stringr::str_pad(num_point, 2, pad = "0", side = "left")),
      passage = case_match(
        as.character(passage),
        "1" ~ "1_passage",
        "2" ~ "2_passages",
        .default = NA
      ),
      maxabd = as.numeric(str_replace(as.character(maxabd), ",", ".")),
      annee = as.numeric(annee)
    ) %>%
    rename(
      SITE2 = id_carre,
      SITE1 = id_point,
      YEAR = annee,
      SPECIES = code_sp,
      SAMPLING = passage,
      ABUNDANCE = maxabd
    ) %>%
    select(SITE2, SITE1, YEAR, SPECIES, ABUNDANCE, SAMPLING, lon, lat, habitat_p, habitat_s) %>%
    filter(SITE2 != "" & SITE1 != "" & str_detect(SITE2, "[0-9]{6}"))
}

clean_sampling_info <- function(df) {
  df %>%
    rename(
      SITE2 = carre,
      SITE1 = point,
      YEAR = annee,
      SAMPLING = passages,
      HABITAT_CODE = p_milieu
    ) %>%
    select(-s_milieux)
}

#### Processed data filtering

filter_survey_coverage <- function(df) {
  df %>%
    filter(N_POINTS >= 3) %>% # need to be filtered before N_YEARS because some communities will be only partially removed from this
    group_by(COMMUNITY_ID) %>%
    mutate(NYEARS = n_distinct(YEAR)) %>%
    filter(NYEARS >= 9) %>%
    group_by(COMMUNITY_ID, SPECIES) %>%
    mutate(
      N_absent = sum(AB_SUM == 0)
    ) %>%
    ungroup
  filter(N_POINTS >= 3) %>% # need to be filtered before N_YEARS because some communities will be only partially removed from this
    group_by(COMMUNITY_ID) %>%
    mutate(NYEARS = n_distinct(YEAR)) %>%
    filter(NYEARS >= 9) %>%
    group_by(COMMUNITY_ID, SPECIES) %>%
    mutate(
      N_absent = sum(AB_SUM == 0)
    ) %>%
    ungroup
}

filter_species_presence <- function(df) {
  df %>%
    filter(
      NYEARS - N_absent >= 0.5*NYEARS
    )
}

filter_habitats <- function(df, col_check = "COMMUNITY_ID") {
  df %>%
    filter(
      !str_detect(!!sym(col_check), "other")
    )
}
