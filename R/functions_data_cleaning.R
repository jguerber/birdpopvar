#' clean function for "short" version of STOC : with species codes + coordinates already present
clean_stoc_short <- function(survey) {
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
    select(SITE2, SITE1, YEAR, SPECIES, ABUNDANCE, SAMPLING) %>%
    filter(SITE2 != "" & SITE1 != "" & str_detect(SITE2, "[0-9]{6}"))
}
