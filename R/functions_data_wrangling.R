split_community_id <- function(df, keep = T) {
  df %>%
    separate_wider_delim(
      COMMUNITY_ID,
      delim = "_",
      names = c("SITE2", "type_milieu"),
      cols_remove = !keep
    )
}

#' split series_id
#' keep = T will keep all intermediate values
#' keep = F will produce only COMMUNITY_ID and SPECIES
split_series_id <- function(df, series_col = "series_id", keep = T) {

  df %>%
    separate_wider_delim(
      !!sym(series_col),
      delim = "_",
      names = c("SITE2", "type_milieu", "SPECIES"),
      cols_remove = !keep
    ) %>%
    mutate(
      COMMUNITY_ID = paste0(SITE2, "_", type_milieu),
      .keep = ifelse(keep, "all", "unused")
    )
}
