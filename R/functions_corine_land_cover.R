#{} Clean Corine Land Cover legend and group classes in broad land cover classes
clean_clc_legend <- function(df) {
  df %>%
    mutate(
      LABEL_CUSTOM = ifelse( # split forests and semi natural habitats
        CLC_CODE > 300 & CLC_CODE < 336,
        ifelse(CLC_CODE > 310 & CLC_CODE < 320, "Forests", "Semi natural areas"),
        LABEL1
      ),
      LABEL_CUSTOM = ifelse(
        CLC_CODE > 200 & CLC_CODE < 245,
        ifelse(
          CLC_CODE <= 223, "Cropland", ifelse(
            CLC_CODE > 231, "Heterogeneous agricultural areas",
            "Pastures"
          )
        ),
        LABEL_CUSTOM
      )
    ) %>%
    mutate(HEX = rgb_to_color(RGB)) %>%
    select(CLC_CODE, LABEL1, LABEL_CUSTOM, HEX) %>%
    mutate(CLC_CODE = as.character(CLC_CODE)) %>%
    mutate(LABEL_CUSTOM = str_replace_all(LABEL_CUSTOM, " ", "_"))
}

#' Load CLC and join with broad land cover categories
load_clc <- function(rel_path, legend_df, legend_code = "Code_18") {
  path <- here::here(rel_path)
  # read corine land cover
  clc18 <- read_sf(
    path
  ) %>%
    st_transform(crs = 2154) %>% # Lambert-93 projection for France
    left_join(legend_df, by = join_by(Code_18 == CLC_CODE))
}
