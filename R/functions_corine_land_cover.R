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
  sf::read_sf(
    path
  ) %>%
    st_transform(crs = 2154) %>% # Lambert-93 projection for France
    left_join(legend_df, by = join_by(!!sym(legend_code) == CLC_CODE))
}


#' RGB in xxx-xxx-xxx to hex value
rgb_to_color <- function(x) {
  cols <- sapply(str_split(x, "-"), function(s) {
    if (all(!is.na(s)) & all(length(s) > 1)) {
      c <- as.numeric(s)
      c <- c / 255

      rgb(c[1], c[2], c[3])
    } else {
      return(NA)
    }
  })

  as.vector(cols)
}
