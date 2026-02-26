
point_coordinates_to_sf <- function(df, coords = c("lon", "lat"), crs_code = 4326) {

  df %>%
    dplyr::filter(if_all(
      all_of(coords),
      ~ !is.na(.x)
    )) %>%
    st_as_sf(coords = coords, crs = st_crs(crs_code))

}

intersect_buffer_clc <- function(points, clc, buffer_radius_m = 250) {

  # buffers around each community : set buffers around **points**, then pivot,
  # group by community and unite the buffers
  buffers <- points %>%
    point_coordinates_to_sf(
      coords = c("lon1", "lat1"),
      crs_code = 4326
    ) %>%
    st_transform(crs = st_crs(clc)) %>%
    st_buffer(dist = buffer_radius_m) %>%
    dplyr::filter(!is.na(COMMUNITY_ID)) %>%
    group_by(
      SITE2, COMMUNITY_ID
    ) %>%
    summarise(
      geometry = st_union(geometry) # this also drops the LABEL_CUSTOM that was kept to describe points_coordinates_wide
      # this is intended because the join with clc is going to add a new Label_custom column
    )

  clc %>%
    st_intersection(buffers)
}


build_habitat_proportions <- function(intersections, clc_year = "2018") {

  communities_grouped <- intersections %>%
    mutate(
      area = as.numeric(st_area(geometry)) # compute area for each polygon of the intersection
    ) %>%
    st_drop_geometry %>%
    as.data.frame %>%
    group_by(SITE2, COMMUNITY_ID)

  proportions_coarse <- communities_grouped %>%
    summarise_areas(
      area_col = "area",
      col_label = "LABEL_CUSTOM", # coarse land use descriptions : LABEL_CUSTOM
      names_suffix = ""
    )

  proportions_fine <- communities_grouped %>%
    summarise_areas(
      area_col = "area",
      col_label = clc_code_column(clc_year),
      names_suffix = "_fine"
    )

  # for now there is one row per (community, method, intersection polygon) :
  # take the coarsest proportions, pivot wider on rel_area, and join back
  # H, area_buffer and H_fine

  proportions_fine$polygons %>%
    select(!area) %>%
    pivot_wider(
      names_from = clc_code_column(clc_year),
      values_from = "rel_area_fine",
      names_prefix = "prop_",
      values_fill = 0
    ) %>%
    left_join( # area_buffer and H from community-level survey of coarse
      proportions_coarse$communities, by = c("SITE2", "COMMUNITY_ID")
    ) %>%
    left_join( # H_fine and area_buffer_fine
      proportions_fine$communities, by = c("SITE2", "COMMUNITY_ID")
    ) %>%
    select(!area_buffer_fine) %>% # exactly the same as area_buffer
    ungroup # drop groups
}


clc_code_column <- function(year) {
  if (!(year %in% c("2000", "2006", "2012", "2018"))) stop("Bad CLC year in build_habitat_proportions")

  case_match(
    year,
    "2000"~ "code_00",
    "2006" ~"Code_06",
    "2012" ~"Code_12",
    "2018" ~"Code_18"
  )
}

summarise_areas <- function(df, area_col = "area", col_label = "LABEL_CUSTOM", names_suffix = "") {
  column_strings <- c("rel_area", "area_buffer", "H")
  new_cols <- paste0(column_strings, names_suffix)
  new_cols <- setNames(as.list(new_cols), column_strings)

  by_polygon <- df %>%
    group_by(!!sym(col_label), .add = TRUE) %>%
    summarise(
      !!sym(area_col) := sum(!!sym(area_col)), .groups = "drop_last"
    ) %>%
    mutate(
      !!sym(new_cols$rel_area) := !!sym(area_col)/sum(!!sym(area_col))
    )

  by_community <- by_polygon %>%
    summarise(
      !!sym(new_cols$area_buffer) := sum(!!sym(area_col)),
      !!sym(new_cols$H) := shannon(!!sym(new_cols$rel_area), quiet = T)
    )

  return(list(
    polygons = by_polygon,
    communities = by_community
  ))
}

shannon <- function(x, na.rm = T, quiet = F) {
  if (sum(x) != 1) {
    if (!quiet) warning("Data is not proportion data, rescaling..")
    x <- x/sum(x, na.rm = na.rm)
  }

  return(-sum(x*log(x)))
}

