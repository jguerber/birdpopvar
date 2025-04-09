#' Build a raster stack from .tif files in a directory (relative path)
stack_from_repo <- function(repo, as_proxy = T) {
  repo <- here::here(repo)

  files <- str_subset(
    list.files(repo),
    ".tif"
  )

  dates <- files %>%
    str_extract("[0-9]{4}-[0-9]{2}-[0-9]{2}") %>%
    lubridate::parse_date_time("ymd")

  read_stars(file.path(repo, files), proxy = as_proxy, along = "time") %>%
    st_set_dimensions("time", values = dates)
}


#' Extract yearly hii from point locations
build_yearly_hii_focal <- function(
    hii_proxy,
    coords_df,
    crs_epsg = 4326,
    buffer_size = "focal"
) {

  coords_col <- c("lon1", "lat1")

  coords_sf <- coords_df %>%
    point_coordinates_to_sf(
      coords = coords_col,
      crs_code = crs_epsg
    )
  browser()
  hii_proxy %>%
    extract_hii_focal(
      coords_sf
    ) %>% # output of st_extract has a weird, wide format : need to convert to
    # easier shape
    clean_hii_extraction(coords_sf, buffer_size)

}


extract_hii_focal <- function(hii, coords_sf, ...) {
  coords_matrix <- st_coordinates(coords_sf)

  hii %>%
    st_extract(coords_matrix) %>%
    as_tibble()
}


clean_hii_extraction <- function(tbl, coords_sf, buffer_size) {

  out <- tbl %>%
    rename_with(\(s) paste0("year_", str_extract(s, "[0-9]{4}"))) %>%
    bind_cols(coords_sf) %>%
    mutate(
      lon = st_coordinates(geometry)[,1],
      lat = st_coordinates(geometry)[,2]
    ) %>%
    st_drop_geometry %>%
    select(!geometry) %>%
    pivot_longer(
      cols = contains("year_"),
      names_to = "YEAR",
      values_to = "HII",
      names_transform = \(s) as.numeric(str_extract(s, "[0-9]{4}"))
    ) %>%
    mutate(
      HII = HII / 100
    )

  # communities are already separated
  out <- out %>%
    mutate(
      buffer = buffer_size
    )

  out %>%
    select(matches("SITE"), matches("COMMUNITY_ID"), all_of(c("lon", "lat", "YEAR", "HII", "buffer")))
}
