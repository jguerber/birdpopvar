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

  empty_row <- as_tibble(t(set_names(rep(NA, 20), names(hii$attr))))

  do.call(bind_rows, map(1:nrow(coords_sf), function(i) {

    tried <- catchConditions({
      hii %>%
        st_extract(st_coordinates(coords_sf[i,])) %>%
        as_tibble()
    })

    if (length(tried$error) > 0) {
      return(empty_row)
    } else {
      return(tried$value)
    }
  }))
}

#### Buffered Human Impact ####


build_yearly_hii_buffer <- function(
    hii_proxy,
    coords_df,
    crs_epsg = 4326,
    buffer_size = "5km"
) {

  coords_col <- c("lon", "lat")

  coords_sf <- coords_df %>%
    point_coordinates_to_sf(
      coords = coords_col,
      crs_code = crs_epsg
    )

  # for very large buffer sizes, extracting HII raster cells is very slow
  # this can be accelerated by downsampling the HII raster before reading
  dist <- buffer_size_to_meters(buffer_size)

  # decide if we should downsample the raster
  if (dist <= 50*1000) { # under 50km of buffer size, it's not worth it to downsample
    downsample <- NULL
  } else {
    downsample <- 32 # 10km grid size
  }

  if (!is.null(downsample)) {
    hii_proxy <- hii_proxy %>%
      st_downsample(
        n = downsample,
        FUN = mean,
        na.rm = T
      ) # since hii is a stars_proxy, this operation is lazy and is only
    # actually computed for coordinates that are requested by the buffers
  }

  hii_proxy %>%
    extract_hii_buffer(
      coords_sf,
      dist
    ) %>%
    clean_hii_extraction(coords_sf, buffer_size)

}


extract_hii_buffer <- function(hii, coords_sf, dist, ...) {

  # (quite slow for big buffers)
  buffers <- st_buffer(
    coords_sf,
    dist = dist
  ) %>%
    st_geometry

  st_extract_proxy(
    hii,
    buffers,
    fun = mean,
    na.rm = T
  ) %>%
    map(function(el) {
      el %>%
        as_tibble %>%
        mutate(time = as.character(time)) %>%
        pivot_wider(names_from = time, values_from = fun)
    }) %>%
    do.call(bind_rows, .)
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

#' convert units of buffer size to meters
buffer_size_to_meters <- function(buffer_size) {
  # validate input
  if (!str_detect(buffer_size, "^[0-9]+(m|km)$")) {
    stop("Only figures followed by m or km are accepted in buffer_size. Got ", buffer_size)
  }

  unit <- stringr::str_extract(buffer_size, "[a-z]+$")
  value <- stringr::str_extract(buffer_size, "^[0-9]+")

  if (unit == "m") {
    dist <- as.numeric(value)
  } else if (unit == "km") {
    dist <- 1000 * as.numeric(value)
  }

  return(dist)
}
