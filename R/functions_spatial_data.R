#' Compute coordinates of centroids of points in each group_level
#'
#' @param df data with at least group_level and coords as columns
#' @param coords columns where to look for (lon,lat) values for single points
#' @param source_epsg EPSG code for CRS of data in coords columns
#' @param latlon_epsg EPSG code for CRS to compute lon/lat in
#' @param xy_epsg EPSG code for CRS to compute easting/northing in
centroid_coordinates <- function(
    df,
    coords = c("lon1", "lat1"),
    source_epsg = 4326,
    latlon_epsg = 4326,
    xy_epsg = 32631 # UTM zone 31
) {

  if (length(groups(df)) == 0) {
    message("df is ungrouped, a single pair of coordinates will be computed")
  }

  df %>%
    st_as_sf(coords = coords, crs = st_crs(source_epsg)) %>%
    summarise(geometry = st_union(geometry)) %>%
    mutate(
      geometry = st_centroid(geometry)
    ) %>%
    st_transform(crs = st_crs(latlon_epsg)) %>%
    mutate(
      lon = st_coordinates(geometry)[,1],
      lat = st_coordinates(geometry)[,2]
    ) %>%
    st_transform(crs = st_crs(xy_epsg)) %>%
    mutate(
      X = st_coordinates(geometry)[,1],
      Y = st_coordinates(geometry)[,2]
    ) %>%
    st_drop_geometry
}


#' same as starsExtra::extract2 but working with a stars_proxy
st_extract_proxy <- function(proxy, layer, fun, dims = NULL, ...) {
  if (is.data.frame(layer)) {
    geometries <- st_geometry(layer)
  } else {
    geometries <- layer
  }

  return_list <- (length(geometries) > 1)

  if (is.null(dims)) {
    dims <- length(dim(proxy))
  }

  result <- list()

  for (i in 1:length(geometries)) {
    result[[i]] <- proxy[geometries[i]] %>%
      st_apply(dims, fun, ...) %>%
      st_as_stars
  }

  if (!return_list) {
    return(result[[1]])
  }

  return(result)
}
