# download HII rasters for continental France only, for required years only

library(dplyr)
library(purrr)
library(raster)
library(lazyraster) # from github : remotes::install_github("hypertidy/lazyraster")
devtools::load_all()

req_dates <- 2001:2020 # which dates to download ?
containing_dir <- here::here("data/HII") # a writeable directory for data
out_repo <- "HII_France" # will create a directory in containing_dir ?
version_after_2015 <- "v1" # which HII version for 2015+ ?

# ROI bounding box : Continental France
# (coordinates from https://data.humdata.org/dataset/bounding-boxes-for-countries)
roi_bbox = c(-5.225, 9.55, 41.333,	51.2)

prefix_pre_2015 <- "https://storage.googleapis.com/hii-export/"

prefix_2015_onwards <- ifelse(
  version_after_2015 == "v1",
  "https://storage.googleapis.com/hii-no-osm-export/",
  "https://storage.googleapis.com/hii-export/"
)

setup_requests <- data.frame(
  year = req_dates
) %>%
  mutate(
    url = case_when(
      year < 2015 ~ paste0(prefix_pre_2015, year, "-01-01/hii_", year, "-01-01.tif"),
      .default = paste0(prefix_2015_onwards, year, "-01-01/hii_", year, "-01-01.tif")
    ),
    out_filename = paste0("hii_", year, "-01-01.tif")
  )

#' Lazy crop to bbox then download
#'
#' @param name filename
#' @param url url to fetch (must directly point to a file readable by GDAL)
#' @param bbox bounding box to crop the raster before downloading. Must be understandable by raster::extent
#' @param out_repo absolute path to a directory where to put the file
single_lazy_download <- function(
    name,
    url,
    bbox,
    out_dir,
    quiet = T
) {

  lazy <- lazyraster(file.path("/vsicurl", url)) # url prefix for GDAL

  if (!quiet) message("Found url for ", name, " , downloading..")

  lazy %>%
    crop(raster::extent(bbox)) %>%
    as_raster(native = T) %>% # download with the exact resolution/dimensions as the remote
    writeRaster(
      filename = file.path(out_dir, name)
    )
}

dir.create(file.path(containing_dir, out_repo), showWarnings = F, recursive = T)

map2(
  setup_requests$out_filename,
  setup_requests$url,
  single_lazy_download,
  bbox = roi_bbox,
  out_dir = file.path(containing_dir, out_repo),
  quiet = F
)
