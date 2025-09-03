#' Assert that files exist at the correct location to run the pipeline with the specified project name
#'
#' Does *not* check file contents
#'
#' @param tar_proj usually read from $TAR_PROJECT, supply anything for testing
assert_data_files <- function(tar_proj) {
  assertthat::assert_that(
    tar_proj %in% c("shortcut", "all_steps"),
    msg = "$TAR_PROJECT should be 'shortcut' or 'all_steps'. Edit your .Renviron and restart R."
  )

  if (tar_proj == "shortcut") {
    assert_dir_content(
      here::here("data", "processed"),
      processed_expected_files()
    )
  } else {
    assert_data_all()
  }
}

#' Assert that the directory in `dir` at least has file names provided in `expected`
assert_dir_content <- function(dir, expected) {
  current <- list.files(dir)

  checks_expressions <- purrr::map(
    expected,
    \(f) substitute(f %in% current, list(f = f))
    # use an expression so that the errored file name shows up in the error message
  )

  rlang::inject(assertthat::assert_that(
    !!!checks_expressions
  ))
}

assert_data_all <- function() {
    dirs <- list(
      here::here("data", "raw"),
      here::here("data", "raw", "STOC"),
      here::here("data", "raw", "maps"),
      here::here("data", "raw", "maps", "regions"),
      here::here("data", "raw", "HII"),
      here::here("data", "raw", "HII", "HII_France"),
      here::here("data", "raw", "CLC"),
      here::here("data", "raw", "CLC", "CLC_2018")
    )

    expected <- list(
      raw_expected_content(),
      stoc_expected_content(),
      "regions",
      maps_regions_expected_content(),
      "HII_France",
      hii_france_expected_content(),
      clc_expected_content(),
      clc_2018_expected_content()
    )

    purrr::map2(dirs, expected, assert_dir_content)
}

#### Lists of expected content names, by directory ####

processed_expected_files <- function() {
  c(
    "aggregate_survey.csv",
    "all_local_trends_outputs.csv",
    "bird_population_variability.csv",
    "community_coordinates.csv",
    "metadata.md"
  )
}

raw_expected_content <- function() {
  c(
    "STOC",
    "CLC",
    "HII",
    "maps",
    "metadata.md"
  )
}

stoc_expected_content <- function() {
  c(
    "sampling.csv",
    "fbbs_200m.csv"
  )
}

maps_regions_expected_content <- function() {
  c(
    "LICENCE.txt",
    "regions-20180101.cpg",
    "regions-20180101.dbf",
    "regions-20180101.prj",
    "regions-20180101.shp",
    "regions-20180101.shx",
    "regions-descriptif.txt"
  )
}

hii_france_expected_content <- function() {
  paste0("hii_", 2001:2014, "-01-01.tif")
}

clc_expected_content <- function() {
  c("CLC_2018", "clc_legend.csv")
}

clc_2018_expected_content <- function() {
  exts <- c(".shx", ".cpg", ".dbf", ".prj", ".shp")

  paste0("U2018_CLC2018_V2020_20u1", exts)
}
