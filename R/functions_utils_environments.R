#' Convert a relative path to absolute path from the root of the pipeline project
#'
#' @export
here_from_pipeline <- function(...) {
  here::here(Sys.getenv("PATH_BIRDPOPVAR"), ...) %>%
    str_replace("//", "/")
}

running_from_pipeline <- \() here_from_pipeline() == here::here("")

#' Where to look for the current target store within the pipeline repository ?
#'
#' If within a running pipeline, will build a path to tar_path_store
#' If outside a running pipeline, read selected project from $TAR_PROJECT and
#' build a path to there
store_in_pipeline <- function() {

  if (tar_active()) {
    path_store <- tar_path_store()
  } else {
    current_proj <- tar_get_current_project()

    assertthat::assert_that(current_proj != "", msg = "TAR_PROJECT must be defined in environment")

    path_store <- tar_config_get(
      "store",
      project = tar_get_current_project(), # read $TAR_PROJECT
      config = here_from_pipeline("_targets.yaml") # retrieve the config file from the pipeline
    )
  }

  here_from_pipeline(
    path_store
  )
}
