#' Convert a relative path to absolute path from the root of the pipeline project
#'
#' @export
here_from_pipeline <- function(...) {
  here::here(Sys.getenv("PATH_BIRDPOPVAR"), ...)
}

running_from_pipeline <- \() here_from_pipeline() == here::here("")
