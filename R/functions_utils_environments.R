here_from_pipeline <- function(...) {
  here::here(Sys.getenv("PATH_BIRDPOPVAR"), ...)
}
