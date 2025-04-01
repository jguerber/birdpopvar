#' Wrapper around here::here to read from relative path while not depending on absolute path
read_path <- function(x, ...) {
  read.table(
    here::here(x),
    ...
  )
}
