#' Wrapper around here::here to read from relative path while not depending on absolute path
read_path <- function(x, method = read.table, ...) {
  method(
    here::here(x),
    ...
  )
}

write_path <- function(df, rel_path, method = write.csv, ...) {

  method(
    df,
    file = here::here(rel_path),
    ...
  )

  return(rel_path)
}
