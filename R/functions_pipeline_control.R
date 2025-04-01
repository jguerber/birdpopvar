#' Vector of default list of packages that should be made available to the
#' targets
default_dependencies <- function() {
  c(
    "dplyr",
    "readr",
    "ggplot2",
    "stringr",
    "tidyr",
    "purrr",
    "lubridate",
    "parallel",
    "cowplot",
    "rmarkdown",
    "here"
  )
}

#' Call different target list based on selected pipeline option
target_list <- function(params) {
  if (params$run_steps == "all") {
    target_list_all()
  }
}
