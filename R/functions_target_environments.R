tar_init_project_env <- function() {
  if (Sys.getenv("TAR_PROJECT") == "") {
    tar_select_project("all_steps")
  }
}

tar_select_project <- function(project = "all_steps") {

  assertthat::assert_that(
    project %in% c("all_steps", "shortcut"),
    msg = "Provided project is not valid"
  )

  Sys.setenv("TAR_PROJECT" = project)
}

tar_get_current_project <- function() {
  Sys.getenv("TAR_PROJECT")
}
