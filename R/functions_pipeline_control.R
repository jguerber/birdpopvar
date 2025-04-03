#' Vector of default list of packages that should be made available to the
#' targets
default_dependencies <- function() {
  c(
    "dplyr",
    "ggplot2",
    "stringr",
    "tidyr",
    "purrr",
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

#' Build a sensible crew controller group depending on the environment
custom_controller_group <- function(parameters, ...) {

  if (Sys.getenv("IS_SLURM") != "") {
    if (Sys.getenv("IS_SLURM") %in%  c("T", "TRUE", TRUE)) {
      return(slurm_controller_group(parameters, ...))
    }
  }

  local_controller_group(parameters, ...)
}

local_controller_group <- function(parameters, ...) {

  controller <- crew::crew_controller_local(
    name = "small_local",
    workers = as.integer(parameters$local_n_cores) - 1
  )

  list(
    controller = controller,
    names = list(normal = "small_local", heavy = "small_local")
  )
}

slurm_controller_group <- function(
    parameters,
    walltime_minutes = 60,
    cpus_per_task = 1,
    mem_gb_per_cpu = c(8, 16),
    ...
  ) {
  # common options
  custom_script_lines <- paste0("module load R/", parameters$remote_r_version)

  custom_crew_options <- crew_options_slurm(
    script_lines = custom_script_lines,
    cpus_per_task = cpus_per_task,
    time_minutes = walltime_minutes,
    memory_gigabytes_required = mem_gb_per_cpu, # request memory for the task
    ...
  )

  controller_normal <-  crew_controller_slurm(
    name = "normal",
    workers = 4,
    seconds_idle = 120, # don't let a SLURM job hanging without work for more than two minutes
    crashes_error = 2, # retry twice on error
    options_cluster = custom_crew_options
  )

  controller_heavy <- crew_controller_slurm(
    name = "heavy",
    workers = as.integer(parameters$remote_n_tasks),
    seconds_idle = 60,
    crashes_error = 2,
    options_cluster = custom_crew_options
  )

  controller_group <- crew_controller_group(
    controller_normal,
    controller_heavy
  )

  list(
    controller = controller_group,
    names = list(
      normal = "normal",
      heavy = "heavy"
    )
  )
}
