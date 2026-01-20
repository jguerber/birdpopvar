step_read_outputs <- function(parameters) {

  tables <- list(
    tar_target(
      survey_stats, # this is tar_read somewhere: keep as it is
      fetch_survey_stats( 
        aggregate_survey,
        population_variability_data,
        all_local_trends_outputs
      )
    )
  )
  list(
    tables
  )
}

step_build_document <- function(parameters) {

  list(
    tar_quarto(
      name = build_figs,
      path = here::here("quarto/build_figures"),
      working_directory = here::here(),
      quiet = F
    )
  )

}
