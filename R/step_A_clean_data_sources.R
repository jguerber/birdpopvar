step_data_cleaning <- function() {
  #### Survey data ####
  survey_data <- list(
    tar_target(
      raw_data,
      read_path(
        file.path("data", parameters$survey_data), sep = ";", row.names = NULL
      )
    ),
    tar_target(
      data_clean,
      clean_fbbs(raw_data)
    ),
    tar_target(
      sampling_info,
      read_path(
        file.path("data", "STOC", "sampling.csv"),
        sep = ",",
        header = T
      ) %>% clean_sampling_info
    )
  )

  #### Corine Land Cover data ####
  clc_setup <- list(
    tar_target(
      clc_legend, # originally from
      # https://www.eea.europa.eu/data-and-maps/data/corine-land-cover-2/corine-land-cover-classes-and/clc_legend.csv
      read_path(file.path("data", "CLC", "clc_legend.csv")) %>% clean_clc_legend
    ),

    tar_target(
      clc_2018,
      load_clc(
        file.path("data", "CLC","CLC_2018", "U2018_CLC2018_V2020_20u1.shp"), clc_legend
      ),
      packages = "sf"
    )
  )

  list(
    survey_data,
    clc_setup
  )
}
