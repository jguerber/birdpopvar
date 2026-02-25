step_data_cleaning <- function() {
  #### Survey data ####
  survey_data <- list(
    tar_target(
      raw_data,
      read_path(
        file.path("data", "raw", survey_data_file), sep = ";", row.names = NULL
      )
    ),
    tar_target(
      fbbs_names_ref,
      read_path(
        file.path("data", "raw", "STOC", "species_names.csv"), sep = ",", row.names = NULL, header = T
      ) %>% 
        filter(niveau_taxo %in% c("espece", "sous-espece", "hybride"))
    ),
    tar_target(
      data_clean,
      clean_fbbs(raw_data, fbbs_names_ref$pk_species)
    ),
    tar_target(
      sampling_info,
      read_path(
        file.path("data", "raw", "STOC", "sampling.csv"),
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
      read_path(file.path("data", "raw", "CLC", "clc_legend.csv"), method = read.csv) %>% clean_clc_legend
    ),

    tar_map(
      values = tibble(
        clc_year = c("2000", "2006", "2012", "2018"),
        file_name = c(
          "U2006_CLC2000_V2020_20u1.shp",
          "U2012_CLC2006_V2020_20u1.shp",
          "U2018_CLC2012_V2020_20u1.shp",
          "U2018_CLC2018_V2020_20u1.shp"
        ),
        clc_code_name = c("code_00", "Code_06", "Code_12", "Code_18")
      ),
      names = "clc_year",
      tar_target(
        clc,
        load_clc(
          file.path(
            "data", "raw", "CLC", paste0("CLC_", clc_year), file_name
          ),
          clc_legend,
          legend_code = clc_code_name
        ),
        packages = "sf"
      )
    )
  )

  list(
    survey_data,
    clc_setup
  )
}
