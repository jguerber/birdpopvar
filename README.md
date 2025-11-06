# Code for Populations trends and variability within bird communities are amplified by intense land use

This repository is organized as a [targets](https://books.ropensci.org/targets/) pipeline. It can be run fully or only step-wise, depending on the computing power of your working environment. Especially, the local time series trend models runs for several thousands of time series and should therefore only be run on a machine that can either be left alone for several days (for sequential computing), or that can spawn several dozens of parallel worker processes (most likely a HPC cluster).

# Repository structure

By order of decreasing relevance for an interested reader who wants to reproduce the results:

- `data`: extract data from the archive (raw and processed) here
- `R`, `scripts` : custom R files and scripts called within the pipeline, sorted by subject or main functionality they relate to
- `quarto` : chunks of quarto documents to reproduce the figures from the manuscript. `quarto/build_figures` contains a small Quarto project to build all figures at once
- `slurm` : job files and utility scripts to run the pipeline on a HPC cluster managed by [SLURM](https://slurm.schedmd.com/overview.html).
- `renv`, `stores` : directories used by `renv`, the R package version manager and by `targets`, the pipeline management package. Best not modified by hand

# Reproducing the analyses

## 1. Preparing data

For bird survey data, extract the `data/raw` and `data/processed` folder in the digital archive to the `data` sub-directory in this project. If you're only interested in reproducing our statistical analyses, jump to step 2.

Land cover, human footprint and maps data were extracted from data sources who all have their own licensing terms. We therefore only share these information as processed data, calculated for each of the communities whose bird data we included in our results (in `data/processed/bird_population_variability.csv`).

For reproducing our estimations of local landscape complexity and human impact index as well as to reproduce the maps, we provide instructions and helper scripts to retrieve raw data from each of the data sources, to be found in `data/external_sources.md`

## 2. On a local desktop or laptop : from local trends summary statistics to manuscript figures

Package dependencies are handled by the R package [renv](https://pkgs.rstudio.com/renv/index.html). Building the final PDF document with the figures requires a working [Quarto](https://quarto.org/docs/get-started/) installation, that `quarto::quarto_path()` can find.

*Important*: copy `.Renviron.sample` to `.Renviron` in the project directory to enable the shortcut pipeline. Then, start an R shell which will start renv. Then,

```r
renv::restore() # setup R packages, might take several minutes
source(here::here("scripts/import_dependencies.R")) # load packages
source(here::here("scripts/build.R")) # run the targets pipeline. should take around 10 minutes
```

## On a more powerful machine : from raw survey data to manuscript figures

Analyses can be run on subsetted data by modifying the relative path of `survey_data` in parameters.yaml. Too much subsetting will generate unpredictable errors when statistical models fail to converge.

To reproduce the environment, use `renv::restore()`. Adding `TAR_PROJECT="all_steps"` to your .Renviron will guarantee that scripts/build.R will try to run the whole pipeline. It is not advised to launch scripts/build.R in an interactive session on a personal computer (computing will take at least several hours). An example of a job script for a HPC cluster managed by SLURM can be found in slurm/run_pipeline.sh. Request at least 4 cores for the pipeline.

The workload is split between several R processes with the help of [`crew`](https://books.ropensci.org/targets/crew.html) workers. To better tailor the number of spawned workers to your machine or to your HPC cluster, you can edit parameters.yaml :

 - local_n_cores : number of available cores when the pipeline is running on a machine when the environment variable $IS_SLURM is undefined or not set to `"TRUE"`. crew will spawn local_n_cores minus one workers.
 - remote_n_tasks : maximum number of crew workers to spawn with `crew_controller_slurm`. Outdated targets will be delegated in parallel to these workers when they are available (i.e. when your job scheduler runs their script)
 - remote_r_version : R version string to pass as `module load R/4.x.x` in SLURM job scripts 
 
Once the pipeline has successfully run on the cluster, you can import the target store and the processed data file with a SFTP client or, on UNIX machines, running `slurm/pull_store.sh` after setting the correct adresses in the .env file. Locally, you can stay with `$TAR_PROJECT="all_steps"` which will now skip the computing-intensive steps, or switch to `$TAR_PROJECT="shortcut"` (but switching back to all_steps will invalidate some targets and may require to re-download the results from the cluster).
 
## Installation problems ?

We provide a Dockerfile and a compose file to run the pipeline in a dedicated [Docker](https://docs.docker.com/) container. This allows to run the pipeline on any computer as long as Docker is installed, without worrying yourself with R and Quarto installations. Since your HPC cluster will likely not let you use a Docker container directly, this is recommended for running the `"shortcut"` project.

Copy the contents of .env.sample to a new file called .env, at the root of the project directory. Inside, modify the RENV_CACHE_HOST path to the path to the renv cache on your computer. (if you do not know what this is, select the one corresponding to your operating system [here](https://rstudio.github.io/renv/reference/paths.html)).

Build the Docker image as a compose service : `docker compose build` from a terminal at the project root, or build from Docker Desktop.

Run the compose service : `docker compose run pipeline bash`. You're now in a shell within the container for the pipeline.

Restore the R packages : `R; renv::restore()`. This will take around half an hour for the first time but installed packages will be stored in the path specified by $RENV_CACHE_HOST, so later calling `renv::restore` from within the container will be almost instantaneous. Once the R environment is restored, you can select the pipeline project (defaults to `"shortcut"`) and run `targets::tar_make()` to build the pipeline.

By default, stores and output directories are reserved for code run from outside the container, but the outputs and target objects computed from within the container can be accessed in container_output and container_stores directories. To change this behavior, remove the directories you want to share between the host and the container from the .dockerignore file, and change the host paths in compose.yaml accordingly.

# Browsing code

## Pipeline definition

- parameters.yaml and $IS_SLURM define how the pipeline is going to run
- $TAR_PROJECT defines what will run (select the project, either all steps or the shortcut)
- `target_list`, defined in functions_pipeline_control.R, builds the list of targets that corresponds to the selected project

## Steps of the analyses

Functions that start with step_ are defined in the corresponding .R files. They define the target lists for each step of the analyses. These targets most often call custom functions defined in functions_ files. Outputs of the targets are stored in the target store for the corresponding project and can be looked up with `tar_read`.

Using your IDE tools (e.g. for Rstudio, the F2 key can be used to go to a function's definition), you can browse back in the function definitions to access the code parts you're interested in.

## Documentation

Function docstrings can be built with `devtools::document`, which enables the `?function` syntax for any function with an available docstring within the project (work in progress). See for example `?fill_absences` or `?component_correlation_plot`.

# Possible errors

When some targets error, you can run `targets::tar_meta(fields = error, complete_only = T) |> pull(error)` to get details of error messages.

`libgdal.so.36: cannot open shared object file`. Ensure that you have a working installation of GDAL (for Debian/Ubuntu, `sudo apt install libgdal-dev` ; for Windows, install [Rtools](https://cran.r-project.org/bin/windows/Rtools/)). Then rebuild sf with `renv::install("sf", rebuild = T)`.

In the `all_steps` pipeline, targets related to HII extraction may fail with `Error in InterpolateAtPoint()` for GDAL >=3.10. Roll back to an older GDAL version (works in 3.6.x and 3.8.x), or install a recent version of `sf` from GitHub.


