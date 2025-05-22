# Code for Land use intensity destabilizes bird population variability beyond the impact of species population trends

This repository is organized as a [targets](https://books.ropensci.org/targets/) pipeline that can be run fully or only step-wise, depending on the computing power of your working environment. Especially, the local time series trend models runs for several thousands of time series and should therefore only be run on a machine that can either be left alone for several days (for sequential computing), or that can spawn several dozens of parallel worker processes (most likely a HPC cluster).

# Repository structure

- `data`: extract raw data from the archive here
- `processed`: extract processed data from the archive here
- `R`, `scripts` : custom R files and scripts called within the pipeline, sorted by subject or main functionality they relate to
- `renv`, `stores` : directories used by `renv`, the R package version manager and by `targets`, the pipeline management package. Best not modified by hand.
- `slurm` : job files and utility scripts to run the pipeline on a HPC cluster managed by [SLURM](https://slurm.schedmd.com/overview.html).

# Reproducing the analyses

Package dependencies are handled by [renv](https://pkgs.rstudio.com/renv/index.html).

## On a local desktop or laptop : from local trends summary statistics to manuscript figures

```r
renv::restore() # setup R packages, might take several minutes
source(here::here("scripts/import_dependencies.R")) # load packages
tar_select_project(project = "shortcut") # select the shortcut project
source(here::here("scripts/build.R")) # run the targets pipeline. should take around 10 minutes
```

## On a more powerful machine : from raw survey data to manuscript figures

Analyses can be run on subsetted data by modifying the relative path of `survey_data` in parameters.yaml. Too much subsetting will generate unpredictable errors when statistical models fail to converge.

To reproduce the environment, use `renv::restore()`. Adding `TAR_PROJECT="all_steps"` to your .Renviron will guarantee that scripts/build.R will try to run the whole pipeline. It is not advised to launch scripts/build.R in an interactive session on a personal computer (computing will take at least several hours). An example of a job script for a HPC cluster managed by SLURM can be found in slurm/run_pipeline.sh. Request at least 4 cores for the pipeline.

The workload is split between several R processes with the help of [`crew`](https://books.ropensci.org/targets/crew.html) workers. To better tailor the number of spawned workers to your machine or to your HPC cluster, you can edit parameters.yaml :

 - local_n_cores : number of available cores when the pipeline is running on a machine when the environment variable $IS_SLURM is undefined or not set to `"TRUE"`. crew will spawn local_n_cores minus one workers.
 - remote_n_tasks : maximum number of crew workers to spawn with `crew_controller_slurm`. Outdated targets will be delegated in parallel to these workers when they are available (i.e. when your job scheduler runs their script)
 - remote_r_version : R version string to pass as `module load R/4.x.x` in SLURM job scripts 
 
## Installation problems ?

We provide a Dockerfile and a compose file to run the pipeline in a dedicated [Docker](https://docs.docker.com/) container. This allows to run the pipeline on any computer as long as Docker is installed, without worrying yourself with R and Quarto installations.

Copy the contents of .env.sample to a new file called .env, at the root of the project directory. Inside, modify the RENV_CACHE_HOST path to the path to the renv cache on your computer. (if you do not know what this is, select the one corresponding to your operating system [here](https://rstudio.github.io/renv/reference/paths.html)).

Build the Docker image as a compose service : `docker compose build` from a terminal at the project root, or build from Docker Desktop.

Run the compose service : `docker compose run pipeline R`. You're now in a shell within the container for the pipeline.

Restore the R packages : `renv::restore()`. This will take around half an hour for the first time but installed packages will be stored in the path specified by $RENV_CACHE_HOST, so later calling `renv::restore` from within the container will be almost instantaneous. Once the R environment is restored, you can select the pipeline project (defaults to `"shortcut"`, because your HPC cluster will likely not let you use a Docker container directly) and run `targets::tar_make()` to build the pipeline.

By default, stores and output directories are reserved for code run from outside the container, but the outputs and target objects computed from within the container can be accessed in container_output and container_stores directories. To change this behavior, remove the directories you want to share between the host and the container from the .dockerignore file, and change the host paths in compose.yaml accordingly.

# Browsing code

## Pipeline definition

- parameters.yaml and $IS_SLURM define how the pipeline is going to run
- $TAR_PROJECT defines what will run (select the project, either all steps or the shortcut)
- `target_list`, defined in functions_pipeline_control.R, builds the list of targets that correspond to the selected project

## Steps of the analyses

Functions that start with step_ are defined in the corresponding .R files. They define the target lists for each step of the analyses. These targets most often call custom functions defined in functions_ files. Outputs of the targets are store in the target store for the corresponding project and can be looked up with `tar_read`.

Using your IDE tools (e.g. for Rstudio, the F2 key can be used to go to a function's definition), you can browse back in the function definitions to access the code parts you're interested in.

## Documentation

Function docstrings can be built with `devtools::document`, which enables the `?function` syntax for any function with an available docstring within the project (work in progress). See for example `?fill_absences`.


