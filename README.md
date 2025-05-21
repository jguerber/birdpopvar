# Code for Land use intensity destabilizes bird population variability beyond the impact of species population trends

This repository is organized as a [targets](https://books.ropensci.org/targets/) pipeline that can be run fully or only step-wise, depending on the computing power of your working environment. Especially, the local time series trend models runs for several thousands of time series and should therefore only be run on a machine that can either be left alone for several days (for sequential computing), or that can spawn several dozens of parallel worker processes (most likely a HPC cluster).

# Repository structure

- `data`: extract raw data from the archive here
- `processed`: extract processed data from the archive here
- `R`, `scripts` : custom R files and scripts called within the pipeline, sorted by subject or main functionality they relate to
- `renv`, `stores` : directories used by `renv`, the R package version manager and by `targets`, the pipeline management package. Best not modified by hand.
- `slurm` : job files and utility scripts to run the pipeline on a HPC cluster managed by [SLURM](https://slurm.schedmd.com/overview.html).

# Reproducing the analyses

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

 - local_n_cores : number of available cores when the pipeline is running on a machine when the environment variable $IS_SLURM is undefined of not set to `"TRUE"`
 - remote_n_tasks : maximum number of crew workers to spawn with `crew_controller_slurm`. Outdated targets will be delegated in parallel to these workers when they are available (i.e. when your job scheduler runs them)
 - remote_r_version : R version string to pass as `module load R/4.x.x` in SLURM job scripts 
