# Code for Land use intensity destabilizes bird population variability beyond the impact of species population trends

This repository is organized as a [targets](https://books.ropensci.org/targets/) pipeline that can be run fully or only step-wise, depending on the computing power of your working environment. Especially, the local time series trend models runs for several thousands of time series and should therefore only be run on a machine that can either be left alone for several days (for sequential computing), or that can spawn several dozens of parallel worker processes (most likely a HPC cluster).

# Repository structure

- `data`: extract raw data from the archive here
- `processed`: extract processed data from the archive here
- `R`, `scripts` : custom R files and scripts called within the pipeline, sorted by subject or main functionality they relate to
- `renv`, `_targets` : directories used by `renv`, the R package version manager and by `targets`, the pipeline management package. Best not modified by hand.
- `slurm` : job files and utility scripts to run the pipeline on a HPC cluster managed by [SLURM](https://slurm.schedmd.com/overview.html).

# Reproducing the analyses

## On a local desktop or laptop : from local trends summary statistics to manuscript figures

```r
renv::restore() # setup R packages, might take several minutes

```

## On a more powerful machine : from raw survey data to manuscript figures

