#!/bin/bash

#SBATCH -p std
#SBATCH -n 1
#SBATCH --time=00:10:00
#SBATCH -J pipeline-main
#SBATCH -o stdout.txt
#SBATCH -e stderr.txt

module load R/4.4.1

cd $SLURM_SUBMIT_DIR

PROJECT='birdpopvar'

# renv config : set renv paths to sensible behavior
export RENV_PATHS_ROOT=$HOME/.cache/R/renv/

# make sure the global R installation is accessible :
# disable sandbox and point to correct path
export RENV_CONFIG_SANDBOX_ENABLED=FALSE
export GLOBAL_LIBRARY="/softs/apps/R/4.4.1/gcc/lib64/R/library"

# setup directories on /scratch
mkdir -p $SCRATCH/$PROJECT

# push code to scratch
rsync -azP --exclude={'output','_targets','.env','.Renviron'} ./* $SCRATCH/$PROJECT

# go there and execute
cd $SCRATCH/$PROJECT

/bin/bash $(which R) CMD BATCH --no-save Scripts/build.R

# pull output folders to $HOME/$PROJECT
cp build.Rout $SLURM_SUBMIT_DIR
cp std* $SLURM_SUBMIT_DIR
rsync -azP output $SLURM_SUBMIT_DIR

exit 0
