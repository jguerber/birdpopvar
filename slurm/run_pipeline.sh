#!/bin/bash

#SBATCH -p std
#SBATCH -n 1
#SBATCH --time=04:00:00
#SBATCH -J pipeline-main
#SBATCH -o stdout.txt
#SBATCH -e stderr.txt

module load R/4.4.1

cd $SLURM_SUBMIT_DIR

PROJECT='birdpopvar'
LIB_NAME='birdpopvar-17b47494'

# project config : we use environment variables to distinguish on which type of
# machine the project is running
export IS_SLURM=TRUE

# renv config : set renv paths to sensible behavior
export RENV_PATHS_ROOT=$HOME/.cache/R/renv/
export RENV_PATHS_LIBRARY=$HOME/.cache/R/renv/library/$LIB_NAME

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

/bin/bash $(which R) CMD BATCH --no-save scripts/build.R

# pull output folders to $HOME/$PROJECT
cp build.Rout $SLURM_SUBMIT_DIR
cp std* $SLURM_SUBMIT_DIR
rsync -azP output $SLURM_SUBMIT_DIR

exit 0
