#!/bin/bash
#SBATCH --cpus-per-task 2
#SBATCH --mem-per-cpu 220G
#SBATCH -J residuals
#SBATCH --output=%x.%j.out
#SBATCH --error=%x.%j.err
#SBATCH -w dlcenode09

xvfb-run Rscript --verbose test_moran.R
