#!/bin/bash
#SBATCH --cpus-per-task 2
#SBATCH --mem-per-cpu 20G
#SBATCH -J residuals
#SBATCH --output=%x.%j.out
#SBATCH --error=%x.%j.err
#SBATCH --partition dlcegpu
#SBATCH -w dlcenode03

xvfb-run Rscript --verbose test_moran.R
