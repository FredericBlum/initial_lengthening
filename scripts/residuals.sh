#!/bin/bash
#SBATCH --cpus-per-task 2
#SBATCH --mem-per-cpu 200G
#SBATCH -J residuals
#SBATCH --output=%x.%j.out
#SBATCH --error=%x.%j.err
#SBATCH --partition dlcegpu
#SBATCH -w dlcenode01

Rscript --verbose 07_moran.R
