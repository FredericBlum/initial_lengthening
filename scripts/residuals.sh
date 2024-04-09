#!/bin/bash
#SBATCH --cpus-per-task 4
#SBATCH --mem-per-cpu 120G
#SBATCH -J residuals
#SBATCH --output=%x.%j.out
#SBATCH --error=%x.%j.err
#SBATCH --partition dlcegpu
#SBATCH -w dlcenode04

xvfb-run Rscript --verbose test_moran.R
