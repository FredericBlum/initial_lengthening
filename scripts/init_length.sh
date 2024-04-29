#!/bin/bash
#SBATCH --cpus-per-task 2
#SBATCH --mem-per-cpu 240G
#SBATCH -J init_length
#SBATCH --output=%x.%j.out
#SBATCH --error=%x.%j.err
#SBATCH --partition dlcegpu
#SBATCH -w dlcenode01

Rscript --verbose 05_ModelConvergence.R
