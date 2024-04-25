#!/bin/bash
#SBATCH --cpus-per-task 8
#SBATCH --mem-per-cpu 20G
#SBATCH -J init_length
#SBATCH --output=%x.%j.out
#SBATCH --error=%x.%j.err
#SBATCH --partition dlcegpu
#SBATCH -w dlcenode02

Rscript --verbose 05_ModelConvergence.R
