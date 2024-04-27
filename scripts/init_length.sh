#!/bin/bash
#SBATCH --cpus-per-task 2
#SBATCH --mem-per-cpu 350G
#SBATCH -J init_length
#SBATCH --output=%x.%j.out
#SBATCH --error=%x.%j.err
#SBATCH -w dlcenode14

Rscript --verbose 05_ModelConvergence.R
