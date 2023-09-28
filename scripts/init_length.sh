#!/bin/bash
#SBATCH --cpus-per-task 8
#SBATCH --mem-per-cpu 64G
#SBATCH -J init_length
#SBATCH --output=%x.%j.out
#SBATCH --error=%x.%j.err

Rscript --verbose 05_finalModel_tiny.R
