#!/bin/bash
#SBATCH --cpus-per-task 4
#SBATCH --mem-per-cpu 10G
#SBATCH -J init_length
#SBATCH --output=%x.%j.out
#SBATCH --error=%x.%j.err
#SBATCH --partition dlcegpu
#SBATCH -w dlcenode03

Rscript --verbose 04_FinalModel.R
