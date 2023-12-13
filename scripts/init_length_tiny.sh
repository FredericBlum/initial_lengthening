#!/bin/bash
#SBATCH --cpus-per-task 8
#SBATCH --mem 10G
#SBATCH -J init_length
#SBATCH --output=%x.%j.out
#SBATCH --error=%x.%j.err
#SBATCH --partition dlcegpu
#SBATCH -w dlcenode04

Rscript --verbose 04_FinalModel_tiny.R
