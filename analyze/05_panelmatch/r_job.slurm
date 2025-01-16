#!/bin/bash
#SBATCH --job-name=pm
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=300G
#SBATCH --time=12:00:00
#SBATCH --mail-type=fail
#SBATCH --mail-user=cb8007@princeton.edu

module purge
module load R/4.2.3

Rscript code/della.R
