#!/bin/bash
##SBATCH -N 1
#SBATCH --job-name=dDSM
#SBATCH --ntasks=1
#SBATCH --partition=general
#SBATCH --time=02-00:00:00
#SBATCH --array=1-120
#SBATCH --output=./SLURMOUT/slurm_log_%A-%a.out

module add r/4.1.0
module add gcc/6.3.0
R CMD BATCH "--no-save" dDSM_indep_test.R ./Rlogfiles/dDSM_testingArray_$SLURM_ARRAY_TASK_ID.Rout

