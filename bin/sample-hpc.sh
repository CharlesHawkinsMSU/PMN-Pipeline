#!/bin/bash

#SBATCH -J $JOBNAME
#SBATCH --partition CLUSTER
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --share
#SBATCH --time=1000:00:00
#SBATCH --mail-type=FAIL

source /etc/profile
ulimit -t unlimited

srun $CMD
