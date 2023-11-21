#!/bin/bash

#SBATCH -J PathoLogic
#SBATCH --partition DPB
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=5000
##SBATCH --share
#SBATCH --time=1000:00:00
#SBATCH --mail-type=FAIL

module load BUSCO

create_script='/Carnegie/DPB/Data/Shared/Labs/Rhee/Private/PMN/dev-pipeline/PMN-Pipeline/bin/create-pgdbs.py'

$create_script $*
