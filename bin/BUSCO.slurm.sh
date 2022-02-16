#!/bin/bash

#SBATCH -J BUSCO
#SBATCH --partition DPB
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=5000
##SBATCH --share
#SBATCH --time=1000:00:00
#SBATCH --mail-type=FAIL

module load BUSCO

busco_dir='/Carnegie/DPB/Data/Shared/Labs/Rhee/Private/PMN/pgdb-pipeline/busco'

busco \
	-i $1 \
	-o $1.busco \
	-m protein \
	-l $busco_dir/eukaryota_odb10 \
	-f
