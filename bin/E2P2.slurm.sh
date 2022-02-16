#!/bin/bash

#SBATCH -J E2P2
#SBATCH --partition DPB
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=5000
##SBATCH --share
#SBATCH --time=1000:00:00
#SBATCH --mail-type=FAIL

module try-load blast BLAST+ java

#module load Python/3.6.1-tf

proj_dir=$(pwd)
e2p2_dir=/Carnegie/DPB/Data/Shared/Labs/Rhee/Private/PMN/pgdb-pipeline/E2P2
python3 \
	${e2p2_dir}/pipeline/run_pipeline.py \
	--input ${proj_dir}/$1 \
	--output ${proj_dir}/$1.e2p2v4 \
	--blastp blastp \
	--java java \
	--priam_search ${e2p2_dir}/PRIAM_search.jar \
	--rpsd ${e2p2_dir}/rpsd_current/blastdb/rpsd-4.2.fasta \
	--priam_profile ${e2p2_dir}/rpsd_current/profiles

