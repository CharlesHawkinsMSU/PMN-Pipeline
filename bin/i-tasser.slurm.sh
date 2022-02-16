#!/bin/bash

#SBATCH -J I-TASSER
#SBATCH --partition DPB
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=5000
##SBATCH --share
#SBATCH --time=1000:00:00
#SBATCH --mail-type=FAIL

i_tasser_dir=/home/wdwyer/Will/I-Tasser/I-TASSER5.1/I-TASSERmod
i_tasser_lib=/home/wdwyer/Will/I-Tasser/ITLIB
 
#module load Python/3.6.1-tf

export TMPDIR="./$SLURM_JOB_ID"
perl $i_tasser_dir/runI-TASSER.pl -libdir $i_tasser_lib  -datadir $1 -seqname $2 -homoflag real -light true -EC true  






