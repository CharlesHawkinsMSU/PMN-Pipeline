### $1 is the plantcyc protein info file, $2 is the subset pgdb fasta sequence file. Provide the file names at command line. 

perl mksubset_pgdb_none.pl $1 $2 > $1.step1
perl parsing_uniprot_id.pl $1.step1 $1 > $1.step2
perl mkfasta_list.pl $1.step2 ../pmn8/uniprot_seq/uniprot_sprot_plants.dat ../pmn8/uniprot_seq/uniprot_trembl_plants.dat > $1.step3
perl mkuniprot_fasta.pl $1.step2 $1 $1.step3 > $1.uniprot.final
