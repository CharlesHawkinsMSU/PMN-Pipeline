import sys, csv, re, os
import argparse
import pandas as pd

#use this script to run BUSCO on fasta files located in one directory, and parse out the BUSCO completeness
#score and return a dataframe containing the fasta file along with their BUSCO completeness scores
#this should work mostly on local desktop macs

parser = argparse.ArgumentParser(description='Copy all corresponding PGDB flat files from ptools_local to pgdb_masters.')
parser.add_argument('--input_fasta_folder', action='store', type = str, help='location of fasta files')
parser.add_argument('--input_lineage_folder', action='store', type = str, help='location of lineage folder')
parser.add_argument('--location_of_BUSCO', action='store', type = str, help='location of run_BUSCO.py')
parser.add_argument('--output', action = 'store', type = str, help='name of tab delimited output file with BUSCO completeness scores')

def run_BUSCO(fasta_folder, lineage_folder, output_folder, BUSCO):
	#os.system("mkdir " + output_folder)
	os.chdir(output_folder)
	all_fasta = []
	fasta_path = ""

	for (dirpath, dirnames, filenames) in os.walk(fasta_folder):
		all_fasta = filenames
		fasta_path = dirpath

	for fasta in all_fasta:
		if "." == fasta[0]:
			os.system("rm " + fasta_folder + "/" + fasta)
			continue
		run_string = "python " + BUSCO + " -i " + fasta_path + "/" + fasta + " -o BUSCO_" + fasta + " -l " + lineage_folder + " -m prot"
		os.system(run_string)

	BUSCO_score_dic = {}
	for fasta in all_fasta:
		BUSCO_score_dic[fasta] = get_BUSCO_score(fasta, output_folder, BUSCO)
	
	BUSCO_score_df = pd.DataFrame.from_dict(BUSCO_score_dic, orient='index')
	BUSCO_score_df.to_csv("BUSCO_completeness_score.txt", sep='\t')

def get_BUSCO_score(fasta_file, output_folder, BUSCO):
	for (dirpath, dirnames, filenames) in os.walk(output_folder):
		if fasta_file in dirpath:
			if any("short_summary" in files for files in filenames):
				busco_outfile = dirpath + "/short_summary_BUSCO_" + fasta_file + ".txt"
				with open(busco_outfile, "r") as readFile:
					readLine = readFile.readlines()
					for lines in readLine:
						if "C:" in lines:
							start = lines.find("C:")
							end = lines.find("%")
							BUSCO_score = float(lines[start + 2:end])
							return(BUSCO_score)

run_BUSCO("/Users/axu/Desktop/pmn14_fastas", "/Applications/busco-master/eukaryota_odb9/", \
	"/Users/axu/Desktop/pmn14_BUSCO", "/Applications/busco-master/scripts/run_BUSCO.py")