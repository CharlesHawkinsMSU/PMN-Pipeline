import sys, csv, re, os
import argparse
import pandas as pd

#creates a tab delimited text file of all pathways found in MetaCyc so it would be easier for curators to walk through all
#PGDBs in the most current/past release and compare existing pathways with MetaCyc to see which pathways need curation in SAVI

#Author: Angela July 26, 2019

parser = argparse.ArgumentParser(description='List all pathways found in PGDBs to a text file. Full pathas must be given.')
parser.add_argument('--input', action='store', type = str, help='location of pathways.dat files (example:/var/ptools/21.5/ptools-local/)')
parser.add_argument('--output', action='store', type = str, help='where to print out text file')

args = parser.parse_args()

#function list_all_pathways should take in the location of a directory and parse through all the pathways.dat files found within
#that listed directory. Then print a text file containing a list of the pathways and PGDBs the pathway was found in along with the 
#location of the directory at the bottom. 
def list_all_pathways(directory_of_interest, write_outfile):
	dictionary_of_all_pathways = {}
	for (dirpath, dirnames, filenames) in os.walk(directory_of_interest):
		if "/data" in dirpath:
			#to get the name of the pgdb first find where "/data" is in the dirpath and make a substring of that dirpath,
			#the text between the last two "/" in that substring should contain the pgdb name
			temp_dirpath = dirpath[:dirpath.find("/data")]
			end = temp_dirpath.rfind("/")
			temp_dirpath = dirpath[:end]
			start = temp_dirpath.rfind("/")
			cyc_name = temp_dirpath[start + 1:end]
			with open(dirpath + "/pathways.dat", "rb") as readFile:
				contents = readFile.readlines()
				for lines in contents:
					if "UNIQUE-ID - " in str(lines):
						unique_id = str(lines.decode("utf-8")).strip()
						pathway_id = unique_id[unique_id.find("UNIQUE-ID - ") + 12:]
						if pathway_id in dictionary_of_all_pathways:
							dictionary_of_all_pathways[pathway_id].append(cyc_name)
						else:
							dictionary_of_all_pathways[pathway_id] = [cyc_name]
	
	final_pwy_dic = list(dictionary_of_all_pathways.keys())
	final_pwy_df = pd.DataFrame(final_pwy_dic)
	final_pwy_df.to_csv(args.output, sep='\t', index=False, header=False)


#list_all_pathways("/var/ptools/21.5/ptools-local")
#list_all_pathways("/opt/ptools/21.5/aic-export/pgdbs/biocyc/metacyc", "/Carnegie/DPB/Data/Shared/Labs/Rhee/Everyone/Angela/for_Jiun/pmn14_9_species/pgdb-creation-package/update_savi/test.txt")
list_all_pathways(args.input, args.output)