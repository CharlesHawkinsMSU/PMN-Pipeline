import sys, csv, re, os
import argparse

#creates a tab delimited text file of number of pathways, reactions, compounds, proteins found in each pgdb to compare each
#PMN run

#Author: Angela July 26, 2019

parser = argparse.ArgumentParser(description='List all pathways found in PGDBs to a text file. Full pathas must be given.')
parser.add_argument('--input', action='store', type = str, help='location of *.dat files (example:/var/ptools/21.5/ptools-local/)')
parser.add_argument('--output', action='store', type = str, help='where to print out text file')

args = parser.parse_args()

def get_counts(directory_of_interest, write_outfile, flat_file_list):
	with open(write_outfile, "a+") as newFile:
		tab = "\t"
		tab = tab.join(flat_file_list)
		newFile.write("\t" + tab + "\n")

	dictionary_of_all_counts = {}
	for (dirpath, dirnames, filenames) in os.walk(directory_of_interest):
		if "plantcyc" in dirpath:
			continue
		if "/data" in dirpath:
			#to get the name of the pgdb first find where "/data" is in the dirpath and make a substring of that dirpath,
			#the text between the last two "/" in that substring should contain the pgdb name
			temp_dirpath = dirpath[:dirpath.find("/data")]
			end = temp_dirpath.rfind("/")
			temp_dirpath = dirpath[:end]
			start = temp_dirpath.rfind("/")
			cyc_name = temp_dirpath[start + 1:end]
			count = 0
			for flat_file in flat_file_list:
				with open(dirpath + "/" + flat_file + ".dat", "rb") as readFile:
					contents = readFile.readlines()
					for lines in contents:
						if "UNIQUE-ID - " in str(lines):
							count = count + 1
					if cyc_name not in dictionary_of_all_counts:
						dictionary_of_all_counts[cyc_name] = [str(count)]
						continue
					else:
						dictionary_of_all_counts[cyc_name].append(str(count))
	for cyc in dictionary_of_all_counts:
		tab = "\t"
		tab = tab.join(dictionary_of_all_counts[cyc])
		each_line = cyc + "\t" + tab
		with open(write_outfile, "a+") as newFile:
			newFile.write(each_line + "\n")


#get_counts("/var/ptools/23.0/ptools-local/pgdbs/user/", "/Carnegie/DPB/Data/Shared/Labs/Rhee/Private/PMN/pmn_release_prep/pmn14_release_prep/pgdb-creation-package/update_savi_23/temp.txt", ["pathways", "reactions", "compounds", "proteins"])
get_counts(args.input, args.output, ["pathways", "reactions", "compounds", "proteins"])