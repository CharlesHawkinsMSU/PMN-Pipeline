import sys, csv, re, os
import argparse

#walk through all directories in pgdb_masters and copy all corresponding PGDB flat files from ptools_local
#to make savitization step faster

#Author: Angela July 26, 2019

parser = argparse.ArgumentParser(description='Copy all corresponding PGDB flat files from ptools_local to pgdb_masters.')
parser.add_argument('--input_ptools_local', action='store', type = str, help='location of ptools_local')
parser.add_argument('--input_pgdb_masters', action='store', type = str, help='location of pgdb-masters')

args = parser.parse_args()

def walk_pgdb_masters(input_pgdb_masters, input_ptools_local):
	all_species = []
	pgdb_creation_package_location = input_pgdb_masters[:input_pgdb_masters.find("pgdb-creation-package/") + 22]
	for (dirpath, dirnames, filenames) in os.walk(input_pgdb_masters):
		if "pgdb-masters/" in dirpath:
			if dirpath.find("/", dirpath.find("pgdb-masters/") + 13) == -1:
				all_species.append(dirpath[dirpath.find("pgdb-masters/") + 13:])
	for species_name in all_species:
		if species_name == "":
			continue
		pgdb_data_location = input_ptools_local + "pgdbs/user/" + species_name.lower() + "cyc"
		for (dirpath, dirnames, filenames) in os.walk(pgdb_data_location):
			if "data" in dirpath:
				os.system("mkdir " + pgdb_creation_package_location + "savi/input/" + species_name)
				os.system("mkdir " + pgdb_creation_package_location + "savi/output/" + species_name)
				os.chdir(dirpath)
				os.system("cp pathways.dat proteins.dat reactions.dat species.dat " + pgdb_creation_package_location + "savi/input/" + species_name)
				for (dirpath, dirnames, filenames) in os.walk(pgdb_creation_package_location + "pgdb-masters/" + species_name):
					for files in filenames:
						if ".pf" in files:
							os.system("cp " + dirpath + "/" + files + " " + pgdb_creation_package_location + "savi/input/" + species_name)

#walk_pgdb_masters("/Carnegie/DPB/Data/Shared/Labs/Rhee/Everyone/Angela/for_Jiun/pmn14_9_species/pgdb-creation-package/pgdb-masters", "/home/axu/pathway-tools-23.0/ptools-local/pgdbs/user/")
walk_pgdb_masters(args.input_pgdb_masters, args.input_ptools_local)