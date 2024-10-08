#!/usr/bin/env python3

from sys import stdin, stdout, stderr
import argparse as ap
import os
from os import path
import shutil
import fix_ncbi_taxon
import pmn

def main():
	par = ap.ArgumentParser(description = 'Prepares pgdbs for running SAVI. Should be run after PathoLogic')

	pmn.add_standard_pmn_args(par, 'prepared for SAVI')
	par.add_argument('-l', '--old-pgdbs', help = 'Folder containing the previous versions of the pgdbs. Optional; SAVI uses this only for giving stats', dest = 'l')
	args = par.parse_args()

	(config, orgtable, orglist) = pmn.read_pipeline_files(args)
	pmn.verbose = args.v
	savi_prepare(config, orgtable, orglist, args.p, args.l)


def savi_prepare(config, orgtable, orglist = None, proj = '.', old_pgdbs = None):
	try:
		# The general SAVI input and output directories
		in_dir = path.join(config['proj-savi-dir'], 'input')
		out_dir = path.join(config['proj-savi-dir'], 'output')
		pmn.info(f'SAVI project input dir: {in_dir}')
		pmn.info(f'SAVI project output dir: {out_dir}')

		# For each database
		for org in orglist:
			pmn.info(f'Preparing {org} for SAVI')
			org_path = path.join(config['proj-masters-dir'], org)
			pmn.info(f'Orgid path is {org_path}')
			try:
				entry = orgtable[org]
			except KeyError:
				pmn.error(f'Orgid {org} not found in {config["proj-pgdb-table"]}')
				exit(1)

			# Find the PGDB version to use
			pgdb_path = path.join(config['ptools-pgdbs'], org.lower() + 'cyc') 
			pmn.info(f'{org}Cyc is located at {pgdb_path}')
			org_version = open(path.join(pgdb_path, 'default-version'), 'r').read().rstrip()
			pmn.info(f'Current version of {org} is {org_version} (usually this is 1.0 until pmn-pipeline newversion is run)')

			# Create the subdirs in input and output for this orgid
			org_in_dir = path.join(in_dir, org)
			org_out_dir = path.join(out_dir, org)
			os.makedirs(org_in_dir, exist_ok = True)
			os.makedirs(org_out_dir, exist_ok = True)

			# Copy the PF file output by E2P2 to the input folder
			pmn.info(f'Copying PF file for {org} ({entry["PF File"]})')
			shutil.copy(entry['PF File'], org_in_dir)

			# Copy the relevant .dat files to the input folder
			pmn.info(f'Copying datfiles for {org}')
			pgdb_dat_path = path.join(pgdb_path, org_version, 'data')
			try:
				shutil.copy(path.join(pgdb_dat_path, 'species.dat'), org_in_dir)
				shutil.copy(path.join(pgdb_dat_path, 'pathways.dat'), org_in_dir)
				shutil.copy(path.join(pgdb_dat_path, 'reactions.dat'), org_in_dir)
				shutil.copy(path.join(pgdb_dat_path, 'proteins.dat'), org_in_dir)
			except FileNotFoundError as e:
				pmn.error(f'{e.filename}: File not found. Did you remember to run pmn-pipeline dump?')
				exit(1)

			# Fix the species.dat file so it includes the NCBI taxon ID
			# This step probably isn't needed any more as ptools seems to have fixed the bug it was intended to work around
			pmn.info(f'Checking if {org} is missing NCBI taxon ID, will fix if needed')
			fix_ncbi_taxon.fix_file(path.join(org_in_dir, 'species.dat'))

			# Copy the old version's pathways.dat if requested
			if old_pgdbs:
				old_pgdb_path = path.join(old_pgdbs, org.lower() + 'cyc') 
				old_org_version = open(path.join(old_pgdb_path, 'default-version'), 'r').read().rstrip()
				pmn.info(f'Copying old pathways.dat from {org} {old_org_version}')
				shutil.copy(path.join(old_pgdb_path, old_org_version, 'data', 'pathways.dat'), path.join(org_in_dir, 'pathways_pgdb.dat'))
			
	except KeyError as e:
		pmn.error(f'{args.c}: Required variable {e.args[0]} not found in config file')
		exit(1)
	except IOError as e:
		pmn.error(f'{e.filename}: {e.strerror}')
		exit(1)
if __name__ == "__main__":
    main()
