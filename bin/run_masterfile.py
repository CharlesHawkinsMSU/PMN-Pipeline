#!/usr/bin/env python3

from sys import stdin, stdout, stderr
import pmn
import argparse as ap
from os import path

def main():
	par = ap.ArgumentParser(description = 'Runs one .master file from the PMN pipeline')
	pmn.add_standard_pmn_args(par)
	par.add_argument('masterfile', help = 'Which .master file to run')
	args = par.parse_args()
	(config, orgtable, orglist) = pmn.read_pipeline_files(args)
	run_masterfile(config, orgtable, orglist, args.masterfile)

def run_masterfile(config, orgtable, orglist = None, masterfile = None, ptools = None):
	if masterfile is None:
		stderr.write('A masterfile is required\n')
		exit(1)
	if orglist is None:
		orglist = orgtable.keys()
	pmn.info(f'Running masterfile {masterfile} for orgids: {", ".join(orglist)}')
	if ptools is None:
		ptools = pmn.PMNPathwayTools(config)
	for org in orglist:
		org_path = path.join(config['proj-masters-dir'], org)
		pmn.info(f'Orgid path is {org_path}')
		masterfile_path = path.join(org_path, masterfile = '.master')
		masterfile_vars = {}
		try:
			for line in open(masterfile_path):
				line = line.rstrip()
				run_masterscript_line(line, masterfile_vars)
		except IOError as e:
			stderr.write(f'{masterfile_path}: {e.strerror}\n')
			exit(1)

def run_masterscript_line(line, mvars):


if __name__ == "__main__":
	exit(main())
