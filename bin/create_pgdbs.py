#!/usr/bin/env python3

from sys import stdin, stdout, stderr
import pmn
import argparse as ap
import subprocess
from os import path

def main():
	par = ap.ArgumentParser(description = 'Runs PathoLogic to create the PGDBs specified in the pgdb table or the --orgids argument if present')

	pmn.add_standard_pmn_args(par, 'created')

	args = par.parse_args()

	(config, table, org_list) = pmn.read_pipeline_files(args)
	create_pgdbs(config, table, org_list)
def create_pgdbs(config, table, org_list = None):
	try:
		if org_list is None:
			org_list = table.keys()
		ptools = config['ptools-exe']
		print(f'Info: Pathway Tools executable is at {ptools}')
		masters_folder = config['proj-masters-dir']
		print(f'Info: Looking for PGDB master files in {masters_folder}\n')
		for orgid in org_list:
			org_path = path.join(masters_folder, orgid)
			log = open(path.join(org_path, f'create-{orgid}.log'), 'w')
			cmd = [ptools, '-patho', org_path, '-disable-metadata-saving', '-no-web-cel-overview', '-no-cel-overview']
			cmd_str = ' '.join(cmd)
			print(cmd_str)
			log.write(cmd_str + '\n')
			patho_process = subprocess.Popen(cmd, stdout=subprocess.PIPE)
			for line in patho_process.stdout:
				log.buffer.write(line)
				print(line.decode('UTF-8').rstrip())
	except KeyError as e:
			stderr.write(f'Required variable {e.args[0]} not found')
			exit(1)
if __name__ == "__main__":
	main()
