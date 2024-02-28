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
def create_pgdbs(config, table, org_list = None, ptools = None):
	try:
		if org_list is None:
			org_list = table.keys()
		#ptools = config['ptools-exe']
		print(f'Info: Pathway Tools executable is at {ptools}')
		masters_folder = config['proj-masters-dir']
		print(f'Info: Looking for PGDB master files in {masters_folder}\n')
		if ptools is None:
			ptools = pmn.PMNPathwayTools(config)
		print(f'ptools: {ptools}')
		for orgid in org_list:
			org_path = path.join(masters_folder, orgid, '')
			entry = table[orgid]
			#log = open(path.join(org_path, f'create-{orgid}.log'), 'w')

			#cmd = [ptools, '-patho', org_path, '-disable-metadata-saving', '-no-web-cel-overview', '-no-cel-overview']
			#cmd_str = ' '.join(cmd)
			#print(cmd_str)
			#log.write(cmd_str + '\n')
			#patho_process = subprocess.Popen(cmd, stdout=subprocess.PIPE)
			#for line in patho_process.stdout:
			#	log.buffer.write(line)
			#	print(line.decode('UTF-8').rstrip())
			patho_cmd = f'(batch-pathologic-startup-fn "{entry["Version"]}" "{org_path}" :hole-filler? nil :complex-inference? nil :operon-predictor? nil :tip? nil :do-overview? nil :web-cel-ov? nil :download-publications? t :dump-flat-files-biopax? nil :taxonomic-pruning? t :blast-data? t :omit-name-matching? nil)'
			patho_cmd = f'(batch-pathologic "{entry["Version"]}" "{org_path}" :hole-filler? nil :complex-inference? nil :operon-predictor? nil :tip? nil :do-overview? nil :web-cel-ov? nil :download-publications? t :dump-flat-files-biopax? nil :taxonomic-pruning? t :blast-data? t :omit-name-matching? nil :replace-organism? t :private-org-counter? t :suppress-metadata-saving? t)'
			pmn.info(patho_cmd)
			ptools.send_cmd(patho_cmd, handle_errs = False)
	except KeyError as e:
			stderr.write(f'Required variable {e.args[0]} not found')
			exit(1)
if __name__ == "__main__":
	main()
