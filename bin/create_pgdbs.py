#!/usr/bin/env python3

from sys import stdin, stdout, stderr
import pmn
import argparse as ap
from os import path

def main():
	par = ap.ArgumentParser(description = 'Runs PathoLogic to create the PGDBs specified in the pgdb table or the --orgids argument if present')

	pmn.add_standard_pmn_args(par, 'created')

	args = par.parse_args()

	(config, orgtable, orglist) = pmn.read_pipeline_files(args)
	create_pgdbs(config, orgtable, orglist, args)

def create_pgdbs(config, orgtable, orglist, args, ptools = None):
	try:
		print(f'Info: Pathway Tools executable is at {ptools}')
		masters_folder = config['proj-masters-dir']
		print(f'Info: Looking for PGDB master files in {masters_folder}\n')
		if ptools is None:
			ptools = pmn.PMNPathwayTools(config)
		print(f'ptools: {ptools}')
		for org in orglist:
			entry = orgtable[org]
			pgdb_folder = pmn.get_pgdb_folder(org, orgtable, config)
			if args.f and path.exists(pgdb_folder):
				pmn.info(f'Org {org} {entry["Version"]} already exists at {pgdb_folder}; skipping creation of this org')
				continue
			org_path = path.join(masters_folder, org, '')
			use_meta = entry['Also MetaCyc']
			patho_cmd = f'(batch-pathologic "{entry["Version"]}" "{org_path}" :hole-filler? nil :complex-inference? nil :operon-predictor? nil :tip? nil :do-overview? nil :web-cel-ov? nil :download-publications? t :dump-flat-files-biopax? nil :taxonomic-pruning? t :blast-data? t :omit-name-matching? nil :replace-organism? t :private-org-counter? t :suppress-metadata-saving? t :import-org-proteins-from-metacyc {"t" if use_meta else "NIL"} :import-org-pathways-from-metacyc {"t" if use_meta else "NIL"})'
			pmn.info(patho_cmd)
			ptools.send_cmd(patho_cmd, handle_errs = False)
	except KeyError as e:
			pmn.error(f'Required variable {e.args[0]} not found')
			exit(1)
if __name__ == "__main__":
	main()
