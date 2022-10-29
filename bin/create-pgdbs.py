#!/usr/bin/env python3

from sys import stdin, stdout, stderr
import pmn
import argparse as ap
import subprocess

par = ap.ArgumentParser(description = 'Runs PathoLogic to create the PGDBs specified in the pgdb table or the --orgids argument if present')
par.add_argument('-o', '--orgids', help = 'A comma-separated list of orgids to run. If not given, all organisms in the pgdb-table will be created', dest = 'o')
par.add_argument('-c', '--config', default='pgdb-pipeline.txt', help = 'Config file. Will have the location of the pgdb-table if not overridden using -t', dest = 'c')
par.add_argument('-t', '--pgdb-table', help = 'Table file of PGDBs. If not given the filename will be read from the config file', dest = 't')

args = par.parse_args()

config = pmn.read_var_val_file(args.c)

try:
	if args.o:
		org_list = args.o.split(',')
	else:
		if args.t:
			tablefile = args.t
		else:
			tablefile = config['proj-pgdb-table']
			print(f'Info: Got name of table file from {args.c}: {tablefile}')
		ptable = pmn.read_pgdb_table(tablefile)
		org_list = list(ptable.keys())
	print(f'Info: Got list of orgids from {"command-line args" if args.o else tablefile}, will run PathoLogic on the following orgids: {", ".join(org_list)}')
	ptools = config['ptools-exe']
	print(f'Info: Pathway Tools executable is at {ptools}')
	masters_folder = config['proj-masters-dir']
	print(f'Info: Looking for PGDB master files in {masters_folder}\n')
	for orgid in org_list:
		log = open(f'{masters_folder}/{orgid}/create-{orgid}.log', 'w')
		cmd = [ptools, '-patho', f'{masters_folder}/{orgid}/', '-disable-metadata-saving', '-no-web-cel-overview', '-no-cel-overview']
		cmd_str = ' '.join(cmd)
		print(cmd_str)
		log.write(cmd_str + '\n')
		patho_process = subprocess.Popen(cmd, stdout=subprocess.PIPE)
		for line in patho_process.stdout:
			log.buffer.write(line)
			print(line.decode('UTF-8').rstrip())
except KeyError as e:
	stderr.write(f'{args.c}: Required variable {e.args[0]} not found')
	exit(1)
