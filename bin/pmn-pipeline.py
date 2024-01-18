#!/usr/bin/env python3

from sys import stdin, stdout, stderr
from os import path
import os
from shutil import copytree, copy2
import argparse as ap
import subprocess
import pmn
import create_pgdbs
import savi_prepare
import refine_prepare
import create_authors
import create_savi_citations

stages = ['precheck', 'e2p2', 'revise', 'prepare', 'create', 'dump', 'savi-prepare', 'savi', 'refine-prepare', 'refine-a', 'refine-b', 'refine-c', 'checker', 'newversion', 'dump', 'dump-biopax', 'blastset']

par = ap.ArgumentParser(description = 'Used to run each stage of the PMN release pipeline')
pmn.add_standard_pmn_args(par, action='run')
par.add_argument('stage', nargs = '+', help = 'Which stage of the pipeline to run. Valid stages are: newproj, precheck, e2p2, prepare, create, dump, dump-biopax, savi, refine-a, refine-b, refine-c, newversion, overview, checker, blastset')

args = par.parse_args()
pmn.verbose = args.v

def run_stage(stage, config, table, orglist = None, proj = '.'):
	print(f'Running stage {stage}')
	script_path = path.dirname(path.realpath(__file__))
	stage = stage.lower()
	if stage == 'newproj' or stage == 'fixproj':
		if stage == 'newproj':
			print('==Creating new blank project==')
		else:
			print('==Adding any missing project files==')
		proj_tmpl_path = path.realpath(path.join(script_path, '..', 'project-template'))
		if not path.exists(proj):
			os.mkdir(proj)
		for filename in os.listdir(proj_tmpl_path):
			src = path.join(proj_tmpl_path, filename)	
			dst = path.join(proj, filename)
			if not path.exists(dst) or stage == 'newproj':
				if not path.isdir(src):
					copy2(src, dst)
		for dirname in ['e2p2', 'gff', 'fasta', 'maps-in', 'maps-out', 'pgdb-masters', 'savi/input', 'savi/output']:
			dst = path.join(proj, dirname)
			os.makedirs(dst, exist_ok = True)
		print('New project created: %s'%proj)
		return 0
	org_filename = config['proj-pgdb-table']
	config_filename = config['_filename']
	perl_scripts_path = path.realpath(path.join(script_path,  '..','perl_scripts'))

# Runs the specified function for each org in orglist, assuming each one has its own folder in basedir
	def for_each_org(fn, basedir, orglist):
		for org in orglist:
			fn(path.join(basedir, org))
	def check(result, source):
		if result:
			print(pmn.green_text(f'Checks passed for {source}'))
		else:
			print(pmn.red_text(f'One or more checks failed for {source}'))
		return result

	if stage == 'precheck':
		print(pmn.blue_text('==Running pre-checks=='))
		passed = True
		print(('\n==Checking runtime environment=='))
		passed &= check(pmn.check_env(), "runtime environment")
		print((f'\n==Checking pipeline config file {config_filename}=='))
		passed &= check(pmn.check_pipeline_config(config), config_filename)
		print((f'\n==Checking organism table {org_filename}=='))
		passed &= check(pmn.check_pgdb_table(orgtable, config), org_filename)
		par_prov = config.setdefault("parallelism", "none")
		print((f'\n==Checking parallelism provider {par_prov}=='))
		passed &= check(pmn.check_parallelism(config), f'parallelism provider {par_prov}')
		socket = config.setdefault('ptools-socket', '/tmp/ptools-socket')
		print((f'\n==Checking if an existing Pathway Tools API instance is already running=='))
		if path.exists(socket):
			stderr.write(f'Socket {socket} already exists; Pathway Tools may already be running in API mode. Please quit the Pathway Tools instance that is in API mode.\n')
			passed = False
		else:
			print(pmn.green_text('No existing Pathway Tools instance, check passed'))
		if passed:
			print(pmn.green_text('\nAll checks passed!'))
			exit(0)
		else:
			print(pmn.red_text('\nOne or more checks failed; please address the issues before continuing with the pipeline'))
			exit(1)
	elif stage == 'e2p2':
		print(pmn.blue_text('==Running E2P2=='))
		e2p2_exe = path.join(config['e2p2'], 'pipeline', 'run_pipeline.py')
		e2p2_cmds = []
		for orgid, org_entry in orgtable.items():
			cmd = [
					e2p2_exe,
					'--input', org_entry['Sequence File'],
					'--output', org_entry['Initial PF File'],
					'--blastp', 'blastp',
					'--java', 'java',
					'--priam_search', config['priam'],
					'--priam_profile', config['priam-profile'],
					'--rpsd', path.join(config['rpsd'], 'blastdb', 'rpsd-'+config['rpsd-version']+'.fasta'),
					]
			if config['parallelism'] == 'slurm':
				org_masters_dir = path.join(config['proj-masters-dir'], orgid)
				try:
					os.mkdir(org_masters_dir)
				except FileExistsError:
					pass
				slurm_script_path = path.join(org_masters_dir, 'E2P2.slurm.sh')
				slurm_script = f'''#!/bin/bash

#SBATCH -J E2P2_{orgid}
#SBATCH --partition DPB
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=5000
#SBATCH --time=1000:00:00
#SBATCH --mail-type=FAIL

'''
				try:
					slurm_script += f'module load {config["module-load"]}\n\n'
				except KeyError:
					pass
				slurm_script += ' '.join(cmd) + '\n'
				try:
					with open(slurm_script_path, 'w') as slurm_script_file:
						slurm_script_file.write(slurm_script)
				except IOError as e:
					stderr.write(f'Error creating {orgid} slurm script {slurm_script_path}: {e.strerror}\n')
					exit(1)

				cmd = ['sbatch', slurm_script_path]
			e2p2_cmds += [cmd]
		for e2p2_cmd in e2p2_cmds:
			print(' '.join(e2p2_cmd))
			e2p2_result = subprocess.run(e2p2_cmd)

	elif stage == 'prepare':
		print(pmn.blue_text('==Preparing master files=='))
		prepare_result = subprocess.run(['pgdb-prepare.pl', org_filename, config['proj-e2p2-dir'], config['proj-masters-dir'], config['ptools-exe']])
		if prepare_result.returncode != 0:
			stderr.write('Prepare script failed\n')
			exit(prepare_result.returncode)

	elif stage == 'create':
		print(pmn.blue_text('==Creating PGDBs with PathoLogic=='))
		create_pgdbs.create_pgdbs(config, orgtable)
	elif stage == 'revise':
		print(pmn.blue_text('==Revising E2P2 files with gene IDs=='))
		# run revise_pf.py
		# arg_col enumerates columns in the input table used for revise_pf.py and maps them to arguments to be given to revise_pf.py
		arg_col = [
				('-ig', 'GFF File'),
				('-if', 'Sequence File'),
				('-im', 'Map In'),
				('-om', 'Map Out'),
				('-r', 'PF File'),
				('-fs', 'FASTA Sep'),
				('-fg', 'FASTA Field'),
				('-fkv', 'FASTA KV'),
				('-gpf', 'GFF Prot Feature'),
				('-gpn', 'GFF Prot Name'),
				('-gk', 'GFF Key'),
				('-gg', 'GFF Path'),
				('-n', 'Numeric IDs'),
				('-gr', 'Gene Delete')
				]

		try:
			import revise_pf
		except ImportError:
			stderr.write('revise_pf.py not found\n')
			exit(1)
		for org, entry in orgtable.items():
			arglist = [entry['Initial PF File']+'.orxn.pf']
			for arg, col in arg_col:
				try:
					#arg_dict[arg] = entry[col]
					arglist.extend([arg, entry[col]])
				except KeyError as e:
					pass
			print(arglist)
			args = revise_pf.get_pf_args(arglist)
			revise_pf.main(args)
	elif stage == 'savi-prepare':
		print(pmn.blue_text('==Preparing files to run SAVI=='))
		savi_prepare.savi_prepare(config, orgtable, orglist, proj)
	elif stage == 'savi':
		print(pmn.blue_text('==Running SAVI=='))
		prev_wd = os.getcwd()
		os.chdir(proj)
		proj_dir = os.getcwd()
		in_dir = path.join(proj_dir, config['proj-savi-dir'], 'input')
		out_dir = path.join(proj_dir, config['proj-savi-dir'], 'output')
		pmn.info(f'Project SAVI input and output dirs are {in_dir} and {out_dir}')

		savi_dir = config['savi']
		pmn.info(f'Changing to savi program dir {savi_dir}')
		os.chdir(savi_dir)
		for org, entry in orgtable.items():
			org_savi_in_dir = path.join(in_dir, org)
			org_savi_out_dir = path.join(out_dir, org)
			savi_cmd = [path.join('.', 'runSAVI.sh'), org_savi_in_dir, org_savi_out_dir]
			pmn.info(f'Running SAVI for {org}Cyc: {" ".join(savi_cmd)}')
			subprocess.run(savi_cmd)
		pmn.info(f'SAVI finished, returning to previous directory {prev_dir}')
		os.chdir(prev_dir)
	elif stage == 'refine-prepare':
		print(pmn.blue_text('==Preparing for refine steps=='))
		refine_prepare.refine_prepare(config, orgtable, orglist)
	else:
# Stage wasn't any of the internally-defined stages, so it must refer to a .master file
		masterfile = stage
		if not masterfile.endswith('.master'):
			masterfile = masterfile + '.master'
		print(pmn.blue_text(f'==Running master file {masterfile}=='))
		masterscript = path.join(perl_scripts_path, 'pmn-release-pipeline-general.pl')
		for org in orgtable:
			stderr.write('Running %s on %s\n'%(masterfile, org))
			masterfilepath = path.join(config['proj-masters-dir'], org, masterfile)
			subprocess.run(['perl', masterscript, masterfilepath, masterfilepath + '.log'])
	return 0
if args.stage == ['newproj'] or args.stage == ['fixproj']:
	run_stage(args.stage[0], None, None, None, args.proj)
elif 'newproj' in args.stage:
	stderr.write('Error: The newproj command should only be run on its own\n')
	exit(1)
else:
	print(f'Running stages {args.stage}')
	(config, orgtable, orglist) = pmn.read_pipeline_files(args)
	config['_filename'] = args.c
	prev_stage = -1
	stage_list = []
	dash = False
	for stage_req in args.stage:
		if stage_req == '-':
			dash = True
		else:
			try:
				stage_n = stages.index(stage_req)
			except ValueError:
				stderr.write(f'Stage not recognized: {stage_req}\n')
				exit(1)
			if dash:
				if stage_n <= prev_stage:
					stderr.write(f'Stage range given in wrong order; {stage_req} comes before the stage you specified as the beginning of the range\n')
					exit(1)
				for i in range(prev_stage+1, stage_n+1):
					stage_list.append(i)
				dash = False
			else:
				stage_list.append(stage_n)
			prev_stage = stage_n
	if dash:
		stage_n = length(stages)
		for i in range(prev_stage+1, stage_n):
			stage_list.append(i)

	for stage_i in stage_list:
		run_stage(stages[stage_i], config, orgtable, orglist, args.proj)
		print()
