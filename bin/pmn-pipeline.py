#!/usr/bin/env python3

import re
from sys import stdin, stdout, stderr
import sys
from os import path
import os
from shutil import copytree, copy2, rmtree
import argparse as ap
import subprocess
import pmn
import create_pgdbs
import savi_prepare
import refine_prepare
import create_authors
import create_savi_citations
import refine_c

# The standard sequence of stages, used when the user requests a range of stages:
stage_sequence = ['precheck', 'e2p2', 'revise', 'prepare', 'create', 'savi-dump', 'savi-prepare', 'savi', 'refine-prepare', 'refine-a', 'refine-b', 'refine-c', 'final-dump', 'blastset']
# Valid stages that are not part of the standard sequence:
stages_nonsequenced = set(['split', 'join', 'delete', 'dump', 'dump-biopax', 'checker'])
# Stages that require an instance of Pathway Tools to be running:
stages_needing_ptools = set(['create', 'dump', 'savi-dump', 'final-dump', 'refine-prepare', 'refine-a', 'refine-b', 'refine-c', 'checker', 'dump-biopax', 'blastset'])

par = ap.ArgumentParser(description = 'Used to run each stage of the PMN release pipeline')
pmn.add_standard_pmn_args(par, action='run')
par.add_argument('stage', nargs = '+', help = 'Which stage of the pipeline to run. Valid stages are: newproj, precheck, e2p2, prepare, create, dump, dump-biopax, savi, refine-a, refine-b, refine-c, newversion, overview, checker, blastset')
par.add_argument('-s', '--split-id', help = 'Which part of the split fasta(s) to run E2P2 on. Used when running in parallel after splitting each fasta into many parts. You need to have run the split stage with fa-split set to some number in the config file.', dest = 's')

args = par.parse_args()
pmn.verbose = args.v

def run_stage(stage, config, table, orglist = None, proj = '.', ptools = None, split_id = None):
	pmn.message(f'Running stage {stage}')
	if stage == 'savi-dump':
		stage = 'dump'
	elif stage == 'final-dump':
		run_stage('dump', config, table, orglist, proj, ptools)
		run_stage('dump-biopax', config, table, orglist, proj, ptools)
		return 0
	script_path = path.dirname(path.realpath(__file__))
	stage = stage.lower()
	if stage == 'newproj' or stage == 'fixproj':
		if stage == 'newproj':
			pmn.message('==Creating new blank project==')
		else:
			pmn.message('==Adding any missing project files==')
		proj_tmpl_path = path.realpath(path.join(script_path, '..', 'project-template'))
		if not path.exists(proj):
			os.mkdir(proj)
		for filename in os.listdir(proj_tmpl_path):
			src = path.join(proj_tmpl_path, filename)
			dst = path.join(proj, filename)
			if not path.exists(dst) or stage == 'newproj':
				if not path.isdir(src):
					copy2(src, dst)
		for dirname in ['e2p2', 'gff', 'fasta', 'maps-in', 'maps-out', 'pgdb-masters', 'savi/input', 'savi/output', 'blastsets', 'intermediate-pgdbs', 'common', 'sockets', 'pgdbs', 'logs']:
			dst = path.join(proj, dirname)
			os.makedirs(dst, exist_ok = True)
		pmn.message('New project created: %s'%proj)
		return 0
	org_filename = config['proj-pgdb-table']
	config_filename = config['_filename']
	perl_scripts_path = path.realpath(path.join(script_path,  '..','perl_scripts'))

	def check(result, source):
		if result:
			pmn.message(pmn.green_text(f'Checks passed for {source}'))
		else:
			pmn.message(pmn.red_text(f'One or more checks failed for {source}'))
		return result

	if stage == 'precheck':
		pmn.message(pmn.blue_text('==Running pre-checks=='))
		passed = True
		pmn.message(('\n==Checking runtime environment=='))
		passed &= check(pmn.check_env(), "runtime environment")
		pmn.message((f'\n==Checking pipeline config file {config_filename}=='))
		passed &= check(pmn.check_pipeline_config(config), config_filename)
		pmn.message((f'\n==Checking organism table {org_filename}=='))
		passed &= check(pmn.check_pgdb_table(orgtable, config), org_filename)
		par_prov = config.setdefault("parallelism", "none")
		pmn.message((f'\n==Checking parallelism provider {par_prov}=='))
		passed &= check(pmn.check_parallelism(config), f'parallelism provider {par_prov}')
		socket = config.setdefault('ptools-socket', '/tmp/ptools-socket')
		pmn.message((f'\n==Checking if an existing Pathway Tools API instance is already running=='))
		if path.exists(socket):
			pmn.error(f'Socket {socket} already exists; Pathway Tools may already be running in API mode. Please quit the Pathway Tools instance that is in API mode.')
			passed = False
		else:
			pmn.message(pmn.green_text('No existing Pathway Tools instance, check passed'))
		pmn.message('\n==Checking for AraCyc and PlantCyc==')
		aracyc_path = path.join(config['ptools-pgdbs'], 'aracyc')
		if path.exists(aracyc_path):
			pmn.message(pmn.green_text(f'Found AraCyc at {aracyc_path}'))
		else:
			pmn.message(pmn.red_text(f'No AraCyc found in {config["ptools-pgdbs"]}'))
			passed = False
		plantcyc_path = path.join(config['ptools-pgdbs'], 'plantcyc')
		if path.exists(plantcyc_path):
			pmn.message(pmn.green_text(f'Found PlantCyc at {plantcyc_path}'))
		else:
			pmn.message(pmn.red_text(f'No PlantCyc found in {config["ptools-pgdbs"]}'))
			passed = False
		if passed:
			pmn.message(pmn.green_text('\nAll checks passed!'))
			exit(0)
		else:
			pmn.message(pmn.red_text('\nOne or more checks failed; please address the issues before continuing with the pipeline'))
			exit(1)
	elif stage == 'e2p2':
		if split_id:
			from split_fasta import get_split_filename
			os.makedirs(path.join(config['proj-e2p2-dir'], 'splits'), exist_ok = True)

		pmn.message(pmn.blue_text('==Running E2P2=='))
		e2p2v5_exe = path.join(config['e2p2'], 'e2p2.py')
		e2p2v4_exe = path.join(config['e2p2'], 'pipeline', 'run_pipeline.py')
		if path.exists(e2p2v5_exe):
			e2p2_exe = e2p2v5_exe
		else:
			e2p2_exe = e2p2v4_exe
		e2p2_cmds = []
		for orgid in orglist:
			org_entry = orgtable[orgid]
			inpath = org_entry['Sequence File']
			outpath = org_entry['Initial PF File']
			if split_id:
				pmn.info(f'Running E2P2 on split {split_id} of {orgid}')
				infilename = path.split(inpath)[-1]
				inpre = path.join(config['proj-fasta-dir'], 'splits', infilename)
				inpath = get_split_filename(inpre, split_id)
				outfilename = path.split(outpath)[-1]
				outpre = path.join(config['proj-e2p2-dir'], 'splits', outfilename)
				outpath = get_split_filename(outpre, split_id)
			cmd = [
					sys.executable,
					e2p2_exe,
					'--input', inpath,
					'--output', outpath
					]
			if e2p2_exe is e2p2v4_exe:
				# Command-line options that were required for E2P2 v4 (in v5 this stuff is all in the config file)
				cmd += [
					'--blastp', 'blastp',
					'--java', 'java',
					'--priam_search', config['priam'],
					'--priam_profile', config['priam-profile'],
					'--rpsd', path.join(config['rpsd'], 'blastdb', 'rpsd-'+config['rpsd-version']+'.fasta'),
					]
			else: # For E2P2 v5
				# If the user specified an e2p2 config file, put that in the E2P2 args
				try:
					cmd += ['--config', config['e2p2-config']]
				except KeyError:
					pass
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
					pmn.error(f'Error creating {orgid} slurm script {slurm_script_path}: {e.strerror}')
					exit(1)

				cmd = ['sbatch', slurm_script_path]
			e2p2_cmds += [cmd]
		for e2p2_cmd in e2p2_cmds:
			pmn.info(' '.join(e2p2_cmd))
			e2p2_result = pmn.run_external_process(e2p2_cmd, procname = 'E2P2')
			if e2p2_result != 0:
				pmn.error('E2P2 failed')
				exit(e2p2_result)

	elif stage == 'prepare':
		pmn.message(pmn.blue_text('==Preparing master files=='))
		prepare_result = pmn.run_external_process([path.join(script_path, 'pgdb-prepare.pl'), org_filename, config['proj-e2p2-dir'], config['proj-masters-dir'], config['ptools-exe']], procname = 'Prepare')
		if prepare_result != 0:
			pmn.error('Prepare script failed')
			exit(prepare_result)

	elif stage == 'create':
		pmn.message(pmn.blue_text('==Creating PGDBs with PathoLogic=='))
		create_pgdbs.create_pgdbs(config, orgtable, orglist, ptools)
	elif stage == 'revise':
		pmn.message(pmn.blue_text('==Revising E2P2 files with gene IDs=='))
		# Run revise_pf.py
		# arg_col enumerates columns in the input table used for revise_pf.py and maps them to arguments to be given to revise_pf.py. It is constructed for each org from the arg_col_* vars based on which sources of gene ID were requested via the 'Genes From' column for that org
		arg_col_gen = [
				('-n', 'Numeric IDs'),
				('-gr', 'Gene Delete'),
				('-r', 'PF File'),
				('-om', 'Map Out'),
				]
		arg_col_fa = [
				('-if', 'Sequence File'),
				('-fs', 'FASTA Sep'),
				('-fg', 'FASTA Field'),
				('-fkv', 'FASTA KV'),
				]
		arg_col_gff = [
				('-ig', 'GFF File'),
				('-gpf', 'GFF Prot Feature'),
				('-gpn', 'GFF Prot Name'),
				('-gk', 'GFF Key'),
				('-gg', 'GFF Path'),
				]
		arg_col_map = [
				('-im', 'Map In'),
				]

		try:
			import revise_pf
		except ImportError:
			pmn.error('revise_pf.py not found')
			exit(1)
		for org in orglist:
			entry = orgtable[org]
			if path.exists(path.join(config['e2p2'], 'e2p2.py')):
				arglist = [re.sub(r'\.[^.]*$', '.MaxWeightAbsoluteThreshold.orxn.pf', entry['Initial PF File'])]
			else:
				arglist = [entry['Initial PF File']+'.orxn.pf']
			# Figure out where we're getting the gene IDs from and construct arg_col based on that
			genes_from = entry.setdefault('Genes From', '').lower().split(',')
			arg_col = []
			if 'fasta' in genes_from:
				arg_col.extend(arg_col_fa)
			if 'gff' in genes_from:
				arg_col.extend(arg_col_gff)
			if 'map' in genes_from:
				arg_col.extend(arg_col_map)
			if not arg_col:
				pmn.warn('No \'Genes From\' column for {org}, will not map proteins to genes')
				continue
			arg_col.extend(arg_col_gen)
			for arg, col in arg_col:
				try:
					#arg_dict[arg] = entry[col]
					arglist.extend([arg, entry[col]])
				except KeyError as e:
					pass
			pmn.info(f'Args to revise_pf: {arglist}')
			args = revise_pf.get_pf_args(arglist)
			revise_pf.main(args)
	elif stage == 'savi-prepare':
		pmn.message(pmn.blue_text('==Preparing files to run SAVI=='))
		savi_prepare.savi_prepare(config, orgtable, orglist, proj)
	elif stage == 'savi':
		pmn.message(pmn.blue_text('==Running SAVI=='))
		prev_wd = os.getcwd()
		os.chdir(proj)
		proj_dir = os.getcwd()
		in_dir = path.join(proj_dir, config['proj-savi-dir'], 'input')
		out_dir = path.join(proj_dir, config['proj-savi-dir'], 'output')
		pmn.info(f'Project SAVI input and output dirs are {in_dir} and {out_dir}')

		savi_dir = config['savi']
		pmn.info(f'Changing to savi program dir {savi_dir}')
		os.chdir(savi_dir)
		for org in orglist:
			entry = orgtable[org]
			org_savi_in_dir = path.join(in_dir, org)
			org_savi_out_dir = path.join(out_dir, org)
			savi_cmd = [path.join('.', 'runSAVI.sh'), org_savi_in_dir, org_savi_out_dir]
			pmn.info(f'Running SAVI for {org}Cyc: {" ".join(savi_cmd)}')
			pmn.run_external_process(savi_cmd, procname = 'SAVI')
		pmn.info(f'SAVI finished, returning to previous directory {prev_wd}')
		os.chdir(prev_wd)
	elif stage == 'refine-prepare':
		pmn.message(pmn.blue_text('==Preparing for refine steps=='))
		create_authors.create_frames(config, orgtable, orglist, ptools = ptools)
		create_savi_citations.create_savi_citations(config, orgtable, orglist, ptools = ptools)
		refine_prepare.refine_prepare(config, orgtable, orglist, ptools = ptools)
	elif stage == 'refine-b':
		pmn.message(pmn.blue_text('==Running Refine-B=='))
		if ptools is None:
			ptools = pmn.PMNPathwayTools(config)
		for org in orglist:
			entry = orgtable[org]
			ptools.so(org)
			refdbs = [entry['Reference DB']]
			if entry['Also MetaCyc']:
				refdbs += ['Meta']
			ptools.send_cmd(f'(refine-b :ref-kbs \'({" ".join(refdbs)}))')
			ptools.send_cmd('(save-kb)')
	elif stage == 'refine-c':
		pmn.message(pmn.blue_text('==Running Refine-C=='))
		refine_c.refine_c(config, orgtable, orglist, ptools)
	elif stage == 'delete':
		pmn.message('The following PGDBs will be deleted:')
		for org in orglist:
			entry = orgtable[org]
			pmn.message('  '+pmn.blue_text(f'{org}Cyc') + f' at {path.join(config["ptools-pgdbs"], org.lower())}cyc')
		really_delete = pmn.ask_yesno(f'Really {pmn.red_text("delete")} (all versions of) these {len(orglist)} PGDBs (Y/n)? ', config['_y_flag'], True)
		if really_delete:
			pmn.message('Deleting PGDBs')
			for org in orglist:
				org_dir = path.join(config['ptools-pgdbs'], org.lower()+'cyc')
				pmn.info(f'Deleting {org_dir}')
				try:
					rmtree(org_dir)
				except OSError as e:
					pmn.error(f'{org_dir}: {e.strerror}')
		else:
			pmn.message('PGDBs will not be deleted')
	elif stage == 'split':
		if 'split-fa-num-files' in config:
			pmn.message(pmn.blue_text('==Splitting input fasta files=='))
			from split_fasta import split_fasta
			os.makedirs(path.join(config['proj-fasta-dir'], 'splits'), exist_ok = True)
			os.makedirs(path.join(config['proj-e2p2-dir'], 'splits'), exist_ok = True)
			for org in orglist:
				entry = orgtable[org]
				seqfile = entry['Sequence File']
				pmn.info(f'{org}: Splitting {seqfile}')
				out_pre = path.join(config['proj-fasta-dir'], 'splits', path.split(seqfile)[-1])
				split_fasta(in_fa = seqfile, out_prefix = out_pre, parts = int(config['split-fa-num-files']))
		else:
			pmn.info('split-fa-num-files not found in config; will not split input files')
	elif stage == 'join':
		if 'split-fa-num-files' in config:
			from split_fasta import get_split_filename
			pmn.message(pmn.blue_text('==Joining split E2P2 output files=='))
			os.makedirs(path.join(config['proj-e2p2-dir'], 'splits'), exist_ok = True)
			for org in orglist:
				entry = orgtable[org]
				outfilename = re.sub(r'\.[^.]*$', '.MaxWeightAbsoluteThreshold.orxn.pf', entry['Initial PF File'])
				pmn.info(f'{org}: Creating output E2P2 file {outfilename}')
				try:
					outfile = open(outfilename, 'w')
					for s in range(1, int(config['split-fa-num-files'])+1):
						infileprefix = path.join(config['proj-e2p2-dir'], 'splits', path.split(outfilename)[-1])
						infilename = get_split_filename(infileprefix, s)
						pmn.info(f'Copying contents of {infilename} into {outfilename}')
						infile = open(infilename)
						d = infile.read()
						outfile.write(d)
				except IOError as e:
					pmn.error(f'{e.filename}: {e.strerror}')
					exit(1)
		else:
			pmn.info('split-fa-num-files not found in config; will not join e2p2 output files')
	else:
# Stage wasn't any of the internally-defined stages, so it must refer to a .master file
		masterfile = stage
		if not masterfile.endswith('.master'):
			masterfile = masterfile + '.master'
		pmn.message(pmn.blue_text(f'==Running master file {masterfile}=='))
		masterscript = path.join(perl_scripts_path, 'pmn-release-pipeline-parallel.pl')
		for org in orgtable:
			pmn.message('Running %s on %s'%(masterfile, org))
			masterfilepath = path.join(config['proj-masters-dir'], org, masterfile)
			perl_env = os.environ.copy()
			perl_env['PTOOLS-ACCESS-SOCKET']=ptools.pt_socket
			pmn.info(f'Perl environment: {perl_env}')
			mf_result = subprocess.run(['perl', masterscript, masterfilepath, masterfilepath + '.log'], env=perl_env, capture_output = True)
			pmn.subproc_msg(mf_result.stdout)
	return 0

# This code determines the stage list and runs them

# The newproj and fixproj stages are unique in that we don't read in the config files because they don't exist yet, so check for and deal with them separately before reading said config files
if args.stage == ['newproj'] or args.stage == ['fixproj']:
	run_stage(args.stage[0], None, None, None, args.proj)
elif 'newproj' in args.stage:
	pmn.error('The newproj command should only be run on its own')
	exit(1)
else:
	pmn.message(f'Running stages: {", ".join(args.stage)}')
	(config, orgtable, orglist) = pmn.read_pipeline_files(args)
	config['_y_flag'] = args.y
	config['_filename'] = args.c
	prev_stage = -1 # The previous non-dash stage in the list; used in interpreting dashes. So if the user enters e2p2 - create then when we get to 'create', prev_stage will be 'e2p2'
	stage_list = []  # The final list of stages
	dash = False  # Whether we just had a dash and are expecting the end of the dash range
	for stage_req in args.stage:
		if stage_req == '-':
			if prev_stage in stages_nonsequenced:
					pmn.error(f'Stage {prev_stage} cannot be part of a stage range because it is not in the standard stage sequece')
					exit(1)
			dash = True
		else:
			if stage_req in stages_nonsequenced:
				if dash:
					pmn.error(f'Stage {stage_req} cannot be part of a stage range because it is not in the standard stage sequece')
					exit(1)
				stage_list.append(stage_req)
				prev_stage = stage_req
				continue
			try:
				stage_n = stage_sequence.index(stage_req)
			except ValueError:
				pmn.error(f'Stage not recognized: {stage_req}')
				exit(1)
			if dash:
				if stage_n <= prev_stage:
					pmn.error(f'Stage range given in wrong order; {stage_req} should come before the stage you specified as the beginning of the range')
					exit(1)
				for i in range(prev_stage+1, stage_n+1):
					stage_list.append(stage_sequence[i])
				dash = False
			else:
				stage_list.append(stage_sequence[stage_n])
			prev_stage = stage_n
	if dash:
		# This code is executed if the stage request ends in a dash, which is equivalent to putting the last stage (blastsets) as the end of the dash range
		stage_n = length(stage_sequence)
		for i in range(prev_stage+1, stage_n):
			stage_list.append(stage_sequence[i])
	# If any of the requested stages need ptools, pick a socket name that should be unique (if we're under SLURM then use the slurm job ID; otherwise use the PID)
	if stages_needing_ptools.intersection(stage_list):
		sock_name = path.join(config['proj-sock-dir'], pmn.get_run_id() + '.sock')
		pmn.info(f'Starting Pathway Tools using socket {sock_name}')
		# If we're running refine-c, then we need to set up an x-server (PTools needss to put up the GUI to generate the cellular overview) and also start ptools in web mode (required for PTools to generate the web cellular overview)
		running_refine_c = 'refine-c' in stage_list
		pmn.info(f'Stage list contains refine-c: {"yes" if running_refine_c else "no"}')
		ptools = pmn.PMNPathwayTools(config, socket = sock_name, args = ['-www'] if running_refine_c else [], x11 = config['x-server'] if running_refine_c else None)
	else:
		pmn.info(f'No need to start Pathway Tools for stage(s) {", ".join(args.stage)}')
		ptools = None
	for stage in stage_list:
		run_stage(stage, config, orgtable, orglist, args.proj, ptools = ptools, split_id = args.s)
	if ptools:
		# The logging thread has a reference to the ptools object, so we must delete it manually to trigger the code that quits ptools
		del ptools
	print()
