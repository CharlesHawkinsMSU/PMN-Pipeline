#!/usr/bin/env python3

from sys import stdin, stdout, stderr
import pmn
import re
import sys
from os import path
import os
from shutil import copy2, rmtree
import subprocess
import pgdb_prepare
import create_pgdbs
import savi_prepare
import refine_prepare
import create_authors
import create_savi_citations
import refine_c
import revise_pf
import savi_pathways
import tables
from split_fasta import get_split_filename, split_fasta

# The standard sequence of stages, used when the user requests a range of stages:
stage_sequence = ['precheck', 'split', 'e2p2', 'join', 'revise', 'unique-ids', 'prepare', 'create', 'savi-dump', 'savi-prepare', 'savi', 'refine-prepare', 'refine-a', 'refine-b', 'refine-c', 'final-dump', 'blastset', 'pgdb-stats']
# Valid stages that are not part of the standard sequence:
stages_nonsequenced = {'delete', 'dump', 'dump-biopax', 'checker', 'list', 'list-stages', 'fa-stats', 'env', 'backup', 'restore', 'metadata', 'clean', 'debug-dumptable', 'debug-dumpconfig', 'debug-e2p2check', 'savi-check', 'custom-dumps'}
# Stages that can only be executed on their own, and not part of any stage sequence
stages_singleton = {'newproj', 'lisp'}
# Stages that require an instance of Pathway Tools to be running:
stages_needing_ptools = {'create', 'dump', 'savi-dump', 'final-dump', 'refine-prepare', 'refine-a', 'refine-b', 'refine-c', 'checker', 'dump-biopax', 'blastset', 'pgdb-stats', 'metadata', 'custom-dumps', 'unique-ids'}
# Stages that cannot be run in parallel over the organism list; when running in parallel and we get to one of these stages, we have to wait for all previous stages to complete for all orgs before proceeding
stages_nonparallel = {'metadata', 'precheck', 'fa-stats', 'pgdb-stats', 'list', 'stage-list', 'env', 'list-stages', 'clean', 'savi-check', 'unique-ids'}

# Help text for each stage
stage_help = {'precheck': 'Runs quick checks on the configuration to make sure pipeline is good to go',
		  'e2p2': 'Runs E2P2 to predict enzyme functions for the proteins in each input FASTA',
		  'revise': 'Add in gene IDs to the E2P2 output files',
		  'unique-ids':'Gets new unique-ids for orgids that don\'t already have them',
		  'prepare': 'Puts all files in place to create PGDBs',
		  'create': 'Creates initial PGDBs using PathoLogic',
		  'savi-dump': 'Dumps PGDBs to flat-files required for SAVI; alias for "dump"',
		  'savi-prepare': 'Puts all files in place to run SAVI',
		  'savi': 'Runs SAVI to apply pathway-level curation rules to each new PGDB',
		  'refine-prepare': 'Generates author and citation frames, and puts all files in place to run refine A',
		  'refine-a': 'Applies SAVI\'s suggestions for pathways to add/remove; adds in citations for E2P2 and SAVI; fixes common names of enzymatic reactions; adds external database links to enzymes if available',
		  'refine-b': 'Finds any experimentally-validated pathways and enzymes in the reference database(s) for each species and imports them into the appropriate PGDB',
		  'refine-c': 'Runs Pathway Tools\' cconsistency checker; generates the cellular overview; assigns the requested authors to each PGDB',
		  'blastset': 'Generates BLAST dbs for each PGDB so their enzymes can be searched using BLAST',
		  'final-dump':'Dumps both attribute-value and BioPAX flatfiles (i.e. runs "dump" and "dump-biopax")',
		  'split': 'Splits input FASTAs into multiple parts so they can be run through E2P2 in parallel (see the -s option); should be run before "e2p2"',
		  'join': 'Joins the split E2P2 output files for each species into one for each species; should be run before "revise"',
		  'delete': 'Deletes existing PGDBs listed in the input table',
		  'dump': 'Dumps attribute-value flat files for each PGDB to pgdbs/<orgid>cyc/<version>/data',
		  'dump-biopax': 'Dumps BioPAX XML files for each pgdb (normally run as part of final-dump)',
		  'checker': 'Performs the Pathway Tools consistency checker on each PGDB (normally run as part of refine-c)',
		  'list': 'List all (or requested with -o) PGDBs in the input file with their index numbers',
		  'list-stages': 'List all valid stages in the pipeline',
		  'clean': 'Clean temporary files, including pipeline logs, SLURM logs, and E2P2 temporary files',
		  }

if __name__ == "__main__":
	stderr.write('stages.py is a library that contains the PMN pipeline stage definitions and code for running them. It does nothing when executed directly. Use pmn-pipeline.py to run the pipeline\n')
	exit(1)

def run_stage(stage, config, orgtable, orglist, args, ptools = None):
	split_id = args.s
	proj = args.proj
	pmn.message(f'Running stage "{stage}"')
	if stage == 'savi-dump':
		stage = 'dump'
	elif stage == 'final-dump':
		run_stage('dump', config, orgtable, orglist, args, ptools)
		run_stage('dump-biopax', config, orgtable, orglist, args, ptools)
		return 0
	if stage == 'dump':
		ptools.send_cmd('(setf *get-orthologs-from-sri-p* nil)')  # Avoid failure of dump due to inability to connect to SRI ortholog server
	script_path = path.dirname(path.realpath(__file__))
	stage = stage.lower()
	if stage == 'newproj':
		if args.f:
			pmn.message('==Adding any missing project files==')
		else:
			pmn.message('==Creating new blank project==')
		proj_tmpl_path = path.realpath(path.join(script_path, '..', 'project-template'))
		if not path.exists(proj):
			os.mkdir(proj)
		for filename in os.listdir(proj_tmpl_path):
			src = path.join(proj_tmpl_path, filename)
			dst = path.join(proj, filename)
			if not path.exists(dst) or not args.f:
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
		else:
			pmn.message(pmn.red_text('\nOne or more checks failed; please address the issues before continuing with the pipeline'))
			exit(1)
	elif stage == 'fa-stats':
		reqs = ['fa-countseqs', 'fa-avg', 'fa-goodseqs', 'fa-type']
		reqs = [path.join(path.dirname(path.abspath(__file__)), exe) for exe in reqs]
		pmn.check_exists(files = reqs, progname = 'fa-stats') or exit(1)
		pmn.check_access(reqs, os.X_OK, reason = 'required by fa-stats', ignore_missing = True) or exit(1)
		outfilepath = config.setdefault('proj-fa-stats', 'fa-stats.txt')
		def get_fa_stat(exe, fa_file, args = []):
			result = subprocess.run([path.join(path.dirname(path.abspath(__file__)),exe), fa_file] + args, capture_output = True)
			if result.returncode != 0:
				err = result.stderr.decode().strip()
				pmn.error(err)
				return err.split(': ')[-1]
			return result.stdout.strip().decode()
		try:
			outfile = open(outfilepath, 'w')
			pmn.info('Writing FASTA statistics to {outfilepath}')
			outfile.write('\t'.join(['Orgid', 'Sequence File', 'Sequence Count', 'Avg Sequence Length', 'Sequence Type', 'Percent Good Sequences']) + '\n')
			for org in orglist:
				entry = orgtable[org]
				pmn.info(f'Getting fa-stats for {org}')
				fa_file = entry['Sequence File']
				fa_countseqs = get_fa_stat('fa-countseqs', fa_file)
				try:
					if int(fa_countseqs) < 10000:
						pmn.warn(f'FASTA file {fa_file} for orgid {org} has a low number of sequences ({int(fa_countseqs)}). Typical values are on the order of 20,000 or above. This may not be a full proteome for this organism, or it may be a gDNA file')
				except ValueError:
					pass
				fa_avg = get_fa_stat('fa-avg', fa_file)
				pmn.info(fa_avg)
				try:
					if float(fa_avg) < 100:
						pmn.warn(f'FASTA file {fa_file} for orgid {org} has a low average sequence length ({float(fa_avg)}). Typical values are in the 200s to 500s for plants, up to around the 700s for chlorophytes. You may have truncated sequences (e.g. translated ESTs)')
					elif float(fa_avg) > 1500:
						pmn.warn(f'FASTA file {fa_file} for orgid {org} has a high average sequence length ({float(fa_avg)}). Typical values are in the 200s to 500s for plants, up to around the 700s for chlorophytes. You may have a gDNA file')
				except ValueError:
					pass
				fa_type = get_fa_stat('fa-type', fa_file).split('\n')[-1].strip()
				fa_gen_type = fa_type.split(',')[0].split(' ')[0]
				if fa_gen_type == 'NA':
					pmn.error(f'FASTA file {fa_file} for orgid {org} appears to be a nucleic acid fasta file. This will not work with the pipeline. An amino acid fasta file is required')
				elif fa_gen_type == 'Invalid':
					pmn.error(f'FASTA file {fa_file} for orgid {org} appears to contain invalid FASTA characters. This is likely to cause E2P2 to fail. Check for files that use a . for stop codons (a stop codon is supposed be * but a few files use . contrary to the spec, which will cause DeepEC to crash). Also check for base numbers at the beginning of lines, or spaces within the lines inserted for "readability"; you will have to remove these if present')
				fa_goodseqs = get_fa_stat('fa-goodseqs', fa_file, args = ['-s']).split(': ')[-1]
				try:
					fa_goodseqs_pct = float(fa_goodseqs.split(' ')[0])
					if fa_goodseqs_pct < 85:
						pmn.warn(f'FASTA file {fa_file} for orgid {org} has a high percentage of "bad" sequences (100-{fa_goodseqs_pct}%). "Bad" sequences are sequences that don\'t start with a methionine (M) or that contain internal stop codons (*). More than around 15% suggests that there is a problem with the sequencing or translation')
				except ValueError:
					pass
				
				outfile.write('\t'.join([org, fa_file, fa_countseqs, fa_avg, fa_gen_type, fa_goodseqs]) + '\n')
			outfile.close()
				
		except IOError as e:
			pmn.error(f'{e.filename}: {e.strerror}')
			exit(1)

	elif stage == 'env':
		pmn.message(f'export SINGULARITY_BIND={config.setdefault("ptools-pgdbs", "pgdbs")}:/pgdbs')

	elif stage == 'list':
		pmn.message(pmn.blue_text('Index\tOrgID'))
		for org in orglist:
			entry = orgtable[org]
			pmn.message(f'{entry["_index"]}\t{org}')
	elif stage == 'list-stages':
		pmn.message(pmn.blue_text('Stages in the standard sequence:'))
		for i in range(len(stage_sequence)):
			pmn.message(f'{i+1}. {pmn.green_text(stage_sequence[i])} - {stage_help.setdefault(stage_sequence[i], "<no help available>")}')
		pmn.message(pmn.blue_text('Additional stages not in the standard sequence:'))
		for stage in stages_nonsequenced:
			pmn.message(f'- {pmn.green_text(stage)} - {stage_help.setdefault(stage, "<no help available>")}')
	elif stage == 'e2p2':
		def make_e2p2_cmd(e2p2_exe, inpath, outpath, e2p2_version):
			cmd = [
					sys.executable,
					e2p2_exe,
					'--input', inpath,
					'--output', outpath
					]
			if e2p2_version == 4:
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
			return cmd

		if split_id:
			os.makedirs(path.join(config['proj-e2p2-dir'], 'splits'), exist_ok = True)

		pmn.message(pmn.blue_text('==Running E2P2=='))
		e2p2v5_exe = path.join(config['e2p2'], 'e2p2.py')
		e2p2v4_exe = path.join(config['e2p2'], 'pipeline', 'run_pipeline.py')
		if path.exists(e2p2v5_exe):
			pmn.info('E2P2 version 5 detected')
			e2p2_exe = e2p2v5_exe
		else:
			pmn.info('E2P2 version 4 detected')
			e2p2_exe = e2p2v4_exe
		e2p2_cmds = []
		for org in orglist:
			entry = orgtable[org]
			if split_id:
				for (inpath, outpath) in splits_for_org(config, orgtable, org, split_id):
					if not (args.f and path.exists(outpath)):
						pmn.info(f'Running E2P2 on split {inpath} for {org}')
						e2p2_cmds.append(make_e2p2_cmd(e2p2_exe, inpath, outpath, 5 if e2p2_exe is e2p2v5_exe else 4))
					else:
						pmn.info(f'Will not run E2P2 on split {inpath} for {org} because {outpath} exists')
			else:
				inpath = entry['Sequence File']
				outpath = entry['Initial PF File']
				if not (args.f and path.exists(outpath)):
					pmn.info(f'Running E2P2 on {org}')
					e2p2_cmds.append(make_e2p2_cmd(e2p2_exe, inpath, outpath, 5 if e2p2_exe is e2p2v5_exe else 4))
				else:
					pmn.info(f'Will not run E2P2 on {org} because {outpath} exists')

		for e2p2_cmd in e2p2_cmds:
			pmn.info(' '.join(e2p2_cmd))
			e2p2_result = pmn.run_external_process(e2p2_cmd, procname = 'E2P2')
			if e2p2_result != 0:
				pmn.error('E2P2 failed')
				exit(e2p2_result)

	elif stage == 'unique-ids':
		for org in orglist:
			entry = orgtable[org]
			if 'Unique ID' in entry:
				pmn.info(f'{org} already has unique ID {entry["Unique ID"]}')
			else:
				uid = generate_org_uid(org, ptools)
				pmn.info(f'Unique ID {uid} assigned to {org}')
				entry['Unique ID'] = uid
		pmn.write_uids_file(config, orgtable)

	elif stage == 'prepare':
		pmn.message(pmn.blue_text('==Preparing master files=='))
		pgdb_prepare.pgdb_prepare(config, orgtable, orglist)

	elif stage == 'create':
		pmn.message(pmn.blue_text('==Creating PGDBs with PathoLogic=='))
		create_pgdbs.create_pgdbs(config, orgtable, orglist, args, ptools)
	elif stage == 'revise':
		pmn.message(pmn.blue_text('==Revising E2P2 files with gene IDs=='))
		# Run revise_pf.py
		# arg_col enumerates columns in the input table used for revise_pf.py and maps them to arguments to be given to revise_pf.py. It is constructed for each org from the arg_col_* vars based on which sources of gene ID were requested via the 'Genes From' column for that org
		arg_col_gen = [
				('-n', 'Numeric IDs'),
				('-gr', 'Gene Delete'),
				('-pr', 'Prot Delete'),
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

		for org in orglist:
			entry = orgtable[org]
			if path.exists(path.join(config['e2p2'], 'e2p2.py')):
				in_pf_path = re.sub(r'\.[^.]*$', '.MaxWeightAbsoluteThreshold.orxn.pf', entry['Initial PF File'])
			else:
				in_pf_path = entry['Initial PF File']+'.orxn.pf'
			arglist = [in_pf_path]
			# Figure out where we're getting the gene IDs from and construct arg_col based on that
			genes_from = entry.setdefault('Genes From', '').lower().split(',')
			arg_col = []
			if 'fasta' in genes_from:
				pmn.info(f'{org} getting genes from FASTA')
				arg_col.extend(arg_col_fa)
			if 'gff' in genes_from:
				pmn.info(f'{org} getting genes from GFF')
				arg_col.extend(arg_col_gff)
			if 'map' in genes_from:
				pmn.info(f'{org} getting genes from Map file')
				arg_col.extend(arg_col_map)
			if not arg_col:
				pmn.warn(f'No \'Genes From\' column for {org} or no recognized values for the column, will not map proteins to genes')
				copy2(in_pf_path, entry['PF File']);
				continue
			arg_col.extend(arg_col_gen)
			for arg, col in arg_col:
				try:
					#arg_dict[arg] = entry[col]
					arglist.extend([arg, entry[col]])
				except KeyError as e:
					pmn.info(f'{org} has no value for column {col}, will not include {arg} argument to revise_pf')
			pmn.info(f'Args to revise_pf: {arglist}')
			args = revise_pf.get_pf_args(arglist)
			revise_pf.main(args)
	elif stage == 'savi-check':
		pmn.message(pmn.blue_text('==Checking for pathways needing SAVItization=='))
		savi_in_dir = path.join(config['savi'], 'input')
		savi_check_out = config.setdefault("savi-check-output", "to-savitize.txt")
		exclude_dir = config.setdefault('savi-exclude', None)
		pgdb_dirs = [pmn.dir_for_org(org, config) for org in orglist]
		pmn.info(f'SAVI input dir: {savi_in_dir}; PGDB dir: {config["ptools-pgdbs"]}; {"exclude PGDBs from: " + exclude_dir if exclude_dir else ""}; output to {savi_check_out}')
		savi_pathways.savi_pathways(savi_in_dir, pgdb_dirs, savi_check_out, exclude_dir)

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
	elif stage == 'metadata':
		pmn.info('Updating PGDB metadata')
		#pmn.run_external_process([config['ptools-exe'], '-lisp', '-update-pgdb-metadata'], procname = 'Ptools (metadata)')
		# We may be in Singularity and therefore ptools is installed on a read-only filesystem. Therefore we should only update the user PGDB metadata; using the -update-pgdb-metadata flag will attempt to also update the metadata for the biocyc pgdbs and will therefore fail when it can't write to that location. So we have to manually use the lisp function to update the metadata
		ptools.send_cmd(f'(update-pgdb-metadata-kb (sri:get-pathname (get-ptools-local-pathname) :offset \'("pgdbs" "user")))')
	elif stage == 'backup':
		prev_dir = os.getcwd()
		backups_dir = path.abspath(config.setdefault('proj-backups-dir', 'intermediate-pgdbs'))
		os.chdir(config['ptools-pgdbs'])
		for org in orglist:
			tarfile = path.join(backups_dir, org.lower()+'.tar.bz2')
			pmn.info(f'Backing up {org} in {tarfile}')
			pmn.run_external_process(['tar', 'cjf', tarfile, org.lower()+'cyc'])
		os.chdir(prev_dir)
	elif stage == 'restore':
		prev_dir = os.getcwd()
		backups_dir = path.abspath(config.setdefault('proj-backups-dir', 'intermediate-pgdbs'))
		os.chdir(config['ptools-pgdbs'])
		for org in orglist:
			org_dir = pmn.dir_for_org(org, config)

			tarfile = path.join(backups_dir, org.lower()+'.tar.bz2')
			pmn.info(f'Restoring backup of {org} from {tarfile}')
			try:
				rmtree(org_dir)
			except OSError as e:
				pass
			pmn.run_external_process(['tar', 'xf', tarfile])
		os.chdir(prev_dir)
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
			ptools.send_cmd('(close-kb :save-updates-p nil :flush-memory-p t)')
	elif stage == 'refine-c':
		pmn.message(pmn.blue_text('==Running Refine-C=='))
		refine_c.refine_c(config, orgtable, orglist, ptools)
	elif stage == 'pgdb-stats':
		stats = []
		try:
			statsfilepath = config.setdefault('pgdb-stats', 'pgdb-stats.txt')
			statsfile = open(statsfilepath, 'w')
			for org in orglist:
				ptools.so(org)
				num_pathways = ptools.send_cmd('(length (gcai "Pathways"))')
				num_rxns = ptools.send_cmd('(length (gcai "Reactions"))')
				num_enz = ptools.send_cmd('(length (all-enzymes))')
				num_cpds = ptools.send_cmd('(length (gcai "Compounds"))')
				stats.append([org, num_pathways, num_rxns, num_enz, num_cpds])
			statsfile.write('\t'.join(['Database ID', 'Pathways', 'Reactions', 'Enzymes', 'Compounds']) + '\n')
			statsfile.write('\n'.join(['\t'.join(statline) for statline in stats]) + '\n')
			statsfile.close()
		except IOError as e:
			pmn.error(f'{e.filename}: {e.strerror}')
			exit(1)

	elif stage == 'clean':
		pmn.info('==Cleaning temporary files==')

		# Delete logs from the logs dir
		logdir = config['proj-logs-dir']
		logfiles = os.listdir(logdir)
		if len(logfiles) > 0:
			pmn.info(f'Logs to delete: {", ".join(logfiles)}')
			if pmn.ask_yesno(f'Delete {len(logfiles)} logfiles in directory "{logdir}"? (y/N)', config['_y_flag'], True):
				pmn.message('Deleting logfiles')
				for logfile in logfiles:
					to_delete = path.join(logdir, logfile)
					pmn.info(f'Deleting {to_delete}')
					os.remove(to_delete)
			else:
				pmn.message('Will not delete log files')
		else:
			pmn.message(f'No logfiles to delete in directory "{logdir}"')

		# Delete SLURM logs
		slurm_logs = []
		slurm_re = re.compile(r'slurm-[0-9]+(_[0-9]+)?.out')
		for f in os.listdir(args.proj):
			if slurm_re.match(f):
				slurm_logs.append(f)
		if len(f) > 0:
			pmn.info(f'SLURM logs to delete: {", ".join(slurm_logs)}')
			if pmn.ask_yesno(f'Delete {len(slurm_logs)} SLURM output logs? (y/N)', config['_y_flag'], True):
				for f in slurm_logs:
					pmn.info(f'Deleting {f}')
					os.remove(f)
		else:
			pmn.message('No SLURM logs to delete')

		# Delete e2p2 temp files

	elif stage == 'delete':
		pmn.message('The following PGDBs will be deleted:')
		for org in orglist:
			entry = orgtable[org]
			pmn.message('  '+pmn.blue_text(f'{org}Cyc') + f' at {path.join(config["ptools-pgdbs"], org.lower())}cyc')
		really_delete = pmn.ask_yesno(f'Really {pmn.red_text("delete")} (all versions of) these {len(orglist)} PGDBs (y/N)? ', config['_y_flag'], True)
		if really_delete:
			pmn.message('Deleting PGDBs')
			# Delete pgdb directories
			for org in orglist:
				org_dir = pmn.dir_for_org(org, config)
				pmn.info(f'Deleting {org_dir}')
				try:
					rmtree(org_dir)
				except FileNotFoundError:
					pmn.warn(f'{org_dir} not found')
				except OSError as e:
					pmn.error(f'{org_dir}: {e.strerror}')
			# Delete cached overview files in /tmp/ov
			tmpfile_re_orglist = '|'.join([re.escape(org.upper()) for org in orglist])
			tmpfile_re = re.compile(f'{tmpfile_re_orglist}(Z[0-4])?-') # NB: re.match only matches the beginning of a string so a ^ is not required
			deleted_files = []
			ov_dir = '/tmp/ov'
			try:
				file_list = os.listdir(ov_dir)
			except IOError:
				file_list = []
			for ov_file in file_list:
				if tmpfile_re.match(ov_file):
					os.remove(path.join(ov_dir, ov_file))
					deleted_files.append(ov_file)
			if deleted_files:
				pmn.info(f'Deleted cached overview files {pmn.andlist(deleted_files)} from {ov_dir}')
			else:
				pmn.info(f'No cached overview files to delete in {ov_dir}')
		else:
			pmn.message('PGDBs will not be deleted')
	elif stage == 'split':
		if 'split-fa-num-files' in config:
			pmn.message(pmn.blue_text('==Splitting input fasta files=='))
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
	elif stage == 'debug-dumptable':
		try:
			dbgtablepath = path.join(args.proj, 'DEBUG-table.txt')
			pmn.message(f'Writing final table to {dbgtablepath}')
			with open(dbgtablepath, 'w') as dbgtable:
				for org in orglist:
					entry = orgtable[org]
					dbgtable.write(org + ' = ' + ', '.join([key + ': ' + str(val) for (key, val) in entry.items()]) + '\n')
		except IOError as e:
			pmn.error(f'{e.filename}: {e.strerror}')
			exit(1)
	elif stage == 'debug-dumpconfig':
		try:
			dbgconfpath = path.join(args.proj, 'DEBUG-config.txt')
			pmn.message(f'Writing final configuration to {dbgconfpath}')
			with open(dbgconfpath, 'w') as dbgconf:
				dbgconf.write('Args: ' + str(args) + '\nConfig:\n')
				for key, val in config.items():
					dbgconf.write(f'{key}: {val}\n')
		except IOError as e:
			pmn.error(f'{e.filename}: {e.strerror}')
			exit(1)
	elif stage == 'custom-dumps':
		tables.export_custom_dumps(config, orgtable, orglist, ptools, args)

	else: # Stage wasn't any of the internally-defined stages, so it must refer to a .master file
		masterfile = stage
		if not masterfile.endswith('.master'):
			masterfile = masterfile + '.master'
		pmn.message(pmn.blue_text(f'==Running master file {masterfile}=='))
		masterscript = path.join(perl_scripts_path, 'pmn-release-pipeline-parallel.pl')
		for org in orglist:
			masterfilepath = path.join(config['proj-masters-dir'], org, masterfile)
			pmn.message('Running %s on %s'%(masterfilepath, org))
			perl_env = os.environ.copy()
			perl_env['PTOOLS_ACCESS_SOCKET']=ptools.pt_socket # the var used by ptools is PTOOLS-ACCESS-SOCKET because some men just want to watch the world burn, and so we have to change it to underscores here because perl doesn't like dashes in its env variable names (they won't get passed to commands it calls with 'system')
			#perl_env['PMN_ORGLIST'] = args.o
			pmn.info(f'Perl environment: {perl_env}')
			#mf_result = subprocess.run(['perl', masterscript, masterfilepath, masterfilepath + '.log'], env=perl_env, capture_output = True)
			#pmn.subproc_msg(mf_result.stdout)
			pmn.run_external_process(['perl', masterscript, masterfilepath, masterfilepath + '.log'], env = perl_env, procname = masterfile)
	return 0

# Takes a requested list of stages as from the command line and expands it into a full stage list. The main work it does is to expand dash expressions so that, e.g., ["prepare", "-", "savi"] becomes ["prepare", "create", "metadata", "savi-dump", "savi-prepare", "savi"]
def compile_stage_list(requested_stages):
	prev_stage = -1 # The previous non-dash stage in the list; used in interpreting dashes. So if the user enters e2p2 - create then when we get to 'create', prev_stage will be 'e2p2'
	stage_list = []  # The final list of stages
	dash = False  # Whether we just had a dash and are expecting the end of the dash range
	for stage_req in requested_stages:
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
	return stage_list

# Given a request for a set of splits to run, usually from the -s argument, converts it into a range or pmn.multi_range that can be iterated over
def split_range(split_req, config):
	return range(1, int(config['split-fa-num-files'])+1) if split_req == 'all' else pmn.multi_range(split_req)

# Generator that iterates over the splits for a given org. Yields 2-ples of (Nth split fasta file, Nth split E2P2 pf file)
def splits_for_org(config, orgtable, org, split_req):
	entry = orgtable[org]
	for split in split_range(split_req, config):
		inpath = entry['Sequence File']
		outpath = entry['Initial PF File']
		infilename = path.split(inpath)[-1]
		inpre = path.join(config['proj-fasta-dir'], 'splits', infilename)
		inpath = get_split_filename(inpre, split)
		outfilename = path.split(outpath)[-1]
		outpre = path.join(config['proj-e2p2-dir'], 'splits', outfilename)
		outpath = get_split_filename(outpre, split)
		yield (inpath, outpath)

def generate_org_uid(org, ptools):
	uid = ptools.send_cmd(f'(base36 (get-pgdb-unique-id \'{org}))').strip('"')
	return uid
