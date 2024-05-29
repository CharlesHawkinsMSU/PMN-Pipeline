#!/usr/bin/env python3

from sys import stdin, stdout, stderr
from os import path
import argparse as ap
import pmn
import stages
import concurrent.futures
import multiprocessing
import copy

par = ap.ArgumentParser(description = 'Used to run each stage of the PMN release pipeline')
pmn.add_standard_pmn_args(par, action='run')
par.add_argument('stage', nargs = '+', help = 'Which stage of the pipeline to run. Valid stages are: newproj, precheck, e2p2, prepare, create, dump, dump-biopax, savi, refine-a, refine-b, refine-c, newversion, overview, checker, blastset')
par.add_argument('-s', '--split-id', help = 'Which part of the split fasta(s) to run E2P2 on. Used when running in parallel after splitting each fasta into many parts. You need to have run the split stage with fa-split set to some number in the config file.', dest = 's')
par.add_argument('-l', '--parallel', action = 'store_true', help = 'Run tasks in parallel; use the max-parallel-tasks option in pgdb-pipeline.txt to control the max number of parallel processes', dest = 'l')

args = par.parse_args()
pmn.verbose = args.v



# Executes any and all stages that are waiting in the given stage queue (in parallel for all requested organisms), blocks until all organisms have finished, and clears the queue
# **modifies stage_queue**
def execute_queued_stages(stage_queue, config, orgtable, orglist, args):
	fs = []
	for org in orglist:
		pmn.info(f'Executing a sequence of stages on {org}: {stage_queue}')
		fs.append(pool.submit(run_stage_list, stage_queue, config, orgtable, [org], args))
	for r in concurrent.futures.as_completed(fs):
		r.result()
	stage_queue.clear()

def start_ptools(stage_list, config):
	# Pick a socket name that should be unique (if we're under SLURM then use the slurm job ID; otherwise use the PID)
	sock_name = path.join(config['proj-sock-dir'], pmn.get_run_id() + '.sock')
	pmn.info(f'Starting Pathway Tools using socket {sock_name}')
	# If we're running refine-c, then we need to set up an x-server (PTools needss to put up the GUI to generate the cellular overview) and also start ptools in web mode (required for PTools to generate the web cellular overview)
	running_refine_c = 'refine-c' in stage_list
	pmn.info(f'Stage list contains refine-c: {"yes" if running_refine_c else "no"}')
	return pmn.PMNPathwayTools(config, socket = sock_name, args = ['-www'] if running_refine_c else [], x11 = config.setdefault('x-server', 'external'))

def run_stage_list(stage_list, config, orgtable, orglist, args):
	print(args)
	pmn.info(f'Running stage list: {stage_list}')
	ptools = None
	for stage in stage_list:
		if ptools is None and stage in stages.stages_needing_ptools:
			ptools = start_ptools(stage_list, config)
		stages.run_stage(stage, config, orgtable, orglist, args = args, ptools = ptools)
	if ptools:
		del ptools

# The newproj and fixproj stages are "singleton" stages; they can only be run on their own. Also, we don't read in the config files for these stages because they don't exist yet, so we should check for and deal with them separately before trying to read said config files
if stages.stages_singleton.intersection(args.stage):
	if len(args.stage) == 1:
		stages.run_stage(args.stage[0], None, None, None, args)
	else:
		pmn.error('The stages {pmn.andlilst(stages.stages_singleton)} should only be run on their own')
		exit(1)
else:
	pmn.info(f'Stages requested to be run: {", ".join(args.stage)}')
	(config, orgtable, orglist) = pmn.read_pipeline_files(args)
	config['_y_flag'] = args.y
	config['_filename'] = args.c

	stage_list = stages.compile_stage_list(args.stage)

	stage_andlist = pmn.andlist(stage_list, quote = "\"")
	pmn.message(f'Will run {len(stage_list)} stage{"s" if len(stage_list) != 1 else ""}: {stage_andlist}')

	if args.l: # User requested to run stages in parallel
		stage_queue = []
		# We're going to use a thread pool here (rather than a process pool) because all the actually difficult work is done by external programs rather than by the threads themselves, so they're going to spend most of their time waiting for the external processes to do things
		with concurrent.futures.ThreadPoolExecutor(max_workers = int(config.setdefault('max-parallel-cpus', multiprocessing.cpu_count()))) as pool:
			pmn.info(f'Parallelism requested, starting thread pool with {config["max-parallel-cpus"]} threads')
			for stage in stage_list:
				if stage == 'e2p2':
					pmn.info('e2p2 stage requires special handling; waiting for all prior stages to finish')
					execute_queued_stages(stage_queue, config, orgtable, orglist, args)
					fs = []
					for org in orglist:
						for split in stages.split_range(args.s, config):
							e2p2_args = copy.copy(args)
							e2p2_args.s = split
							fs.append(pool.submit(run_stage_list, ['e2p2'], config, orgtable, [org], e2p2_args))
					for r in concurrent.futures.as_completed(fs):
						r.result()
				elif stage in stages.stages_nonparallel:
					pmn.info(f'{stage} stage is non-parallel; waiting for all prior stages to finish')
					execute_queued_stages(stage_queue, config, orgtable, orglist, args)
					run_stage_list([stage], config, orgtable, orglist, args)
				else:
					stage_queue.append(stage)
			pmn.info('All remaining stages are parallelizable, queuing them up')
			execute_queued_stages(stage_queue, config, orgtable, orglist, args)
	else:
		run_stage_list(stage_list, config, orgtable, orglist, args)
