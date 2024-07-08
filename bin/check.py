#!/usr/bin/env python3

from sys import stdin, stdout, stderr
import pmn
import stages

check_fn_table = {'e2p2': check_e2p2_outupts,
				  'join': check_join_outputs,
				  'split': check_split_outputs,
				  'create': check_create_outputs,
				  }
def check_pipeline_outputs(stage, config, orgtable, orglist, args):
	if isinstance(stage, list):
		passed = True
		for one_stage in stage:
			passed &= check_pipeline_outputs(one_stage, config, orgtable, orglist, args)
		return passed
	else:
		check_fn = check_fn_table.setdefault(stage, no_check)
		passed = True
		for org in orglist:
			passed &= check_fn(stage, config, orgtable, org, args)
		return passed

def no_check(stage, config, orgtable, org, args):
	pmn.warn(f'Stage {stage} has no checks available in this version of the pipeline')
	return True

def check_e2p2_outputs(stage, config, orgtable, org, args):
	if args.s:
		passed = True
		for (inpath, outpath) in stages.splits_for_org(config, orgtable, org, split_id):
			if path.exists(outpath):
				pmn.info(f'Confirmed split {outpath} exists')
				try:
					if os.stat(outpath).st_size == 0:
						pmn.warn(f'Split {outpath} is empty')
						passed = False
					else:
						pmn.info(f'Split {outpath} is non-empty')
				except IOError as e:
					pmn.warn(f'Could not determine filesize of {outfile} to check if it\'s empty: {e.strerror}')
					passed = False
		return passed
	else:
		return check_join_outputs(stage, config, orgtable, org, args)

def check_join_outputs(stage, config, orgtable, org, args):

def check_split_outputs(stage, config, orgtable, org, args):

def check_create_outputs(stage, config, orgtable, org, args):

if __name__ == '__main__':
	stderr.write('This is a library for checking the output of different pipeline stages. It does nothing when run directly. To use the pipeline you should run pmn-pipeline.py\n')
	exit(1)
