#!/usr/bin/env python3

from sys import stderr
from os import path
import pmn
import argparse as ap

def main():
	par = ap.ArgumentParser(description = 'Used to create the SAVI frames in aracyc, which will be imported into individual databases in refine-a')
	pmn.add_standard_pmn_args(par)

	args = par.parse_args()
	(config, orgtable, orglist) = pmn.read_pipeline_files(args)
	pmn.verbose = args.v
	return create_savi_citations(config, orgtable, orglist)
def create_savi_citations(config, orgtable, orglist, ptools = None):
	if ptools is None:
		ptools = pmn.PMNPathwayTools(config)
	years = set()
	for orgid, entry in orgtable.items():
		years.add(entry['Citation Year'])

	pmn.info(f'Will create SAVI citations in AraCyc for {", ".join(years)}')
	e2p2_vers = config['e2p2-version']
	rpsd_vers = config['rpsd-version']
	savi_vers = config['savi-version']
	#savi_dir = path.join(config['savi'], '..')
	savi_dir = config['savi']
	orgs_avail = ptools.get_org_list()
	if 'ARA' not in orgs_avail:
		pmn.error('AraCyc not found. This pipeline requires a copy of AraCyc in order to run. AraCyc may be downloaded from https://plantcyc.org after requesting a free license\n')
		exit(1)
	ptools.so('ara')
	for year in years:
		cit_cmd = f'(create-savi-citations :year "{year}" :input-dir "{savi_dir}" :e2p2-version "{e2p2_vers}" :savi-version "{savi_vers}" :rpsd-version "{rpsd_vers}")'
		pmn.info(cit_cmd)
		ptools.send_cmd(cit_cmd)

#(year (util.date-time:date-time-year (util.date-time:ut-to-date-time (get-universal-time))))
# (input-dir (sys:getenv "PMN_SAVI"))
# e2p2-version
# (ptools-version (ptools-version))
# savi-version
# (rpsd-version e2p2-version)
# metacyc-version
# (kb-list (list (current-kb))))
if __name__ == "__main__":
	exit(main())
