#!/usr/bin/env python3

from sys import stdin, stdout, stderr
from os import path

import pmn
import argparse as ap

def main():
	par = ap.ArgumentParser(description = 'Export the drupal table. Requires the initial drupal table generated from pathway tools')
	pmn.add_standard_pmn_args(par, action='run')
	par.add_argument('-l', '--pmn-release', help = 'The PMN release this is for, e.g. 16 or 15.5', dest = 'l')
	par.add_argument('-d', '--release-date', help = 'Date of the PMN release (e.g. July 2024)', dest = 'd')
	par.add_argument('-b', '--prev-table', help = 'Previous release\'s table, to take some things from for orgs that were previously there', dest = 'b')

	args = par.parse_args()

	(config, orgtable, orglist) = pmn.read_pipeline_files(args)
	pmn.verbose = args.v

	sock_name = path.join(config['proj-sock-dir'], pmn.get_run_id() + '.sock')
	pmn.info(f'Starting Pathway Tools using socket {sock_name}')
	initial_table = '/tmp/drupal-table-initial.tsv'
	if not path.exists(initial_table) or not args.f:
		with pmn.PMNPathwayTools(config, socket = sock_name) as ptools:
			pmn.info(f'Saving initial table to {initial_table}')
			ptools.send_cmd(f'(export-drupal-table \'({" ".join(orglist)}) "{initial_table}")')

	write_drupal_table(config, orgtable, orglist, args, initial_table, args.l, args.d, args.b)

def write_drupal_table(config, orgtable, orglist, args, initial_table, pmn_release, release_date, prev_table_file):
	pmn.info(f'Reading in the initial table {initial_table}')
	in_table = pmn.read_table_file(initial_table, 'Cyc')
	prev_table = pmn.read_table_file(prev_table_file, 'Cyc')

	output_table_file = config.setdefault('proj-drupal-table', path.join(args.proj, 'drupal.tsv'))
	pmn.info(f'Writing final drupal table to {output_table_file}')
	output_table = open(output_table_file, 'w')
	output_table.write('Name	Cyc	Version	Release	TaxName	TaxLink	Sequence_Source	E2P2HTML	Ptools	Pathways	Enzymes	Reactions	Compounds	Citations	EnzEvHTML	PwyEvHTML	SAVI	URL\n')
	pt_ver = get_ptools_version(config) or '??'
	savi_ver = get_savi_versions(config)
	e2p2_ver = config["e2p2-version"]
	for org in orglist:
		entry = orgtable[org]
		in_entry = in_table[org.upper()]
		prev_entry = prev_table.setdefault(org.upper(), dict())
		out_fields = [
				org+'Cyc',
				in_entry['Cyc'],
				entry['Version'],
				f'<a href="/releases/pmn-release-{pmn_release}">PMN {pmn_release} ({release_date})</a>',
				in_entry['TaxName'],
				prev_entry.setdefault('TaxLink', f''),
				prev_entry.setdefault('Sequence_Source', f''),
				f'<a href="/e2p2/e2p2-v{e2p2_ver}-ensemble-enzyme-prediction-pipeline-version-{e2p2_ver}">E2P2v{e2p2_ver}</a>',
				f'Pathway Tools {pt_ver}',
				in_entry['Pathways'],
				in_entry['Enzymes'],
				in_entry['Reactions'],
				in_entry['Compounds'],
				in_entry['Citations'],
				prev_entry.setdefault('EnzEvHTML', '<ul><li>Almost exclusively large-scale computational predictions of enzyme function not subject to curator review</li><li>Small number of manually curated enzymes</li></ul>'),
				prev_entry.setdefault('PwyEvHTML', '<ul><li>Primarily computational prediction of pathways followed by <a href="/about/savi-pipeline">SAVI</a> refinement and curator review</li><li>Additional pathways imported from MetaCyc based on experimental evidence or curator inference</li></ul>'),
				f'<a href="/savi/mcp-{savi_ver["MCP"]}">MCP {savi_ver["MCP"]}</a><br><a href="/savi/aipp-{savi_ver["AIPP"]}">AIPP {savi_ver["AIPP"]}</a><br><a href="/savi/capp-{savi_ver["CAPP"]}">CAPP {savi_ver["CAPP"]}</a><br><a href="/savi/cvp-{savi_ver["CVP"]}">CVP {savi_ver["CVP"]}</a><br><a href="/savi/npp-{savi_ver["NPP"]}">NPP {savi_ver["NPP"]}</a><br><a href="/savi/upp-{savi_ver["UPP"]}">UPP {savi_ver["UPP"]}</a><br>',
                f'content/{org.lower()}cyc-{entry["Version"]}',
				]
		output_table.write('\t'.join(out_fields)+'\n')
	output_table.close()
				


def get_ptools_version(config):
	for line in open(config['ptools-exe']):
		line = line.strip()
		if line.startswith('VERSION='):
			return line.split('=')[1]
	return None

def get_savi_versions(config):
	savi_in_dir = path.join(config['savi'], 'input')
	savi_vers = {}
	for p in ['MCP', 'AIPP', 'CAPP', 'CVP', 'NPP', 'UPP']:
		 savi_vers[p] = open(path.join(savi_in_dir, p+'.txt')).readline().strip().split('\t')[-1]
	return savi_vers

if __name__ == '__main__':
	exit(main())
