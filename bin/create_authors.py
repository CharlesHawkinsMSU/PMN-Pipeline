#!/usr/bin/env python3

from sys import stdin, stdout, stderr
import argparse as ap
import pmn

def main():
	par = ap.ArgumentParser(description='Creates the Organization and Author frames for the given PGDB. Organization and Author info will be taken from the files specified in the pipeline config file')
	pmn.add_standard_pmn_args(par, 'given the Organization and Author frames')
	args = par.parse_args()
	(config, orgtable, orglist) = pmn.read_pipeline_files(args)
	pmn.verbose = args.v
	create_frames(config, orgtable, orglist)

def create_frames(config, orgtable, orglist = None, ptools = None):
	pmn.info(f'Will create organizations and authors for {len(orglist)} organism(s)')
	organization_table = pmn.read_table_file(config['organizations-file'], 'FrameID')
	pmn.info(f'Read {len(organization_table)} organization(s) from {config["organizations-file"]}')
	author_table = pmn.read_table_file(config['authors-file'], 'FrameID')
	pmn.info(f'Read {len(author_table)} author(s) from {config["authors-file"]}')


	# Add to each author a set of orgids as "_orgids"
	for orgid in orglist:
		entry = orgtable[orgid]
		orgid_authors = entry['Authors'].split(' ')
		orgn_set = set()
		for author in orgid_authors:
			try:
				auth_entry = author_table[author]
			except KeyError:
				pmn.error(f'Author {author} referenced by pgdb {orgid} does not have an entry in {config["authors-file"]}')
				exit(1)
			author_orgns = auth_entry['Affiliations'].split(' ')
			for orgn in author_orgns:
				if orgn not in organization_table:
					pmn.error(f'Organization {orgn} referenced by author {author} in {config["authors-file"]} does not have an entry in {config["organizations-file"]}')
					exit(1)
				orgn_set.add(orgn)
			auth_entry.setdefault('_orgids', set())
			auth_entry['_orgids'].add(orgid)
		entry['_orgns'] = orgn_set

	# Add to each organization a set of authors as "_authors" and a set of orgids as "_orgids"
	for author, entry in author_table.items():
		entry.setdefault('_orgids', set())
		author_orgns = entry['Affiliations'].split(' ')
		for orgn in author_orgns:
			try:
				orgn_entry = organization_table[orgn]
			except KeyError:
				pmn.error(f'Organization {orgn} referenced by author {author} in {config["authors-file"]} does not have an entry in {config["organizations-file"]}')
				exit(1)
			orgn_entry.setdefault('_authors', set())
			orgn_entry['_authors'].add(author)
			orgn_entry.setdefault('_orgids', set())
			orgn_entry['_orgids'].update(entry['_orgids'])


	if ptools is None:
		pmn.info(f'Create Authors step will start Pathway Tools from {config["ptools-exe"]} and connect to the socket at {config["ptools-socket"]}')
		ptools = pmn.PathwayTools(config['ptools-exe'])
	orgs_avail = ptools.get_org_list()
	origin_orgids = {}  # Which organism db has been used to create each Organization frame (it will then be propagated to all the others)

	# We will go through each pgdb in the orglist, then go through each organization it needs (the "_orgns" field). For each such organization, we check if it already exists in this pgdb (would've been imported from metacyc or plantcyc due to authorship of one or more frames) in which case we fill in the info from our Organizations file. Otherwises create it anew in the database
	for orgid in orglist:
		pmn.info(f' Creating Organization frames for {orgid}')
		org_entry = orgtable[orgid]
		ptools.so(orgid)
		for orgn in org_entry['_orgns']:
			orgn_symbol = pmn.as_lisp_symbol(orgn)
			if ptools.send_cmd(f'(coercible-to-frame-p {orgn_symbol})') != 'T':
				pmn.info(f'  Organization frame {orgn} does not exist, creating blank frame')
				create_cmd = f'(create-instance {orgn_symbol} \'(|Organizations|))'
				pmn.info(f'  {create_cmd}')
				ptools.send_cmd(create_cmd)
			else:
				pmn.info(f'  Organization frame {orgn} exists, will fill in info')
			for slot, val in organization_table[orgn].items():
				if not (slot == 'FrameID' or slot.startswith('_')):
					put_cmd = f'(put-slot-value {orgn_symbol} \'{slot} "{val}")'
					pmn.info(f'  {put_cmd}')
					ptools.send_cmd(put_cmd)
		pmn.info(f' Creating Author frames for {orgid}')
		for author in org_entry['Authors'].split(' '):
			author_symbol = pmn.as_lisp_symbol(author)
			if ptools.send_cmd(f'(coercible-to-frame-p {author_symbol})') != 'T':
				pmn.info(f'  Author frame {author} does not exist, creating blank frame')
				create_cmd = f'(create-instance {author_symbol} \'(|People|))'
				pmn.info(f'  {create_cmd}')
				ptools.send_cmd(create_cmd)
			else:
				pmn.info(f'  Author frame {author} exists, will fill in info')
			for slot, val in author_table[author].items():
				if not (slot == 'FrameID' or slot.startswith('_')):
					put_cmd = f'(put-slot-value {author_symbol} \'{slot} "{val}")'
					pmn.info(f'  {put_cmd}')
					ptools.send_cmd(put_cmd)

		pmn.info(f' Saving database {orgid}')
		ptools.send_cmd('(save-kb)')
				

if __name__ == "__main__":
	exit(main())
