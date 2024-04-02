#!/usr/bin/env python3

from sys import stdin, stdout, stderr, version_info
import argparse as ap
import pmn
import time
from os import path
import os
import shutil

def main():

	par = ap.ArgumentParser(description = 'Prepare the master files for running refine-a, b, and c')

	pmn.add_standard_pmn_args(par, 'prepared for the refine steps')

	args = par.parse_args()

	(config, table, org_list) = pmn.read_pipeline_files(args)
	pmn.verbose = args.v
	return refine_prepare(config, table, org_list)
def generate_common_files(config, ptools, refs = ['Plant', 'Meta']):
	common_dir = config['proj-common-dir']
	pmn.info(f'Generating common files in {common_dir}')
	ptools.require_pgdbs(refs)
	all_pwy_file_pre = path.join(common_dir, "all_pwy")
	main_ref = refs[0]
	date = time.strftime('%d-%b-%Y')
	for ref in refs:
		all_pwy_file = all_pwy_file_pre + '.' + ref.lower()
		pmn.info(f'Generating all-pathway list for {ref}Cyc: {all_pwy_file}')
		ptools.so(ref)
		#ref_vers = ptools.send_cmd('(kb-version (current-kb))').strip('"')
		all_pwy_cmd = f'(to-file-or-stream "{all_pwy_file}" (format stream "#{ref}Cyc Version: ~A~%" (kb-version (current-kb))) (write-list stream (loop for p in (all-pathways) collect (get-frame-handle p))))'
		ptools.send_cmd(all_pwy_cmd)
	pmn.info(f'Generating ec_name.map.parsed')
	ptools.so('ec-numbers')
	ec_names_file = path.join(common_dir, "ec_name.map.parsed")
	ec_numbers_cmd = f'(to-file-or-stream "{ec_names_file}" (format stream "# ENZYME nomenclature database Version: {date}~%")(print-alist stream (loop for ec in (get-class-all-instances "EC-Numbers") collect (list (gsv ec \'ec-id) (gsv ec \'common-name)))))'
	ptools.send_cmd(ec_numbers_cmd)

	pmn.info('Generating {main_ref}cyc-rxn-name-mapping')
	ptools.so(main_ref)
	ref_names_file = path.join(common_dir, "{main_ref}-rxn-name-mapping")
	ptools.send_cmd(f'(setq eckb (find-kb \'ec-numbers))')
	ptools.send_cmd(f'(to-file-or-stream "{ref_names_file}" (format stream "# ENZYME nomenclature database Version: #{main_ref}Cyc Version: ~%" (kb-version (current-kb)))(print-alist stream (loop for r in (get-class-all-instances "Reactions") for cn = (gsv r \'common-name) for name = (if cn cn (if (coercible-to-frame-p (setq ec (get-slot-value r \'ec-number)) :kb eckb) (get-slot-value ec \'common-name :kb eckb) nil)) when name collect (list (gfh r) name))))')

		

def refine_prepare(config, table, org_list, ptools = None):
	if version_info.major < 3 or version_info.minor < 8:
		pmn.error(f'Refine-prepare requires Python 3.8 or later to run (currently running Python {version_info.major}.{version_info.minor}.{version_info.micro})')
		exit(1)
	try:
		ptools_exe = config['ptools-exe']
		pmn.info(f'Pathway Tools executable is at {ptools_exe}')
		masters_folder = config['proj-masters-dir']
		pmn.info(f'Looking for PGDB master files in {masters_folder}')
		common_dir = config['proj-common-dir']
		if not ptools:
			ptools = pmn.PMNPathwayTools(config)
		generate_common_files(config, ptools)
		pt_vers = ptools.send_cmd('(ptools-version)').strip('"')

		# Iterate over the species
		for orgid in org_list:
			org_path = path.join(masters_folder, orgid)
			pmn.info(f'Preparing {orgid}')
			org_entry = table[orgid]
			year = org_entry['Citation Year']

			refdb = org_entry['Reference DB']

			pmn.info(f'Writing savi-comment for {orgid}')
			org_common_dir = path.join(org_path, 'common')
			os.makedirs(org_common_dir, exist_ok = True)
			savi_comment = open(path.join(org_common_dir, 'savi-comment'), 'w')
			savi_comment_text = f'''

# Provides SAVI citations and comments
#
# Current version by Charles Hawkins @ Rhee Lab
# Michigan State University
# Dec 21, 2023
#
# Original by Chuan Wang and Peifen Zhang @ Rhee-lab
# Date: June 19, 2014
#
# The code that generates this file is in refine_prepare.py
#
# Columns: savi-file	citation	comment
#
## To-do:
## comment of ic is the same as comp-upp
## superpathway is not included in this file yet
#
comp-rxn	PMNRXN{year}:EV-COMP-HINF	<i>Supporting evidence for this pathway in $species</i>: Enzyme(s) that catalyzes one or more of the key reactions in this pathway were predicted in $species. <A href=\\"http://www.plantcyc.org/about/savi_pipeline\\">[more info]</A>
aipp	PMNAIPP{year}:EV-COMP-HINF	<i>Supporting evidence for this pathway in $species</i>: This pathway is on the list of Accept-If-Predicted Pathways (AIPP) generated for the COMET project, which includes pathways that are present widely throughout the plant kingdom. The AIPP list contains pathways that are automatically accepted if they are predicted for any plant species database. However, AIPP pathways are not automatically imported into databases if they are not predicted by the Pathologic program. <A href=\\"https://www.plantcyc.org/about/savi-pipeline\\">[more info]</A>
comp-rxn-taxon	PMNRXNTAXON{year}:EV-COMP-HINF	<i>Supporting evidence for this pathway in $species</i>: The expected taxonomic range of the pathway includes $species, and, enzyme(s) that catalyzes one or more of the key reactions in this pathway were predicted in $species. <A href=\\"http://www.plantcyc.org/about/savi_pipeline\\">[more info]</A>
comp-rxn-warning	PMNRXN{year}:EV-COMP-HINF	<i>Supporting evidence for this pathway in $species</i>: Enzyme(s) that catalyzes one or more of the key reactions in this pathway were predicted in $species. However, there is some evidence in the literature that this pathway is specialized to species that do not include $species. <A href=\\"http://www.plantcyc.org/about/savi_pipeline\\">[more info]</A>
comp-taxon	PMNTAXON{year}:EV-COMP-HINF	<i>Supporting evidence for this pathway in $species</i>: The expected taxonomic range of the pathway includes $species. <A href=\\"http://www.plantcyc.org/about/savi_pipeline\\">[more info]</A>
comp-upp	PMNUPP{year}:EV-COMP-HINF	<i>Supporting evidence for this pathway in $species</i>: This pathway is on the list of Ubiquitous Plant Pathways (UPP) that was generated for the COMET project, which includes pathways that are thought to be present in all or most land plants. All UPP pathways are automatically imported into databases regardless of Pathologic predictions. <A href=\\"https://www.plantcyc.org/about/savi-pipeline\\">[more info]</A>
ic	PMNIC{year}:EV-IC	<i>Supporting evidence for this pathway in $species</i>: This pathway is on the list of Ubiquitous Plant Pathways (UPP) that was generated for the COMET project, which includes pathways that are thought to be present in all or most land plants. All UPP pathways are automatically imported into databases regardless of Pathologic predictions. <A href=\\"https://www.plantcyc.org/about/savi-pipeline\\">[more info]</A>
comp-cvp	PMNCVP{year}:EV-COMP-HINF	<i>Supporting evidence for this pathway in $species</i>: This pathway is on the list of Common Viridiplantae Pathways (CVP) generated for the COMET project, which includes pathways that are thought to be present in all or most Viridiplantae species including algae. All UPP pathways are automatically imported into databases regardless of Pathologic predictions. <A href=\\"http://www.plantcyc.org/about/savi_pipeline\\">[more info]</A>
super	PMNSP{year}:EV-COMP-HINF	<i>This superpathway was predicted to exist in this species using PathoLogic (version {pt_vers}). All of its sub-pathway(s) have been accepted using the SAVI (v{config["savi-version"]}) pipeline. Please see the subpathways to determine the level of support for this superpathway in $species</i>: <A href=\\"http://www.plantcyc.org/about/savi_pipeline\\">[more info]</A>
'''
			savi_comment.write(savi_comment_text)

			# write overview.master foreach species
			pmn.info (f'Writing overview.master for {orgid}')
			overview_master = open(path.join(org_path, 'overview.master'), 'w')
			overview_master.write(f's_ptools\t{ptools_exe}\nlisp\t(so \'{orgid})\nlisp\t(update-overview :save? t :batch-mode? t :show-progress? nil)\nlisp\t(save-kb)\n')
			overview_master.close()

			# write refine-a.master foreach species
			pmn.info (f'Writing refine-a.master for {orgid}')
			start_timestamp = org_entry.setdefault('START timestamp', 0)
			end_timestamp = org_entry.setdefault('END timestamp', 99999999999)


			refine_a_text = f'''# Master file for PMN release pipeline
# Provides all the data needed for release the PGDB of a species
#
# Current version by Charles Hawkins @ Rhee-lab, Michigan State University, Dec 2023
#
# Original by Chuan Wang and Peifen Zhang @ Rhee-lab
# Date: June 19, 2014

# The code that generates this file is in refine_prepare.py

# Columns: type, value

# species parameters
#
# s_* are string values
# fs_* are files that contain string values to be passed to lisp commands
# f_* are files that will be used by scripts, only whose name will be passed to commands

s_ptools	{ptools_exe}
s_pgdb	{orgid}
s_sp_name	"{org_entry["Species Name"]}"
s_sp_folder	{orgid}
# set s_start_timestamp to 0 and s_end_timestamp to current timestamp for new PGDB
s_start_timestamp	{start_timestamp}
s_end_timestamp	{end_timestamp}
s_seq_source	{org_entry["Seq Source"]}
s_seq_source_acc	accession-1

s_cm_folder	common
s_script_folder	../perl_scripts
fs_pwy_del	remove
fs_pwy_add	ic

# other parameters
#
# NOTE: Citations are propagated from aracyc to the destination {orgid}, so please create citations first in source PGDB 'ara.

s_curator	|pmngroup|
#s_savi_citation	|PUB-PMNUPP2013| |PUB-PMNAIPP2013| |PUB-PMNRXN2013| |PUB-PMNRXNTAXON2013| |PUB-PMNTAXON2013| |PUB-PMNIC2013| |PUB-PMNSP2013|
s_savi_citation	{org_entry["SAVI Citation"]}
s_e2p2_citation	{org_entry["E2P2 Citation"]}
s_enzrxn_citation	{org_entry["Enzrxn Citation"]}

f_savi_comment	savi-comment
f_enz_name	{org_entry["ENZ name file"]}
f_rxn_name_map	{refdb}-rxn-name-mapping
f_meta_link	{org_entry["PWY Metacyc"]}
f_plant_link	{org_entry["PWY Plantcyc"]}

fs_new_proteins	s_pgdb-s_start_timestamp-s_end_timestamp-new-proteins
fs_new_enzrxns	s_pgdb-s_start_timestamp-s_end_timestamp-new-enzrxns


# lisp and perlcyc commands
#
# delete invalid pathways
lisp	(so 's_pgdb)	select organism
lisp	(loop for p in '(fs_pwy_del) do (if (coercible-to-frame-p p) (delete-frame-and-dependents p) (format T "Warning: Frame ~A is in the list of frames to be deleted from ~ACYC but was not found in ~ACYC~%" p 's_pgdb 's_pgdb)))

# import pathways from the reference DB inferred by curator (this function does not import enzymes with the pathway)
{"lisp	(so 'meta)" if org_entry["Also MetaCyc"] else ""}
lisp	(so '{refdb})
lisp	(so 's_pgdb)
lisp	(loop for pwy in '(fs_pwy_add) do (loop for ref-org in '({refdb}{" meta" if org_entry["Also MetaCyc"] else ""}) for ref-kb = (find-org ref-org) when (coercible-to-frame-p pwy :kb ref-kb) do (import-pathways (list pwy) ref-kb (find-org 's_pgdb)) and return ref-org finally (return nil)))

# add curator |pmngroup| to PGDB
lisp	(so 'ara)
lisp	(so 's_pgdb)
lisp	(import-frames :frames '(s_curator) :src-kb (find-org 'ara) :dst-kb (find-org 's_pgdb))

# add PMN citations about the latest SAVI pipeline
lisp	(import-frames :frames '(s_savi_citation) :src-kb (find-org 'ara) :dst-kb (find-org 's_pgdb))

# add PMN citation about the latest E2P2 pipeline
lisp	(import-frames :frames '(s_e2p2_citation) :src-kb (find-org 'ara) :dst-kb (find-org 's_pgdb))

# Edit 07/06/2017: Create external DBs in pgdbs
lisp	(so 'ara)
lisp	(so 's_pgdb)
lisp	(import-frames :frames '(|PHYTOZOME|) :src-kb (find-org 'ara) :dst-kb (find-org 's_pgdb))
lisp	(import-frames :frames '(|MAIZEGDB|) :src-kb (find-org 'ara) :dst-kb (find-org 's_pgdb))
lisp	(import-frames :frames '(|ENSEMBL-PROTEIN|) :src-kb (find-org 'ara) :dst-kb (find-org 's_pgdb))

# Edit 07/06/2017: Update Author List:
#	lisp	(so 'ara)
#	lisp	(so 's_pgdb)
#	lisp	(import-frames :frames '({org_entry["Authors"]}) :src-kb (find-org 'ara) :dst-kb (find-org 's_pgdb))

# get newly added proteins and enzrxns within the time range
perl	s_script_folder/get_new_proteins_enzrxns_within_timestamp_range.pl s_pgdb s_start_timestamp s_end_timestamp s_sp_folder

# add PMN credits "created by |pmngroup|" to protein and enzrxn
# depends on the files generated by last step
lisp	(loop for p in '(fs_new_proteins) unless (slot-has-value-p p 'Credits) do (add-credit-event p (make-credit-event 'created '(s_curator))) )
lisp	(loop for p in '(fs_new_enzrxns) unless (slot-has-value-p p 'Credits) do (add-credit-event p (make-credit-event 'created '(s_curator))) )

# add/reload SAVI citation:EV and generic comments to pathways
# BTW, update metacyc comments, keep other comments as is
perl	s_script_folder/savi_citations_and_comments.pl s_pgdb s_sp_name s_sp_folder s_cm_folder/f_savi_comment

# add/reload E2P2 citation:EV-COMP-AINF to enzrxns
perl	s_script_folder/e2p2_enzrxn_citations.pl s_pgdb s_enzrxn_citation s_start_timestamp s_end_timestamp

# add/reload enzyme common name to enzrxns
perl	s_script_folder/add_name_enzrxn_timestamp.pl s_pgdb s_cm_folder/f_enz_name s_cm_folder/f_rxn_name_map s_start_timestamp s_end_timestamp

# add/reload sequence source (i.e. phytozome) DBLink to genes
perl	s_script_folder/reload_dblinks_phytzome_protein.pl s_pgdb s_seq_source s_seq_source_acc

# add/reload metacyc and plantcyc DBLink to pathways
#perl	s_script_folder/reloadPWYlinks2metacyc.plantcyc.pl s_pgdb s_cm_folder/f_meta_link s_cm_folder/f_plant_link
lisp	(so 's_pgdb)
lisp	(loop for p in (all-pathways) do (loop for (dbname . dbrest) in (gsvs p 'dblinks) when (or (eq dbname 'meta) (eq dbname '{refdb})) do (remove-slot-value p 'dblinks (cons dbname dbrest))) when (coercible-to-frame-p (gfh p) :kb (find-org '{refdb})) do (add-slot-value p 'dblinks (list '{refdb} (gfh p))) when (coercible-to-frame-p (gfh p) :kb (find-org 'meta)) do (add-slot-value p 'dblinks (list 'meta (gfh p))))


# select the current PGDB
lisp	(so 's_pgdb)
# save the kb
lisp	(save-kb)
'''
			pmn.info(f'Writing refine-a.master for {orgid}')
			refine_a = open(path.join(org_path, 'refine-a.master'), 'w')
			refine_a.write(refine_a_text)
			refine_a.close()

			# Copy SAVI output and common directories into the org's master directory
			pmn.info(f'Copying SAVI output for {orgid}')
			shutil.copytree(path.join(config['proj-savi-dir'], 'output', orgid), path.join(org_path, orgid), dirs_exist_ok = True)
			pmn.info(f'Copying SAVI common files to {orgid}')
			shutil.copytree(common_dir, path.join(org_path, 'common'), dirs_exist_ok = True)

			# Fix newlines. Subsequent scripts expect files with unix newlines and the .txt extension stripped off
			pmn.info(f'Fixing newlines for {orgid}')
			for file in os.listdir(path.join(org_path, orgid)):
				file = path.join(org_path, orgid, file)
				newfilename = file.replace('.txt', '')
				file_handle = open(file)
				file_contents = file_handle.read()
				file_handle.close()
				newfile_handle = open(newfilename, 'w')
				newfile_handle.write(file_contents)
				newfile_handle.close()

			# Write blastset.master
			pmn.info(f'Writing blastset.master for {orgid}')
			blastset = open(path.join(org_path, 'blastset.master'), 'w')
			blastset.write(f's_ptools\t{ptools_exe}\ns_script_folder\t.\nperl\ts_script_folder/create-blast-dataset-pgdb.pl {orgid} "{org_entry["Version"]}" "{org_entry["Species Name"]}" "{org_entry["Seq Source"]}" {org_entry["Sequence File"]} {config["proj-blastsets-dir"]}\n')
			blastset.close()

	except KeyError as e:
		pmn.error(f'{config["_filename"]}: Required variable {e.args[0]} not found')
		exit(1)
	except IOError as e:
		pmn.error(f'{e.filename}: {e.strerror}')
		exit(1)
if __name__ == "__main__":
	exit(main())
