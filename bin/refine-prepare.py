#!/usr/bin/env python3

from sys import stdin, stdout, stderr
import argparse as ap
import pmn
from os import path
import shutil
import subprocess

par = ap.ArgumentParser(description = 'Prepare the master files for running refine-a, b, and c')

pmn.add_standard_pmn_args(par, 'prepared')

args = par.parse_args()

(config, table, org_list) = pmn.read_pipeline_files(args)
try:
	ptools = config['ptools-exe']
	print(f'Info: Pathway Tools executable is at {ptools}')
	masters_folder = config['proj-masters-dir']
	print(f'Info: Looking for PGDB master files in {masters_folder}\n')

	# Iterate over the species
	for orgid in org_list:
		org_path = path.join(masters_folder, orgid)
		print(f'Info: Preparing {orgid}')
		org_entry = table[orgid]

		# write overview.master foreach species
		overview_master = open(path.join(org_path, 'overview.master'), 'w')
		overview_master.write(f's_ptools\t{ptools}\nlisp\t(so \'{orgid})\nlisp\t(update-overview :save? t :batch-mode? t :show-progress? nil)\nlisp\t(save-kb)\n')
		overview_master.close()

		# write refine-a.master foreach species
		start_timestamp = org_entry.setdefault('START timestamp', 0)
		end_timestamp = org_entry.setdefault('END timestamp', 99999999999)
		refine_a_text = f'''# Master file for PMN release pipeline
# Provides all the data needed for release the PGDB of a species
#
# Current version by Charles Hawkins @ Rhee-lab, October 2022
# Original by Chuan Wang and Peifen Zhang @ Rhee-lab
# Department of Plant Biology
# Carnegie Institution for Science
# Date: June 19, 2014

# Columns: type, value

# species parameters
#
# s_* are string values
# fs_* are files that contain string values to be passed to lisp commands
# f_* are files that will be used by scripts, only whose name will be passed to commands

s_ptools	{ptools}
s_pgdb	{orgid}
s_sp_name	"{org_entry["Species Name"]}"
s_sp_folder	{orgid}
# set s_start_timestamp to 0 and s_end_timestamp to current timestamp for new PGDB
s_start_timestamp	{start_timestamp}
s_end_timestamp	{end_timestamp}
s_seq_source	{org_entry["Seq Source"]}
s_seq_source_acc	accession-1

s_cm_folder	common
s_script_folder	perl_scripts
fs_pwy_del	remove
fs_pwy_add	ic

# other parameters
#
# NOTE: Citations are propagated from aracyc to the destination {orgid}, so please create citations first in source PGDB 'ara.

s_curator	{org_entry["Curator"]}
#s_savi_citation	|PUB-PMNUPP2013| |PUB-PMNAIPP2013| |PUB-PMNRXN2013| |PUB-PMNRXNTAXON2013| |PUB-PMNTAXON2013| |PUB-PMNIC2013| |PUB-PMNSP2013|
s_savi_citation	{org_entry["SAVI Citation"]}
s_e2p2_citation	{org_entry["E2P2 Citation"]}
s_enzrxn_citation	{org_entry["Enzrxn Citation"]}

f_savi_comment	savi-comment
f_enz_name	{org_entry["ENZ name file"]}
f_enz_name_meta	{org_entry["RXN Map"]}
f_meta_link	{org_entry["PWY Metacyc"]}
f_plant_link	{org_entry["PWY Plantcyc"]}

fs_new_proteins	s_pgdb-s_start_timestamp-s_end_timestamp-new-proteins
fs_new_enzrxns	s_pgdb-s_start_timestamp-s_end_timestamp-new-enzrxns


# lisp and perlcyc commands
#
# delete invalid pathways
lisp	(so 's_pgdb)	select organism
lisp	(loop for p in '(fs_pwy_del) do (delete-frame-and-dependents p))

# import pathways from metacyc inferred by curator (this function does not import enzymes with the pathway)
lisp	(so 'meta)
lisp	(so 's_pgdb)
lisp	(import-pathways '(fs_pwy_add) (find-kb 'metabase) (current-kb))

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
lisp	(so 'ara)
lisp	(so 's_pgdb)
lisp	(import-frames :frames '({org_entry["Authors"]}) :src-kb (find-org 'ara) :dst-kb (find-org 's_pgdb))

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
perl	s_script_folder/add_name_enzrxn_timestamp.pl s_pgdb s_cm_folder/f_enz_name s_cm_folder/f_enz_name_meta s_start_timestamp s_end_timestamp

# add/reload sequence source (i.e. phytozome) DBLink to genes
perl	s_script_folder/reload_dblinks_phytzome_protein.pl s_pgdb s_seq_source s_seq_source_acc

# add/reload metacyc and plantcyc DBLink to pathways
perl	s_script_folder/reloadPWYlinks2metacyc.plantcyc.pl s_pgdb s_cm_folder/f_meta_link s_cm_folder/f_plant_link

# select the current PGDB
lisp	(so 's_pgdb)
# save the kb
lisp	(save-kb)
'''
		refine_a = open(path.join(org_path, 'refine-a.master'), 'w')
		refine_a.write(refine_a_text)
		refine_a.close()

		# Copy SAVI output and common directories into the org's master directory
		shutil.copytree(path.join(config['proj-savi-dir'], 'output', orgid), path.join(org_path, orgid))
		shutil.copytree(config['savi-common-dir'], path.join(org_path, 'common'))

		# Fix newlines. Subsequent scripts expect files with unix newlines and the .txt extension stripped off
		for file in os.listdir(path.join(org_path, orgid)):
			newfilename = file.replace('.txt', '')
			file_handle = open(file)
			file_contents = file_handle.read
			file_handle.close()
			newfile_handle = open(newfilename, 'w')
			newfile_handle.write(file_contents)
			newfile_handle.close()

		# Write blastset.master
		blastset = open(path.join(org_path, 'blastset.master'), 'w')
		blastset.write('s_ptools\t{ptools}\ns_script_folder\tperl_scripts\nperl\ts_script_folder/create-blast-dataset-pgdb.pl $s "{org_entry["Version"]}" "{org_entry["Species Name"]}" "{org_entry["Seq Source"]}" {path.join(config["proj-fasta-dir"], org_entry["Sequence File"])} {config["proj-blastsets-dir"]}\n')
		blastset.close()

except KeyError as e:
	stderr.write(f'{args.c}: Required variable {e.args[0]} not found')
	exit(1)
except IOError as e:
	stderr.write(f'{e.filename}: {e.strerror}\n')
	exit(1)

