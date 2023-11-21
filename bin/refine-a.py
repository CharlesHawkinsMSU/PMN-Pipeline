#!/usr/bin/env python3
import pmn
import argparse as ap
from sys import stderr
from os import path

par = ap.ArgumentParser(description = 'Runs the refine-a step of PMN\'s PGDB creation pipeline. Refine-a applies SAVI\'s curation decisions to the database, imports authors from AraCyc, fixes enxrxn names, adds E2P2 and SAVI citations and PMN Group curation credits, adds a SAVI comment to pathways added or approved by SAVI, and adds sequence source DBLink to genes and metacyc/plantcyc DBLink to pathways')

pmn.add_standard_pmn_args(par, action = 'refined')

args = par.parse_args()

try:
    (config, table, org_list) = pmn.read_pipeline_files(args)
    ptools_exe = config['ptools-exe']
    ptools_sock = config['ptools-socket']
    authors = config['authors']
    savi_dir = config['proj-savi-dir']
    common_dir = config['savi-common-dir']
except KeyError as e:
    stderr.write(f'The key {e.args[0]} is required for refine-a, but was not found in {args.c}\n')
    exit(1)
pmn.check_exists(files = [ptools_exe], sockets = [ptools_sock], dirs = [savi_dir, common_dir], progname = 'refine-a')

for org in org_list:
    try:
        org_entry = table[orgid]
    except KeyError:
        stderr.write('Organism {orgid} not found in PGDB table\n')
        exit(1)
    savi_out = path.join(savi_dir, 'output', orgid)
    ic = path.join(savi_out, 'ic.txt')
    remove = path.join(savi_out, 'remove.txt')
    pmn.check_exists(files = [ic, remove])
    pgdb_authors = org_entry['Authors']
    curator = org_entry['Curator']
    savi_citation = org_entry['SAVI Citation']
    print(f'Initial sanity checks passed for organism {org}, proceeding with refine-a for this organism')
    pmn.verbose = True
    pmn.ptools_so('meta')
    pmn.ptools_so('plant')
    pmn.ptools_so('ara')
    pmn.ptools_so(org)
    print(f'Deleting pathways from SAVI output file remove.txt from {org}')
    print(pmn.send_ptools_cmd("(loop for p in '(fs_pwy_del) do (if (coercible-to-frame-p p) (delete-frame-and-dependents p) (format nil \"Warning: Frame ~A is in the list of frames to be deleted from ~ACYC but was not found in ~ACYC~%\" p 's_pgdb 's_pgdb)))"))
    print(f'Adding pathways from SAVI output file ic.txt to {org}')
    print(pmn.send_ptools_cmd("(loop for pwy in '(fs_pwy_add) do (if (coercible-to-frame-p pwy :kb (find-org 'meta)) (import-pathways (list pwy) (find-org 'meta) (current-kb)) (if (coercible-to-frame-p pwy :kb (find-org 'plant)) (import-pathways (list pwy) (find-org 'plant) (current-kb)) (format nil \"Warning: Pathway ~A not found in MetaCyc or PlantCyc, will not be imported into ~ACYC~%\" pwy 's_pgdb))))"))
    print(f'Adding curator {curator} to {org}')
    print(pmn.send_ptools_cmd(f"(import-frames :frames '({curator}) :src-kb (find-org 'ara) :dst-kb (find-org '{org}))"))
    print(f'Adding SAVI citations {savi_citations} to {org}')
    print(pmn.send_ptools_cmd(f"(import-frames :frames '({savi_citations}) :src-kb (find-org 'ara) :dst-kb (find-org '{org}))"))
    print(f'Adding E2P2 citation {e2p2_citation} to {org}')
    print(pmn.send_ptools_cmd(f"(import-frames :frames '({e2p2_citation}) :src-kb (find-org 'ara) :dst-kb (find-org '{org}))"))

