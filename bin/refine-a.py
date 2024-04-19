#!/usr/bin/env python3
import pmn
import argparse as ap
from sys import stderr
from os import path
def main():
    par = ap.ArgumentParser(description = 'Runs the refine-a step of PMN\'s PGDB creation pipeline. Refine-a applies SAVI\'s curation decisions to the database, imports authors from AraCyc, fixes enxrxn names, adds E2P2 and SAVI citations and PMN Group curation credits, adds a SAVI comment to pathways added or approved by SAVI, and adds sequence source DBLink to genes and metacyc/plantcyc DBLink to pathways')

    pmn.add_standard_pmn_args(par, action = 'refined')

    args = par.parse_args()
    (config, orgtable, orglist) = pmn.read_pipeline_files(args)
    refine_a(config, orgtable, orglist)

def refine_a(config, orgtable, orglist, ptools = None)
    try:
        savi_dir = config['proj-savi-dir']
        common_dir = config['proj-common-dir']
    except KeyError as e:
        stderr.write(f'The key {e.args[0]} is required for refine-a, but was not found in {args.c}\n')
        exit(1)
    if ptools is None:
        ptools = pmn.PMNPathwayTools(config)
    for org in orglist:
        try:
            entry = orgtable[org]
        except KeyError:
            stderr.write('Organism {org} not found in PGDB table\n')
            exit(1)
        savi_out = path.join(savi_dir, 'output', org)
        ic = path.join(savi_out, 'ic.txt')
        remove = path.join(savi_out, 'remove.txt')
        pmn.check_exists(files = [ic, remove])
        pgdb_authors = entry['Authors']
        curator = '|pmngroup|'
        savi_citation = entry['SAVI Citation']
        ext_dbs = ['|PHYTOZOME|', '|MAIZEGDB|', '|ENSEMBL-PROTEIN|']
        pmn.info(f'Initial sanity checks passed for organism {org}, proceeding with refine-a for this organism')
        pmn.verbose = True
        ptools.so('meta')
        ptools.so('plant')
        ptools.so('ara')
        ptools.so(org)
        pmn.info(f'Deleting pathways from SAVI output file remove.txt from {org}')
        pmn.info(ptools.send_cmd("(loop for p in '(fs_pwy_del) do (if (coercible-to-frame-p p) (delete-frame-and-dependents p) (format nil \"Warning: Frame ~A is in the list of frames to be deleted from ~ACYC but was not found in ~ACYC~%\" p 's_pgdb 's_pgdb)))"))
        pmn.info(f'Adding pathways from SAVI output file ic.txt to {org}')
        pmn.info(ptools.send_cmd("(loop for pwy in '(fs_pwy_add) do (if (coercible-to-frame-p pwy :kb (find-org 'meta)) (import-pathways (list pwy) (find-org 'meta) (current-kb)) (if (coercible-to-frame-p pwy :kb (find-org 'plant)) (import-pathways (list pwy) (find-org 'plant) (current-kb)) (format nil \"Warning: Pathway ~A not found in MetaCyc or PlantCyc, will not be imported into ~ACYC~%\" pwy 's_pgdb))))"))
        pmn.info(f'Adding curator {curator} to {org}')
        pmn.info(ptools.send_cmd(f"(import-frames :frames '({curator}) :src-kb (find-org 'ara) :dst-kb (find-org '{org}))"))
        pmn.info(f'Adding SAVI citations {savi_citations} to {org}')
        pmn.info(ptools.send_cmd(f"(import-frames :frames '({savi_citations}) :src-kb (find-org 'ara) :dst-kb (find-org '{org}))"))
        pmn.info(f'Adding E2P2 citation {e2p2_citation} to {org}')
        pmn.info(ptools.send_cmd(f"(import-frames :frames '({e2p2_citation}) :src-kb (find-org 'ara) :dst-kb (find-org '{org}))"))
        pmn.info(f'Importing external-db frames from AraCyc to {org}')
        pmn.info(ptools.send_cmd(f"(import-frames :frames '({' '.join(ext_dbs)}) :src-kb (find-org 'ara) :dst-kb (find-org '{org}))"))
        pmn.info(f'Setting PGDB author list for {org} to {pgdb_authors}')
        pmn.info(ptools.send_cmd(f"(put-slot-values '{org} 'PGDB-AUTHORS '({pgdb_authors}))"))

if __name__ == '__main__':
    main()
