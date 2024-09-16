#!/usr/bin/env python3

from sys import stdin, stdout, stderr
import os
from os import path
import datetime
import pmn

if __name__ == '__main__':
    stderr.write('tables.py is a library used by PMN for exporting various information tables during the pipeline. It does nothing when run on its own. The front-end script for the PMN pipeline is pmn-pipeline.py')
    exit(1)

def export_custom_dumps(config, orgtable, orglist, ptools, args):
    dumps_dir = config.setdefault('proj-dumps-dir',  os.path.join(args.proj, 'custom-dumps'))
    os.makedirs(dumps_dir, exist_ok = True)
    for org in orglist:
        ptools.so(org)
        date = datetime.date.today().strftime('%Y%m%d')
        cpd_filename = path.join(dumps_dir, f'{org.lower()}cyc_compounds.{date}')
        pwy_filename = path.join(dumps_dir, f'{org.lower()}cyc_pathways.{date}')
        pmn.info(f'Dumping compounds for {org}Cyc to {cpd_filename}')
        ptools.send_cmd(f'(pmn-dump-compounds "{cpd_filename}")')
        pmn.info(f'Dumping pathways for {org}Cyc to {pwy_filename}')
        ptools.send_cmd(f'(pmn-dump-pathways "{pwy_filename}")')

