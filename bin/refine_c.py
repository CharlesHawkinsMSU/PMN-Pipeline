#!/usr/bin/env python3

from sys import stdin, stdout, stderr
import argparse as ap
import pmn

def main():
    par = ap.ArgumentParser(description = 'Runs refine-c on the PMN pipeline')
    pmn.add_standard_args(par)

    args = par.parse_args()
    (config, orgtable, orglist) = pmn.read_pipeline_files(args)
    refine_c(config, orgtable, orglist)

def refine_c(config, orgtable, orglist, ptools = None):
    pmn.info('==Running Refine-C==')
    if not ptools:
        ptools = pmn.PMNPathwayTools(config, args = ['-www'])
    pt_disp = ptools.send_cmd('(sys:getenv "DISPLAY")')
    pmn.info(f'Ptools reports that DISPLAY is {pt_disp}')
    if not config.setdefault('no-init-gui', False):
        ptools.send_cmd('(initialize-gui)')
    for org in orglist:
        entry = orgtable[org]
        pmn.info(f'==Running checks for {org}Cyc==')
        ptools.so(org)
        ptools.send_cmd('(run-all-checks)')
        pmn.info(f'==Setting author list for {org}Cyc==')
        ptools.send_cmd(f'(put-slot-values \'{org} \'pgdb-authors \'({entry["Authors"]}))')
        pmn.info(f'==Generating cellular overview for {org}Cyc==')
        ptools.send_cmd('(update-overview :batch-mode? t :save? t :show-progress? nil :web-cel-ov? nil)')
        pmn.info(f'==Saving {org}Cyc==')
        ptools.send_cmd('(save-kb)')

if __name__ == "__main__":
    main()
