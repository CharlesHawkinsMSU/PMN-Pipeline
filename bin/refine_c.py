#!/usr/bin/env python3

from sys import stdin, stdout, stderr
import argparse as ap
import re
import pmn
import subprocess
import shutil
import os

re_disp = re.compile(b'Actual display used: (:[0-9]+)')
def main():
    par = ap.ArgumentParser(description = 'Runs refine-c on the PMN pipeline')
    pmn.add_standard_args(par)

    args = par.parse_args()
    (config, orgtable, orglist) = pmn.read_pipeline_files(args)
    refine_c(config, orgtable, orglist)

def refine_c(config, orgtable, orglist):
    pmn.info('==Running Refine-C==')
    xpra_path = shutil.which('xpra')
    if xpra_path is not None:
        pmn.info('xpra found, using xpra')
        res = subprocess.run([xpra_path, 'start'], capture_output = True)
        display = re_disp.search(res.stderr).group(1).decode()
        env = os.environ.copy()
        env['DISPLAY'] = display
    else:
        env = None
    with pmn.PMNPathwayTools(config, env = env, args = ['-www']) as ptools:
        pt_disp = ptools.send_cmd('(sys:getenv "DISPLAY")')
        pmn.info(f'Display is {pt_disp}')
        for org in orglist:
            org_entry = orgtable[org]
            pmn.info(f'==Running checks for {org}Cyc==')
            ptools.so(org)
            ptools.send_cmd('(run-all-checks)')
            pmn.info(f'==Setting author list for {org}Cyc==')
            ptools.send_cmd(f'(put-slot-values \'{org} \'pgdb-authors \'({org_entry["Authors"]}))')
            pmn.info(f'==Generating cellular overview for {org}Cyc==')
            ptools.send_cmd('(initialize-gui)')
            ptools.send_cmd('(update-overview :batch-mode? t :show-progress? nil :web-cel-ov? t)')
            pmn.info(f'==Saving {org}Cyc==')
            ptools.send_cmd('(save-kb)')
    if xpra_path is not None:
        pmn.info('Stopping xpra')
        subprocess.run([xpra_path, 'stop', display])

if __name__ == "__main__":
    main()
