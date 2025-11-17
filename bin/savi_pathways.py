#!/usr/bin/env python3

from sys import stdout, stderr
import argparse as ap
from os import path, listdir

import pmn
import util

# List of savi files and the number of header lines in each one
savi_files = {'AIPP.txt':2, 'CAPP.txt':4, 'CVP.txt':2, 'MANUAL.txt':2, 'MCP.txt':2, 'NPP.txt':2, 'UPP.txt':2}

def main():
    par = ap.ArgumentParser(description = 'This script can either list out all the unsavitized pathways in a PGDB or list out all savitized pathways in the savi input files')
    par.add_argument('-i', '--savi-input', required = True, help = 'The SAVI input directory containing AIPP.txt, etc.', dest = 'i')
    par.add_argument('-o', '--output', required = False, help = 'Where to save the list of pathways. If not given, output is to stdout', dest = 'o')
    par.add_argument('-p', '--pgdb', '--pgdbs', nargs='*', required = False, help = 'Either a single pgdbd directory or a directory containing one or more of them (if the given directory contains a file called default-version then it is assumed to be a single pgdb). If this option is given, the final output will be a list of pathways that appear in the given pgdb(s) but not in the SAVI input files. If no -p option is given, the final output will instead be a list of all pathways present in the savi input files. Can be given multiple times for the union of PGDBs in all given directories', dest = 'p')
    par.add_argument('-x', '--exclude', nargs = "*", required = False, help = 'Ignore pathways that are present in the given PGDB(s). Follows the same rules as -p for finding PGDBs.', dest = 'x')
    args = par.parse_args()
    savi_pathways(args.i, args.p, args.o, args.x)

def savi_pathways(savi_indir, pgdb_in, outfilename, exclude = []):
    if outfilename is None or outfilename == '-':
        outfile = stdout
    else:
        try:
            outfile = open(outfilename, 'w', errors = 'surrogateescape')
        except IOError as e:
            pmn.error('%s: %s\n'%(outfilename, e.strerror))
            exit(1)
    with outfile:
        savi_pathways = get_savi_pathways(savi_indir)
        if pgdb_in:
            pathways = set()
            for pgdb_indir in util.as_list(pgdb_in):
                pgdb_pathways = get_pathways_for_pgdbs(pgdb_indir)
                pathways.update(pgdb_pathways)
            pathways.difference_update(savi_pathways)
            pmn.info('There are %s unsavitized pathways'%len(pathways))
        else:
            pathways = savi_pathways
            pmn.info('There are %s savi pathways'%len(pathways))
        for exclude_dir in util.as_list(exclude):
            exclude_pathways = get_pathways_for_pgdbs(exclude_dir)
            pathways.difference_update(exclude_pathways)
            pmn.info('There are %s non-excluded pathways'%len(pathways))
        outfile.write('\n'.join(pathways) + '\n')

# Returns savi pathway as a set
def get_savi_pathways(savi_dir):
    if not path.exists(savi_dir):
        stderr.write('Error: %s: not found'%savi_dir)
        exit(1)

    if not path.isdir(savi_dir):
        stderr.write('Error: %s is not a directory'%savi_dir)
        exit(1)
    # This dict maps from paths to the first file that contained it. The only reason we track the file is so if a pathway appears in more than one file we can tell the user which ones
    pathways = set()
    for savi_file, header_lines in savi_files.items():
        filepath = path.join(savi_dir, savi_file)
        try:
            with open(filepath, 'r', errors = 'surrogateescape') as infile:
                pmn.info('Reading %s'%filepath)
                for _ in range(header_lines):
                    next(infile)
                for line in infile:
                    line = line.rstrip()
                    pwy = line.split('\t')[0]
                    pathways.add(pwy)
        except IOError as e:
            pmn.warn('%s: %s'%(filepath, e.strerror))
            continue
    pmn.info('SAVI has %s pathways'%len(pathways))
    return pathways

# Returns the path to the pgdb default version pathway.dat or None if there is no default-version file
def get_pgdb_pwy_dat(pgdb_dir):
    try:
        versfile = open(path.join(pgdb_dir, 'default-version'), 'r', errors = 'surrogateescape')
        version = versfile.read().rstrip()
        pmn.info('%s appears to be a pgdb'%pgdb_dir)
    except IOError as e:
        return None
    return path.join(pgdb_dir, version, 'data', 'pathways.dat')

def get_pathways_for_pgdbs(pgdbs_dir):
    pwy_file = get_pgdb_pwy_dat(pgdbs_dir)
    if pwy_file is not None:
        return get_pathways_from_datfile(pwy_file)
    else:
        pathways = set()
        for d in listdir(pgdbs_dir):
            dirname = path.join(pgdbs_dir, d)
            pmn.info('Checking %s as a possible pgdb'%dirname)
            if path.isdir(dirname):
                pwy_file = get_pgdb_pwy_dat(dirname)
                if pwy_file is not None:
                    pathways = pathways.union(get_pathways_from_datfile(pwy_file))
                else:
                    pmn.warn('%s does not appear to be a pgdb'%dirname)
                    continue
        pmn.info('PGDBs in %s have %s pathways in total'%(pgdbs_dir, len(pathways)))
        return pathways

def get_pathways_from_datfile(datfile):
    try:
        infile = open(datfile, 'r', errors = 'surrogateescape')
    except IOError as e:
        pmn.warn('%s: %s'%(datfile, e.strerror))
        return set()
    pathways = set()
    with infile:
        for line in infile:
            if line.startswith('UNIQUE-ID'):
                line = line.rstrip()
                pathways.add(line.split(' - ')[1])
    pmn.info('%s has %s pathways'%(datfile, len(pathways)))
    return pathways

if __name__ == '__main__':
    exit(main())
