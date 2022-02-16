#!/usr/bin/env python3

import argparse as ap
par = ap.ArgumentParser(help =  'Generates to-create.txt from a simplified pgdb-info.txt file')
par.add_argument(help = 'Input file. The columns are:\
        1: cyc name without the cyc, e.g. Ara\
        2: Species name, e.g. Arabidopsis thaliana col. Everything after the first two words is assumed to be subspecies names\
        3: NCBI Taxon ID\
        4: Fasta file\
        5: E2P2 file (optional, defaults to column 4.e2p2v4.orxn.pf)\
        5: Genome source\
        6: Unique ID (optional, defaults to auto-generated)', dest = 'i')
par.add_argument('-t', '--old-to-create', nargs = '*', help = 'Old to-create.txt files. All entries from these files will have their version invremented and will be included in the new file unless specified otherwise with -n', dest = 't')
par.add_argument('-n', '--no-keep-old-pgdbs', action = 'store_true', help = 'Don\'t include the PGDBs from the old file in the new one, only use them to generate a new unique ID', dest = 'n')
par.add_argument('-y', '--year', required = False, help = 'Year to use for the SAVI citations. Defaults to the current year according to the system clock', dest = 'y')
args = par.parse_args()

def read_tabbed(filename, prev = {}):
    try:
        fd = open(filename, 'r')
    except IOError as e:
        stderr.write('%s:%s\n'%(filename, e.strerror))
        exit(1)
    for line in fd:
        if not line:
            continue
        line = line.rstrip()
        f = line.split('\t')
        if len(f) == 0:
            continue
        key = f[0]
        prev[key] = f
    return prev

def abbrev_species(taxon, short):
    if not taxon:
        return ''
    genus_letter = taxon[0]
    words = taxon.split(' ')
    if len(words < 2):
        return genus_letter + '' if short else '.'
    species = words[1]
    subspecies = ' '.join(words[2:])
    return '%s%s%s%s'%(genus_letter, '' if short else '. ', species, '' if short or not subspecies else ' ' + subspecies)

def make_unique_ids(n):
    
