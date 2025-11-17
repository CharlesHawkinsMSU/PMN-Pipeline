#!/usr/bin/env python3

from sys import stdin, stdout, stderr
import argparse as ap
from collections import defaultdict

def main():
    par = ap.ArgumentParser(description = 'Auto-generates PGDB names in a pgdb table based on the species name')
    par.add_argument('Input', help = 'Input file')
    par.add_argument('-o', '--output', help = 'Output file', dest = 'o')
    par.add_argument('-i', '--ignore-subspecies', help = 'Do not includes subspecies in the generated names', dest = 'i')
    par.add_argument('-r', '--replace-existing', action = 'store_true', help = 'Generate a database ID even if one already exists. Without -r, entries that already have a Database ID will not be given a new one', dest = 'r')
    
    args = par.parse_args()

    try:
        if args.Input is None or args.Input == '-':
            infile = stdin
        else:
            infile = open(args.Input, 'r')
    except IOError as e:
            stderr.write(f'{e.filename}:{e.strerror}\n')
            exit(1)
    header, entries = read_tsv(infile)
    if 'Species Name' not in header:
        stderr.write(f'Error: Column "Species Name" is required in the input file\n')
        exit(1)
    for entry in entries:
        if not args.r and entry['Database ID']:
            continue
        sp_name = entry['Species Name']
        sp_components = sp_name.split(' ')
        if args.i:
            sp_components = sp_components[2:]
        else:
            if len(sp_components) >= 4 and sp_components[2].endswith('.'):
                sp_components.pop(2)
        auto_name = ''.join([c[0].upper() + c.rstrip('.')[1:] for c in sp_components])
        entry['Database ID'] = auto_name

    if infile != stdin:
        infile.close()
    try:
        if args.o is None or args.o == '-':
            outfile = stdout
        else:
            outfile = open(args.o, 'w')
        outfile.write('\t'.join(header) + '\n')
        for entry in entries:
            outfile.write(mk_tsv_line(header, entry))
        outfile.close()
    except IOError as e:
            stderr.write(f'{e.filename}:{e.strerror}\n')
            exit(1)
def blank():
    return ''
def read_tsv(file, crash = True):
    'Reads an already-open tsv file. Returns a list with the header fields and a list of rows. Each row is a dict mapping from column names (from the header) to value in that column for the row.'
    header_line = file.readline().rstrip()
    header_fds = header_line.split('\t')
    entries = []
    for line in file:
        entry = defaultdict(blank)
        line = line.rstrip()
        fds = line.split('\t')
        for i, f in enumerate(fds):
            try:
                entry[header_fds[i]] = f
            except IndexError:
                if crash:
                    stderr.write(f'Error: Line has different number of fields than header:\n{line}\n')
                    exit(1)
                else:
                    raise
        entries.append(entry)
    return header_fds, entries

def mk_tsv_line(header, entry):
    fds = []
    for col in header:
        fds.append(entry.setdefault(col, ''))
    return '\t'.join(fds)+'\n'

if __name__ == '__main__':
    exit(main())
