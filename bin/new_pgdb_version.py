#!/usr/bin/env python3

from sys import stdin, stdout, stderr

import argparse as ap

def main():
    par = ap.ArgumentParser(description = 'Operates on a pgdb table as used by the PMN pipeline. Increments the version numbers of all the listed PGDBs. Presets with a version number are left unchanged. Incrementing the version number means adding one to the first number and setting the rest to 0')
    par.add_argument('-v', '--version-parts', type = int, help = 'If specified, makes sure all generated version numbers have this many parts; e.g. giving 2 gets you versions like 4.0 while giving 3 gives version numbers like 4.0.0. If -v is not given, each PGDB keeps the number of version parts its version string already has', dest = 'v')
    par.add_argument('Input', help = 'Input file. A TSV file that includes columns labelled "Database ID" and "Version" (case-sensitive). Other columns are ignored but preserved in the output')
    par.add_argument('-o', '--output', help = 'Output file', dest = 'o')
    
    args = par.parse_args()
    
    try:
        if args.Input is None or args.Input == '-':
            infile = stdin
        else:
            infile = open(args.Input, 'r')
        if args.o is None or args.o == '-':
            outfile = stdout
        else:
            outfile = open(args.o, 'w')
    except IOError as e:
            stderr.write(f'{e.filename}:{e.strerror}\n')
            exit(1)
    if args.v is not None and args.v < 1:
        stderr.write(f'Error: -v argument cannot be less than 1\n')
        exit(1)
    header, entries = read_tsv(infile)
    if 'Database ID' not in header or 'Version' not in header:
        stderr.write(f'Error: Columns named "Database ID" and "Version" are required in the input file\n')
        exit(1)
    outfile.write('\t'.join(header)+'\n')
    for i, entry in enumerate(entries):
        orgid = entry['Database ID']
        if not orgid.startswith('/'):
            oldversion = entry['Version']
            v_parts = oldversion.split('.')
            if args.v:
                n_fds = args.v
            else:
                n_fds = len(v_parts)
            try:
                old_maj_v = int(v_parts[0])
            except ValueError:
                stderr.write(f'Error in {infile.name}, line {i+2}: Cannot interpret {oldversion} as a version number. Expected numbers separated by "." characters\n')
                exit(1)
            new_maj_v = old_maj_v + 1
            entry['Version'] = '.'.join([str(new_maj_v)]+(['0']*(n_fds-1)))
        outfile.write(mk_tsv_line(header, entry))

def read_tsv(file, crash = True):
    'Reads an already-open tsv file. Returns a list with the header fields and a list of rows. Each row is a dict mapping from column names (from the header) to value in that column for the row.'
    header_line = file.readline().rstrip()
    header_fds = header_line.split('\t')
    entries = []
    for line in file:
        entry = {}
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
