#!/usr/bin/env python3

from sys import stdin, stdout, stderr
import argparse as ap

import tsv
import taxonomy

verbose = False
def info(msg):
    if verbose:
        stderr.write(msg+'\n')

def main():
    global verbose
    par = ap.ArgumentParser(description = 'Filter a tsv table based on a column')
    par.add_argument('Input', nargs='?', help = 'Input file. Should be a tsv (tab-separated value) file with the first line containing column names. Its rows will be filtered based on the value of the column given in -c')
    par.add_argument('-o', '--output', help = 'Output file', dest = 'o')
    par.add_argument('-c', '--colname', required = True, help = 'Column in the input file to filter on. A column of this name must exist in the input file', dest = 'c')
    par.add_argument('-l', '--list-colname', help = 'Optional column to use from the whitelist/blacklist files. If -l is given, the whitelist or blacklist file is treated as a tab-delimited table file with named columns, and the column with this name is used as the white/blacklist. If -l is not given, the files are assumed to be simple text files with no column header and one entry per line', dest = 'l')
    par.add_argument('-t', '--taxonomy-file', help = 'This optional file defines taxonomic relationships between values in the filter column, for taxonomic filtering. It should be a two-column tab-delimited file that defines a taxonomy of parent-child relationships. Child terms should go in the left column and parents in the right. Each term should appear only once as a child term but may appear any number of times as parent (each parent can have many children but each child has only one parent). If a -t file is given, terms (values in the filter column) will match the white/black list if they are a parent or child (direct or indirect) of any term there in addition to the usual exact match', dest = 't')
    par.add_argument('-b', '--blacklist', help = 'A file with values for the selected column, one per line (unless -l is given). Any rows from the input file that have one of these values in the filtered column will be removed', dest = 'b')
    par.add_argument('-w', '--whitelist', help = 'A file with values for the selected column, one per line (unless -l is given). Any rows from the input file that do not have one of these values in the filtered column will be removed', dest = 'w')
    par.add_argument('-v', '--verbose', action = 'store_true', help = 'Print additional debug info', dest = 'v')
    
    args = par.parse_args()

    verbose = args.v
    
    try:
        if args.Input is None or args.Input == '-':
            infile = stdin
        else:
            infile = open(args.Input, 'r')
        if args.o is None or args.o == '-':
            outfile = stdout
        else:
            outfile = open(args.o, 'w')
        if args.w:
            wlist = tsv.read_set_from_file(args.w, args.l)
            info(f'Read {len(wlist)} entries from whitelist {args.w}')
        if args.b:
            blist = tsv.read_set_from_file(args.b, args.l)
            info(f'Read {len(blist)} entries from blacklist {args.b}')
        if args.t:
            tax_tree = taxonomy.TaxTree.from_dict(tsv.dict_from_2col(args.t))
            info(f'Created taxonomy with {len(tax_tree)} nodes from {args.t}')
            if args.w:
                wlist = tax_tree.expand_set(wlist)
                info(f'Whitelist expanded to {len(wlist)} entries using taxonomy')
            if args.b:
                blist = tax_tree.expand_set(blist)
                info(f'Blacklist expanded to {len(blist)} entries using taxonomy')

    except IOError as e:
        stderr.write(f'{e.filename}:{e.strerror}\n')
        exit(1)
    header, entries = tsv.read_tsv(infile)
    info(f'Read {len(entries)} rows from TSV file {infile.name}')
    if args.c not in header:
        stderr.write(f'Error: The specified column "{args.c}" does not appear in the input file {infile.name}\n')
        exit(1)
    outfile.write('\t'.join(header)+'\n')
    n = 0
    for entry in entries:
        v = entry[args.c]
        if args.b and v in blist:
                continue
        if args.w and v not in wlist:
                continue
        outfile.write(tsv.mk_tsv_line(header, entry))
        n += 1
    outfile.close()
    info(f'Wrote {n} rows to TSV file {outfile.name}')

if __name__ == '__main__':
    exit(main())
