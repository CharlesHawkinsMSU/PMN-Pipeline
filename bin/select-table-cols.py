#!/usr/bin/env python3

from sys import stdin, stdout, stderr
import argparse as ap

import tsv
import util

def main():
    par = ap.ArgumentParser(description = 'Give it a TSV file with named columns and a list of column names. It will output a new TSV file with only those columns from the input file. Can be used to pull out specific colulmns or to re-order columns in a TSV file')
    par.add_argument('Input', help = 'Input file. Should be in TSV (tab-separated value) format, with named columns')
    par.add_argument('-n', '--no-header', action = 'store_true', help = 'Remove the header line from the output file', dest = 'n')
    par.add_argument('-c', '--columns', nargs = '*', help = 'A list of columns to select. All arguments after the -c until the next dashed argument are taken to be column names from the input file (so select multiple columns with -c Col1 Col2 Col3). The output file will contain these columns in this order', dest = 'c')
    par.add_argument('-r', '--remove', action = 'store_true', help = 'Interpret the columns in -c as columns to remove rather than columns to keep. Columns not named in -c are retained in their original order. If a column is not found in the input file, only a warning is generated rather than an error as is the case without -r', dest = 'r')
    par.add_argument('-o', '--output', help = 'Output file', dest = 'o')
    
    args = par.parse_args()
    
    try:
        if args.Input is None or args.Input == '-':
            infile = stdin
        else:
            infile = open(args.Input, 'r')
        header, entries = tsv.read_tsv(infile)
        if infile != stdin:
            infile.close()
    except IOError as e:
            stderr.write(f'{e.filename}:{e.strerror}\n')
            exit(1)
    colset = set(args.c) if args.c else set()
    if (missing_cols := colset.difference(header)):
        mis_col_list = util.andlist(missing_cols, quote = True)
        mis_col_msg = f'The {util.pluralize(missing_cols, "column")} {mis_col_list} {util.pluralize(missing_cols, "was", "were")} not found in {args.Input}'
        if args.r:
            stderr.write(f'Warning: {mis_col_msg}\n')
        else:
            stderr.write(f'Error: {mis_col_msg}\n')
            exit(1)
    if args.r:
        col_list = [col for col in header if col not in colset]
    else:
        col_list = args.c
    try:
        if args.o is None or args.o == '-':
            outfile = stdout
        else:
            outfile = open(args.o, 'w')
        if not args.n:
            outfile.write('\t'.join(col_list)+'\n')
        for entry in entries:
            outfile.write(tsv.mk_tsv_line(col_list, entry))

        outfile.close()
    except IOError as e:
            stderr.write(f'{e.filename}:{e.strerror}\n')
            exit(1)

if __name__ == '__main__':
    exit(main())
