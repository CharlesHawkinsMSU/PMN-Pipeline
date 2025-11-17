#!/usr/bin/env python

from sys import stdin, stdout, stderr
import argparse as ap
import re

import tsv
import util

word_delim_re = re.compile('[ _.-]')

dbg = False
def info(msg):
    if dbg:
        stderr.write(msg + '\n')

def main():
    global dbg
    par = ap.ArgumentParser(description = 'Merge one or more TSV table files, taking into account the header rows of each. Input files should each have a header row with column names. The columns of the output table will the union of the columns from the input files, and all output rows will be rearranged to match the new column order. If an input file lacks a column present in others, rows from it will be blank in that column')
    par.add_argument('Input', nargs = '*', help = 'Input files, in TSV format')
    par.add_argument('-o', '--output', help = 'Output file', dest = 'o')
    par.add_argument('-k', '--key', help = 'Name of the key column; if present, rows with identical values in this column will be merged. Values from input files *later* in the Input list will supersede those from files earlier in the list. Likewise for multiple entries in the same file that share a key. However, blank fields do not overwrite non-blank fields unless -b is given. No row merges occur for rows where the key column is blank or missing. When -k is provided, the output row order may differ from the input row order', dest = 'k')
    par.add_argument('-b', '--blanks-overwrite', action = 'store_true', help = 'If set, when merging entries due to -k, blank fields from later entries will overwrite non-blank fields from earlier entries, and earlier entries will only affect the value if the later file is missing that column altogether. Without -b, blank fields do not overwrite non-blank fields. Has no effect if -k is not given', dest = 'b')
    par.add_argument('-d', '--debug', action = 'store_true', help = 'Turn debug messages on', dest = 'd')
    
    args = par.parse_args()

    dbg = args.d
    
    try:
        if args.o is None or args.o == '-':
            outfile = stdout
        else:
            outfile = open(args.o, 'w')
    except IOError as e:
            stderr.write(f'{e.filename}:{e.strerror}\n')
            exit(1)
    
    final_entries = []
    headers = []
    tsv_dict = {}
    did_stdin = False
    if not args.Input:
        args.Input = ['-']
    for inpath in args.Input:
        info(f'Reading {inpath}')
        try:
            if inpath == '-':
                if did_stdin:
                    stderr.write(f'Error: Cannot read from stdin more than once\n')
                    exit(1)
                infile = stdin
                did_stdin = True
            else:
                infile = open(inpath)
        except IOError as e:
            stderr.write(f'{inpath}: {e.strerror}\n')
            exit(1)
        header, entries = tsv.read_tsv(infile)
        info(f'Header: {header}')
        info(f'Data: {entries}')
        if args.k and args.k in header:
            for tsv_entry in entries:
                key_fd = tsv_entry[args.k]
                info(f'Key is {key_fd}')
                if key_fd:
                    try:
                        existing_entry = tsv_dict[key_fd]
                        info(f'Prev entry {existing_entry}')
                        tsv_entry = merge_entries(existing_entry, tsv_entry, args.b)
                    except KeyError:
                        pass
                    tsv_dict[key_fd] = tsv_entry
                else:
                    final_entries.append(tsv_entry)
        else:
            final_entries += entries
        headers.append(header)
    final_entries = final_entries + list(tsv_dict.values())
    #tsvs = tsvs or tsv_dict
    warn_headers(headers)
    combo_header = combine_headers(headers)
    info(f'Combined headers to {combo_header}')
    outfile.write('\t'.join(combo_header)+'\n')
    for entry in final_entries:
        outfile.write(tsv.mk_tsv_line(combo_header, entry))

def merge_entries(e1, e2, b):
    e = e2.copy()
    for k, v1 in e1.items():
        try:
            v2 = e2[k]
            if not (v2 or b):
                e[k] = v1
        except KeyError:
            e[k] = v1
    return e

def combine_headers(headers):
    new_header = []
    colnames = set() 
    for header in headers:
        info(f'Merging in header {header}')
        for field in header:
            info(f'Merging in field {field}')
            if field not in colnames:
                colnames.add(field)
                new_header.append(field)
    return new_header

def standardize_name(colname):
    return word_delim_re.sub('', colname.lower())

def warn_headers(headers):
    headers_dups = {}
    for header in headers:
        for colname in header:
            colname_std = standardize_name(colname)
            info(f'Standardized {colname} to {colname_std}')
            if colname_std:
                try:
                    headers_dups[colname_std].add(colname)
                except KeyError:
                    headers_dups[colname_std] = set([colname])
    for col_std, cols in headers_dups.items():
        if len(cols) > 1:
            stderr.write(f'Warning: Column names include {util.andlist(cols, quote = True)}. Did you mean for these to be the same?\n')

if __name__ == '__main__':
    exit(main())
