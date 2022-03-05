#!/usr/bin/env python3
from sys import stdin, stdout, stderr
import argparse as ap
import pmn
import re

var_re = re.compile('\\$(\\$|[a-zA-Z0-9]+)')

par = ap.ArgumentParser(description='Rearranges the fields in a fasta header. Useful if a fasta does not have the protein ID as its first header field, which will confuse BUSCO and E2P2')
par.add_argument("fa", help = 'Input fasta file')
par.add_argument("spec", help = 'What to write in the output file\'s headers. Should be a string; enclosing in single-quotes is recommended. Use $0, $1, $2, etc. to access the Nth field of the input fasta header; use $gene, $locus, etc. to access named fields like gene=gene12345. Do not include the leading ">"')
par.add_argument('-o', '--output', default = '-', help = 'Output fasta file', dest = 'o')
par.add_argument('-kv', '--key-val-sep', default = '=', help='Key-value separator for the input fasta file (i.e. gene=gene12345 vs gene:gene12345)', dest = 'kv')
par.add_argument('-s', '--field-sep', default = ' ', help='Separator for the fasta fields (i.e. ">gene12345.1 gene=gene12345 chr=chr3" vs ">gene12345.1|gene=gene12345|chr=chr3")', dest = 's')

args = par.parse_args()

infile = pmn.openfile(args.fa)
outfile = pmn.openfile(args.o, 'w')

with infile, outfile:
    line_n = 1
    for line in infile:
        if line and line[0] == '>':
            fa_dict = pmn.parse_fasta_header(line, sep=args.s, kv_sep=args.kv)
            print(fa_dict)
            outfile.write('>')
            from_pos = 0
            while True:
                var_match = var_re.search(args.spec, from_pos)
                if not var_match:
                    break
                outfile.write(args.spec[from_pos:var_match.start()])
                var_ref = var_match.group(1)
                if var_ref == '$':
                    outfile.write('$')
                else:
                    try:
                        outfile.write(fa_dict[var_ref])
                    except KeyError:
                        stderr.write('Warning, line %s of %s has no entry %s; leaving blank\n'%(line_n, args.fa, var_ref))
                from_pos = var_match.end()
            outfile.write(args.spec[from_pos:])
            outfile.write('\n')
        else:
            outfile.write(line)
        line_n += 1
