#!/usr/bin/env python3

from sys import stdin, stdout, stderr
import argparse as ap
import revise_pf
import pmn

def fa_get_isoforms(fafile, fg, fs, fkv):
    prot2gene = revise_pf.parse_fasta(fafile, fg, fs, fkv)
    return pmn.invert_dictlist(prot2gene)


def main():
    par = ap.ArgumentParser(description = 'Given a fasta file, outputs a TSV file mapping genes to a semicolon-separated list of all isoforms of that gene')
    par.add_argument('input', help = 'Input amino acid fasta file')
    par.add_argument('-o', '--outupt', help = 'Output TSV file', dest = 'o')
    par.add_argument('-fs', '--fasta-sep', default = ' ', help = 'For fasta input, field separator used in the fasta header lines. In most fastas this should be either \' \' (the default) or \'|\'', dest = 'fs')
    par.add_argument('-fg', '--fasta-gene', default = 'gene', help = 'For fasta input, the field name or number that contains the gene accession. Anything that parses as an integer is assumed to be a field number (starting from 0); anything else is assumed to be a field name separated from the accession by \'=\' (or whatever -fkv is set to). So if the fasta headers were formatted as >p22345.1|g22345 then you could give the options -fs \'|\' -fg 1 to get the g22345. If the headers were instead formatted as >p22345.1 locus:g22345 then you could give the options -fg \'locus\' -fkv \':\' (with -fs taking the default of \' \') to get the same g22345 accession', dest = 'fg')
    par.add_argument('-fkv', '--fasta-kv-sep', default = '=', help = 'For fasta input, the separator of the key and value for named fields; so for fields like gene=g22345 you would use \'=\' (the default), while for locus:g22345 you would use \':\'', dest = 'fkv')
    par.add_argument('-ops', '--out-prot-sep', default = ';', help = 'Separator for proteins in the output file', dest = 'ops')
    args = par.parse_args()

    try:
        if not args.o or args.o == '-':
            outfile = stdout
        else:
            outfile = open(args.o, 'w')
    except IOError as e:
        stderr.write(f'{e.filename}: {e.strerror}\n')
        exit(1)
    gene2prots = fa_get_isoforms(args.input, args.fg, args.fs, args.fkv)
    for gene, prots in gene2prots.items():
        outfile.write(f'{gene}\t{args.ops.join(prots)}\n')

if __name__ == "__main__":
    main()
