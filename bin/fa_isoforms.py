#!/usr/bin/env python3

from sys import stdin, stdout, stderr
import argparse as ap
import re

def main():
    par = ap.ArgumentParser(description = 'Generate a table of isoforms based on a fasta file')
    par.add_argument('Input', help = 'Input file, in FASTA format')
    par.add_argument('-o', '--output', help = 'Output file, in two-column tsv format', dest = 'o')
    par.add_argument('-s', '--sep', default = ' ', help = 'Fasta field separator. Usually " ", "|", or " | "', dest = 's')
    par.add_argument('-k', '--kv', default = '=', help = 'Fasta key-value separator. Usually "=" or ":"', dest = 'kv')
    par.add_argument('-g', '--gene', default = 'gene', help = 'Fasta gene field. Can be a named field ("locus" for locus=g12345) or a field number (protein ID is field 0)', dest = 'g')
    par.add_argument('-r', '--gene-regex', help = 'Get the gene ID by finding and removing the specified regex from the protein ID. Overrides -g', dest = 'r')
    par.add_argument('-m', '--mode', default = 'list', choices = ['list', 'reverse', 'longest', 'single'], help = 'Output mode. Options are list (colums are gene, list of isoforms separated by semicolons), reverse (columns are isoform, gene; each gene may appear on multiple lines), longest (columns are gene, longest isoform; in the case of a tie, the isoform appearing earlier in the file is used), and single (colums are gene, isoform; but only single-isoform genes are listed)', dest = 'm')
    
    args = par.parse_args()
    
    try:
        if args.Input is None or args.Input == '-':
            fafile = stdin
        else:
            fafile = open(args.Input, 'r')
        if args.o is None or args.o == '-':
            outfile = stdout
        else:
            outfile = open(args.o, 'w')
    except IOError as e:
            stderr.write(f'{e.filename}:{e.strerror}\n')
            exit(1)
    if args.r:
        try:
            gene_field = re.compile(args.r)
        except re.PatternError as e:
            stderr.write(f'Regex error:\n{args.r}\n{" "*e.pos}^\n{e.msg}\n')
            exit(1)
        get_prot_and_gene = get_prot_and_gene_regex
    else:
        try:
            gene_field = int(args.g)
            get_prot_and_gene = get_prot_and_gene_numbered
        except ValueError:
            gene_field = args.g
            get_prot_and_gene = get_prot_and_gene_named
    if args.m == 'reverse':
        for header, seq in fa_entries(fafile):
            prot, gene = get_prot_and_gene(header, args.s, args.kv, gene_field)
            outfile.write(f'{prot}\t{gene}\n')
    else:
        prot_map = {}
        gene_set = set()
        orphan_prots = []
        for header, seq in fa_entries(fafile):
            prot, gene = get_prot_and_gene(header, args.s, args.kv, gene_field)
            if gene:
                gene_set.add(gene)
                try:
                    prot_map[gene].append((prot, seq))
                except KeyError:
                    prot_map[gene] = [(prot, seq)]
            else:
                orphan_prots.append(prot)
        match args.m:
            case 'list':
                for gene in gene_set:
                    prot_seq_list = prot_map[gene]
                    prot_list = ";".join([prot for prot, seq in prot_seq_list])
                    outfile.write(f'{gene}\t{prot_list}\n')
            case 'longest':
                for gene in gene_set:
                    l_len = 0
                    l_prot = None
                    for prot, seq in prot_map[gene]:
                        l = len(seq)
                        if l > l_len:
                            l_len = l
                            l_prot = prot
                    outfile.write(f'{gene}\t{l_prot}\n')
            case 'single':
                for gene in gene_set:
                    prot_seq_list = prot_map[gene]
                    if len(prot_seq_list) == 1:
                        outfile.write(f'{gene}\t{prot_seq_list[0][0]}\n')
    outfile.close()

def get_prot_and_gene_named(header, s, kv, g):
    fds = header.split(s)
    for fd in fds[1:]:
        try:
            k, v = fd.split(kv)
            if k == g:
                return fds[0], v
        except ValueError:
            stderr.write(f'Warn: cannot split key-value pair "fd" in header line:\n>{header}\n')
    else:
        stderr.write(f'No field "{g}" found in header line:\n>{header}\n')
        return fds[0], None

def get_prot_and_gene_numbered(header, s, kv, g):
    fds = header.split(s)
    try:
        return fds[0], fds[g]
    except IndexError:
        stderr.write(f'Requested field {g} but header only has fields 0 - {len(fds)-1}:\n>{header}\n')

def get_prot_and_gene_regex(header, s, kv, g):
    fds = header.split(s)
    prot = fds[0]
    m = g.search(prot)
    if m:
        gene = prot[:m.start()]+prot[m.end():]
        return prot, gene
    else:
        stderr.write(f'No regex match for protein "{prot}"\n')
        return prot, None

def fa_entries(fafile):
    header = None
    seq = ''
    for line in fafile:
        if line.startswith('>'):
            header = line.rstrip()[1:]
            break
    for line in fafile:
        line = line.rstrip()
        if line.startswith('>'):
            yield header, seq
            header = line[1:]
            seq = ''
        else:
            seq += line
    yield header, line

if __name__ == '__main__':
    exit(main())
