#!/usr/bin/env python3

from sys import stdin, stdout, stderr
import argparse as ap
import pmn

def main():
    par = ap.ArgumentParser(description = 'Find new genomes for PMN. Give it the faa_list.txt produced by get_refseq.sh and the last release\'s pgdb table file and it will output a list of only those entries from faa_list.txt that are for taxa not present in the last release')

    pmn.add_standard_pmn_args(par)

    par.add_argument('-n', '--new-genomes', nargs = '+', action = 'extend', required=True, help = 'Genome taxon list file, as produced by get_refseq.sh or get_phytozome.py. Should have 3+ columns (tab-separated) with all after the first two containing NCBI taxon IDs', dest = 'r')
    par.add_argument('-a', '--all', action = 'store_true', help = 'Print all entries, whether new or not, but with a column saying if they are new', dest = 'a')
    par.add_argument('--output', help = 'Output file', dest = 'out')
    args = par.parse_args()

    (config, orgtable, orglist) = pmn.read_pipeline_files(args)
    find_new_orgs(config, orgtable, orglist, args)

def find_new_orgs(config, orgtable, orglist, args):
    print(args.r)
    pmn.info('==Finding new orgs==')
    existing_ncbi_taxa = set()
    existing_taxon_names = set()
    for org in orglist:
        entry = orgtable[org]
        existing_ncbi_taxa.add(entry['NCBI Taxon ID'])
        existing_taxon_names.add(entry['Species Name'])
    if args.out:
        outfile = open(args.out, 'w')
    else:
        outfile = stdout
    for refseq_filename in args.r:
        try:
            refseq_file = open(refseq_filename)
            for line in refseq_file:
                fds = line.rstrip().split('\t')
                if len(fds) > 2:
                    taxon_name = fds[0].replace('_', ' ')
                    #faa = fds[1]
                    taxa = fds[2:]
                    for taxon in taxa:
                        if taxon in existing_ncbi_taxa:
                            if args.a:
                                outfile.write('No (ncbi)\t' + refseq_filename + '\t' + line)
                            break
                    else:
                        if taxon_name not in existing_taxon_names:
                            if args.a:
                                outfile.write('Yes\t' + refseq_filename + '\t' + line)
                            else:
                                outfile.write(refseq_filename + '\t' + line)
                            existing_taxon_names.add(taxon_name)
                            existing_ncbi_taxa.update(taxa)
                        else:
                            if args.a:
                                outfile.write('No (taxon name)\t' + refseq_filename + '\t' + line)
        except IOError as e:
            stderr.write(f'{e.filename}:{e.strerror}')
            exit(1)

if __name__ == "__main__":
    main()
