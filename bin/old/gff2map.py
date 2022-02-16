#!/usr/bin/env python3

from sys import stdin, stdout, stderr
import argparse as ap


class idict:
    def __getitem__(self, i):
        return i

def parse_attrs(attr_str):
	attr_dict = {}
	for pair in attr_str.split(';'):
		key_val = pair.split('=')
		if len(key_val) < 2:
			continue
		attr_dict[key_val[0]] = key_val[1]
	return attr_dict

# Parse in the gff file
def parse_gff(gfffile, prot_feature, prot_name, ref_key, gene_path):
    if gfffile is None or gfffile == '-':
            infile = stdin
    else:
            try:
                    infile = open(gfffile, 'r')
            except IOError as e:
                    stderr.write('%s: %s\n'%(gfffile, e.strerror))
                    exit(1)
    gff_entries = {}
    proteins = {}
    line_n = 0
    for line in infile:
            line_n += 1
            if line is None:
                    break
            line = line.rstrip()
            if line == "###FASTA":
                    break
            if line == '' or line[0] == '#':
                    continue
            fields = line.split('\t')

            # Make sure we have at least 9 tab-separated fields
            if len(fields) < 9:
                    stderr.write('Warning: Line %s has only %s fields, expected at least 9\n'%(line_n, len(fields)))
                    continue

            # Parse the attribute field (field 8) into a dictionary
            attrs = parse_attrs(fields[8])

            # Make sure the key attribute (as specified with -k) is present for this entry
            if ref_key not in attrs:
                    #stderr.write('Warning: Line %s has no attribute %s, which is supposed to be the key as specified by the -k option\n'%(line_n, ref_key))
                    continue
            key = attrs[ref_key]

            # All checks passed, put it into the dictionary
            gff_entries[key] = attrs

            if fields[2] == prot_feature:	# This is one of the protein entries; save it and its name
                    if prot_name not in attrs:
                            stderr.write('Warning: Protein %s has no attribute %s, which is supposed to be the protein name according to the -p option. The protein will therefore not be included in the output\n'%(key, prot_name))
                            continue
                    proteins[key] = attrs[prot_name]

    path = gene_path.split('.')

# Now we go through the list of proteins and find the associated gene for each one using the path the user specified
    p2g = {}
    for protein in proteins.keys():
            name = proteins[protein]
            gene = protein
            for path_entry in path:
                    gene = gff_entries[gene]
                    if path_entry not in gene:
                            stderr.write('Path entry %s not found in gff entry %s while looking for the gene for protein %s\n'%(path_entry, gene[ref_key], protein))
                            gene = None
                            break
                    gene = gene[path_entry]
            if gene:
                p2g[name] = gene
    return p2g

def write_map(p2g, p2a, g2a, mapfile, header = ["Protein ID", "Gene ID", "Protein Accession", "Gene Accession"]):
    if mapfile is None or mapfile == '-':
            outfile = stdout
    elif callable(getattr(mapfile, "write", None)):
        outfile = mapfile
    else:
            try:
                    outfile = open(mapfile, 'w')
            except IOError as e:
                    stderr.write('%s: %s\n'%(mapfile, e.strerror))
                    exit(1)
    outfile.write('\t'.join(header)+'\n')
    for pid, gid in p2g.items():
        outfile.write('\t'.join([pid, gid, p2a[pid], g2a[gid]])+'\n')

def add_args(par):
    par.add_argument('i', help = 'Input file, in gff format')
    par.add_argument('-o', '--output', help = 'Output file; the mapping file', dest = 'o')
    par.add_argument('-pf', '--protein-feature', default='mRNA', help = 'Feature to use as the protein. Default is \'mRNA\'', dest = 'f')
    par.add_argument('-p', '--protein-name', default = 'ID', help = 'Name field to use as protein_id', dest = 'p')
    par.add_argument('-k', '--key', default = 'ID', help = 'Attribute used as a key for references such as Parent. Default is \'ID\'', dest = 'k')
    par.add_argument('-g', '--gene-path', default = 'Parent.ID', help = 'Specification for how to find a gene from a protein. Dot separated list of attributes, with the last taken as the name', dest = 'g')

def main():
    par = ap.ArgumentParser(description = '')
    add_args(par)

    args = par.parse_args()
    identity = idict()
    p2g = parse_gff(args.i, args.f, args.p, args.k, args.g)
    write_map(p2g, identity, identity, args.o)

if __name__ == "__main__":
    main()
