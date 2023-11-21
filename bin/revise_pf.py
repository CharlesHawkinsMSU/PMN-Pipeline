#!/usr/bin/env python3

from sys import stdin, stdout, stderr
import argparse as ap
from collections import defaultdict
import pmn

# Parses a map file with gene accessions in the first column and protein accessions in the second column, and returns a dictionary mapping from proteins to genes. If another two columns are present, then they will be taken as gene and protein IDs and dictionaries mapping from gene accessions to gene ids and protein accessions to protein ids will also be returned; otherwise the other two returns will be empty dicts
def parse_input_map(mapfile):
    try:
            infile = open(mapfile, 'r')
    except IOError as e:
            stderr.write('%s: %s\n'%(mapfile, e.strerror))
            exit(1)
    prot2gene = {}
    gene2id = {}
    prot2id = {}
    for line in infile:
        line = line.strip()
        if not line:
            continue
        fields = line.split('\t')
        l = len(fields)
        if l != 2 and l != 4:
            stderr.write('Warning: line "%s" in %s has %s fields, expected 2 or 4. Ignoring this line.\n'%(line, mapfile, l))
            continue
        prot2gene[fields[1]] = fields[0]
        if l == 4:
            gene2id[fields[0]] = fields[2]
            prot2id[fields[1]] = fields[3]
    return prot2gene, gene2id, prot2id

# Parse a fasta file, looking only at the headers, and return a dictionary that maps from protein accessions (assumed to be the first field) to gene accessions
def parse_fasta(fafile, gene_key, sep, kv_sep):
    try:
        gene_key = int(gene_key)
        numbered_field = True
    except ValueError:
        numbered_field = False
    try:
        infile = open(fafile, 'r')
    except IOError as e:
        stderr.write('%s: %s\n'%fafile, e.strerror)
        exit(1)
    prot2gene = {}
    with infile:
        for line in infile:
            if not line.startswith('>'):
                continue
            fields = line[1:].rstrip().split(sep)
            if len(fields) == 0:
                continue
            prot = fields[0]
            if numbered_field:
                if len(fields) > gene_key:
                    prot2gene[prot] = fields[gene_key]
            else:
                for field in fields:
                    kv = field.split(kv_sep, 1)
                    if len(kv) == 2 and kv[0] == gene_key:
                        prot2gene[prot] = kv[1]
                        break
    return prot2gene

# Parse a gff file and return a dictionary that maps from protein accessions to gene accessions
def parse_gff(gfffile, prot_feature, prot_name, ref_key, gene_path, cds_path = None):
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
                    stderr.write('Warning: Line %s of %s has only %s fields, expected at least 9\n'%(line_n, gfffile, len(fields)))
                    continue

            # Parse the attribute field (field 8) into a dictionary
            attrs = pmn.parse_attrs(fields[8])

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
    # Next we go through the list of CDS's and find the associated proteins
    p2cds = {}

    return p2g

# Saves the output mapping file to the filename mapfile
def write_map(prot2gene, gene2id, prot2id, mapfile):
    # First, determine what mapfile is and act accordingly
    # First possibility: it's '-' meaning send to stdout
    if mapfile == '-':
            outfile = stdout
    # Second possibility: it's an already-open stream, so we can just write to it
    elif callable(getattr(mapfile, "write", None)):
        outfile = mapfile
    # Remaining possibility: it's a filename; open the file
    else:
            try:
                    outfile = open(mapfile, 'w')
            except IOError as e:
                    stderr.write('%s: %s\n'%(mapfile, e.strerror))
                    exit(1)
    for prot, gene in prot2gene.items():
        try:
            geneid = gene2id[gene]
            protid = prot2id[prot]
            outfile.write('\t'.join([gene, prot, geneid, protid])+'\n')
        except KeyError:
            pass

# Read the input pf file and write the output revised pf file. The input pf file produced by E2P2 will have protein accessions in the ID and NAME fields. The output revised pf we write will have gene IDs in ID, gene accessions in NAME, protein IDs in PRODUCT-ID, and protein accessions in PRODUCT-ACCESSION; all other fields will be unchanged. If numeric_prefix is None then accessions will be used as IDs when gene2id or prot2id don't have an entry already; otherwise numeric IDs will be assigned. The function returns the (updated) gene2id and prot2id maps including all new assignments that have been made
def process_pf(pfname, rpfname, prot2gene, gene2id = {}, prot2id = {}, numeric_prefix = None):
    # Open the input pf and output revised pf files
    try:
        pffile = open(pfname, 'r')
    except IOError as e:
        stderr.write('%s: %s\n'%(pffile, e.strerror))
        exit(1)
    try:
        rpffile = open(rpfname, 'w')
    except IOError as e:
        stderr.write('%s: %s\n'%(rpffile, e.strerror))
        exit(1)

    # List of ids that are already taken (based on the input mapfile). When assigning new numeric IDs we need to make sure not to re-use an ID that's already taken
    taken_geneids = pmn.set_from_values(gene2id)
    taken_protids = pmn.set_from_values(prot2id)

    # Numeric gene ids will be assigned sequentially from 0
    gene_num = 0

    # Transcript IDs will be assigned sequentially from 1, separately for each gene
    transcr_num = defaultdict(int)
    with pffile, rpffile:
        # So the way this works is we go through the PF file just looking for an ID line, and when we find one we do the lookups and generation and output several lines. Any other line we just output as-is except NAME lines which are just removed since there's already a NAME line that's part of the ID output
        for line in pffile:
            line = line.strip()
            if not line:
                continue
            if line.startswith("ID"):
                try:
                    prot_acc = line.split('\t')[1]
                except IndexError:
                    stderr.write('Warning: ID line "%s" in %s has only one field. Ignoring this protein.\n'%(line, pfname))
                    rpffile.write(line + '\n')
                    continue
                # The input pf file will have protein accessions in the ID field; first we need to look up the gene ID for that protein (from the input map and/or gff file)
                try:
                    gene_acc = prot2gene[prot_acc]
                except KeyError:
                    stderr.write('Warning: Protein %s not found in gff or mapping file. This protein will not be mapped\n'%prot_acc)
                    rpffile.write(line + '\n')
                    continue
                # First, check if this gene already has an ID in the input map file; if so just use that
                try:
                    gene_id = gene2id[gene_acc]
                except KeyError:
                    # If not then we either assign it a numeric ID or just use the gene accession, depending on the arguments
                    if numeric_prefix:
                        # Loop until we find a new gene ID that isn't taken
                        while True:
                            gene_id = '%s%s'%(numeric_prefix, gene_num)
                            gene_num += 1
                            if gene_id not in taken_geneids:
                                break
                    else:
                        gene_id = gene_acc
                    gene2id[gene_acc] = gene_id
                # Now do the same for the protein - first see if it was already assigned an ID in the input map file
                try:
                    prot_id = prot2id[prot_acc]
                except KeyError:
                    # If not then either assign it a numeric ID based on the gene ID or just use the accession, depending on the arguments
                    if numeric_prefix:
                        # Loop until we find a new protein ID that isn't taken
                        while True:
                            transcr_num[gene_id] += 1
                            prot_id = '%s-MONOMER-%s'%(gene_id, transcr_num[gene_id])
                            if prot_id not in taken_protids:
                                break
                    else:
                        prot_id = prot_acc
                    prot2id[prot_acc] = prot_id
                rpffile.write('ID\t%s\n'%gene_id)
                rpffile.write('NAME\t%s\n'%gene_acc)
                rpffile.write('PRODUCT\t%s\n'%prot_acc)
                rpffile.write('PRODUCT-ACCESSION\t%s\n'%prot_acc)
                rpffile.write('PRODUCT-ID\t%s\t\n'%prot_id)
            # The existing NAME line should be removed since we print a new NAME line when we  find the ID line
            elif not line.startswith('NAME'):
                rpffile.write(line + '\n')
    return gene2id, prot2id
            
def get_pf_args():
    par = ap.ArgumentParser(description = '')
    par.add_argument('i', help = 'Input .pf file')
    par.add_argument('-ig', '--input-gff', metavar = 'gff', help = 'Input gff file we want to get gene-protein mappings from', dest = 'f')
    par.add_argument('-if', '--input-fasta', metavar = 'fasta', help = 'Input fasta file to get gene-protein mappings from', dest = 'ifa')
    par.add_argument('-im', '--input-map', metavar = 'map', help = 'An input mapping file, tab-delimited, with gene accessions in the first column and protein accessions in the second. Can be given instead of or in addition to the gff file and/or fasta file input; if multiple are given, the map file overrides the fasta file overrides the gff file. If another two columns are present, they are taken as gene and protein IDs and used in the output .pf and .map files.', dest = 'im')
    par.add_argument('-om', '--output-map', required = False, help = 'The mapping file to save to. Default is the input pf filename with the extension changed to .map. The output map maps from protein and gene accessions to protein and gene IDs, and can be passed to future runs of this script in the -im argument along with "-i map" to ensure that the same frame IDs are used in future versions of the same genome', dest = 'o')
    par.add_argument('-r', '--revised-pf', required = False, help = 'Revised .pf file to save. Default is the input filename with the extension changed to .revised.pf', dest = 'r')
    par.add_argument('-fs', '--fasta-sep', default = ' ', help = 'For fasta input, field separator used in the fasta header lines. In most fastas this should be either \' \' (the default) or \'|\'', dest = 'fs')
    par.add_argument('-fg', '--fasta-gene', default = 'gene', help = 'For fasta input, the field name or number that contains the gene accession. Anything that parses as an integer is assumed to be a field number (starting from 0); anything else is assumed to be a field name separated from the accession by \'=\' (or whatever -fkv is set to). So if the fasta headers were formatted as >p22345.1|g22345 then you could give the options -fs \'|\' -fg 1 to get the g22345. If the headers were instead formatted as >p22345.1 locus:g22345 then you could give the options -fg \'locus\' -fkv \':\' (with -fs taking the default of \' \') to get the same g22345 accession', dest = 'fg')
    par.add_argument('-fkv', '--fasta-kv-sep', default = '=', help = 'For fasta input, the separator of the key and value for named fields; so for fields like gene=g22345 you would use \'=\' (the default), while for locus:g22345 you would use \':\'', dest = 'fkv')
    par.add_argument('-gpf', '--gff-protein-feature', default='mRNA', help = 'For GFF input, feature to use as the protein. Default is \'mRNA\'', dest = 'pf')
    par.add_argument('-gpn', '--gff-protein-name', default = 'ID', help = 'For GFF input, name field to use as protein_id. Default is \'ID\'', dest = 'p')
    par.add_argument('-gk', '--gff-key', default = 'ID', help = 'For GFF input, attribute used as a key for references such as Parent. Default is \'ID\'', dest = 'k')
    par.add_argument('-gg', '--gff-gene-path', default = 'Parent', help = 'For GFF input, specification for how to find a gene from a protein. Dot separated list of attributes, with the last taken as the name. Default is \'Parent\'', dest = 'g')
    par.add_argument('-n', '--numeric-ids', action = 'store_true', help = 'Auto-generate numeric gene and protein frame IDs instead of using the accessions as frame IDs. The gene IDs will start with whatever is given with -gp, followed by a number; the numbers are assigned based on the order that proteins derived from each gene appears in the input pf file. Protein IDs are the gene ID followed by "-MONOMER-" followed by a transcript number. The transcript numbers start from 1 and follow the order in which proteins derived from the same gene appear in the input .pf file. Useful if the accessions for this organism are not valid as frame IDs (meaning they are either too long or contain disallowed characters like #). If there are IDs in the input mapping file (and -d is not specified), then only proteins in the input .pf file that don\'t have mappings in that file are assigned new numeric IDs. If a new numeric ID would conflict with one from the mapping file for another gene or protein, then the new ID is incremented (gene ID for genes, transcript ID for proteins) until a "free" one is found.', dest = 'n')
    par.add_argument('-ngp', '--gene-prefix', default = 'G-', help = 'Prefix for the generated gene IDs', dest = 'gp')
    par.add_argument('-d', '--discard-map-ids', action = 'store_true', help = 'If the input mapping file contains gene and protein IDs, discard them and go with either the accessions or, if -n is specified, newly-assigned numeric IDs', dest = 'd')
    par.add_argument('-gr', '--gene-delete', help='Optional regex used to remove unwanted elements from the gene ID as it appears in its field. Any text matching the regex will be removed. So for example if the gene ID is given as g12345:102938:12038:chr3 where only the first part is what you want, you can give -gr \':.*\' to remove the unwanted chromosome coordinates. Applies to gene IDs read from fasta and gff, but not to input mapping files', dest='gd')

    args = par.parse_args()
    if not (args.f or args.ifa or args.im):
        stderr.write('Must specify at least one of: -ig, -if, -im\n')
        exit(1)
    return args

def main(args = None):
    if not args:
        args = get_pf_args()
    prot2gene = {}
    gene2id_from_map = {}
    prot2id_from_map = {}
    taken_geneids = set()
    taken_protids = set()
    if args.f:
        prot2gene = parse_gff(args.f, args.pf, args.p, args.k, args.g)
    if args.ifa:
        prot2gene.update(parse_fasta(args.ifa, args.fg, args.fs, args.fkv))
    if args.gd:
        gd_re = re.compile(args.gd)
        for prot, gene in prot2gene.items():
            prot2gene[prot] = gd_re.sub('', gene)
    if args.im:
        prot2gene_from_map, gene2id_from_map, prot2id_from_map = parse_input_map(args.im)
        prot2gene.update(prot2gene_from_map)
        if args.d:
            gene2id_from_map = {}
            prot2id_from_map = {}
    if(args.r is None):
        rpffilename = args.i.rsplit('.',1)[0]+'.revised.pf'
    else:
        rpffilename = args.r
    gene2id, prot2id = process_pf(args.i, rpffilename, prot2gene, gene2id_from_map, prot2id_from_map, args.gp if args.n else None)

    if args.im:
        gene2id.update(gene2id_from_map)
        prot2id.update(prot2id_from_map)

    if(args.o is None):
        mapfilename = args.i.rsplit('.',1)[0]+'.map'
    else:
        mapfilename = args.o
    write_map(prot2gene, gene2id, prot2id, mapfilename)

if __name__ == "__main__":
    main()
