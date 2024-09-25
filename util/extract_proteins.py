#!/usr/bin/env python3

import argparse as ap
import re
import sys

complement = {'n':'n', 'a':'t', 't':'a', 'g':'c', 'c':'g', 'u':'a', 'N':'N', 'A':'T', 'T':'A', 'G':'C', 'C':'G', 'U':'A'}
codons = {'TAA':'*', 'TAG':'*', 'TGA':'*', 'GCA':'A', 'GCC':'A', 'GCG':'A', 'GCT':'A', 'AGA':'R', 'AGG':'R', 'CGA':'R', 'CGC':'R', 'CGG':'R', 'CGT':'R', 'AAC':'N', 'AAT':'N', 'GAC':'D', 'GAT':'D', 'TGC':'C', 'TGT':'C', 'GAA':'E', 'GAG':'E', 'CAA':'Q', 'CAG':'Q', 'GGA':'G', 'GGC':'G', 'GGG':'G', 'GGT':'G', 'CAC':'H', 'CAT':'H', 'ATA':'I', 'ATC':'I', 'ATT':'I', 'CTA':'L', 'CTC':'L', 'CTG':'L', 'CTT':'L', 'TTA':'L', 'TTG':'L', 'AAA':'K', 'AAG':'K', 'ATG':'M', 'TTC':'F', 'TTT':'F', 'CCA':'P', 'CCC':'P', 'CCG':'P', 'CCT':'P', 'AGC':'S', 'AGT':'S', 'TCA':'S', 'TCC':'S', 'TCG':'S', 'TCT':'S', 'ACA':'T', 'ACC':'T', 'ACG':'T', 'ACT':'T', 'TGG':'W', 'TAC':'Y', 'TAT':'Y', 'GTA':'V', 'GTC':'V', 'GTG':'V', 'GTT':'V'}

stops = '(TAA|TAG|TGA)'
re_stop_int = re.compile(f'(...)*?{stops}(?!$)')
re_stop_end = re.compile(f'(...)*{stops}$')
re_wildcard = re.compile('[^ATGC]')

def rev_comp(seq):
	seq = seq[::-1]	# Reverse the sequence
	seq = [complement[base] for base in seq]	# Complement the sequence
	return(''.join(seq))

def translate(cds):
	codon_seq = [cds[i:i+3] for i in range(0, len(cds), 3)]	# Break up into codons
	aa_seq = ''.join([codons[codon] if codon in codons else 'X' for codon in codon_seq])	# Look up each codon in the translation table
	return(aa_seq)

# Parse a dictionary from a string in the form 'var1=val1;var2=val2'
def parse_dict(dict_str):
	entries = dict_str.split(';')
	dct = {}
	for entry in entries:
		key_val = entry.split('=')
		if len(key_val) != 2:
			continue
		dct[key_val[0]] = key_val[1]
	return(dct)

# Assembles a full cds from individual CDS elements. cds should be a list of tuples of the form: (start, seq, strand)
def assemble_cds(cds):
	def key_start(exon):
		return(exon[0])
	cds.sort(key=key_start)
	cds_str = ''
	for exon in cds:
		cds_str += exon[1]
	if cds[0][2] == '-':
		cds_str = rev_comp(cds_str)
	return(cds_str)

# Apply the requested filters and transforms to the nucleic acid sequence seq, as specified in the argparses arguments
def apply_seq_filters(seq, args):
	# Standardize the sequence (upper-case, DNA bases)
	seq = seq.upper().replace('U','T')

	# Apply --no-start filter
	if args.nt == 'exclude':
		if not seq.startswith('ATG'):
			return None
	elif args.nt == 'find' or args.nt == 'try-find':
		start = seq.find('ATG')
		if start == -1:
			if args.nt == 'find':
				return None
			# else args.nt must be 'try-find', so pass the sequence as-is to the next filter step
		else:
			seq = seq[start:]

	# Apply --internal-stop filter
	if args.int_stp == 'exclude':
		if re_stop_int.match(seq):
			return None
	elif args.int_stp == 'truncate':
		m = re_stop_int.match(seq)
		if m:
			seq = seq[:m.end()]

	# Apply --no-stop filter
	if args.np == 'exclude':
		if not re_stop_end.match(seq):
			return None
	elif args.np == 'add':
		if not re_stop_end.match(seq):
			# If needed, pad out sequence length to a multiple of 3, using N's 
			seq += 'N'*((3-len(seq)%3)%3)
			seq += 'TAA'

	# Apply --exclude-unknown filter
	if args.x and (len(seq)%3 != 0 or re_wildcard.find(seq)):
		return None
	return seq

def print_cds(attr, seq, args, out):
	seq = apply_seq_filters(seq, args)
	if not seq:
		return
	if not args.c:
		seq = translate(seq)
	# Put in line breaks
	seq = '\n'.join([seq[i:i+args.l] for i in range(0, len(seq), args.l)])
	if args.t:
		out.write(f'{attr}\n')
	else:
		out.write(">%s%s transcript=%s locus=%s ID=%s\n"%(
			attr['Name'] if 'Name' in attr else attr['ID'],
			' pacid='+attr['pacid'] if 'pacid' in attr else '',
			attr['Name'] if 'Name' in attr else attr['ID'],
			attr['Parent'],
			attr['ID']))
	out.write('%s\n'%seq)

# One chromosome in the fasta index (fai) file
class FaiChr:
	def __init__(self, name, length, offset, bases, chars):
		self.name = name
		self.length = int(length)
		self.offset = int(offset)
		self.bases = int(bases)
		self.nl = int(chars)-int(bases)
def extract_from_genome(args):
	fai_fn = args.g + '.fai'
	try:
		gen_file = open(args.g, 'r')
	except IOError as e:
		sys.stderr.write('Error: Could not open genome assmebly file %s: %s\n'%(args.g, e.strerror))
		exit(1)
	try:
		ann_file = open(args.a, 'r')
	except IOError as e:
		sys.stderr.write('Error: Could not open annotation file %s: %s\n'%(args.a, e.strerror))
		exit(1)
	try:
		fai_file = open(fai_fn, 'r')
	except IOError as e:
		sys.stderr.write('Error: Could not open fasta index file %s: %s\nYou can create this file using samtools, with:\n\tsamtools faidx %s\n'%(fai_fn, e.strerror, args.g))
		exit(1)

	if args.o is None:
		out = sys.stdout
	else:
		try:
			out = open(args.o, 'w')
		except IOError as e:
			sys.stderr.write('Error: Could not open output file %s for writing: %s\n'%(args.o, e.strerror))
			exit(1)
#def get_cds(gen_file, ann_file, fai_file, out)
	fai = {}	# All chromosomes in the fai indexed by chromosome name
	cds = None	# A list of tuples containing info on the currently-building cds
	with gen_file, ann_file, fai_file, out:
		for fai_line in fai_file:
			fai_fields = fai_line.split('\t')
			fai[fai_fields[0]] = FaiChr(fai_fields[0], fai_fields[1], fai_fields[2], fai_fields[3], fai_fields[4])
		for ann_line in ann_file:
			ann_line = ann_line.strip()
			if ann_line == '##FASTA':	# Indicates that the rest of the file is not gff3 data and should be ignored
				break
			if ann_line == '' or ann_line[0] == '#' or ann_line[0] == '@':	# Skip over comments and blank lines
				continue
			ann_fields = ann_line.split('\t')
			try:
				attr = parse_dict(ann_fields[8])
			except IndexError:
				sys.stderr.write('Line does not have at least 7 fields: %s\n'%ann_line)
				continue
			if ann_fields[2] == 'gene':	# We've encountered a gene. That means we're done with the previous gene, so print out its info and clear things for this gene
				if cds is not None and len(cds) >= 1:
					print_cds(mRNA_attr, assemble_cds(cds), args, out)
				cds = [];
				mRNA_attr = None
				skip_alt_splicing = False
			elif ann_fields[2] == 'mRNA' or ann_fields[2] == 'transcript':	# Each transcript has one mRNA entry in the gff file. We only want the first transcript if there is more than one. We also want to save the atributes for use in naming the fasta sequence
				if mRNA_attr == None:
					mRNA_attr = attr
				else:
					skip_alt_splicing = True	# Set to True to skip over subsequence CDS's in this gene because they represent alternative splicing
			#elif ann_fields[2] == 'CDS' or ann_fields[2] == 'exon':
			elif ann_fields[2] == 'CDS':
				if skip_alt_splicing:
					continue
				chrm = ann_fields[0]
				start = int(ann_fields[3])-1	# The -1 is because of 0-indexing vs 1-indexing
				end = int(ann_fields[4])		# -1 for 0-indexing, +1 because end is the last base rather than one past it which we want for the later subtraction to work
				stra = ann_fields[6]
				try:
					fai_entry = fai[chrm]
				except KeyError:
					sys.stderr.write('Error: Chromosome %s not found in fasta index %s\n'%(chrm, fai_fn))
					exit(1)
				# The _adj variables are adjusted for the presence of newlines in the fasta file
				start_adj = start + start//fai_entry.bases * fai_entry.nl
				end_adj = end + end//fai_entry.bases * fai_entry.nl
				len_adj = end_adj - start_adj
				gen_file.seek(fai_entry.offset+start_adj)
				seq = gen_file.read(len_adj).replace('\r','').replace('\n','')
				cds += [(start, seq, stra)]
		print_cds(mRNA_attr, assemble_cds(cds), args, out)	# Print the last gene

def extract_from_transcriptome(args):
	try:
		cds_file = open(args.t, 'r')
	except IOError as e:
		sys.stderr.write('Error: Could not open transcriptome file %s: %s\n'%(args.g, e.strerror))
		exit(1)
	if args.o is None:
		out = sys.stdout
	else:
		try:
			out = open(args.o, 'w')
		except IOError as e:
			sys.stderr.write('Error: Could not open output file %s for writing: %s\n'%(args.o, e.strerror))
			exit(1)
	if args.c and not args.e:
		#sys.stderr.write(f'Warning: -c (cds output) given with -t (cds input) without -e for input file {args.t}. This will result in the input file simply being copied to {args.o if args.o else "stdout"}\n')
		for line in cds_file:
			out.write(line)
	else:
		cds = ''
		name = None
		for line in cds_file:
			line = line.rstrip()
			if not line:
				continue
			if line.startswith('>'):
				if name and cds:
					print_cds(name, cds, args, out)
				name = line
				cds = ''
			else:
				cds += line
		if name and cds:
			print_cds(name, cds, args, out)


def main():
	par = ap.ArgumentParser(description = 'Extract amino acid or CDS sequences from a genomic fasta file using a gff3 annotation file')
	par.add_argument('-g', '--genome', metavar = 'genome.fa', required = False, help = 'Genome assembly. Should be in fasta format, uncompressed. It needs to have been indexed by samtools using the samtools faidx command.', dest = 'g')
	par.add_argument('-a', '--annotation', metavar = 'annotation.gff3', required = False, help = 'Annotation in GFF3 format. Should use the same chromosome / scaffold names as appear in the genome assembly. The GFF3 file is expected to be ordered as gene > mRNA > CDS.', dest = 'a')
	par.add_argument('-t', '--transcriptome', metavar = 'transcriptome.fa', required = False, help = 'Transcriptome fasta file. Can be given as an alternative to the genome and annotation. Introns should be already removed, and UTRs should be removed also unless -u is also specified.', dest = 't')
	par.add_argument('-o', '--output', required = False, metavar = 'proteins.fa', help = 'Output file. Output will be in fasta format. If unspecified, output will be to stdout.', dest = 'o')
	par.add_argument('-c', '--cds-output', action = 'store_true', help = 'Extract the CDS of each gene and output as nucleic acid sequences. Otherwise, each CDS will be translated and the output will be amino acid sequences.', dest = 'c')
	par.add_argument('-nt', '--no-start', choices = ['exclude', 'find', 'try-find', 'ignore'], default = 'ignore', help = 'Action to take when a sequence does not start with ATG (methionine). Options are: "exclude" to drop the sequence from the output file; "find" to search for the first ATG in the sequence and start translation there (may change reading frame) and exclude the sequence if none is found; "try-find" which operates like "find" except that if no ATG is found the sequence is let through as-is; and "ignore" (the default) to let all sequences not starting with ATG through as-is', dest = 'nt')
	par.add_argument('-np', '--no-stop', choices = ['exclude', 'add', 'ignore'], default = 'ignore', help = 'Action to take when a sequence does not end with a stop codon (TAA/TGA/TAG). Options are: "exclude" to drop the sequence from the output; "add" to add the missing stop codon to the end; and "ignore" (the default) to let such sequences through as-is', dest = 'np')
	par.add_argument('-is', '--internal-stop', choices = ['exclude', 'truncate', 'ignore'], default = 'ignore', help = 'Action to take when a sequence contains an internal in-frame stop codon (TAA/TGA/TAG). Options are: "exclude" to drop the seqence from the output; "truncate" to remove all amino acids past the stop codon; and "ignore" (the default) to let such sequences through as-is', dest = 'int_stp')
	par.add_argument('-x', '--exclude-unknown', action = 'store_true', help = 'Exclude sequences that contian unknown amino acids.', dest = 'x')
	par.add_argument('-l', '--line-length', type=int, default = 100, help = 'Length of lines in output fasta. Default is 100 characters.', dest = 'l')
	args = par.parse_args()
	if args.g and args.a:
		if args.t:
			sys.stderr.write('Error: Should provide either genome (-g) with annotation (-a) or transcriptome (-t), but not both\n')
			exit(1)
		else:
			extract_from_genome(args)
	elif args.t:
		extract_from_transcriptome(args)
	else:
		sys.stderr.write('Error: Should provide either genome (-g) with annotation (-a) or transcriptome (-t)\n')
		exit(1)


if __name__ == "__main__":
	main()
