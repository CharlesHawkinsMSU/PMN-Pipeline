#!/usr/bin/python3

from sys import stdin, stdout, stderr

import argparse as ap
import gzip

esc = '\x1b'	# The escape character
csi = esc + '['	# Control Sequence Introducer, used for terminal control sequences
def sgr(n):	# Return a string that when printed will send a Select Graphic Rendition command to the terminal. n should be an integer indicating the display mode to select
    return(csi + str(n) + 'm')
def with_sgr(n, string):	# Return a string containing the given string with graphic rendition code n, and a code that resets the terminal after
    return(sgr(n)+string+sgr(0))
def curs_up(n = 1):
    return csi + str(n) + 'A'
def curs_dn(n = 1):
    return csi + str(n) + 'B'
def curs_fw(n = 1):
    return csi + str(n) + 'C'
def curs_bk(n = 1):
    return csi + str(n) + 'D'
def curs_col(c = 1):
    return csi + str(c) + 'G'
def clr_ln():
    return csi + '2K'

def color_fa_line(line, sep, kv, gene_fd):
    outilne = []
    if not sep:
        return '>' + with_sgr(1, line) + '\n'
    else:
        fds = line.split(sep)
        outline = ['>' + with_sgr(1, fds[0])]
        for i, f in enumerate(fds[1:]):
            if i+1 == gene_fd:
                outline += [with_sgr(32, f)]
            elif kv:
                try:
                    k, v = f.split(kv, 1)
                    if k == gene_fd:
                        outline += [with_sgr(34, k) + with_sgr(43, kv) + with_sgr(32, v)]
                    else:
                        outline += [k + with_sgr(43, kv) + v]
                except ValueError:
                    outline += [with_sgr(31, f)]
            else:
                outline += [f]
    return with_sgr(47, sep).join(outline) + '\n'

par = ap.ArgumentParser(description = 'Looks at one or more fasta files and guesses where / how to find the gene info')
par.add_argument('Input', nargs='+', help = 'Input file(s)')
par.add_argument('-o', '--output', help = 'Output file', dest = 'o')

args = par.parse_args()

try:
    if args.o is None or args.o == '-':
        outfile = stdout
        info = stderr
    else:
        outfile = open(args.o, 'w')
        info = stdout
except IOError as e:
        stderr.write(f'{e.filename}:{e.strerror}\n')
        exit(1)

with outfile:
    # Write header line compatible with PMN table
    outfile.write('\t'.join(['Sequence File', 'Genes From', 'FASTA Field', 'FASTA Sep', 'FASTA KV'])+'\n')
    for inpath in args.Input:
        try:
            infile = gzip.open(inpath, 'rt')
            infile.read(1)
            infile.seek(0)
        except gzip.BadGzipFile:
            try:
                infile = open(inpath, 'r')
            except IOError as e:
                stderr.write(f'{inpath}: {e.strerror}\n')
                exit(1)
        except IOError as e:
            stderr.write(f'{inpath}: {e.strerror}\n')
            exit(1)
        for i, line in enumerate(infile):
            line = line.rstrip()
            if line:
                if line.startswith('>'):
                    break
                else:
                    stderr.write(f'{inpath}, line {i}: First non-blank line should start with ">". Is this a fasta file? The line in question:\n{line}\n')
                    exit(1)
        else:
            stderr.write(f'{inpath} is empty or has all blank lines\n')
            exit(1)
        line = line[1:]  # Remove leading '>'
        # Guess FASTA Sep (the field separator for fields in the FASTA header)
        sep = ''
        if len(line.split(' | ')) > 1:  # Need special handling for ' | ' because it will give false positives for '|' and ' ' if it's used
            sep = ' | '
        else:
            # We will try common field separators and go with the one that results in the most fields
            sep_guesses = ['|', ' ', '\t']
            max_seps = 1
            best_sep = None
            for sep_guess in sep_guesses:
                result = len(line.split(sep_guess))
                if result > max_seps:
                    max_seps = result
                    sep = sep_guess
        if not sep:
            kv = ''
            gene_fd = ''
        else:
            fields = line.split(sep)
            # Guess FASTA KV (the key-value separator within each field)
            # The way this works is that we have a set of possible kv seps, and we go through each field in the header and see which of the possible seps occurs first (on the logic that e.g. loc=chr3:55123123:123 means that = should win), if any do. Then we see which sep (including no sep at all for fields where none of our guesses were found) got the most wins and use that as the kv sep
            kv_guesses = [':', '=']
            kv_wins = {}
            for field in fields[1:]:
                best_kv = ''
                best_kv_loc = None
                for kv_guess in kv_guesses:
                    loc = field.find(kv_guess)
                    if loc > -1:
                        if best_kv_loc is None or loc < best_kv_loc:
                            best_kv = kv_guess
                            best_kv_loc = loc
                try:
                    kv_wins[best_kv] += 1
                except KeyError:
                    kv_wins[best_kv] = 1
            kv = ''
            best_kv_wins = 0
            for kv_guess in kv_guesses:
                wins = kv_wins.setdefault(kv_guess, 0)
                if wins > best_kv_wins:
                    best_kv_wins = wins
                    kv = kv_guess
            # Guess the gene field name
            # The way this works is we look for fields with a field name that is in our set of guesses for the gene field. If none of the field names match, or there are no field names, it looks for a field whose value is found at the start of the protein name (to find instances where the prot id looks like Asdf1g234.1.p and the gene ld looks like Asdf1g234)
            gene_fd = ''
            gene_fd_guess_contain = ''
            prot = fields[0]
            if kv:
                # We got a kv sep so we're guessing the field name to use
                gene_fd_guesses = set(['gene', 'locus', 'Gene', 'Locus'])
                transcript_fds = set(['transcript', 'Transcript', 'cds', 'CDS', 'mrna', 'mRNA'])
                for fn, field in enumerate(fields[1:]):
                    try:
                        key, val = field.split(kv, 1)
                        if key in gene_fd_guesses:
                            gene_fd = key
                            break
                        if len(val) > 5 and prot.startswith(val) and key not in transcript_fds:
                            gene_fd_guess_contain = key
                    except ValueError:
                        if prot.startswith(field):
                            gene_fd_guess_contain = fn+1
                gene_fd = gene_fd or gene_fd_guess_contain
            else:
                for fn, field in enumerate(fields[1:]):
                    if prot.startswith(field):
                        gene_fds = fn+1
                        break
        if gene_fd:
            outfile.write('\t'.join([inpath, 'FASTA', str(gene_fd), sep, kv])+'\n')
        else:
            outfile.write('\t'.join([inpath, 'None', '', '', ''])+'\n')
        info.write(color_fa_line(line, sep, kv, gene_fd))

