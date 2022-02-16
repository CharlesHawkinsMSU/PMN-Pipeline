#!/usr/bin/python3

from sys import stdin, stdout, stderr
import argparse as ap

par = ap.ArgumentParser(description='Give it two fasta files you suspect contain the same or nearly the same sequences. Gives you a list of the sequences by ID and for each says whether it is in the first file, the second file, or both, and in the latter case whether the sequences are identical')
par.add_argument('input1', metavar='file1.fa', help='The first fasta to compare')
par.add_argument('input2', metavar='file2.fa', help='The second fasta to compare')
par.add_argument('-o', '--output', help = 'Output file to save the results', dest = 'o')

