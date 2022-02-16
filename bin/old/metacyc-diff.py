C#!/usr/bin/python3

from sys import stdin, stderr, stdout
import argparse as ap
import re

par = ap.ArgumentParser(description='Find reactions that are in the old reactions.dat and not the new one')
par.add_argument('-o', '--old', help='The old reactions.dat')
par.add_argument('-n', '--new', help='The new reactions.dat')

args = par.parse_args()

is_rxn = re.compile('^UNIQUE-ID - ')

try:
    oldfile = open(args.o, 'r')
except IOError as e:
    stderr.write('%s: %s\n'%(args.o, e.strerror))
    exit(1)

try:
    newfile = open(args.n, 'r')
except IOError as e:
    stderr.write('%s: %s\n'%(args.n, e.strerror))
    exit(1)

oldrxns = {}
with oldfile, newfile:
    for line in oldfile:
        if is_rxn.match(line):
            oldrxns[' - '.split(line.rstrip())[1]] = True
    for line in newfile:
        if is_rxn.match(line):
            r = ' - '.split(line.rstrip())[1]
            if r not in oldrxns:
                print(r)
