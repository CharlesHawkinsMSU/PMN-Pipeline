#!/usr/bin/env python3

from sys import stdin, stdout, stderr
import argparse as ap
import pmn

par = ap.ArgumentParser(description = 'Try to load the given PGDB table, generating errors if anything is wrong with it')
par.add_argument('Table', help = 'The table to load and verify')

args = par.parse_args()

table = pmn.read_pgdb_table(args.Table)
