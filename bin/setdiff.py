#!/usr/bin/env python3

from sys import argv, stderr

set_a = set()
set_b = set()

try:
    a_file = open(argv[1], 'r')
except IOError as e:
    stderr.write('%s: %s\n'%(argv[1], e.strerror))
    exit(1)
try:
    b_file = open(argv[2], 'r')
except IOError as e:
    stderr.write('%s: %s\n'%(argv[2], e.strerror))
    exit(1)

with a_file, b_file:
    for line in a_file:
        line = line.rstrip()
        set_a.add(line)
    for line in b_file:
        line = line.rstrip()
        set_b.add(line)
    set_diff = set_a.difference(set_b)
    for line in set_diff:
        print(line)
