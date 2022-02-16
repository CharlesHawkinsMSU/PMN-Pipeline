#!/usr/bin/env python3

import argparse as ap
from sys import stdin, stdout, stderr

par = ap.ArgumentParser(description='Determines the structure of the given gff file; what is parent to what')
par.add_argument('-i', '--input', help = 'Input file', dest = 'i')
par.add_argument('-o', '--output', help = 'Output file', dest = 'o')
par.add_argument('-l', '--limit', type=int, default=-1, help = 'Only look at the first l lines', dest = 'l')

args = par.parse_args()

if args.i is None:
	infile = stdin
else:
	try:
		infile = open(args.i, 'r')
	except IOError as e:
		stderr.write('Error: Could not open input file %s: %s\n'%(args.i, e.strerror))
		exit(1)

if args.o is None:
	outfile = stdout
else:
	try:
		outfile = open(args.o, 'w')
	except IOError as e:
		stderr.write('Error: Could not open output file %s for writing: %s\n'%(args.o, e.strerror))
		exit(1)

def add_val_to_dict_list (key, val, dictlist):
	if key not in dictlist:
		dictlist[key] = [val]
	elif val not in dictlist[key]:
		dictlist[key].append(val)

def add_vals_to_dict_list (key, vals, dictlist):
	if key not in dictlist:
		dictlist[key] = vals
	elif val not in dictlist[key]:
		dictlist[key].extend(vals)

parents_dict = {}	# Dict mapping child types to parent types
types_set = {}	# Set containing all types that have been observed
types_dict = {}	# Dict mapping individual features to their type
lost_parents = {}	# Dict mapping parents that have been referenced but not seen yet to a list of child nodes that reference them
line_n = 0
for gffline in infile:
	gffline = gffline.rstrip()
	line_n += 1
	if line_n == args.l:
		break
	if gffline == '##FASTA':	# We've reached the FASTA section, there's no more gff data
		break
	if gffline[0] == '#':	# Some other kind of comment; ignore it
		continue
	fields = gffline.split('\t')
	if len(fields) < 7:
		print("Warning: line %s only has %s fields"%(line_n, len(fields)))
		continue
	feature_type=fields[2]
	feature_attributes = {}
	for attribute in fields[8].split(';'):
		key_val = attribute.split('=')
		try:
			feature_attributes[key_val[0]] = key_val[1]
		except IndexError:
			continue
	if 'ID' in feature_attributes:
		feature_name = feature_attributes['ID']
	elif 'Name' in feature_attributes:
		feature_name = feature_attributes['Name']
	else:
		feature_name = ''
	if feature_name in lost_parents:
		for child_type in lost_parents[feature_name]:
			add_val_to_dict_list(child_type, feature_type, parents_dict)
		del lost_parents[feature_name]
	types_dict[feature_name] = feature_type
	if 'Parent' in feature_attributes:
		parent = feature_attributes['Parent']
		try:
			parent_type = types_dict[parent]
		except KeyError:
			add_val_to_dict_list(parent, feature_type, lost_parents)
			#print("Feature %s appears before its parent %s"%(feature_name, parent))
			continue
		add_val_to_dict_list(feature_type, parent_type, parents_dict)
	types_set[feature_type] = True
# Invert the parents_dict so that it maps from parent to children
child_dict = {}
for child_type in parents_dict:
	for parent_type in parents_dict[child_type]:
		add_val_to_dict_list(parent_type, child_type, child_dict)

# Find the root types, types that exist in the file but have no parent
root_types = []
for feature_type in types_set:
	if feature_type not in parents_dict:
		root_types += [feature_type]

def print_tree(f_type, indent = 0):
	outfile.write("\t"*indent + f_type + '\n')
	try:
		for c_type in child_dict[f_type]:
			print_tree(c_type, indent+1)
	except KeyError:
		pass
for f_type in root_types:
	print_tree(f_type)
