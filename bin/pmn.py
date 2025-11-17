# pmn.py
# 
# Charles Hawkins at Rhee Lab, Department of Plant Biology, Carnegie
# Institution for Science
#
# Created: 2022
# Modified: October 2022
#
# This file contains various python utility functions used
# by the other PMN pipeline python scripts
#

from sys import stdin, stdout, stderr, version, version_info, executable
import subprocess
import os
from os import path
import stat
import re
import socket
import shutil
import time
import threading
import fcntl

import util

if __name__ == "__main__":
	stderr.write("The pmn.py file contains utility functions used by the other PMN python scripts; it doesn't do anything when run directly. The pipeline is mostly run via the pmn-pipeline script\n")
	exit(0)

esc = '\x1b'
csi = esc + '['
def sgr(n):
	return csi + str(n) + 'm'
def with_sgr(string, n, reset = 0):
	return sgr(n)+string+sgr(reset)
def red_text(string):
	return with_sgr(string, 31, 39)
def green_text(string):
	return with_sgr(string, 32, 39)
def blue_text(string):
	return with_sgr(string, 34, 39)
def yellow_text(string):
	return with_sgr(string, 33, 39)
def purple_text(string):
	return with_sgr(string, 35, 39)
def gray_text(string):
	return with_sgr(string, 37, 39)
def bold_text(string):
	return with_sgr(string, 1, 22)

# Converts a string to a boolean, where the string can be Y/yes/1/t/True/etc. for True or n/NO/0/F/false/etc. for False. Raises a ValueError if it doesn't recognize the string as a boolean. The second argument indicates the value to return in the case of an empty string; it should be True, False, or None (meaning raise a ValueError). Used when reading boolean columns from the PGDB table and boolean variables from the PGDB config file
def fuzzy_bool(bool_str, accept_empty_as = False):
	bool_str_lc = bool_str.lower()
	if bool_str == '':
		if accept_empty_as is None:
			raise ValueError(bool_str)
		else:
			return accept_empty_as
	elif 'yes'.startswith(bool_str_lc) or 'true'.startswith(bool_str_lc) or bool_str_lc == '1':
		return True
	elif 'no'.startswith(bool_str_lc) or 'false'.startswith(bool_str_lc) or bool_str_lc == '0':
		return False
	else:
		raise ValueError(bool_str)

# When given a dictionary that maps from strings to strings, converts the selected keys' values to bools using fuzzy_bool(). If keys is None, it converts all the keys in the dict. Set if_not_found to control what happens when a requested key isn't found; it should be "ignore" (leave key unset), "true" (put in key with value True), "false" (put in key with value False), or "error" (raise a KeyError). Set if_empty to control what happens for an empty string; it is passed to fuzzy_bool as accept_empty_as
def fuzzy_bool_dict(d, keys = None, if_not_found = "false", if_empty = False):
	if keys is None:
		keys = d.keys()
	for key in keys:
		try:
			val = d[key]
			d[key] = fuzzy_bool(val, if_empty)
		except KeyError:
			if if_not_found == 'false':
				d[key] = False
			elif if_not_found == 'true':
				d[key] = True
			elif if_not_found == 'ignore':
				pass
			elif if_not_found == 'error':
				raise
			else:
				e = ValueError(if_not_found)
				e.add_note(f'The if_not_found argument to fuzzy_bool_dict() should be "true", "false", "ignore", or "error"; instead received "{if_not_found}"')
				raise e
# Convenience function that allows filenames and already-open files to be used interchangeably. 'where' should be either an already-open file (returned as-is; 'mode' is ignored), a filename (opened with the specified mode, and the file handle returned), or either None or '-' (stdin or stdout is returned, as appropriate given the value of mode).
def openfile(where, mode = 'r'):
	if where is None or where == '-':
		return stdin if mode.startswith('r') else stdout
	elif isinstance(where, str):
		try:
			return open(where, mode)
		except IOError as e:
			error(f'{where}: {e.strerror}')
			exit(1)
	else:
		return where

# Takes the values in the given dictionary and makes a set from them. Used in revise_pf.py to determine which gene and protein IDs are already taken when assigning new ones
def set_from_values(d):
	s = set()
	for _, val in d.items():
		s.add(val)
	return s

# Ask the user a yes/no question and get their response, returning it as a bool. Pass in args.y as y_flag so that the question will be bypassed and assumed True if the command was run with -y. If it doesn't understand the user's response as a boolean (using fuzzy_bool), it will ask again, unless unknown_is_no is True, in which case it will return False on an unrecognzed response
def ask_yesno(prompt, y_flag = False, unknown_is_no = False):
	if y_flag:
		print(prompt + 'Yes')
		return True
	while True:
		try:
			response = input(prompt)
			return fuzzy_bool(response, None)
		except ValueError:
			if unknown_is_no:
				return False
			else:
				error('Please answer Yes, No, y, n, T, false, 1, 0, etc')
		except KeyboardInterrupt:
			return False


def dir_for_org(org, config):
	return path.join(config['ptools-pgdbs'], org.lower()+'cyc')

# Parses an attribute string of the form "attr1=val1;attr2=val2;..." into a dictionary. Used in parsing gff files
def parse_attrs(attr_str):
	attr_dict = {}
	for pair in attr_str.split(';'):
		key_val = pair.split('=')
		if len(key_val) < 2:
			continue
		attr_dict[key_val[0]] = key_val[1]
	return attr_dict

# Reads a two-column tsv file and returns a dict mapping the first column to the second. Columns past the second are ignored. If multiple lines share the same value for the first column, only the last is retained. "file" can be a filename or an already-open file.
def tsv_to_dict(file, sep = '\t'):
	if isinstance(file, string):
		try:
			infile = open(file, "r")
		except IOError as e:
			error("%s: %s"%(file, e.strerror))
	else:
		infile = file
	d = {}
	for line in infile:
		line = line.rstrip()
		if not line:
			continue
		fields = line.split(sep)
		if len(fields)<2:
			continue
		d[fields[0]] = fields[1]
	return d

# If <d> is a dictlist (dictionary with lists as values), adds <val> to the list pointed to by <key> in <d>. If <val> already exists in that list, nothing happens. If <key> isn't in <d>, it is added, pointing to a one-element list containing <val>. Modifies <d>.
def add_to_dictlist(d, key, val):
	try:
		cv = d[key]
		if val not in cv:
			cv.append(val)
	except KeyError:
		d[key] = [val]

# Inverts a dictionary, returning a new one with <d>'s values as keys and vice-versa. If multiple of <d>'s keys point to the same value only one of them is retained; which one is undefined
def invert_dict(d):
	di = {}
	for key, val in d.items():
		di[val] = key
	return di

# Inverts a dictlist (dictionary with lists as values) so that all elements of <d>'s values become keys pointing to a list of all the keys whose values in <d> contained that key. So for example invert_dictlist({'rose':['red','white'], 'buttercup':['yellow'], 'daffodil':['yellow','white']}) returns {'red':['rose'], 'white':['rose','daffodil'], 'yellow':['buttercup', 'daffodil']}
# Also works with scalar values in the input dict, but still returns a dictlist as output; {'a': 'b', 'c': 'd', 'e': 'b', 'f': ['g', 'b']} returns {'b': ['a', 'e', 'f'], 'd': ['c'], 'g': ['f']}
def invert_dictlist(d):
	di = {}
	for key, vals in d.items():
		if isinstance(vals, list):
			for val in vals:
				add_to_dictlist(di, val, key)
		else:
			add_to_dictlist(di, vals, key)
	return di

# Parses a fasta header line, returning a dictionary. <sep> is the string separating fields in the header, while <kv_sep> is the string separating each key from its value. So to parse ">ssp12345.1 gene=ssp12345 chr=chr1" you would give sep = ' ' and kv_sep = '='
def parse_fasta_header(line, sep = ' ', kv_sep = '='):
	fasta_dict = {}
	line = line.rstrip()
	if line:
		if line[0] == '>':
			line = line[1:]
		fields = line.split(sep)
		i = 0
		for field in fields:
			kv = field.split(kv_sep, 1)
			if len(kv) == 2:
				fasta_dict[kv[0]] = kv[1]
			fasta_dict[str(i)] = field
			i += 1
	return fasta_dict

# Represents a GFF entry
class GFF_entry:
	def __init__(self, seq, src, feature, start, end, score, strand, frame, attrs):
		self.seqname = seq
		self.source = None if src == '.' else src
		self.feature = feature
		self.start = None if start == '.' else int(start)
		self.end = None if end == '.' else int(end)
		self.score = None if score == '.' else float(score)
		self.strand = None if strand == '.' else strand
		self.frame = None if frame == '.' else frame
		self.attrs = attrs
		self.parent = None
		self.children = []
	def __getitem__(self, item):
		return self.attrs[item]

# Represents all the GFF entries in a GFF file, indexed by one of the keys in their attributes ('ID' by default)
class GFF_dict(dict):
# Reads in a GFF file from disk. 'id_key' should  be one of the attributes in the gff entries, and is used as the dictionary key as well as what parent links will refer to. Does not populate the parent or children fields; use add_parents() and add_children() to populate those fields
	def __init__(self, gff_file, id_key = 'ID'):
		dict.__init__(self)
		self.id_key = id_key
		self.parents_added = False
		self.children_added = False
		line_n = 0
		for line in openfile(gff_file):
			line = line.rstrip()
			line_n += 1
			if line == "###FASTA":
					break
			if line == '' or line[0] == '#':
					continue
			fields = line.split('\t')

			# Make sure we have at least 9 tab-separated fields
			if len(fields) < 9:
					warn('Line %s of %s has only %s fields, expected at least 9'%(line_n, gff_file, len(fields)))
					continue

			# Parse the attribute field (field 8) into a dictionary
			attrs = parse_attrs(fields[8])
			try:
				entry_id = attrs[id_key]
			except KeyError:
				continue
			self[entry_id] = GFF_entry(fields[0], fields[1], fields[2], fields[3], fields[4], fields[5], fields[6], fields[7], attrs)

# Adds in the parent links for all gff entries. Afterward, each entry's parent fields will point to another GFF entry, its parent. The 'via' argument is the field name in each entry that refers to the parent (usually 'Parent', the default, but could be, e.g., 'Component_of'). This should refer to the field in the parent entry whose name was given as id_key when the GFF_dict was created
	def add_parents(self, via = 'Parent'):
		for key, entry in self.items():
			try:
				parent_id = entry[via]
			except KeyError:
				continue
			try:
				entry.parent = self[parent_id]
			except KeyError:
				warn(f'Feature {key} refers to feature {parent} as its parent, but no such feature exists in the file')
				continue
		self.parents_added = True

# Adds in the children links for all gff entries. Afterward, each entry's chilren field will be a list containing references to other GFF entries, its (direct) children. The 'via' argument is the field name in each entry that refers to the parent (usually 'Parent', the default, but could be, e.g., 'Component_of'). This should refer to the field in the parent entry whose name was given as id_key when the GFF_dict was created. 
	def add_children(self, via = 'Parent'):
		for key, entry in self.items():
			try:
				parent_id = entry[via]
			except KeyError:
				continue
			try:
				self[parent_id].children.append(entry)
			except KeyError:
				warn(f'Feature {key} refers to feature {parent} as its parent, but no such feature exists in the file')
				continue
		self.children_added = True

# Reads in a file with lines of the form var = val. Used to parse the pgdb-pipeline.txt config file
def read_var_val_file(filename):
	file = openfile(filename)
	d = {}
	for line in file:
		line = line.strip()
		if not line or line.startswith('#'):
			continue
		try:
			(key, val) = [s.strip() for s in line.split('=', 1)]
			if key in d:
				warn(f'In file {file.name}, variable {key} is defined more than once')
			d[key] = val
		except ValueError:
			warn(f'line not recognized in {file.name}:\n{line}')
	return d

# Reads in the pgdb-pipeline.txt config file for the PMN pipeline, and fills in default values as needed. Returns a dictionary, generally passed as <config> to other functions
def read_pipeline_config(configfile, default_file = '/etc/pmn-pipeline.conf'):
	if default_file and path.exists(default_file):
		config = read_var_val_file(default_file)
		config.update(read_var_val_file(configfile))
	else:
		config = read_var_val_file(configfile)
	try:
		config.setdefault('priam', path.join(config['e2p2'], 'PRIAM_search.jar'))
		config.setdefault('rpsd', path.join(config['e2p2'], 'rpsd_current'))
		config.setdefault('priam-profile', path.join(config['rpsd'], 'profiles'))
	except KeyError:
		pass
	config.setdefault('parallelism', 'none')
	return config

# Reads in a tab-separated text file with header, and returns a dictionary mapping from the value in the column whose name matches <key_col> to a dictionary mapping from column header names to the value for that column in that row
# So if you have the following file as table.txt:
#   UID   Name   Comment
#   U01   Alice  I'm a user!
#   U02   Bob	I'm also a user!
# then read_table_file("table.txt", "UID") will return:
# {'U01':{'UID':'U01','Name':'Alice','Comment':'I\'m a user!'},
#  'U02':{'UID':'U02','Name':'Bob','Comment':'I\'m also a user!'}}
def read_table_file(filename, key_col):
	tablefile = openfile(filename)
	header = [f.strip('"') for f in tablefile.readline().rstrip().split('\t')]
	line_n = 1
	table = {}
	if key_col not in header:
		raise KeyError(key_col)
	for line in tablefile:
		line = [f.strip('"') for f in line.rstrip().split('\t')]
		entry = {}  # Dict for this row, mapping column names to values
		entry['_filename'] = filename

		# Go through each column and put it into the dict for this orgid (which is the entry var)
		for i in range(len(line)):
			try:
				val = line[i]
				if val:
					entry[header[i]] = val
				info(f'{i}. {header[i]}: {line[i]}')
			except IndexError:
				error(f'{tablefile.name}: Line {line_n} has more fields than the header (line has {len(line)}, header has {len(header)})')
				error(str(line))
				exit(1)
		table[entry[key_col]] = entry
		line_n += 1
	return table

pf_re = re.compile(r'(.pf)?$')
orxn_pf = re.compile(r'\.orxn\.pf$')
recognized_columns = set([
		'Presets', 'Database ID', 'ID/Name', 'Database Name',
		'Species Name', 'Abbrev Name', 'Sequence File', 
		'Initial PF File', 'PF File', 'Citation Year',
		'SAVI Citation', 'E2P2 Citation', 'Enzrxn Citation',
		'ENZ name file', 'RXN Map', 'PWY Metacyc', 'PWY Plantcyc',
		'Reference DB', 'Also MetaCyc', 'Map In', 'Map Out',
		'Authors', 'Genes From', 'FASTA Field', 'FASTA Sep',
		'FASTA KV', 'GFF File', 'GFF Prot Feature', 'GFF Prot Name',
		'GFF Key', 'GFF Path', 'Species Name', 'NCBI Taxon ID',
		'Sequence File', 'Unique ID', 'Version', 'Seq Source',
		'Curator', 'Citation Year', 'START timestamp', 'END timestamp',
		'Gene Delete', 'Prot Delete', 'Numeric IDs',
		])

# Reads in one or more PGDB tables (if argument is a list, the tables will be concatinated together), performs appropriate interpolation, and returns a dictionary mapping from orgids ("Database ID" column) to (dictonaries mapping from column name to the value in that column for that orgid). Database IDs starting with a / (e.g. "/phytozome") are presets. Any entry with one or more presets in the "Presets" column (give multiple as,  e.g., "/phytozome/pmn2022") will have those presets' values placed in any column that is blank for that orgid. Presets earlier in the list take precedence. The special /default preset is applied to all orgids with the lowest precedence. Presets can be referenced across files but must be defined before they are used. Also returns a dict from indexes to orgids
def read_pgdb_table(tables, config):
	# proj_dirs enumerates project directories, with a tuple that includes the key in the config file specifying the directory and the column in the table file that contains a file that should be located there
	proj_dirs = [
		('proj-fasta-dir', 'Sequence File'),
		('proj-gff-dir', 'GFF File'),
		('proj-maps-dir', 'Map In'),
		('proj-maps-output-dir', 'Map Out'),
		('proj-e2p2-dir', 'Initial PF File'),
		('proj-e2p2-dir', 'PF File')]
	if not isinstance(tables, list):
		tables = [tables]
	pgdb_dict = {}
	preset_dict = {}
	line_n = -1
	index = 1
	index_table = {}
	try:
		for table in tables:
			tablefile = openfile(table)
			header = [f.strip('"') for f in tablefile.readline().rstrip().split('\t')]
			unrecognized_columns = set(header) - recognized_columns
			if unrecognized_columns:
				plural = len(unrecognized_columns) > 1
				uc_andlist = util.andlist(unrecognized_columns, quote = "\"")
				message(f'Column{"s" if plural else ""} {uc_andlist} in {table} are not used in the pipeline and will be ignored', attn = 'Note', attn_style = [green_text, bold_text])
			line_n = 1
			for line in tablefile:
				line = [f.strip('"') for f in line.rstrip().split('\t')]
				entry = {}  # Dict for this orgid, mapping column names to values
				entry['_filename'] = table

				# Go through each column and put it into the dict for this orgid (which is the entry var)
				for i in range(len(line)):
					try:
						val = line[i]
						if val:
							entry[header[i]] = val
					except IndexError:
						error(f'{tablefile.name}: Line {line_n} has more fields than the header')
						exit(1)
				# Look for any presets in the Presets column and apply
				try:
					presets = entry['Presets']
					if not presets.startswith('/'):
						error(f'{tablefile.name}, line {line_n}: Preset names should start with a "/"')
						exit(1)
					presets = presets.split('/')[1:]
					if 'default' in preset_dict:
						presets += ['default']
				except KeyError:
					presets = ['default'] if 'default' in preset_dict else []
				for preset in presets:
					try:
						preset_entry = preset_dict[preset]
					except KeyError:
						error(f'{tablefile.name}, line {line_n}: Reference to non-existent preset "{preset}"')
						exit(1)
					for key, val in preset_entry.items():
						if key not in entry or not entry[key]:
							entry[key] = preset_entry[key]

				# Operations to only be performed on actual PGDBs, not on preset definitions
				if not entry['Database ID'].startswith('/'):
					entry['_index'] = index
					# Fill in computed fields

					entry.setdefault('Database Name', entry['Database ID']+'Cyc')
					species_words = entry['Species Name'].split(' ')
					if len(species_words) < 2:
						error(f'{tablefile.name}, line {line_n}: Species name "{entry["Species Name"]}" should be at least two words long')
						exit(1)

					entry.setdefault('ID/Name', species_words[0][0]+species_words[1])
					abbrev_name = species_words[0][0] + '. ' + species_words[1]
					for w in species_words[2:]:
						if not w.endswith('.'):
							abbrev_name += ' '+w
					entry.setdefault('Abbrev Name', abbrev_name)
					entry.setdefault('Initial PF File', entry['Sequence File']+'.e2p2')
					entry['Initial PF File'] = orxn_pf.sub('', entry['Initial PF File'])
					entry.setdefault('PF File', pf_re.sub('.orxn.revised.pf', entry['Initial PF File'], 1))
					year = entry['Citation Year']
					entry.setdefault('SAVI Citation', f'|PUB-PMNUPP{year}| |PUB-PMNAIPP{year}| |PUB-PMNRXN{year}| |PUB-PMNRXNTAXON{year}| |PUB-PMNTAXON{year}| |PUB-PMNIC{year}| |PUB-PMNSP{year}|')
					entry.setdefault('E2P2 Citation', f'|PUB-E2P2PMN{year}|')
					entry.setdefault('Enzrxn Citation', f'E2P2PMN{year}:EV-COMP-AINF')
					entry.setdefault('ENZ name file', f'ec_name.map.parsed')
					entry.setdefault('RXN Map', f'metacyc-rxn-name-mapping')
					entry.setdefault('PWY Metacyc', f'all_pwy.meta')
					entry.setdefault('PWY Plantcyc', f'all_pwy.plant')
					entry.setdefault('Reference DB', 'Plant')
					try:
						entry.setdefault('Map Out', entry['Map In'] + '.out')
					except KeyError:
						entry.setdefault('Map Out', entry['Sequence File'] + '.map.out')
					try:
						entry['_Authors'] = [as_lisp_symbol(a) for a in entry['Authors'].split(' ')]
					except KeyError:
						pass

					# Deal with boolean columns, converting "fuzzy" bools like "Yes", "T", or "0" to Python bools, and defaulting empty ones to False
					fuzzy_bool_dict(entry, ['Also MetaCyc'])

					# Fields below are required for all pgdb entries; reference the required fields so they generate an error if absent
					entry['Species Name']
					entry['NCBI Taxon ID']
					entry['Sequence File']
					entry['Version']
					entry['Seq Source']
					entry['Authors']
					entry['Curator']
					entry['Citation Year']

				# Now put in the directories for those columns that use the project directories from the config file
				if config:
					for key, col in proj_dirs:
						try:
							col_val = entry[col]
							if col_val:
								entry[col] = path.join(config[key], col_val)
						except KeyError:
							pass

				# Finished building this entry, put it into the appropriate dictionary (preset_dict or pgdb_dict depending on whether it's a preset definition or an actual pgdb)
				org = entry['Database ID'] 
				if org.startswith('/'):
					preset_dict[org[1:]] = entry
				else:
					pgdb_dict[org] = entry
					index_table[index] = org
					index += 1
				line_n += 1

	except KeyError as e:
		error(f'{tablefile.name}: Column {e.args[0]} is required for all entries but is missing from line {line_n}')
		exit(1)
	read_uids_file(config, pgdb_dict)
	return pgdb_dict, index_table

def write_pgdb_tablefile(orgtable, orglist, path, fields = None):
	if fields is None:
		# We need to get a set of all the fields that are present and non-empty for at least one of the requested orgids
		fields = set()
		for org in orglist:
			entry = orgtable[org]
			entry_fields = entry.keys()
			entry_fields = [field for field in entry_fields if not field.startswith('_') and entry[field]]
			fields.update(entry_fields)
		fields = list(fields) # We need the order of the set to be stable. It is currently in CPython as long as the set is unchanged, but it is not guaranteed that this will always remain the case in the future and for all implementations of Python
	with openfile(path, 'w') as outfile:
		outfile.write('\t'.join(fields)+'\n')
		for org in orglist:
			outfile.write('\t'.join([entry.setdefault(field, '') for field in fields])+'\n')

# Adds some standard arguments used in all pipeline scripts
def add_standard_pmn_args(par, action = 'operated on'):
	par.add_argument('-o', '--orgids', help = f'A comma-separated list of orgids to be {action}. If not given, all organisms in the pgdb-table will be {action}', dest = 'o')
	par.add_argument('-c', '--config', default='pgdb-pipeline.txt', help = 'Config file with general (non-organism-specific) pipeline parameters.', dest = 'c')
	par.add_argument('-t', '--pgdb-table', help = 'Table file of PGDBs. If not given the filename will be read from the config file', dest = 't')
	par.add_argument('-v', '--verbose', action = 'store_true', help = 'Print info messages to stdout while running. Info messages are always included in the logfile regardless', dest = 'v')
	par.add_argument('--timestamps', action = 'store_true', help = 'Print timestamps for all messages to stdout. Timestamps are always included in the logfile regardless', dest = 'timestamps')
	par.add_argument('-p', '--proj', default = '.', help = 'Project directory. Defaults to the current working directory', dest = 'proj')
	par.add_argument('-y', '--yes', action = 'store_true', help = 'Assume yes to all prompts', dest = 'y')
	par.add_argument('-f', '--fix', '--no-overwrite', action = 'store_true', help = 'Only perform operations when the expected output files don\'t already exist. May be useful to continue from a failed or cancelled sequence of operations without redoing everything. Currently affects the newproj, e2p2, and create stages. Note: Does not check that existing files are correct or valid in any way, merely whether they exist', dest = 'f')

verbose = False # Print "verbose" messages to the console, not just to the logfile
logfile = None # The open logfile
time_fmt = '%F %T' # The strftime format for message timestamps
time_always = False # Include a timestamp when printing messages to the console, not just in the logfile

# Delivers the message <msg> to the console. Also prints it to the logfile if one is open. Messages always get a timestamp in the logfile; they get one on the console too only if the time_always var (usually set with --timestamps) is True
# 
# <attn> is the attention/marker to use, such as 'Info' to print "Info: {msg}"
# <attn_style> is a list of styling functions to run on the attn, such as bold_text or red_text
# <verbose_only> will only print the msg to the console if the verbose variable (usually set with --verbose) is True. Messages will always be printed to the logfile if there is one, regardless of verbosity

def message(msg, attn = None, attn_style = [], verbose_only = False):
	if attn:
		for s in attn_style:
			attn = s(attn)
		imsg = f'{attn}: {msg}'
	else:
		imsg = msg
	tmsg = f'[{gray_text(time.strftime(time_fmt))}] {imsg}'
	if verbose or not verbose_only:
		print(tmsg if time_always else imsg)
	if logfile:
		print(tmsg, file = logfile)

def info(msg):
	message(msg, attn = 'Info', attn_style = [blue_text, bold_text], verbose_only = True)

def error(msg):
	message(msg, attn = 'Error', attn_style = [red_text, bold_text])

def warn(msg):
	message(msg, attn = 'Warning', attn_style = [yellow_text, bold_text])

newline_re = re.compile(r'\r?\n')
def subproc_msg(output, procname = "SubCmd"):
	for msg in newline_re.split(output.decode(errors='replace').rstrip()):
		message(msg, attn = procname, attn_style = [purple_text, bold_text])

# Gets a unique ID for this run. Bases it on the process ID unless we're running in SLURM, in which case it's based on the job ID and, if it exists, the array task ID
def get_run_id():
	# Different versions of SLURM use either $SLURM_JOB_ID or $SLURM_JOBID, so we need to check both
	try:
		i = 's-' + os.environ['SLURM_JOB_ID']
	except KeyError:
		try:
			i = 's-' + os.environ['SLURM_JOBID']
		except KeyError:
			return 'p-' + str(os.getpid())
	try:
		i += '_' + os.environ['SLURM_ARRAY_TASK_ID']
	except KeyError:
		pass
	i += '-' + str(threading.get_native_id())
	return i

def open_logfile(config):
	global logfile
	try:
		logfilename = path.join(config['proj-logs-dir'], get_run_id() + '.log')
		differentiator = 0
		while path.exists(logfilename):
			differentiator += 1
			logfilename = path.join(config['proj-logs-dir'], get_run_id() + '-' + str(differentiator) + '.log')
		logfile = open(logfilename, 'x')
		open_msg = f'Logfile {logfilename} opened at {time.strftime(time_fmt)}'
		print(open_msg)
		print(open_msg, file = logfile)
	except IOError as e:
		stderr.write('Error creating logfile {logfile}: {e.strerror}. No logging will occur for this run\n')
	except KeyError:
		stderr.write('No proj-logs-dir in config. No logging will occur for this run\n')

def interp_backslashes(s):
	s2 = ''
	i = 0
	l = len(s)
	while i < l:
		c = s[i]
		if c == '\\':
			i += 1
			try:
				c2 = s[i]
				if c2 == 't':
					s2 += '\t'
				else:
					s2 += c2
			except IndexError:
				s2 += c
		else:
			s2 += c
		i += 1
	return s2

# Read the pmn config files in the standard way and get the list of organsims to operate on. <args> should be a results of parsing args with a parser that has had add_standard_pmn_args() called on it. Returns a tuple of the config, the table, and the org list. Also opens a logfile for this run
def read_pipeline_files(args):
	global verbose, time_always
	config = read_pipeline_config(args.c)

	verbose = args.v
	time_always = args.timestamps

	open_logfile(config)
	try:
		if args.t:
			config['proj-pgdb-table'] = args.t
			tablefile = args.t
		else:
			tablefile = config['proj-pgdb-table']
			info(f'Got name of table file from {args.c}: {tablefile}')
		ptable, index_table = read_pgdb_table(tablefile, config)
		if args.o:
			org_list_in = args.o.split(',')
			org_list = []
			for org in org_list_in:
				try:
					org = index_table[int(org)]
				except ValueError:
					pass
				except IndexError:
					error(f'No organism with index {org}')
					exit(1)
				if org not in ptable:
					error(f'Organism {org} not found in table')
					exit(1)
				if org in org_list:
					warn(f'Organism {org} appears multiple times in the requested org list. The organism will only be run once despite this because running the same organism multiple times could cause serious errors and/or incorrect output, especially if running in parallel')
				else:
					org_list.append(org)
		else:
			org_list = list(ptable.keys())
		info(f'Got list of orgids from {"command-line args" if args.o else tablefile}, will run the following orgids: {", ".join(org_list)}')
	except KeyError as e:
		error(f'{args.c}: Required variable {e.args[0]} not found')
		exit(1)
	return (config, ptable, org_list)

# This function is for the "Unique ID" field in the input table file; e.g. QT for aracyc, which results in most kinds of frame created in aracyc having QT in their frame ID, e.g. ENZRXNQT-12345, which is done to ensure that these frames don't end up having name collisions with frames created in metacyc. These IDs are generated by taking the "org counter", a number unique to the organism, and encoding it in Base36. PathoLogic expects to find the original org counter in the organism parameters file it takes as input instead of the encoded version, so this function is here to perform the decoding. This Python function is a manual translation of the equivalent Perl function written by Chuan
def counter_from_id(uid):
	counter = 0
	for i in range(len(uid)):
		c = uid[-(i+1)]
		if c >= '0' and c <= '9':
			v = ord(c) - ord('0')
		elif c >= 'A' and c <= 'Z':
			v = ord(c) - ord('A') + 10
		else:
			raise ValueError(c)
		counter += 36**i*v
	return counter

def id_from_counter(counter):
	counter = int(counter)
	if counter == 0:
		return '0'
	uid = ''
	while counter:
		counter, digit = divmod(counter, 36)
		uid = str(chr(digit) if digit < 10 else chr(digit-10+ord('A'))) + uid
	return uid

def read_uids_file(config, orgtable):
	uid_filename = config.setdefault('uids-file', 'uids.txt')
	try:
		uid_file = open(uid_filename)
		uid_file.readline()
		for line in uid_file:
			try:
				org, uid = line.rstrip().split('\t')
				orgtable[org].setdefault('Unique ID', uid)
			except (ValueError, KeyError):
				pass
		uid_file.close()
	except FileNotFoundError:
		pass
	except IOError as e:
		pmn.error(f'{uid_filename}: {e.strerror}')

def write_uids_file(config, orgtable):
	uid_filename = config.setdefault('uids-file', 'uids.txt')
	try:
		uid_file = open(uid_filename, 'w')
		uid_file.write('Orgid\tUID\n')
		for org, entry in orgtable.items():
			entry = orgtable[org]
			uid_file.write(f'{org}\t{entry.setdefault("Unique ID", "")}\n')
		uid_file.close()
	except IOError as e:
		error(f'{uid_filename}: {e.strerror}')

# Checks if the given version meets or exceeds the given minimum required version. Versions should be strings, consisting of a .-separated list of version components. Each component of version is compared to the corresponding component of min_version until they differ, at which point True is returned if version is greater or false if min_version is greater. If they match the whole way through then True is returned. Components will be compared numerically if both parse as integers, or compared as strings if one or both do not parse as integers
def version_meets_min(version, min_version):
	version_list = version.split('.')
	min_list = min_version.split('.')
	ldiff = len(min_list) - len(version_list)
	if ldiff > 0:
		version_list += [0]*(ldiff)
	else:
		min_list += [0]*(-ldiff)
	for i in range(len(version_list)):
		try:
			if int(version_list[i]) < int(min_list[i]):
				return False
			elif int(version_list[i]) > int(min_list[i]):
				return True
		except ValueError:
			if str(version_list[i]) < str(min_list[i]):
				return False
			elif str(version_list[i]) > str(min_list[i]):
				return True
	else:
		return True

# Check the general runtime environment, that we have the required python version, that required binaries are in the path
def check_env():
	passed = True
	if version_info.major < 3 or version_info.minor < 8:
		error(f'The pipeline requires Python 3.8 or later; currently running on {version_info.major}.{version_info.minor}.{version_info.micro}, located at {executable}')
		passed = False
	for exe in ['java', 'blastp']:
		if not shutil.which(exe):
			error(f'Required executable \'{exe}\' is not in the path')
			passed = False
	return passed

# Checks that all given files exist and are of the correct type. The progam will crash with an error if any don't exist or are the wrong kind of file
def check_exists(files = [], dirs = [], sockets = [], progname = 'this script'):
	passed = True
	for f in files:
		info(f'Checking for file {f}')
		if not os.path.exists(f):
			error(f'File {f}, required by {progname}, was not found')
			passed = False
		elif not os.path.isfile(f):
			error(f'File {f}, required by {progname}, exists but is not a standard file or a symlink to a standard file')
			passed = False
	for d in dirs:
		info(f'Checking for directory {d}')
		if not os.path.exists(d):
			error(f'Directory {d}, required by {progname}, was not found')
			passed = False
		elif not os.path.isdir(d):
			error(f'Directory {d}, required by {progname}, exists but is not a directory')
			passed = False
	for s in sockets:
		info(f'Checking for socket {s}')
		if not os.path.exists(s):
			error(f'Socket {s}, required by {progname}, was not found')
			passed = False
		elif not stat.S_ISSOCK(os.stat(s).st_mode):
			error(f'DSocket {s}, required by {progname}, exists but is not a socket')
			passed = False
	return passed

# Checks the given list of files for the given type of access for the current user. <access> should be os.R_OK, os.W_OK, os.X_OK, or os.F_OK. <reason> will be put after the filename in error messages to indicate to the user where the requirement for this file came from. If ignore_missing is True, nonexistant files will not generate an error or failure (can be used if existence has already been checked and an error printed)
def check_access(file_list, access, reason = 'required by the pipeline', ignore_missing = False):
	passed = True
	acc_errs = {
			os.F_OK: 'Cannot find file or directory',
			os.R_OK: 'No read access to',
			os.W_OK: 'No write access to',
			os.X_OK: 'No execute permission for'}
	try:
		access_err = acc_errs[access]
	except KeyError:
		error('Internal error: attempt to check for file access type {access}, an unrecognized access type. Please use check_access with a single value in the set os.F_OK, os.R_OK, os.W_OK, or os.X_OK')
		exit(2)

	for filename in file_list:
		if ignore_missing and not path.exists(filename):
			continue
		if not os.access(filename, access):
			error(f'{access_err} {filename}, {reason}.')
			passed = False
	return passed

def get_pgdb_folder(org, orgtable, config):
	entry = orgtable[org]
	pgdbs_folder = config['ptools-pgdbs']
	cyc_folder = path.join(pgdbs_folder, org.lower()+'cyc', entry['Version'])
	return cyc_folder


# Checks that all values in the given config dictionary (as produced by read_pipeline_config) are valid and, where applicable, refer to extant files / directories of the correct type. Crashes the program with an error if there are any problems. Used by the pipeline precheck in pgdb-pipeline.py
def check_pipeline_config(config):
	passed = True
	configfilename = config.setdefault('_filename', '(unknown filename)')
	if config.setdefault('save-intermediates','no') == 'yes':
		inter = True
	else:
		inter = False
	try:
		e2p2v4_exe = path.join(config['e2p2'], 'pipeline', 'run_pipeline.py')
		e2p2v5_exe = path.join(config['e2p2'], 'e2p2.py')
		if path.exists(e2p2v4_exe):
			passed &= check_access([e2p2v4_exe], os.X_OK, f'specified in {configfilename}')
		elif not path.exists(e2p2v5_exe):
			error(f'Require either {e2p2v4_exe} (for E2P2v4) or {e2p2v5_exe} (for E2P2v5), but neither exists')
			passed = False
			e2p2_exe = '.'

		passed &= check_exists(
				files = [
					config['proj-pgdb-table'],
					config['ptools-exe'],
					config['priam'],
					config['authors-file'],
					config['organizations-file'],
					config['pmn-lisp-funs']
					],
				dirs = [
					config['proj-masters-dir'],
					config['proj-fasta-dir'],
					config['proj-e2p2-dir'],
					config['proj-savi-dir'],
					config['proj-gff-dir'],
					config['proj-maps-dir'],
					config['proj-maps-output-dir'],
					config['proj-blastsets-dir'],
					config['proj-intermediate-dir'] if inter else '.',
					config['ptools-pgdbs'],
					config['savi'],
					config['proj-common-dir'],
					],
				progname = f'the config file {configfilename}')
		passed &= check_access(
				[
					config['proj-masters-dir'],
					config['proj-fasta-dir'],
					config['proj-e2p2-dir'],
					config['proj-savi-dir'],
					config['proj-gff-dir'],
					config['proj-maps-dir'],
					config['proj-maps-output-dir'],
					config['proj-blastsets-dir'],
					config['proj-intermediate-dir'] if inter else '.',
					config['ptools-pgdbs'],
					config['savi'],
					config['proj-common-dir'],
					config['e2p2'],
					config['authors-file'],
					config['organizations-file'],
					config['pmn-lisp-funs']
					],
				os.R_OK,
				f'specified in {configfilename}',
				ignore_missing = True)
		passed &= check_access(
				[
					config['proj-masters-dir'],
					config['proj-e2p2-dir'],
					config['proj-savi-dir'],
					config['proj-maps-output-dir'],
					config['proj-blastsets-dir'],
					config['proj-intermediate-dir'] if inter else '.',
					config['ptools-pgdbs'],
					],
				os.W_OK,
				f'specified in {configfilename}',
				ignore_missing = True)
		passed &= check_access([config['ptools-exe']], os.X_OK, f'specified in {configfilename}')
		xserver = config.setdefault('x-server', None)
		if xserver == 'xpra':
			if shutil.which('xpra') is None:
				error(f'Config option x-server in {configfilename} is set to "xpra", but xpra was not found in $PATH')
				passed = False
		elif xserver == 'xvfb':
			if shutil.which('Xvfb') is None:
				error(f'Config option x-server in {configfilename} is set to "xvfb", but Xvfb was not found in $PATH')
				passed = False
		elif xserver is not None and xserver != 'external':
			error(f'Config option x-server in {configfilename} is set to "{xserver}, which is not recognized (valid values are external, xpra, or xvfb)')
			passed = False

	except KeyError as e:
		error(f'Config file {configfilename} is missing required key {e.args[0]}')
		passed = False
	return passed

re_taxid = re.compile(r'^[0-9]+$')
def check_pgdb_table(table, config):
	passed = True
	cyc_set = set()
	uid_set = set()
	for org, entry in table.items():
		table_file = entry['_filename']
		info(f'Checking PGDB table entry for {org}')
		tax = entry['NCBI Taxon ID']
		if not re_taxid.match(tax):
			error(f'NCBI taxon IDs should be only digits; instead found {tax} for orgid {org} in {table_file}')
			passed = False
		cyc = entry['Database ID']
		if cyc in cyc_set:
			error(f'Duplicate Database ID: {cyc} in {table_file}')
			passed = False
		try:
			uid = entry['Unique ID']
			if uid and uid in uid_set:
				error(f'Duplicate Unique ID: {uid} in {table_file}')
				passed = False
			uid_set.add(uid)
		except KeyError:
			pass
		cyc_set.add(cyc)

		files_list = [entry['Sequence File']]
		try:
			files_list += [entry['GFF File']]
		except KeyError:
			pass
		try:
			files_list += [entry['Map In']]
		except KeyError:
			pass
		passed &= check_exists(files=files_list, progname=f'the table file {table_file}')
		cyc_folder = get_pgdb_folder(cyc, table, config)
		if path.exists(cyc_folder):
			warn(f'PGDB {cyc}Cyc {entry["Version"]} from {table_file} already exists at {cyc_folder}. It will be overwritten if you proceed')
	return passed

def check_parallelism(config):
	passed = True
	par = config.setdefault('parallelism', 'none')
	if par not in ['none', 'slurm', 'parallel']:
		error(f'Unrecognized parallelism provider \'{par}\'. Valid values are \'parallel\', \'slurm\', and \'none\'')
		exit(1)
	if par == 'slurm':
		if not shutil.which('sbatch'):
			error(f'Slurm selected for parallelism, but cannot find the sbatch command. Is Slurm installed?')
			passed = False
	if par == 'parallel':
		if not shutil.which('parallel'):
			error(f'GNU Parallel selected for parallelism, but cannot find the parallel command. Is GNU Parallel installed?')
			passed = False
	return passed

# Logs all output of the given process using subproc_msg.
# The proc should have been started using subprocess.Popen with stdout = PIPE and probably stderr = STDOUT
# For concurrent external processes, the function should be started in its own thread via start_log_process
# To make the external process blocking, start it the same way (with Popen) and call log_process directly
def log_process(proc, proc_name = 'SubProc'):
	while proc_output := proc.stdout.readline():
		subproc_msg(proc_output.rstrip(), proc_name)

# Start a thread to monitor the given process (as returned by subprocess.Popen) and to print and log all output it produces.
# NOTE: The thread keeps a reference to the process object you pass in! You must destroy any object with a reference to the thread object (e.g. a PathwayTools object) manually, else the logging process will make your program hang when it should exit
def start_log_process(proc, proc_name = 'SubProc'):
	th = threading.Thread(target = log_process, args = [proc, proc_name], daemon = True)
	th.start()
	return th

# Run a blocking external process in the standard way, printing and logging all output. If crash is True, a nonzero exit from the process will make this one crash with the same code. Otherwise returns the process's return code
def run_external_process(args, env = None, procname = 'SubProc', crash = True):
	if env is None:
		env = os.environ
	info(f'Running command: {" ".join(args)}')
	proc = subprocess.Popen(args, env = env, stdout = subprocess.PIPE, stderr = subprocess.STDOUT)
	log_process(proc, procname)
	proc.wait()
	rc = proc.poll()
	if rc != 0 and crash:
		error(f'Process {procname} failed')
		exit(rc)
	else:
		return rc

re_disp = re.compile(b'Actual display used: (:[0-9]+)')
def start_xpra():
	xpra_path = shutil.which('xpra')
	if xpra_path is not None:
		info('xpra found, using xpra')
		res = subprocess.run([xpra_path, 'start'], capture_output = True)
		info(res.stderr.decode())
		display = re_disp.search(res.stderr).group(1).decode()
		return xpra_path, display
	else:
		info('xpra not found')
		return None, None

def start_xvfb():
	xvfb_path = shutil.which('Xvfb')
	if xvfb_path is not None:
		info(f'Xvfb found at {xvfb_path}, starting')
		(r, w) = os.pipe()
		proc = subprocess.Popen([xvfb_path, '-displayfd', str(w)], pass_fds = [w])
		display = ':'+os.read(r, 128).strip().decode()
		return proc, display
	else:
		info('Xvfb not found')
		return None, None
	

# Sends a message via the given unix socket, waits for a reply, then returns that reply. Used by send_ptools_cmd()
def socket_msg(cmd, socket_path):
	s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
	s.connect(socket_path)
	s.sendall(cmd.encode())
	resp = b''
	while segment := s.recv(1024):
		resp += segment
	s.close()
	return resp.decode().rstrip()

# Send a lisp command via the API. Pathway Tools should have been started with the -api flag (NB: not the -python flag). Commands are automatically wrapped in an error handler unless handle_errs is set to False.
def send_ptools_cmd(cmd, socket_path = '/tmp/ptools-socket', handle_errs = True):
	info(f'Sending Ptools Command: {cmd}')
	if not os.path.exists(socket_path):
		error(f'Pathway tools socket {socket_path} does not exits. Did you remeember to start Pathway Tools with -api?')
		raise FileNotFoundError(socket_path)
	if not stat.S_ISSOCK(os.stat(socket_path).st_mode):
		error(f'Pathawy tools socket {socket_path} exists but is not a socket')
		raise IOError(socket_path)
	if handle_errs:
		socket_msg('(setq *status* nil)', socket_path)
		cmd_he = f'(handler-case {cmd} (error (e) (setq *status* `(,(type-of e) ,(format nil "~A" e)))))'
		#info(f'{cmd_he}\n')
		r = socket_msg(cmd_he, socket_path)
		e = socket_msg('*status*\n', socket_path)
		if e != 'NIL':
			error(f'{e}')
			raise ChildProcessError()
		else:
			return r
	else:
		return socket_msg(cmd, socket_path)
# Switch organisms in the ptools instance. This function should be used instead of the lisp function "so" because (so 'org) uses (break) when the org was not found rather than raise a catchable condition, freezing up the api and preventing recovery
def ptools_so(org, socket_path = '/tmp/ptools-socket'):
	found_org = send_ptools_cmd(f"(setq org (find-org '{org}))", socket_path = socket_path)
	if found_org == "NIL":
		error(f"Organism {org} was not found by the Pathway Tools instance connected to {socket_path}")
		raise ChildProcessError()
	else:
		return send_ptools_cmd('(select-organism :org org)', socket_path = socket_path)

def as_lisp_symbol(l):
	l = l.lstrip("'")
	l = l.strip('|')
	l = f'\'|{l}|'
	return l
whitespace = re.compile(r'\s+')
class PathwayTools:
	def __init__(this, exe, socket = '/tmp/ptools-socket', args = [], env = None, timeout = 60, x11 = None, no_load_pgdbs = False):
		this.pt_exe = exe
		this.pt_socket = socket
		if '-lisp' not in args:
			args += ['-lisp']
		args += ['-no-patch-download']
		if no_load_pgdbs:
			args += ['-no-pgdbs']
		# Under Singularity, environment variables aren't making it to pathway tools for some reason, so as a workaround we will set the socket path and tell it to start the api daemon manually using -eval. Also, there is an apparent bug in ptools 27.0 where spaces are not accepted in an -eval statement (it treats them as if they were EOF markers and stops reading), so as another workaround we use newlines instead
		args += ['-eval', f'(progn (setf\n*socket-pathname*\n"{socket}")(start-external-access-daemon))']
		this.pt_args = args
		cmd = [exe] + args
		this.pt_cmdline = ' '.join(cmd)
		if os.path.exists(socket):
			error(f'There is already a Pathway Tools instance using {socket} as a socket. Please quit it before trying to use this function')
			raise FileExistsError(socket)
		pt_env = os.environ.copy()
		if x11 == 'xpra':
			info('Starting xpra')
			this.xpra_path, this.display = start_xpra()
			this.xvfb_proc = None
		elif x11 == 'xvfb':
			info('Starting Xvfb')
			this.xvfb_proc, this.display = start_xvfb()
			this.xpra_path = None
		elif x11 == 'external' or x11 is None:
			info('No X server requested; will use the system\'s X server')
			this.xvfb_proc, this.xpra_path, this.display = None,None,None
		else:
			error(f'x-server "{x11}" not recognized; valid values are "xpra", "xvfb", and "external"')
		if this.display:
			info(f'X11 display is {this.display}')
			pt_env['DISPLAY'] = this.display
		else:
			info('no X11 display found, xserver start may have failed')
		if env:
			pt_env.update(env)
		#pt_env = os.environ.copy()
		#pt_env.update(env or {})
		#pt_env['PTOOLS-ACCESS-SOCKET'] = socket

		info(f'Starting Pathway Tools as {this.pt_cmdline}')
		info(f'Env is {pt_env}')
		this.pt_proc = subprocess.Popen(cmd, stdin = subprocess.DEVNULL, stdout = subprocess.PIPE, stderr = subprocess.STDOUT, env = pt_env)
		this.log_thread = start_log_process(this.pt_proc, 'PTools')
		this.timeout = timeout
		start_time = time.monotonic()
		while not os.path.exists(socket):
			if time.monotonic()-start_time > timeout:
				this.pt_proc.kill()
				error(f'Timed out waiting for pathway tools to create an API socket at {socket} ({timeout} secs)')
				raise TimeoutError()
			if this.pt_proc.poll() is not None:
				error(f'Pathway Tools failed to start')
				raise ChildProcessError()
			time.sleep(0.1)
	def __enter__(this):
		return this
	def send_cmd(this, cmd, handle_errs = True):
		if this.pt_proc.poll() is not None:
			error(f'This instance of Pathway Tools has exited. Please create another instance')
			raise ChildProcessError()
		return send_ptools_cmd(cmd, this.pt_socket, handle_errs)
	def so(this, org):
		if this.pt_proc.poll() is not None:
			error(f'This instance of Pathway Tools has exited. Please create another instance')
			raise ChildProcessError()
		return ptools_so(org, this.pt_socket)
	def get_org_list(this):
		org_response = this.send_cmd('(all-orgids)')
		orglist = whitespace.split(org_response.strip('() \t\n\r'))
		info(f'Got list of orgids from ptools: {", ".join(orglist)}')
		return orglist

	def quit_ptools(this):
		try:
			if this.pt_proc.poll() is not None or not path.exists(this.pt_socket):
				info('Pathway Tools has already quit, nothing to do')
				return
		except AttributeError:
			info('Pathway Tools failed to start, so no need to quit it')
			return
		info(f'Telling Pathway Tools process {this.pt_proc.pid} to quit')
		send_ptools_cmd('(exit)', this.pt_socket, False)
		start_time = time.monotonic()
		while time.monotonic() - start_time < this.timeout:
			if this.pt_proc.poll() is not None:
				break
			time.sleep(.1)
		else:
			warn(f'Pathway Tools has not responded to the (exit) command for {this.timeout} seconds, sending terminate signal')
			this.pt_proc.terminate()
			start_time = time.monotonic()
			while time.monotonic() - start_time < this.timeout:
				if this.pt_proc.poll() is not None:
					break
				time.sleep(.1)
			else:
				warn(f'Pathway Tools has not responded to the terminate signal for {this.timeout} seconds, sending kill signal')
				this.pt_proc.kill()
				start_time = time.monotonic()
				while time.monotonic() - start_time < this.timeout:
					if this.pt_proc.poll() is not None:
						break
					time.sleep(.1)
				else:
					error(f'Pathway Tools has not responded to the kill signal for {this.timeout} seconds. You will have to terminate the process manually. Process ID is {this.pt_proc.pid}')
		info('Pathway Tools exited')
		if this.xpra_path:
			info('Stopping xpra')
			subprocess.run([this.xpra_path, 'stop', this.display])
		if this.xvfb_proc is not None:
			info('Stopping Xvfb')
			this.xvfb_proc.terminate()
	def __del__(this):
		info(f'Pathway Tools connection object for {this.pt_proc.pid} is being deallocated, making sure the ptools instance has quit')
		this.quit_ptools()
	def __exit__(this, exc_type, exc_value, traceback):
		this.quit_ptools()

# Derived class of Pathway Tools instance that takes ptools info from a config dict (as returned by read_pipeline_config()), and also loads the PMN lisp functions unless requested not to do so by setting load_pmn_funs = False
class PMNPathwayTools (PathwayTools):
	def __init__(self, config, args = [], socket = None, env = None, load_pmn_funs = True, x11 = None, no_load_pgdbs = False):
		exe = config['ptools-exe']
		pmn_lisp = config['pmn-lisp-funs']
		try:
			timeout = int(config.setdefault('ptools-start-timeout', 60))
		except ValueError:
			pmn.warn(f'Value of ptools-start-timeout should be a interger; got "{config["ptools-start-timeout"]}" instead. Going with the default of 60')
			timeout = 60
		if socket is None:
			socket = path.join(config['proj-sock-dir'], get_run_id() + '.sock')
		super().__init__(exe = exe, socket = socket, args = args, env = env, timeout = timeout, x11 = x11)
		self.send_cmd(f'(load "{pmn_lisp}")')
	def require_pgdbs(self, orglist):
		if not isinstance(orglist, list):
			orglist = [orglist]
		orgids_avail = self.send_cmd('(all-orgids)')
		orgids_missing = []
		for org in orglist:
			if org.upper() not in orgids_avail:
				orgids_missing += org
		if orgids_missing:
			error(f'Required database{"s" if len(orgids_missing) > 1 else ""} not found: {", ".join([org+"Cyc" for org in orgids_missing])}. PMN databases can be downloaded from https://plantcyc.org after requesting a free license')
			raise ChildProcessError()
