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


if __name__ == "__main__":
	stderr.write("The pmn.py file contains utility functions used by the other PMN python scripts; it doesn't do anything when run directly. The pipeline is mostly run via the pmn-pipeline script\n")
	exit(0)

esc = '\x1b'
csi = esc + '['
def sgr(n):
	return csi + str(n) + 'm'
def with_sgr(string, n):
	return sgr(n)+string+sgr(0)
def red_text(string):
	return with_sgr(string, 31)
def green_text(string):
	return with_sgr(string, 32)
def blue_text(string):
	return with_sgr(string, 34)

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

# When given a dictionary that maps from strings to strings, converts the selected keys' values to bools using fuzzy_bool(). If keys is None, it converts all the keys in the dict. Set if_not_found to control what happens when a requested key isn't found; it should be "ignore" (leave key unset), "true" (put in key with value True), "false" (put in key with value false), or "error" (raise a KeyError). Set if_empty to control what happens for an empty string; it is passed to fuzzy_bool as accept_empty_as
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
			stderr.write(f'{where}: {e.strerror}\n')
			exit(1)
	else:
		return where

# Takes the values in the given dictionary and makes a set from them. Used in revise_pf.py to determine which gene and protein IDs are already taken when assigning new ones
def set_from_values(d):
	s = set()
	for _, val in d.items():
		s.add(val)
	return s

def ask_yesno(prompt, y_flag = False, unknown_is_no = False):
	if y_flag:
		return True
	while True:
		try:
			response = input(prompt)
			return fuzzy_bool(response, None)
		except ValueError:
			if unknown_is_no:
				return False
			else:
				stderr.write('Please answer Yes, No, y, n, T, false, 1, 0, etc\n')
		except KeyboardInterrupt:
			return False

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
			stderr.write("%s: %s\n"%(file, e.strerror))
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
					stderr.write('Warning: Line %s of %s has only %s fields, expected at least 9\n'%(line_n, gff_file, len(fields)))
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
				stderr.write(f'Warning: Feature {key} refers to feature {parent} as its parent, but no such feature exists in the file\n')
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
				stderr.write(f'Warning: Feature {key} refers to feature {parent} as its parent, but no such feature exists in the file\n')
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
				stderr.write(f'Warning: In file {file.name}, variable {var} is defined more than once\n')
			d[key] = val
		except ValueError:
			stderr.write(f'Warning: line not recognized in {file.name}:\n{line}\n')
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
#  UID   Name	Comment
#  U01   Alice   I'm a user!
#  U02   Bob	 I'm also a user!
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
			except IndexError:
				stderr.write(f'{tablefile.name}: Line {line_n} has more fields than the header\n')
				exit(1)
		table[entry[key_col]] = entry
	return table

pf_re = re.compile(r'(.pf)?$')
orxn_pf = re.compile(r'\.orxn\.pf$')
# Reads in one or more PGDB tables (if argument is a list, the tables will be concatinated together), performs appropriate interpolation, and returns a dictionary mapping from orgids ("Database ID" column) to (dictonaries mapping from column name to the value in that column for that orgid). Database IDs starting with a / (e.g. "/phytozome") are presets. Any entry with one or more presets in the "Presets" column (give multiple as,  e.g., "/phytozome/pmn2022") will have those presets' values placed in any column that is blank for that orgid. Presets earlier in the list take precedence. The special /default preset is applied to all orgids with the lowest precedence. Presets can be referenced across files but must be defined before they are used.
def read_pgdb_table(tables, config = None):
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
	try:
		for table in tables:
			tablefile = openfile(table)
			header = [f.strip('"') for f in tablefile.readline().rstrip().split('\t')]
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
						stderr.write(f'{tablefile.name}: Line {line_n} has more fields than the header\n')
						exit(1)
				#print(f'Entry: {entry}')
				# Look for any presets in the Presets column and apply
				try:
					presets = entry['Presets']
					if not presets.startswith('/'):
						stderr.write(f'{tablefile.name}, line {line_n}: Preset names should start with a "/"\n')
						exit(1)
					presets = presets.split('/')[1:]
					if 'default' in preset_dict:
						presets += ['default']
				except KeyError:
					presets = ['default'] if 'default' in preset_dict else []
				#print(f'Presets for {entry["Database ID"]}: {presets}')
				for preset in presets:
					try:
						preset_entry = preset_dict[preset]
					except KeyError:
						stderr.write(f'{tablefile.name}, line {line_n}: Reference to non-existent preset "{preset}"\n')
						exit(1)
					for key, val in preset_entry.items():
						if key not in entry or not entry[key]:
							entry[key] = preset_entry[key]

				# Operations to only be performed on actual PGDBs, not on preset definitions
				if not entry['Database ID'].startswith('/'):
					# Fill in computed fields
					entry.setdefault('Database Name', entry['Database ID']+'Cyc')
					species_words = entry['Species Name'].split(' ')
					if len(species_words) < 2:
						stderr.write(f'{tablefile.name}, line {line_n}: Species name "{entry["Species Name"]}" should be at least two words long\n')
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
						entry['_Authors'] = [as_lisp_symbol(a) for a in entry['Authors'].split(' ')]
					except KeyError:
						pass

					# Deal with boolean columns, converting "fuzzy" bools like "Yes", "T", or "0" to Python bools, and defaulting empty ones to False
					fuzzy_bool_dict(entry, ['Also MetaCyc'])

					# Fields below are required for all pgdb entries; reference the required fields so they generate an error if absent
					entry['Species Name']
					entry['NCBI Taxon ID']
					entry['Sequence File']
					entry['Unique ID']
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
				orgid = entry['Database ID'] 
				if orgid.startswith('/'):
					#print(f'Preset {orgid}')
					preset_dict[orgid[1:]] = entry
				else:
					#print(f'Non-preset {orgid}')
					pgdb_dict[orgid] = entry
				line_n += 1

	except KeyError as e:
		stderr.write(f'{tablefile.name}: Column {e.args[0]} is required for all entries but is missing from line {line_n}\n')
		exit(1)
	return pgdb_dict

def write_pgdb_tablefile(orgtable, orglist, path, fields = None):
	if fields is None:
		# We need to get a set of all the fields that are present and non-empty for at least one of the requested orgids
		fields = set()
		for orgid in orglist:
			entry = orgtable[orgid]
			entry_fields = entry.keys()
			entry_fields = [field for field in entry_fields if not field.startswith('_') and entry[field]]
			fields.update(entry_fields)
		fields = list(fields) # We need the order of the set to be stable. It is currently in CPython as long as the set is unchanged, but it is not guaranteed that this will always remain the case in the future and for all implementations of Python
	with openfile(path, 'w') as outfile:
		outfile.write('\t'.join(fields)+'\n')
		for orgid in orglist:
			outfile.write('\t'.join([entry.setdefault(field, '') for field in fields])+'\n')

# Adds some standard arguments used in all pipeline scripts
def add_standard_pmn_args(par, action = 'operated on'):
	par.add_argument('-o', '--orgids', help = f'A comma-separated list of orgids to be {action}. If not given, all organisms in the pgdb-table will be {action}', dest = 'o')
	par.add_argument('-c', '--config', default='pgdb-pipeline.txt', help = 'Config file with general (non-organism-specific) pipeline parameters.', dest = 'c')
	par.add_argument('-t', '--pgdb-table', help = 'Table file of PGDBs. If not given the filename will be read from the config file', dest = 't')
	par.add_argument('-v', '--verbose', action = 'store_true', help = 'Print info messages while running', dest = 'v')
	par.add_argument('-p', '--proj', default = '.', help = 'Project directory. Defaults to the current working directory', dest = 'proj')
	par.add_argument('-y', '--yes', action = 'store_true', help = 'Assume yes to all prompts', dest = 'y')

verbose = False
def info(msg, logfile = None):
	if verbose:
		imsg = f'Info: {msg}'
		print(imsg)
		if logfile:
			print(imsg, file = logfile)
# Read the pmn config files in the standard way and get the list of organsims to operate on. <args> should be a results of parsing args with a parser that has had add_standard_pmn_args() called on it. Returns a tuple of the config, the table, and the org list
def read_pipeline_files(args):
	config = read_pipeline_config(args.c)

	verbose = args.v
	try:
		if args.t:
			config['proj-pgdb-table'] = args.t
			tablefile = args.t
		else:
			tablefile = config['proj-pgdb-table']
			info(f'Got name of table file from {args.c}: {tablefile}')
		ptable = read_pgdb_table(tablefile, config)
		if args.o:
			org_list = args.o.split(',')
			for org in org_list:
				if org not in ptable:
					stderr.write(f'Organism {org} not found in table\n')
					exit(1)
		else:
			org_list = list(ptable.keys())
		info(f'Got list of orgids from {"command-line args" if args.o else tablefile}, will run the following orgids: {", ".join(org_list)}')
	except KeyError as e:
		stderr.write(f'{args.c}: Required variable {e.args[0]} not found')
		exit(1)
	return (config, ptable, org_list)

# This function is for the "unique ID" field in the input table file; e.g. QT for aracyc, which results in most kinds of frame created in aracyc having QT in their frame ID, e.g. ENZRXNQT-12345, which is done to ensure that they don't have name collisions with frames created in metacyc. PathoLogic, however, expects this unique ID to be encoded in Base36 in the organism parameters file it takes as input, so this function is here to perform that encoding. This Python function is a manual translation of the equivalent Perl function written by Chuan
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
		stderr.write(f'Error: The pipeline requires Python 3.8 or later; currently running on {version_info.major}.{version_info.minor}.{version_info.micro}, located at {executable}\n')
		passed = False
	for exe in ['java', 'blastp']:
		if not shutil.which(exe):
			stderr.write(f'Error: Required executable \'{exe}\' is not in the path\n')
			passed = False
	return passed

# Checks that all given files exist and are of the correct type. The progam will crash with an error if any don't exist or are the wrong kind of file
def check_exists(files = [], dirs = [], sockets = [], progname = 'this script'):
	passed = True
	for f in files:
		if not os.path.exists(f):
			stderr.write(f'Error: File {f}, required by {progname}, was not found\n')
			passed = False
		elif not os.path.isfile(f):
			stderr.write(f'Error: File {f}, required by {progname}, exists but is not a standard file or a symlink to a standard file\n')
			passed = False
	for d in dirs:
		if not os.path.exists(d):
			stderr.write(f'Error: Directory {d}, required by {progname}, was not found\n')
			passed = False
		elif not os.path.isdir(d):
			stderr.write(f'Error: Directory {d}, required by {progname}, exists but is not a directory\n')
			passed = False
	for s in sockets:
		if not os.path.exists(s):
			stderr.write(f'Error: Socket {s}, required by {progname}, was not found\n')
			passed = False
		elif not stat.S_ISSOCK(os.stat(s).st_mode):
			stderr.write(f'Error: DSocket {s}, required by {progname}, exists but is not a socket\n')
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
		stderr.write('Internal error: attempt to check for file access type {access}, an unrecognized access type. Please use check_access with a single value in the set os.F_OK, os.R_OK, os.W_OK, or os.X_OK\n')
		exit(2)

	for filename in file_list:
		if ignore_missing and not path.exists(filename):
			continue
		if not os.access(filename, access):
			stderr.write(f'{access_err} {filename}, {reason}.\n')
			passed = False
	return passed

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
			stderr.write(f'Error: Require either {e2p2v4_exe} (for E2P2v4) or {e2p2v5_exe} (for E2P2v5), but neither exists')
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
				stderr.write(f'Config option x-server in {configfilename} is set to "xpra", but xpra was not found in $PATH\n')
				passed = False
		elif xserver == 'xvfb':
			if shutil.which('Xvfb') is None:
				stderr.write(f'Config option x-server in {configfilename} is set to "xvfb", but Xvfb was not found in $PATH\n')
				passed = False
		elif xserver is not None and xserver != 'external':
			srderr.write(f'Config option x-server in {configfilename} is set to "{xserver}, which is not recognized (valid values are external, xpra, or xvfb)\n')
			passed = False

	except KeyError as e:
		stderr.write(f'Config file {configfilename} is missing required key {e.args[0]}\n')
		passed = False
	return passed

re_taxid = re.compile(r'^[0-9]+$')
def check_pgdb_table(table, config):
	passed = True
	cyc_set = set()
	uid_set = set()
	for org, entry in table.items():
		table_file = entry['_filename']
		info(f'Checking table entry for {org}')
		tax = entry['NCBI Taxon ID']
		if not re_taxid.match(tax):
			stderr.write(f'Error: NCBI taxon IDs should be only digits; instead found {tax} for orgid {org} in {table_file}\n')
			passed = False
		cyc = entry['Database ID']
		uid = entry['Unique ID']
		if cyc in cyc_set:
			stderr.write(f'Error: Duplicate Database ID: {cyc} in {table_file}\n')
			passed = False
		if uid in uid_set:
			stderr.write(f'Error: Duplicate Unique ID: {uid} in {table_file}\n')
			passed = False
		cyc_set.add(cyc)
		uid_set.add(uid)

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
		pgdb_folder = config['ptools-pgdbs']
		cyc_folder = path.join(pgdb_folder, cyc.lower()+'cyc')
		if path.exists(cyc_folder):
			stderr.write(f'Error: PGDB {cyc}Cyc from {table_file} already exists at {cyc_folder}\n')
			passed = False
	return passed

def check_parallelism(config):
	passed = True
	par = config.setdefault('parallelism', 'none')
	if par not in ['none', 'slurm', 'parallel']:
		stderr.write(f'Unrecognized parallelism provider \'{par}\'. Valid values are \'parallel\', \'slurm\', and \'none\'\n')
		exit(1)
	if par == 'slurm':
		if not shutil.which('sbatch'):
			stderr.write(f'Slurm selected for parallelism, but cannot find the sbatch command. Is Slurm installed?\n')
			passed = False
	if par == 'parallel':
		if not shutil.which('parallel'):
			stderr.write(f'GNU Parallel selected for parallelism, but cannot find the parallel command. Is GNU Parallel installed?\n')
			passed = False
	return passed

re_disp = re.compile(b'Actual display used: (:[0-9]+)')
def start_xpra():
	xpra_path = shutil.which('xpra')
	if xpra_path is not None:
		info('xpra found, using xpra')
		res = subprocess.run([xpra_path, 'start'], capture_output = True)
		info(res.stderr)
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
	r = s.recv(1024)
	s.close()
	return r.decode().rstrip()

# Send a lisp command via the API. Pathway Tools should have been started with the -api flag (NB: not the -python flag). Commands are automatically wrapped in an error handler unless handle_errs is set to False.
def send_ptools_cmd(cmd, socket_path = '/tmp/ptools-socket', handle_errs = True):
	info(f'Sending Ptools Command: {cmd}')
	if not os.path.exists(socket_path):
		stderr.write(f'Pathway tools socket {socket_path} does not exits. Did you remeember to start Pathway Tools with -api?\n')
		raise FileNotFoundError(socket_path)
	if not stat.S_ISSOCK(os.stat(socket_path).st_mode):
		stderr.write(f'Pathawy tools socket {socket_path} exists but is not a socket\n')
		raise IOError(socket_path)
	if handle_errs:
		socket_msg('(setq *status* nil)', socket_path)
		cmd_he = f'(handler-case {cmd} (error (e) (setq *status* `(,(type-of e) ,(format nil "~A" e)))))'
		#info(f'{cmd_he}\n')
		r = socket_msg(cmd_he, socket_path)
		e = socket_msg('*status*\n', socket_path)
		if e != 'NIL':
			stderr.write(f'{e}\n')
			raise ChildProcessError()
		else:
			return r
	else:
		return socket_msg(cmd, socket_path)
# Switch organisms in the ptools instance. This function should be used instead of the lisp function "so" because (so 'org) uses (break) when the orgid was not found rather than raise a catchable condition, freezing up the api and preventing recovery
def ptools_so(orgid, socket_path = '/tmp/ptools-socket'):
	found_org = send_ptools_cmd(f"(setq org (find-org '{orgid}))", socket_path = socket_path)
	if found_org == "NIL":
		stderr.write(f"Organism {orgid} was not found by the Pathway Tools instance connected to {socket_path}\n")
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
	def __init__(this, exe, socket = '/tmp/ptools-socket', args = [], env = None, timeout = 30, x11 = None):
		this.pt_exe = exe
		this.pt_socket = socket
		if '-lisp' not in args:
			args += ['-lisp']
		args += ['-no-patch-download']
		# Under Singularity, environment variables aren't making it to pathway tools for some reason, so as a workaround we will set the socket path and tell it to start the api daemon manually using -eval. Also, there is an apparent bug in ptools 27.0 where spaces are not accepted in an -eval statement (it treats them as if they were EOF markers and stops reading), so as another workaround we use newlines instead
		args += ['-eval', f'(progn (setf\n*socket-pathname*\n"{socket}")(start-external-access-daemon))']
		this.pt_args = args
		cmd = [exe] + args
		this.pt_cmdline = ' '.join(cmd)
		if os.path.exists(socket):
			stderr.write(f'Error: There is already a Pathway Tools instance using {socket} as a socket. Please quit it before trying to use this function\n')
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
		else:
			this.xvfb_proc, this.xpra_path, this.display = None,None,None
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
		this.pt_proc = subprocess.Popen(cmd, stdin = subprocess.DEVNULL, env = pt_env)
		this.timeout = timeout
		start_time = time.monotonic()
		while not os.path.exists(socket):
			if time.monotonic()-start_time > timeout:
				this.pt_proc.kill()
				stderr.write(f'Error: Timed out waiting for pathway tools to create an API socket at {socket} ({timeout} secs)\n')
				raise TimeoutError()
			if this.pt_proc.poll() is not None:
				stderr.write(f'Error: Pathway Tools failed to start\n')
				raise ChildProcessError()
			time.sleep(0.1)
	def __enter__(this):
		return this
	def send_cmd(this, cmd, handle_errs = True):
		if this.pt_proc.poll() is not None:
			stderr.write(f'Error: This instance of Pathway Tools has exited. Please create another instance\n')
			raise ChildProcessError()
		return send_ptools_cmd(cmd, this.pt_socket, handle_errs)
	def so(this, org):
		if this.pt_proc.poll() is not None:
			stderr.write(f'Error: This instance of Pathway Tools has exited. Please create another instance\n')
			raise ChildProcessError()
		return ptools_so(org, this.pt_socket)
	def get_org_list(this):
		org_response = this.send_cmd('(all-orgids)')
		orglist = whitespace.split(org_response.strip('() \t\n\r'))
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
			stderr.write(f'Warning: Pathway Tools has not responded to the (exit) command for {this.timeout} seconds, sending terminate signal\n')
			this.pt_proc.terminate()
			start_time = time.monotonic()
			while time.monotonic() - start_time < this.timeout:
				if this.pt_proc.poll() is not None:
					break
				time.sleep(.1)
			else:
				stderr.write(f'Warning: Pathway Tools has not responded to the terminate signal for {this.timeout} seconds, sending kill signal\n')
				this.pt_proc.kill()
				start_time = time.monotonic()
				while time.monotonic() - start_time < this.timeout:
					if this.pt_proc.poll() is not None:
						break
					time.sleep(.1)
				else:
					stderr.write(f'Error: Pathway Tools has not responded to the kill signal for {this.timeout} seconds. You will have to terminate the process manually. Process ID is {this.pt_proc.pid}\n')
		info('Pathway Tools exited')
		if this.xpra_path:
			info('Stopping xpra')
			subprocess.run([this.xpra_path, 'stop', this.display])
		if this.xvfb_proc is not None:
			this.xvfb_proc.terminate()
	def __del__(this):
		info(f'Pathway Tools connection object for {this.pt_proc.pid} is being deallocated, making sure the ptools instance has quit')
		this.quit_ptools()
	def __exit__(this, exc_type, exc_value, traceback):
		this.quit_ptools()

# Derived class of Pathway Tools instance that takes ptools info from a config dict (as returned by read_pipeline_config()), and also loads the PMN lisp functions unless requested not to do so by setting load_pmn_funs = False
class PMNPathwayTools (PathwayTools):
	def __init__(self, config, args = [], socket = None, timeout = 30, env = None, load_pmn_funs = True, x11 = None):
		exe = config['ptools-exe']
		pmn_lisp = config['pmn-lisp-funs']
		if socket is None:
			socket = config['ptools-socket']
		super().__init__(exe = exe, socket = socket, args = args, env = env, timeout = timeout, x11 = x11)
		self.send_cmd(f'(load "{pmn_lisp}")')
	def require_pgdbs(self, orglist):
		if not isinstance(orglist, list):
			orglist = [orglist]
		orgids_avail = self.send_cmd('(all-orgids)')
		orgids_missing = []
		for orgid in orglist:
			if orgid.upper() not in orgids_avail:
				orgids_missing += orgid
		if orgids_missing:
			stderr.write(f'Error: Required database{"s" if len(orgids_missing) > 1 else ""} not found: {", ".join([org+"Cyc" for org in orgids_missing])}. PMN databases can be downloaded from https://plantcyc.org after requesting a free license')
			raise ChildProcessError()
