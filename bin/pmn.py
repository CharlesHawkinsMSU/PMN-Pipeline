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

from sys import stdin, stdout, stderr
import re


if __name__ == "__main__":
    stderr.write("The pmn.py file contains utility functions used by the other PMN python scripts; it doesn't do anything when run directly. The pipeline is mostly run via the pmn-pipeline script\n")
    exit(0)

# Convenience function that allows filenames and already-open files to be used interchangeably. 'where' should be either an already-open file (returned as-is), a filename (opened with the specified mode, and the file handle returned), or either None or '-' (stdin or stdout is returned, as appropriate given the value of mode)
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
def invert_dictlist(d):
    di = {}
    for key, vals in d.items():
        for val in vals:
            add_to_dictlist(di, val, key)
    return di

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

# Reads in a file with lines of the form var = val
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

pf_re = re.compile('(pf)?$')
# Reads in one or more PGDB tables (if argument is a list, the tables will be concatinated together), performs appropriate interpolation, and returns a dictionary mapping from orgids ("Database ID" column) to (dictonaries mapping from column name to the value in that column for that orgid). Database IDs starting with a / (e.g. "/phytozome") are presets. Any entry with one or more presets in the "Presets" column (give multiple as,  e.g., "/phytozome/pmn2022") will have those presets' values placed in any column that is blank for that orgid. Presets earlier in the list take precedence. The special /default preset is applied to all orgids with the lowest precedence. Presets can be referenced across files but must be defined before they are used.
def read_pgdb_table(tables):
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

				# Go through each column and put it into the dict for this orgid (which is the entry var)
				for i in range(len(line)):
					try:
						entry[header[i]] = line[i]
					except IndexError:
						stderr.write(f'{tablefile.name}: Line {line_n} has more fields than the header\n')
						exit(1)
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
				for preset in presets:
					try:
						preset_entry = preset_dict[preset]
					except KeyError:
						stderr.write(f'{tablefile.name}, line {line_n}: Reference to non-existent preset "{preset}"\n')
					for key, val in preset_entry.items():
						entry.setdefault(key, val)

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
					entry.setdefault('Initial PF File', entry['Sequence File']+'.e2p2v4.orxn.pf')
					entry.setdefault('PF File', pf_re.sub('revised.pf', entry['Initial PF File'], 1))
					year = entry['Citation Year']
					entry.setdefault('SAVI Citation', f'|PUB-PMNUPP{year}| |PUB-PMNAIPP{year}| |PUB-PMNRXN{year}| |PUB-PMNRXNTAXON{year}| |PUB-PMNTAXON{year}| |PUB-PMNIC{year}| |PUB-PMNSP{year}|')
					entry.setdefault('E2P2 Citation', f'|PUB-E2P2PMN{year}|')
					entry.setdefault('Enzrxn Citation', f'E2P2PMN{year}:EV-COMP-AINF')
					entry.setdefault('ENZ name file', f'ec_name.map.parsed')
					entry.setdefault('RXN Map', f'metacyc-rxn-name-mapping')
					entry.setdefault('PWY Metacyc', f'all_pwy.meta')
					entry.setdefault('PWY Plantcyc', f'all_pwy.plant')

					# Fields below are required for all pgdb entries; if this is a pgdb entry and not a preset definition, we reference the required fields so they generate an error if absent
					entry['Species Name']
					entry['NCBI Taxon ID']
					entry['Sequence File']
					entry['Unique ID']
					entry['Version']
					entry['Seq Source']
					entry['Authors']
					entry['Curator']
					entry['Citation Year']

				# Finished building this entry, put it into the appropriate dictionary (preset_dict or pgdb_dict depending on whether it's a preset definition or an actual pgdb)
				orgid = entry['Database ID'] 
				if orgid.startswith('/'):
					preset_dict[orgid[1:]] = entry
				else:
					pgdb_dict[orgid] = entry
				line_n += 1

	except KeyError as e:
		stderr.write(f'{tablefile.name}: Column {e.args[0]} is required for all entries but is missing form line {line_n}\n')
		exit(1)
	return pgdb_dict
