# pmn.py
# 
# Charles Hawkins, 2022
#
# This file contains various python utility functions used
# by the other PMN pipeline python scripts
#

from sys import stdin, stdout, stderr

if __name__ == "__main__":
    stderr.write("The pmn.py file contains utility functions used by the other PMN python scripts; it doesn't need to be run directly\n")
    exit(0)

# Convenience function that allows filenames and already-open files to be used interchangeably. 'where' should be either an already-open file (returned as-is), a filename (opened with the specified mode, and the file handle returned), or either None or '-' (stdin or stdout is returned, as appropriate given mode)
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
            stderr.write("%s: %s\n"(file, e.strerror))
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

# If d is a dictlist (dict with lists as values), adds val to the list pointed to by key in d. If val already exists in that list, nothing happens. If key isn't in d, it is added pointing to a one-element list containing val. Modifies d.
def add_to_dictlist(d, key, val):
    try:
        cv = d[key]
        if val not in cv:
            cv.append(val)
    except KeyError:
        d[key] = [val]

# Inverts a dictionary, returning a new one with d's values as keys and vice-versa. If multiple of d's keys point to the same value only one of them is retained; which one is undefined
def invert_dict(d):
    di = {}
    for key, val in d.items():
        di[val] = key
    return di

# Inverts a dictlist (dictionary with lists as values) so that all elements of d's values become keys pointing to a list of all the keys whose values in d contained that key. So for example invert_dictlist({'rose':['red','white'], 'buttercup':['yellow'], 'daffodil':['yellow','white']}) returns {'red':['rose'], 'white':['rose','daffodil'], 'yellow':['buttercup', 'daffodil']}
def invert_dictlist(d):
    di = {}
    for key, vals in d.items():
        for val in vals:
            add_to_dictlist(di, val, key)
    return di

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

