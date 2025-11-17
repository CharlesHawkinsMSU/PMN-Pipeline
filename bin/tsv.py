# Python library

import _io
import csv

csv.register_dialect('tsv', delimiter = '\t', doublequote = False, escapechar = '\\', lineterminator = '\n', quotechar = '"', skipinitialspace = True)

def read_tsv(file, crash = True):
    'Reads a tsv file (file can be a path or already-open file). Returns a 2-tuple consisting of a list of the column names (from the header) and a list of rows. Each row is a dict mapping from column names to the value in that column for the row. If crash is True, any errors will exit out of the program with an appropriate error message; otherwise exceptions will be raised for the caller to catch'
    wasopen = isinstance(file, _io.TextIOWrapper)
    try:
        if not wasopen:
            file = open(file)
        reader = csv.DictReader(file, dialect = 'tsv', restval = '')
        entries = list(reader)
        return reader.fieldnames, entries
    except IOError as e:
        if crash:
            stderr.write(f'{file.name if wasopen else file}: {e.strerror}\n')
            exit(1)
        else:
            raise

def read_tsv_keyed(file, key, crash = True):
    'Reads a tsv file (file can be a path or already-open file). Returns a 2-tuple consisting of a list of the column names (from the header) and a dict of rows. The dict of rows maps from values in the key column to rows (key should match the name of a column in the file). Each row is a dict mapping from column names to the value in that column for the row. If multiple rows share the same key, only the last will appear in the dictionary. If crash is True, any errors will exit out of the program with an appropriate error message; otherwise exceptions will be raised for the caller to catch'
    wasopen = isinstance(file, _io.TextIOWrapper)
    try:
        if not wasopen:
            file = open(file)
        reader = csv.DictReader(file, dialect = 'tsv', restval = '')
        entries = {entry[key]: entry for entry in reader}
        return reader.fieldnames, entries
    except IOError as e:
        if crash:
            stderr.write(f'{file.name if wasopen else file}: {e.strerror}\n')
            exit(1)
        else:
            raise
    except KeyError as e:
        if crash:
            stderr.write(f'Error: {file.name if wasopen else file} has no column named "{key}"\n')
            exit(1)
        else:
            raise

def mk_tsv_line(header, entry):
    'Give it a header (list of column names) and a row (dict mapping from col names to values, and it will return a TSV line ready for output. Col names present in the header but absent from the row will not generate an error, but will instead be output as an empty cell'
    fds = []
    for col in header:
        fds.append(entry.setdefault(col, ''))
    return '\t'.join(fds)+'\n'

def read_set_from_file(file, colname):
    'Reads the specified file and returns a set. If colname is None, it just returns a set of the lines (assumes no header). If colname is not None, it reads the file as TSV and grabs the named column as a set'
    try:
        wasopen = isinstance(file, _io.TextIOWrapper)
        if not wasopen:
            file = open(file)
        if colname:
            header, entries = read_tsv(file)
            if colname not in header:
                stderr.write(f'Error: The specified column "{colname}" does not appear in the list file {file.name if wasopen else file}\n')
                exit(1)
            return {entry[colname] for entry in entries}
        else:
            return {line.rstrip() for line in file}
    except IOError as e:
        stderr.write(f'{file.name if wasopen else file}: {e.strerror}\n')
        exit(1)

def dict_from_2col(file):
    'Reads a two-column TSV file (no header) and returns a dict mapping from the first colum to the second'
    wasopen = isinstance(file, _io.TextIOWrapper)
    if not wasopen:
        file = open(file)
    d = {}
    for line in file:
        try:
            key, val = line.rstrip().split('\t')
            d[key] = val
        except ValueError:
            stderr.write(f'Error: Line in {file.name} has less than two columns:\n{line}')
            exit(1)
    return d

if __name__ == '__main__':
    print('This is a library for reading TSV (tab-separated value) files. It does nothing when run directly')
