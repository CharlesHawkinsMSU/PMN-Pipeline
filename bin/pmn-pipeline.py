#!/usr/bin/env python3

from sys import stdin, stdout, stderr
from os import path, listdir, mkdir
from shutil import copytree, copy2
import argparse as ap
import subprocess as sp
import pmn


par = ap.ArgumentParser(description = 'Used to run each stage of the PMN release pipeline')
pmn.add_standard_pmn_args(par, action='run')
par.add_argument('stage', help = 'Which stage of the pipeline to run. Valid stages are: newproject, setup, create, dump, dump-biopax, savi, refine-a, refine-b, refine-c, newversion, overview, checker, blastset')
par.add_argument('-p', '--proj', default = '.', help = 'Project directory. Defaults to the current working directory', dest = 'proj')
#par.add_argument('-c', '--config-file', default = 'pgdb-pipeline.txt', help = 'The input config file to use, which defines the various folders and locations used by the pipeline. The default is pgdb-pipeline.txt', dest = 'c')

args = par.parse_args()

def tryopen(filename, mode):
    try:
        stream = open(filename, mode)
    except IOError as e:
        stderr.write('%s: %s\n'%(e.filename, e.strerror))
        exit(1)
    return stream

script_path = path.dirname(path.realpath(__file__))

stage = args.stage.lower()
if args.stage == 'newproj':
    proj_tmpl_path = path.realpath(path.join(script_path, '..', 'project-template'))
    if not path.exists(args.proj):
        mkdir(args.proj)
    for filename in listdir(proj_tmpl_path):
        src = path.join(proj_tmpl_path, filename)    
        dst = path.join(args.proj, filename)
        if path.isdir(src):
            copytree(src, dst)
        else:
            copy2(src, dst)
    stderr.write('New project: %s\n'%args.proj)
    exit(0)
line_n = 1
#config = {}
#with tryopen(args.c, 'r') as cfile:
#    for line in cfile:
#        line = line.strip()
#        if not line or line[0] == '#':
#            continue
#        try:
#            (key, val) = line.split('=', 1)
#        except ValueError:
#            stderr.write('Warning: Could not parse line %s of %s, ignoring\n'%(line_n, args.c))
#        (key, val) = (key.strip(), val.strip())
#        config[key] = val
#        line_n += 1
config = pmn.read_var_val_file(args.c)

# Read in the org list
try:
    orgfilename = config['proj-pgdb-table']
except KeyError:
    stderr.write('proj-pgdb-table, required for stage %s, not found in %s\n'%(stage, args.c))
    exit(1)

# Enumerates project directories, with a tuple that includes the key in the config file specifying the directory and the column in the table file that contains a file that should be located there
proj_dirs = [
        ('fasta_dir', 'proj-fasta-dir', 'Sequence File'),
        ('gff_dir', 'proj-gff-dir', 'GFF File'),
        ('maps_dir', 'proj-maps-dir', 'Map In'),
        ('maps_output_dir', 'proj-maps-output-dir', 'Map Out'),
        ('e2p2_dir', 'proj-e2p2-dir', 'Initial PF File'),
        ('e2p2_dir', 'proj-e2p2-dir', 'PF File')]
orgtable = {}
with tryopen(orgfilename, 'r') as orgfile:
    next(orgfile) # skip header
    line_n = 2
    for line in orgfile:
        line = line.strip()
        if not line or line[0] == '#':
            continue
        fields = line.split('\t')
        if len(fields) < 13:
            stderr.write('Line %s of %s has %s fields, expected at least 13\n'%(line_n, orgfilename, len(fields)))
            exit(1)
        org = {}
        orgid = fields[0]
        org['orgid'] = orgid
        org['cyc'] = fields[1] if fields[1] else fields[0] + 'Cyc'
        org['fullname'] = fields[2]
        org['idname'] = fields[3] if fields[3] else fields[2][0] + fields[2].split(' ')[1]
        org['abbrevname'] = fields[4] if fields[4] else fields[2][0] + '. ' + fields[2].split(' ', 1)[1]
        org['taxid'] = fields[5]
        org['seqfile'] = fields[6]
        org['pffile'] = fields[7] if fields[7] else fields[6]+'.e2p2v4.orxn.revised.pf'
        org['uid'] = fields[8]
        org['version'] = fields[9]
        org['seqsrc'] = fields[12]
        
        orgtable[orgid] = org
        for dir_key, col in proj_dirs:
        line_n += 1
print(orgtable)
ps_path = path.realpath(path.join(script_path,  '..','perl_scripts'))
print(script_path)

# Runs the specified function for each org in orglist, assuming each one has its own folder in basedir
def for_each_org(fn, basedir, orglist):
    for org in orglist:
        fn(path.join(basedir, org))

if stage == 'revise':
    # run revise_pf.py
    # arg_col enumerates columns in the input table used for revise_pf.py and maps them to arguments to be given to revise_pf.py
    arg_col = [
            ('i', 'Initial PF File'),
            ('f', 'GFF File'),
            ('ifa', 'Sequence File'),
            ('im', 'Map In'),
            ('o', 'Map Out'),
            ('r', 'PF File'),
            ('fs', 'FASTA Sep'),
            ('fg', 'FASTA Field'),
            ('fkv', 'FASTA KV'),
            ('pf', 'GFF Prot Feature'),
            ('p', 'GFF Prot Name'),
            ('k', 'GFF Key'),
            ('g', 'GFF Path'),
            ('n', 'Numeric IDs'),
            ('gd', 'Gene Delete')
            ]

    try:
        import revise_pf
    except ImportError:
        stderr.write('revise_pf.py not found\n')
        exit(1)
    # argset is an empty class used to build the argument list for revise_pf.py to use. This is a slight hack due to revise_pf originally being only a standalone script that expects an arguments object such as would be  generated by argparse
    class argset:
        pass
    for org in orgtable:
        args = argset()
        arg_dict = args.__dict__
        for arg, col in arg_col:
            try:
                #args.i = org['Initial PF File']
                arg_dict[arg] = org[col]
            except KeyError as e:
                pass
        print(arg_dict)
        revise_pf.main(args)

# Stage wasn't any of the internally-defined stages, so it must refer to a .master file
masterfile = stage
if not masterfile.endswith('.master'):
    masterfile = masterfile + '.master'
masterscript = path.join(ps_path, 'pmn-release-pipeline-general.pl')
for org in orgtable:
    stderr.write('Running %s on %s\n'%(masterfile, org))
    masterfilepath = path.join(config['proj-masters-dir'], org, masterfile)
    sp.run(['perl', masterscript, masterfilepath, masterfilepath + '.log'])
