#!/usr/bin/env python3

from sys import stdin, stdout, stderr
from os import path, listdir, mkdir
from shutil import copytree, copy2
import argparse as ap
import subprocess as sp


par = ap.ArgumentParser(description = 'Used to run each stage of the PMN release pipeline')
par.add_argument('stage', help = 'Which stage of the pipeline to run. Valid stages are: newproject, setup, create, dump, dump-biopax, savi, refine-a, refine-b, refine-c, newversion, overview, checker, blastset')
par.add_argument('-p', '--proj', default = '.', help = 'Project directory. Defaults to .', dest = 'proj')
par.add_argument('-c', '--config-file', default = 'pgdb-pipeline.txt', help = 'The input config file to use, which defines the various folders and locations used by the pipeline. The default is pgdb-pipeline.txt', dest = 'c')

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
config = {}
with tryopen(args.c, 'r') as cfile:
    for line in cfile:
        line = line.strip()
        if not line or line[0] == '#':
            continue
        try:
            (key, val) = line.split('=', 1)
        except ValueError:
            stderr.write('Warning: Could not parse line %s of %s, ignoring\n'%(line_n, args.c))
        (key, val) = (key.strip(), val.strip())
        config[key] = val
        line_n += 1

# Read in the org list
try:
    orgfilename = config['proj-pgdb-table']
except KeyError:
    stderr.write('proj-pgdb-table, required for stage %s, not found in %s\n'%(stage, args.c))
    exit(1)

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
        line_n += 1
print(orgtable)
prp_path = path.realpath(path.join(script_path, '..','pmn-release-pipeline'))
ps_path = path.realpath(path.join(script_path,  '..','perl_scripts'))
print(script_path)
print(prp_path)

# Runs the specified function for each org in orglist, assuming each one has its own folder in basedir
def for_each_org(fn, basedir, orglist):
    for org in orglist:
        fn(path.join(basedir, org))
# Stage wasn't any of the internally-defined stages, so it must refer to a .master file
masterfile = stage
if not masterfile.endswith('.master'):
    masterfile = masterfile + '.master'
masterscript = path.join(prp_path, 'pmn-release-pipeline-general.pl')
for org in orgtable:
    stderr.write('Running %s on %s\n'%(masterfile, org))
    masterfilepath = path.join(config['proj-masters-dir'], org, masterfile)
    sp.run(['perl', masterscript, masterfilepath, masterfilepath + '.log'])
