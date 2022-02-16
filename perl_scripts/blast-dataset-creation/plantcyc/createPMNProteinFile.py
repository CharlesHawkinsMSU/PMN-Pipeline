#!/usr/bin/python

"""
Name:        compilePMNProteinFile.py
Date:        130722
Author:      Lee Chae

Description: Reads in PlantCyc dat files (proteins, classes, genes, enzrxns) and produces
             a datafile of information on proteins found within PlantCyc. The output
             will contain one row per protein, with each row listing in tabbed format:
             
             (1) enzyme ID
             (2) enzyme common name
             (3) enzyme reaction(s) common name
             (4) species name
             (5) gene ID
             (6) gene common names
             (7) UniProt IDs

Usage:       compilePMNProteinFile.py -p <proteins.dat file> -c <classes.dat file> -g <genes.dat file> -e <enzrxns.dat file> -o <output file>

"""


import sys
import os
import re
import getopt
#import prog

## Collect command line options using get_options in prog.
#flags = 'p:c:g:e:o:'
#args = sys.argv[1:]
#options = prog.get_options(args, flags)

## Store the command line arguments in useful form.
#for a in options[:]:
#    if a[0] == "-p":
#        filename_proteins = a[1]
#    if a[0] == "-c":
#        filename_classes = a[1]
#    if a[0] == "-g":
#        filename_genes = a[1]
#    if a[0] == "-e":
#        filename_enzrxns = a[1]
#    if a[0] == "-o":
#        filename_output = a[1]

###parse argument list directly from sys.argv
for i in range(0, (len(sys.argv)-1)/2):
    if (sys.argv[i*2+1]) == '-p':
        filename_proteins = sys.argv[i*2+2]
    if (sys.argv[i*2+1]) == '-c':
        filename_classes = sys.argv[i*2+2]
    if (sys.argv[i*2+1]) == '-g':
        filename_genes = sys.argv[i*2+2]
    if (sys.argv[i*2+1]) == '-e':
        filename_enzrxns = sys.argv[i*2+2]
    if (sys.argv[i*2+1]) == '-o':
        filename_output = sys.argv[i*2+2]

# Create a species map matching species IDs with their common names.
species = {}
data = ""
input = open(filename_classes, 'r')
for i in input:
    if i.startswith("#"):
        continue
    else:
        data += i
input.close()
species_data = data.split("//")

for s in species_data:
    name, id = "", ""
    temp = s.split("\n")
    for t in temp:
        if t.startswith("UNIQUE-ID"):
            id = t.split(" - ")[-1]
        if t.startswith("COMMON-NAME"):
            name = t.split(" - ")[-1]
    if "TAX-" in id or "ORG-" in id:
        species[id] = name

# Create a gene names map matching gene IDs to their common names.
gene_names = {}
data = ""
input = open(filename_genes, 'r')
for i in input:
    if i.startswith("#"):
        continue
    else:
        data += i
input.close()
genes_data = data.split("\n//\n")

for g in genes_data:
    id = ""
    names = {}
    temp = g.split("\n")
    for t in temp:
        if t.startswith("UNIQUE-ID"):
            id = t.split(" - ")[-1]
        if t.startswith("COMMON-NAME"):
            name = t.split(" - ")[-1]
            names[name] = 1
        if t.startswith("SYNONYMS"):
            name = t.split(" - ")[-1]
            names[name] = 1
    temp_names_entry = ""
    if len(names) > 0:
        for n in names:
            temp_names_entry += n + "; "
        names_entry = temp_names_entry.rstrip("; ")
    else:
        names_entry = "NoCommonGeneName"
    gene_names[id] = names_entry

# Create an enzyme reactions map matching enzyme reaction IDs to their common names.
enzrxns = {}
data = ""
input = open(filename_enzrxns, 'r')
for i in input:
    if i.startswith("#"):
        continue
    else:
        data += i
input.close()
enzrxn_data = data.split("\n//\n")

for e in enzrxn_data:
    name, id = "", ""
    temp = e.split("\n")
    for t in temp:
        if t.startswith("UNIQUE-ID"):
            id = t.split(" - ")[-1]
        if t.startswith("COMMON-NAME"):
            name = t.split(" - ")[-1]
    if name and id:
        enzrxns[id] = name

# Read in list of proteins.
monomers = {}
results = ""
input = open(filename_proteins, 'r')
data = ""
for i in input:
    if i.startswith("#"):
        continue
    else:
        data += i
input.close()
protein_data = data.split("\n//\n")

# Process each protein entry, looking for the target values. Indicate
# those entries lacking any of the target values.
for p in protein_data:
    name, id, enzrxn_id, gene_id, species_id, uniprot_id = "", "", "", "", "", ""
    temp_enzrxns = {}
    uniprot_ids = {}
    
    # Process data.
    temp = p.split("\n")
    for t in temp:
        if t.startswith("UNIQUE-ID"):
            id = t.split(" - ")[-1]
        if t.startswith("COMMON-NAME"):
            name = t.split(" - ")[-1]
        if t.startswith("CATALYZES"):
            tname = t.split(" - ")[-1]
            temp_enzrxns[tname] = 1
        if t.startswith("GENE"):
            gene_id = t.split(" - ")[-1]
        if t.startswith("SPECIES"):
            species_id = t.split(" - ")[-1]
        if t.startswith("DBLINKS"):
            if "UNIPROT" in t:
                uniprot_id = t.split("\"")[1]
                uniprot_ids[uniprot_id] = 1
    
    # Translate species ID to name.
    try: 
        species_name = species[species_id]
    except:
        species_name = "NoSpeciesName"
    
    # Assemble Uniprot_ids into one field for output.
    uniprot_entry = ""
    collect_uniprot_entry = ""
    if len(uniprot_ids) > 0:
        for uid in uniprot_ids:
            collect_uniprot_entry += uid + "; "
        uniprot_entry = collect_uniprot_entry.rstrip("; ")
    else:
        uniprot_entry = "NoUniProtID"
    
    # Assemble reaction info into one field for output.
    if len(temp_enzrxns) > 0:
        collect_enzrxn_names = ""
        for te in temp_enzrxns:
                if te in enzrxns:
                    collect_enzrxn_names += enzrxns[te] + "; "
        if collect_enzrxn_names != "":
            enzrxn_names = collect_enzrxn_names.rstrip("; ")
        else:
            enzrxn_names = "NoReactionInformation"
    else:
        enzrxn_names = "NoReactionInformation"

    # Check gene ID and retrieve gene names.
    if gene_id == "":
        gene_id = "NoGeneID"
        gene_name = "NoCommonGeneName"
    else:
        if gene_id in gene_names:
            gene_name = gene_names[gene_id]
        else:
            gene_name = "NoCommonGeneName"
            
    # Check common names value.
    if name == "":
        name = "NoCommonName"

    # Assemble results file.
    results += "%s\t%s\t%s\t%s\t%s\t%s\t%s\n" % (id, name, enzrxn_names, species_name, gene_id, gene_name, uniprot_entry)

# Print out results file.
output = open(filename_output, 'w')
output.write(results)
output.close()

# Notify user of completion and exit.
print "Operation complete. Results are in file: %s" % (filename_output)
sys.exit()
