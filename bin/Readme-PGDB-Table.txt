PGDB Table Columns
==Required==
Species Name: Full name of the species with subspecies if applicable. Arabidopsis thaliana col
Database ID: Database name, without Cyc. Ara
NCBI Taxon ID: Taxon ID from NCBI taxonomy database
Sequence File: The amino acid fasta file
Unique ID: Up to 4 characters. Shouldn't match any of the others. It will be incorporated into frame IDs created in the database. E.g. QT will result in ENZRXNQT-12345
Version: Version number. Start at 1.0.0 for new databases. First number: full rebuild; second number: corrections to the data but not fully rebuilt; third number: updated to new ptools version, data is not changed
Seq Source: Where the genome came from. E.g. PHYTOZOME
Authors: List of authors for the database. Should match author frames in aracyc. E.g. |hawkins| |xue| |rhee|
Curator: Curator email address
Citation Year: Year to generate the citations for SAVI and E2P2

==Optional (computed if left blank)==
Database Name: Databse ID + Cyc. E.g. AraCyc
Abbrev Name: Shortened species name, still with subspecies but not subspecies marker. E.g. B. oleracea Capitata
ID/Name: Shortest species name, without subspecies. E.g. Athaliana
Initial PF File: Pre-revision PF file to be saved by E2P2
PF File: Post-revision PF file, to be fed to PathoLogic

==Gene Mapping==
FASTA Map: "Yes" to try to get gene mapping info from the fasta file, anything else to skip
FASTA Sep: For fasta gene maps, the field separator used in the FASTA header lines. Usually this will be either vertical bar "|" or space " "
FASTA Field: The FASTA field containing the gene ID. This can be iether a name referring to a key (usually "gene" or "locus"), or a number, in which case no key-value parsing is done. The protein ID is field 0.
FASTA KV: Key-value separator used in the FASTA file. Usually this is either "=" as in "gene=loc12345" or ":" as in "locus:g12345"
GFF File: If using a GFF file for protein-gene mapping, give the file name in this field
GFF Prot Feature: The feature name i the GFF file (third column in the GFF file) that contains features with the protein ID. Usually this is "mRNA".
GFF Prot Name: The named attribute in the GFF (within the attributes column, the ninth column) that contains the protin ID, that matches an entry in the input FASTA
GFF Key: Which attribute is used as a reference for fields such as Parent. The GFF spec says this should be "ID" (the default here), but some GFF files ignore this and use "Name" or something else
GFF Path: The path through the gff to get to the feature with the gene ID. Should be a dot-separated list of attribute names, the last of which will have its value used as the gene ID. Default is "Parent"
Map In: A tab-delimited file containing gene and protein accessions as the first two columns. Optionally a second pair of columns may give the gene and protein frame IDs, if they are different from the accessions. If updating from a previous version and numeric frame IDs are being used, and the old and new versions use the same protein annotation, then you should provide the previous PGDB version's output map (see Map Out column below) as an input map this time, in addition to other means of mapping (if you need to also use a new input map, concatenate them with the new map first)
Map Out: Output file to save the gene-protein mapping. If a PGDB uses numeric IDs, it is important to save this file to feed in as the input map during the next update to the PGDB. This ensures that the same genes / proteins have the same numeric IDs between the current and future versions
Numeric IDs: "Yes" to auto-assign numeric frame IDs to all genes and proteins, anything else to use the accessions for this. Using accessions is preferable, so you should only assign numeric IDs if the accessions are unusable as frame IDs (because they are too long or contain disallowed characters)
Gene Delete: Regular expression used to remove unwanted elements from the gene ID as it appears in its field. Any text matching the regex will be deleted from the gene ID. Applies only to gene IDs taken from FASTA and GFF inputs, but not from input mapping files.
