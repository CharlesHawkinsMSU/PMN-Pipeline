# Manual for running the PMN pipeline
This pipeline is used by the Plant Metabolic Network (PMN) to produce the plant metabolism databases hosted at https://plantcyc.org. It can be run to produce similar databases for plant and green algal species. 

## Overview of the pipeline
The PMN pipeline starts with the amino acid sequences for a plant or green algal species and produces as its output a pathway-genome database (PGDB) containing a full predicted metabolism for the species. The databases are accessed using the Pathway Tools software from SRI International and can be hosted on the web using that software. Databases like this can serve as a general reference for the species metabolism, and can perform functions such as mapping between sets of compounds and sets of enzymes or pathways, or finding pathways enriched for a set of genes or metabolites.

To run the pipeline you need, at minimum, the amino acid sequences for your species of interest. If possible you should also have some means to map the protein IDs to their corresponding gene IDs. The pipeline presently requires downloaded copies of PlantCyc and AraCyc; these you can get by filling out a license request at (https://plantcyc.org/downloads/license-agreement); these licenses are free for all users. A copy of Pathway Tools is also required. Information about Pathway Tools can be found at (http://bioinformatics.ai.sri.com/ptools); as of January 2024, SRI offers free licenses to academic and government users, but commercial users will need a paid license to use their software.

The pipeline is built to run in a Linux environment on an x86_64 CPU. On Windows we recommend the Windows Subsystem for Linux (WSL) software from Microsoft (https://learn.microsoft.com/en-us/windows/wsl/install) that creates a Linux environment on Windows. As for macOS, we recommend a virtualization solution, such as Qemu, VirtualBox, VMWare, or Parallels, running a Linux guest. It may be possible to make the pipeline run natively on macOS but this has not been tested at all, although this may be the only way to run the pipeline on M1-based Macs without using emulation. It is unlikely the pipeline can be made to run on non-Mac ARM-based machines without using emulation due to the dependency on Pathway Tools, which at present only offers x84_64 binaries on Linux.

The basic steps for creating a PGDB are to create a new project directory, download the amino acid sequences for the species of interest, write the config files, and run the pipeline. The major stages of the pipeline are:

- **E2P2**, which predicts enzymes from protein sequences
- **PathoLogic**, which generates an initial pathway genome database from the predicted enzymes
- **SAVI**, which applies past curation decisions to improve the database accuracy at the pathway level, by suggesting pathways to add and remove
- **Refine-A**, which applies SAVI's recommended changes and does other fixes such as putting in reaction names and citations for E2P2 and SAVI
- **Refine-B**, which pulls relevant experimental evidence concerning the species of interest from the reference database(s) into the newly-created database
- **Refine-C**, which performs finishing actions such as generating the cellular overview diagram and running Pathway Tools' automated consistency checker
- **Blastset generation**, an optional step that generates a blast database for the species so that protein queries can be used to find enzymes in the database.

For running the pipeline, a basic familiarity with the Linux command line is strongly recommended.

## Downloading and building the Singularity container
The recommended way to run the PMN pipeline is via the Singularity container. The container build scripts will handle installing all needed dependencies and putting them where the pipeline can find them. The only component you need to provide is a copy of the Pathway Tools installer. It is also possible to use a local copy of Pathway Tools that is already installed outside of the container; see "Use an external copy of Pathway Tools" below.

### Download the container build scripts
The container build scripts are available at the Github project (https://github.com/CharlesHawkinsMSU/pmn-container). You may download and unzip one of the releases or git-clone the repository.
You will also need to install Singularity in order to build the container. There are two currently-active Singularity forks, SingularityCE (https://sylabs.io/singularity) and Apptainer (https://apptainer.org), to build the container. SingularityCE is the fork that the PMN pipeline is tested with so it is the one we recommend.

### Link the container script
The container has a front-end script, pmn.sh, that can be used to build and interact with the container, and this script is the recommended way of doing so. It is also recommended to link this file to somewhere in your PATH; such as with:

    sudo ln -s pmn.sh /usr/local/bin/pmn

This will allow you to simply type "pmn" to run the container. Subsequently in this manual we will assume you have done so.

### Obtain Pathway Tools
The pipeline relies on the Pathway Tools software from SRI Internaitonal. Because Pathway Tools is proprietary software, we cannot redistribute it with the pipeline and the container build scripts cannot download it for you. Instead, you will need to supply the installer provided by SRI for installation into your copy of the pipeline container image. You can obtain a license from SRI International at (http://bioinformatics.ai.sri.com/ptools); as of January 2024 SRI offers free licenses to academic, government, and non-profit users, while commercial users will need a paid license. You will want to download the standard Linux installer, with a name like pathway-tools-28.0-linux-64-tier1-install, and place it in the directory with the container build scripts. 

#### Note: Pathway Tools versions
Each release of the PMN pipeline is coded to use a particular version of Pathway Tools (see the release notes of each release), and this is hardcoded into the build scripts; if you want to use a different version instead, you should edit the file pmn-ptools.def and change the $PT_VER variable and the name of the pathway tools installer further down the file to reflect the Pathway Tools version you want to use. Using a different version of Pathway Tools from the one sepcified in the PMN pipeline release you're using will usually work, but be aware that the results may not be exactly comparable to the PMN databases on https://plantcyc.org that were built using that release of the pipeline.

### Build the container
#### Note: Apptainer vs SingularityCE
Currently, the Singularity project has two major forks, SingularityCE and Apptainer. At present there should be no major differences between them that affect the PMN pipeline, however the pipeline is tested using SingularityCE, so that is the fork that we recommend. Most Linux distributions should have one or both available in their package repositories.

If you are using Apptainer, you will need to set the environment variable `SINGULARITY=apptainer` for the build. If you're using SingularityCE, the default value will work.

#### Build the PMN pipeline
Navigate to the build directory and type:

    pmn build

Assuming the build succeeds, you will have a finished container, pmn-ptools.sif. As the message at the end of the build states, this contains an installed copy of Pathway Tools, so please **do not redistribute it** without permission from SRI International. The build will also generate pmn-base.sif, which is a copy of the pipeline that does not contain Pathway Tools. This version of the container can only be used with an external installation of Pathway Tools but is safe to redistribute. If you don't need pmn-base.sif it is safe to delete after the build is finished. If you only want to generate this non-pathway-tools version, you can `make pmm-base.sif` (a copy of Pathway Tools is not required for the build in this case).

## Running the PMN Pipeline
To run the pipeline, the basic steps are to create an empty project directory, download the amino acid fasta file(s) for all the genomes you want to run, write the config files, and run the pipeline.

### Create a new project
The easiest way to do this is to create a new directory (mkdir myproj), cd to it, and execute:

    pmn newproj

This will create all the needed subdirectories and will put in default / example config files. By default the pipeline scripts all assume the current working directory is the project directory, but you can specify a different one with the -p flag:

    pmn newproj -p /path/to/myproj

In this case, newproj will create the specified project directory if it doesn't exist already.

### The project directory structure
A PMN pipeline project has the following directories:

- **blastsets** - This is where the generated blast databases will be saved if you run the optional Blastset step at the end
- **common** - A few misc files common to all the organisms will be generated here, such as lists of all the reactions in the reference database and text for the SAVI and E2P2 citations
- **e2p2** - The output files generated by E2P2 will be placed here
- **fasta** - The amino acid fasta input files should be put in this directory
- **gff** - If a genomic feature format (GFF) file is available for one or more of the genomes of interest, they may be placed here. Currently GFF files are one option for finding the gene ID for each protein ID
- **intermediate-pgdbs** - If you run the 'backup' function of the pipeline to save intermediate stages of the PGDBs, they will be placed here, and the 'restore' function will look here for saved PGDBs to restore from
- **logs** - The main log files for the pipeline will go here
- **maps-in** - If you choose to use mapping files to manally map from protein IDs to gene IDs, the mapping files should be placed here.
- **maps-out** - Regardless of how you choose to map protein to gene IDs, the final mappings will be saved to files in this directory, in the same format as the maps-in files, so they can be used to generate future versions of the same PGDBs and ensure that gene-protein mappings and gene/protein frame IDs remain stable across versions
- **pgdb-masters** - Species-specific scripts will be generated and placed in subdirectories here, along with some log files
- **pgdbs** - This is a directory to hold the PGDBs as they are being generated. It will usually be bound to the /pgdbs directory within the container that Pathway Tools uses to hold its repository of user-generated PGDBs
- **savi** - Files needed for and created by the SAVI step will be placed in subdirectories here
- **sockets** - This directory will hold the temporary sockets used for communicating with Pathway Tools; you usually shouldn't need to worry about the contents of this directory

In addition, several default control files are generated:

- **pgdb-pipeline.txt** - This is the top-level config file for the pipeline, and contains settings that are not specific to any one species. You can use it to change the name / location of any of the project subdirectories listed above, or of the other config files listed below; or to redirect to external installs of software like BLAST and E2P2 that are used by the pipeline
- **pgdb-table.txt** - This is a table file in tab-separated value (tsv) format that contains all the info and settings for each species you want to run. It is fairly modular, with column identity determined by the column headers (so columns can be rearranged, or omitted entirely if not needed), multiple table files can be combined in a single run, and there is a system available for defining presets so you need only enter common settings once
- **authors.txt** - A tab-delimited table with info on the authors to be credited for each PGDB. Each entry in the Authors column in pgdb-table.txt should refer to an entry in authors.txt
- **organizations.txt** - A tab-delimited table with info on all organizations listed as author affiliations in authors.txt

### Download your species genome(s)
For each species you want to run, download the amino acid fasta file containing the full amino acid sequences from the genome annotation, and place it into the fasta directory. You may also need a GFF file for mapping protein IDs to gene IDs, if the fasta file does not contain gene IDs in the headers. In that case, place the GFF file into the gff folder.

### Write the config files
You will need to put in information for your species of interest and the pipeline run in general by editing the generated config files. The default pgdb-pipeline.txt file usually needs minimal changes if any, but the others you will have to write.

#### pgdb-pipeline.txt
This is the top-level config file for the pipeline. It contains configuration options that are not specific to any one species/database. You can use it to change the name or location of any of the pipeline project subdirectories, or to change the location of software used by the pipeline such as E2P2 or BLAST, or of data like the reference protein sequence dataset (RPSD) used by E2P2.

The file is a text file, formatted as variable = value, with one such assignment per line. Spaces are allowed around the = signs. Blank lines are ignored, as are lines starting with #. A full listing of the variables that can be set with this file is available here: (url)

Before reading this file, the pipeline will look for /etc/pmn-pipeline.conf and read that, with the same format; any entries in pgdb-pipeline.txt override the corresponding entries in /etc/pmn-pipeline.conf. The singularity container contains a version of /etc/pmn-pipeline.conf that correctly sets the locations of all software and data the container contains, so your own config file can be small and only contain options that are specific to the pipeline run you're performing.

#### pgdb-table.txt
This file contains info for all the species you want to run using the pipeline. It is a tab-delimited table with each row corresponding to one species database you wish to create. You may change the name or location of this file by changing the proj-pgdb-table variable in pgdb-pipeline.txt.

The header row is required and the column names in this row determine which column is which. The column names are case-sensitive. The order of the columns does not matter, and a column may be omitted entirely if it isn't needed; this is equivalent to leaving all rows blank for that column.

##### Presets
If you have information that is common to all or many of the species you are running, it may be desirable to only have to enter it once,  and to have a single place to edit if changes are needed. To address this need, the pgdb table offers a system of presets. Any entry whose Database ID column starts with a forward slash / is considered a preset, and can be referenced by subsequent entries by putting its name (including the leading /) in the Presets column for those entries. Any blank columns in the entry will then be filled in with the corresponding column from the preset if it has a value. Multiple presets can be applied to a single entry; just join their names together, e.g. /phytozome/gff to apply the /phytozome preset and then the /gff preset. In this case, presets **earlier** in the list take precedence, due to the behavior of presets to only fill in a column if it doesn't already have a value.

There is a special preset called /default that, if defined in the table file, applies to all entries, as if they had /default at the end of their Presets list.

Presets must be defined **before** they are referenced (i.e. earlier in the table). Presets may reference other presets via the Presets column. The requirement that presets be defined before being referenced still applies, so circular references are impossible.

##### Required Columns

These columns are required for all entries, though they can be specified either directly or via a preset.

###### Database ID

This is the database name. It is the 'cyc name without "cyc" at the end; so we would put Ara for AraCyc or Corn for CornCyc. It must be unique, and not match the database ID of any other cyc database. It should start with a letter, and may contain letters, digits, and underscores. Case is preserved for some purposes but the requirement that the ID be unique is case-insensitive; i.e. you cannot have Corn and CORN as different databases. This column is also where you put the preset name when defining presets; start it with a / (e.g. /phytozome) to indicate that this row defines a preset.

###### Species Name

The full binomial species name, with subspecies if present. E.g. Arabidopsis thaliana col or Zea mays.

###### NCBI Taxon ID

The taxon ID from the NCBI taxon database that refers to the organism the database is for. You can find these by searching NCBI Taxonomy (https://www.ncbi.nlm.nih.gov/taxonomy) and copy-pasting the Taxonomy ID (e.g. 3702 for Arabidopsis thaliana).

###### Sequence File

The name of the input amino acid sequence fasta file to be used to create this database. The pipeline will look in the "fasta" project subdirectory (or whatever directory is specified by proj-fasta-dir in pgdb-pipeline.txt) for these files. The pipeline requires full amino acid sequences to produce useful results.

The pipeline requires amino acid sequences; nucleic acid sequences will not work. Because A, T, C, G, and U are all valid FASTA amino acid letters, nucleic acid fasta files in the input cannot be autodetected, so it is important to make sure of what you are using as input.

###### Version

The version number for the database. Typically a new database will start at 1.0. PMN uses three version numbers; e.g. 12.1.2 (The first is incremented for a full rebuild of the database; the second for edits and corrections that change the contents in any way; the third for updating to a new Pathway Tools version without changing database contents), but you are free to use whatever versioning scheme makes sense to you.

###### Seq Source

A short string identifying where you got the sequence file. E.g. "Phytozome", or "The &lt;species&gt; genome project"

###### Authors

A list of authors to be credited for this database. They will be listed on the main page for the database as its authors. The contents here should be a list of author IDs that each match an entry in authors.txt via the FrameID column in that file; authors.txt will be looked to for the full info for each author. Each author ID should be surrounded by vertical bars and they should be separated by spaces; e.g. "|hawkins| |xue| |rhee|".

###### Citation Year

The year for the SAVI and E2P2 citations. Give the year that these were last updated.

##### Optional columns

Most of these are computed from the other columns if left blank. It is rare that you will need to include them in your table file.

###### Unique ID

This will be an ID unique to the PGDB. These are used when a new frame (a "frame" is any data object in Pathway Tools, such as a pathway or an enzyme) is created within the database; the frame ID will include the unique ID of the PGDB it was created in, so as to avoid frame ID collisions with frames created in other PGDBs.

For new PGDBs, a unique id will be generated for each database by the unique-ids stage of the pipeline and stored in the uids.txt text file. If you are updating an existing PGDB, however, you will want to make sure the unique ID stays the same as it was in the previous version. The two ways of doing this are to copy the uids.txt from the previous version's project directory into the new project, or to enter the previous UIDs into this column of the table file.

###### Database Name

The database ID *with* Cyc at the end. There is almost never a need to specify this explicitly.

###### Abbrev Name

A shortened version of the species name, e.g. A. thaliana col. It should still contain the subspecies name if present, but without the subspecies marker; so if the full Species Name is "Brassica oleracea var. capitata", then this column would be "B. oleracea capitata". When this column is computed from Species Name, the first word is abbreviated to one letter and any word ending with "." is assumed to be a subspecies marker like var. or subsp. and omitted.

###### ID/Name

An even more abbreviated species name, of the form Athaliana for Arabidopsis thaliana. Subspecies is left off entirely. When computed from Species Name, the first word is abbreviated to one letter, the second word is kept, and all subsequent words are dropped.

###### Initial PF File

The output file to be saved by E2P2. If not specified, it will default to the fasta file name with the extension (e.g. .fa or .fasta or .pep) replaced with .e2p2v5.orxn.pf. These files will be placed in the e2p2 project subdirectory, or whatever directory is given by proj-e2p2-dir in pgdb-pipeline.txt.

###### PF File

The final PF file produced by the revise step and fed into PathoLogic. If not specified, it will default to the fasta file name with the extension (e.g. .fa or .fasta or .pep) replaced with .e2p2v5.orxn.revised.pf. It will also be placed in the e2p2 directory, so the name should not be the same as Initial PF File.

##### Gene mapping columns

These columns are used in mapping from protein accessions to gene accessions. The finished database should ideally have gene accessions for each enzyme in addition to the protein accession, but there is no one standard way to represent the mapping from one to the other. These columns in the file are therefore used to specify how to do the mapping.

There are three ways of finding the gene ID for each protein supported by the pipeline: Extract them from the fasta headers, find them from a GFF input file, or take them from a tab-delimited mapping file. To select the method used, set the **Genes From** column in the input table to FASTA, GFF, Map, or None.

There is also a means of auto-generating frame IDs for each protein and gene. Frame IDs are IDs used by Pathway Tools to uniquely identify each database object (e.g. proteins, genes, reactions, pathways, compounds). Auto-generating these for genes and proteins is usually not necessary because the accessions are used by default and this usually works best. The only exception is if the protein or gene IDs are not valid as frame IDs, usually because they are too long; the max length of a frame ID is 40 chars. It could also be the case that they contain invalid characters for a frame ID, such as '#'. In these cases auto-generated frame IDs can be assigned; see the Numeric IDs column.

Gene Delete and Prot Delete are optional columns available for both FASTA and GFF input. If you put a regular expression in these columns, any text matching the regex will be deleted from the gene or protein accessions taken from the FASTA or GFF file. The common use for this is if the accessions have some extra text associated with them, such as if the gene IDs are formatted as ID=gene:accession1234 where the correct gene ID is "accession1234", in which case you could set "Gene Delete" to "^gene:". These fields are more commonly needed for GFF input but they are available in FASTA mode as well. However you generally should not use Prot Delete with FASTA files because whatever was in front of the protein ID will have been put into E2P2's output file, so if you change it using Prot Delete then the revise step won't be able to find the protein. (TBA: In the future the pipeline may handle Prot Delete for FASTA input by searching the E2P2 output for the original ID and replacing it with the modified one)

###### Gene mapping from the FASTA header

To extract gene accessions from the fasta header, there are four fields: FASTA Map, FASTA Sep, FASTA Field, and FASTA KV. The columns Gene Delete and Prot Delete may also be used in FASTA or GFF mapping. FASTA Map is a binary column and the master switch for trying to extract gene IDs from the fasta headers; put "Yes" to instruct the pipeline to use this method to extract the gene IDs.

FASTA Sep is the separator that separates fields in the fasta header; usually this is a space, a tab (enter \t for the column), or a vertical bar. It defaults to a space if not specified.

FASTA Field indicates which field contains the gene ID. It can be a named field, e.g. locus for locus=gene12345, or a number if the fields do not have names, in which case the fields will be counted from 0 (the protein accession). So for &gt;prot12345|chr2|gene12345 you would enter 2 for FASTA Field to get gene12345. If not given, the default is "gene"

FASTA KV gives the key-value separator for named fields. For most fasta files it will be = or :. So for gene=gene12345 you'd enter "=", or for locus:gene12345 you'd enter ":". It defaults to "=" if not given, and has no effect if FASTA Field is a number.


###### Gene mapping from a GFF file

If gene IDs are not present in the fasta file, another option is to use a GFF (Genomic Feature Format) file. These may be provided by the sequencing project and are usually made available for download alongside the fasta files. GFF files specify the location and other properties of regions of interest in a genome, which often includes genes and CDS's. GFF is a tab-delimited text table format, with each row representing a single "feature" on the genome. The two columns relevant to our purposes are the feature type column (Xth column) and the attributes (Yth column). The attributes column contains a list of key=val pairs separated by semicolons that can contain any amount of info on the feature. Some of these can be references to another feature, usually one the feature is part of; so an exon might have Parent=mRNA12345 to indicate it is part of mRNA12345, another feature in the file. 

The way that GFF gene mapping works in the PMN pipeline is that the GFF file is scanned for features whose feature type matches a specified value (usually "mRNA"), and then its attributes are used to find the corresponding gene. The following columns in pgdb-table.txt are relevant:

GFF File is used to give the file name of the GFF file to use. Giving any value here turns on GFF mapping. The pipeline will look in the gff folder for the file specified here, or whatever folder is specified by proj-gff-dir in pgdb-pipeline.txt.

GFF Prot Feature is used to tell the gene mapper what features refer to a protein, looking at column X in the GFF file. Often this is "mRNA", the default. 

GFF Prot Name tells the gene mapper which field in the attributes column contains the protein accession. The value of this field should match the protein names found in the fasta file. The default is "Name". 

GFF Path tells the gene mapper how to get from the protein feature to the corresponding gene feature and find its accession. It is a dot-separated list of attribute names. All but the last are treated as references to other features in the file, while the last has its value used as-is as the accession. So if there is an mRNA feature that has a gene feature as its Parent and the gene has its accession in the Name field, then you would put "Parent.Name" for GFF Path. As another example, if there is a "protein" feature that references an "mRNA" feature via Component_of, which references a "gene" feature via "Transcript_of", which has the accession in the "Accession" field, then you would put "protein" for GFF Prot Feature and "Component_of.Transcript_of.Accession" for GFF Path.

GFF Key is used to tell the gene mapper what field is used for references like Parent. The GFF spec says that each feature should have a unique field called "ID" that should be used for all such references, and most GFF files follow this. But a few do not, and their references are via another field like "Name". For these cases you can put "Name" (or whatever field is being used) in this column. Otherwise you can leave it out and the default "ID" will be used.

###### Explicit mapping via mapping files

As a last option, you can manually specify the mapping by providing a two-column tab-delimited file with protein accessions in the first column and gene accessions in the second. An optional third and fourth column can be used to manually specify the frame IDs for each one; if they are absent the accessions will also be used as frame IDs, which is recommended unless the accessions are too long to be used as frame IDs. You can give a mapping file in addition to using fasta or gff mapping; in this case the mapping file takes precedence and the other method is used to fill in any proteins that are not in the mapping file.

A word of caution: If you use a script to generate a mapping file like this by modifying the protein IDs in some way, for example by removing a .1 or .2 or -t1 etc. from the end, make sure you are confident that this will work and produce correct results for *all* the proteins in the fasta file. There are cases where 99% of the protein IDs follow one format but the remaining 1% scattered through the file follow a different format; or cases where past revisions to the annotation may have resulted in a small number of the protein accessions that don't match with their corresponding gene accessions. Use caution to avoid accidentally producing a database with incorrect gene IDs!

If you have a mapping file you want to use, place it in the maps-in directory (or another as specified by proj-maps-dir in pgdb-pipeline.txt) and give its name in the Map In column in pgdb-table.txt

###### No gene IDs

If there are no gene IDs at all (which could be the case, for example, if you're working purely from a transcriptome), then you can put None in the **Genes From** column to skip the gene mapping entirely. No gene IDs wil be added to the enzymes in the database.

###### Output mapping files

Regardless of what method you choose to map proteins to genes, if you don't skip the revise step entirely then an output file will be produced and saved in maps-out (or another directory specified by proj-maps-output-dir in pgdb-pipeline.txt) that gives the final mapping of protein accessions, gene accessions, and frame IDs for both, as a 4-column tab-delimited table file. By default it is named by replacing the fasta file extension with .map, or another name can be given using the Map Out column in pgdb-table.txt. This mapping file can be used as an input mapping file for future runs of the pipeline for this species. This is strongly recommended if you use numeric frame IDs, as it will ensure that the auto-assigned IDs remain stable accross versions while still allowing new ones to be assigned to enzymes that weren't present in previous versions.

###### Numeric frame IDs

Frame IDs are IDs used by Pathway Tools to uniquely identify each database object, including pathways, enzymes, genes, reactions, compounds, and more. For genes and enzymes, it is standard practice to use the gene/protein accession as the frame ID. This however occasionally can't be done because the accessions are not valid as frame IDs, either because they contain diallowed characters or (more commonly) they are too long; frame IDs are limited to 40 characters. There may also be a concern that the accessions are not unique to the species; for example if the only IDs available are generic ones like gene00001, gene00002, etc.
In these cases, the PMN pipeline can auto-assign numeric frame IDs to each gene and protein, while still keeping the accession in the Accession-1 field of the gene/protein, which isn't limited by length or special characters. Put a "Yes" in the Numeric IDs column of pgdb-table to activate this feature for a given species database. If you do this, you should save the output map that gets saved in maps-out, and use it as an input map when you generate any future version of this database (this may be done in addition to another mapping method). Doing this will ensure that the frame IDs for all genes and enzymes present in both versions will remain the same from one version to the next.

#### authors.txt

This file gives full info for each author to be credited with one or more of the databases. As with pgdb-table.txt, the column names are relevant, but their order is not. It is a tab-delimited table file with the following columns:

##### Frame ID

This ID will be used internally to refer to this author. It will not be publicly-visible but you will use it to assign authors to databases in the pgdb-table.txt file's Authors column. These IDs can be anything that starts with a letter and contains only letters, digits, underscores, and dashes. They should be enclosed in vertical bars, like |hawkins|. They must be unique to each author.

##### First-Name

The author's first name. Multi-word first names are accepted, as are names with hyphens or apostraphes.

##### Last-Name

The author's last name. Multi-word last names are accepted, as are names with hyphens or apostraphes.

##### Email

Email at which the author can be reached. Optional. **This email will be publicly visible if you publish the PGDB to the web**.

##### Login-Account

The user name on the local machine that identifies this user. The whoami command will give your current user name. Required, but only used if you do curation into the database using Pathway Tools (to assign you credit for the curation).

##### Affiliations

A list of affiliations to which this author belongs (e.g. university, company, etc.), separated by spaces if the author has multiple affiliations. Each should refer to an entry in organizations.txt via the Frame ID column in that file.

##### Comment

Optional field. It will be shown when a user clicks the author in the author list; can be used to give a brief biography if desired.

#### organizations.txt

This file is to give full info for the affiliations to which authors belong, such as universities, companies, nonprofits, etc. As with pgdb-table.txt, the column names are relevant, but their order is not. It is a tab-delimited table file with the following columns:

##### FrameID

Unique identifier for the organization within the database, similar to the same column in authors.txt. The Affiliations column in authors.txt should refer to organizations using this column.

##### common-name

The full name of the organization, such as Michigan State University

##### abbrev-name

A shortened name for the organization, such as MSU

##### url

The main homepage of the organization, such as https://msu.edu

##### email

Contact email for the organizaion. Optional. The email will be publicly visible if you publish the PGDB to the web.

### Run the pipeline

With the config files all written, you are finally ready to run the pipeline! All pipeline operations can be accessed via the container. To get a general overview of the options, run:

    pmn -h

To get a list of pipeline stages you can run, with a brief description of each, run:

    pmn list-stages

In most cases, you can simply proceed to run the desired pipeline stages. The pipeline can be instructed to run one stage, as in:

    pmn precheck

or to run multiple stages in sequence:

    pmn precheck e2p2 revise prepare create

or to run a range of them in the standard order by putting a dash between the first and last stage to run:

    pmn e2p2 - create

#### The stages of the pipeline

The standard pipeline stages are, in order: newproj, precheck, unique-ids, split, e2p2, join, revise, prepare, create, savi-dump, savi-prepare, savi, refine-prepare, refine-a, refine-b, refine-c, final-dump, blastsets.

Additionally, several other "stages" are available that aren't part of the standard sequence: list, list-stages, fa-stats, savi-check, backup, restore, delete, clean, dump, dump-biopax, pgdb-stats, custom-dumps, shell, and lisp.

##### newproj

This stage creates a new project directory based on a template inside the container. It creates the standard project subdirectories and puts in example config files. By default all of this is placed in the current working directory, but another project directory can be specified using -p; in this case the specified directory will be created if it does not exist. Any existing config files with the default names will be overwritten. When using -p the path must be one the container has access to.
Note that if you rerun newproj on the same directory it will copy the default config files, overwriting any modifications you have made. If you only need to fix the project directory (i.e. you accidentally deleted some needed directories) without overwrting, use the -f or --fix option; only files that do not already exist will be copied over.

##### precheck

This stage runs a number of checks on the configuration. It checks that all required fields and columns exist in pgdb-pipeline.txt and pgdb-table.txt, makes sure all files, directories, and executables referenced in pgdb-pipeline.txt and pgdb-table exist and have the needed permissions, makes sure that none of the specified PGDBs already exist (this only generates a warning), and performs other minor checks. All errors found are reported. You should correct any errors before proceeding with the rest of the pipeline.

##### fa-stats

This optional stage compiles some basic statistics on the input FASTA files. Any stats that are too far outside "reasonable" values will generate an error or warning when you run the command, and all stats are saved into a tab-delimted table file called fa-stats.txt (the filename to use can be overridden with the proj-fa-stats option in the config file).  This will contain the following columns:

- Orgid: The Organism ID, from the Database ID column in the input table.
- Sequence File: The input FASTA being analyzed
- Sequence Count: How many sequences are in the file; i.e. how many proteins are in the annotation you are using. Reasonable values for plants are from 20,000 or so on up. Much smaller values may indicate you don't have the complete proteome, or that the file is a genomic DNA assembly instead of a protein annotation. Values lower than 10,000 generate a warning.
- Avg Sequence Length: The average length of sequences in the file. For a protein FASTA for a plant or green alga, reasonable values are anywhere from the 200s to the 700s. Much smaller may indicate that you have truncated sequences (e.g. the sequences are translated ESTs). A bit larger (low thousands) may indicate a CDS file. Much larger may indicate the file is a gDNA assembly. Values lower than 100 or larger than 1,500 generate a warning.
- Sequence Type: The best guess as to whether this is an amino acid (AA) or nucleic acid (NA) file, based on the characters present. A file will only be diagnosed as NA if all sequence characters are valid NA characters for FASTA, including the various wildcard characters. This column will contain "Invalid" if any sequence contains an invalid FASTA character. Common reasons for seeing "Invalid" are:
    - The fasta uses a dot "." as the stop codon, rather than the asterisk "*" required by the FASTA spec. This will cause E2P2 to fail, and you should correct the file with an advanced text editor if this is the case.
    - There is "readability" formatting such as base numbers at the beginning of the line or spaces within the line. This may be the case if sequences were copied from NCBI Nucleotide without setting the format to "fasta". You will need to remove these to use the pipeline.
- Percent Good Sequences: What percent and how many of the sequences in the file are "good". In this instance, "good" means that they start with a methionine (M) and contain no stops (*) except at the end. This should probably be at least in the high 80% range. Low values may indicate any number of problems, from incorrect machine translation resulting in frameshifts to cDNA input to simple low sequencing quality.

Together these stats can be useful to avoid or diagnose problems in the pipeline caused by the input sequences.

##### split

This stage is used to split the input fasta files into smaller parts before running E2P2. Doing this lets E2P2 run better in parallel, but is recommended even if not running the pipeline with parallelism turned on because having too large an input file can result in very high memory usage from BLAST.

The splits are placed in fasta/splits. The number of splits is controlled by the config variable **split-fa-num-files** (present but commented out in the default config file). 20 is a reasonable number to use in most cases. After splitting the input files, you can run E2P2 in parallel either manually or using the pipeline's built-in parallelism; see 'e2p2' below for details.

##### e2p2

This e2p2 stage runs E2P2. E2P2 is the ensemble enzyme predition pipeline, a piece of software developed by Rhee lab to predict enzyme functions from amino acid sequences. E2P2 is an ensemble predictor that combines predictions from other software using a weighting scheme. The default predictors are BLASTP and DeepEC; it is possible to configure others. E2P2 bases its predictions on the reference protein sequence dataset (RPSD), a set of experimentally-validated enzymes assembled by the E2P2 developers from various sources including MetaCyc, PlantCyc, BRENDA, and SwissProt. The current RPSD is included in the container, so no configuration is necessary on this front.

E2P2's main outputs, *.e2p2.orxn.pf, are placed in the e2p2 project directory. They will be revised in the next stage to add in gene IDs.

If you have split the input fasta files using 'split' (see above), you can have e2p2 automatically run in parallel over all the splits:

    pmn e2p2 -s all -l

The -s option indicates which splits to run; you can give a single number (1-20 or however many splits you reqested), a range of numbers like  2-4, or 'all' which runs all splits. If you want to use your own parallelism solution here, like SLURM, you can do something like this in the slurm job file:

    pmn e2p2 -s $SLURM_ARRAY_TASK_ID

then run your slurm job file with something like:

    sbatch --array=1-20 myjobscript.slurm.sh

In either case, E2P2 will produce split output files in e2p2/splits, which can then be joined using the 'join' stage.

##### join

This stage joins the split E2P2 output files into one file per species. It is needed if you split the input files using 'split'

##### revise

The revise stage modifies the .pf files produced by E2P2 to add in gene IDs. It finds these using the gene mapping-related fields in pgdb-table.txt; see the section "Gene mapping columns" above. Its outputs, *.e2p2.orxn.revised.pf, will be fed into PathoLogic to create the initial versions of the PGDBs.

##### prepare

The prepare stage creates subdirectories for each PGDB in pgdb-masters, copies the final .pf files into them, and creates PathoLogic input files and .master scripts for each organism based on the contents of pgdb-table.txt. 

##### create

The create stage invokes PathoLogic on the input files assembled by the `prepare` stage in order to create the initial versions of the PGDBs.

##### savi-dump

The savi-dump stage runs the `dump` master script, and as such is a synonym for `dump`. It instructs Pathway Tools to dump the contents of all the PGDBs to text-based flat files that are required by SAVI. This synonym exists because `dump` is called multiple times in the pipeline, so `savi-dump` can be used to refer to this specific point in the standard pipeline sequence, meaning that if you request the range e2p2 - savi-dump, it is unambigous which dump step you mean.

##### savi-prepare

The savi-prepare stage creates subdirectories for all organisms in savi/input and assembles the required files for running SAVI into them.

##### savi

The savi stage runs SAVI. SAVI, the Semi-Automated Validation Infrastructure, is a piece of software created by Rhee lab that applies past curation decisions for each pathway to the newly-created PGDBs. It relies on a set of input files (included with the container) previously generated by the PMN team that specify rules for each pathway that might be predicted. For example a pathway might have a rule like "this pathway is universal to all plants; if it isn't predicted then that's a mistake and it should be added" or "this pathway is not found in plants; if it is predicted that's a mistake and it should be removed" or "this alkaloid pathway is only found in Solanaceae; if it's predicted outside that clade then that's a mistake and it should be removed". 

SAVI does not make changes to the PGDBs itself; instead it produces files (ic.txt and remove.txt) in each organism-specific subdirectory in savi/output listing pathways that should be added and removed, respectively. These changes will be applied by the refine-a stage.

##### refine-prepare

The refine-prepare stage prepares for refine-a, refine-b, and refine-c. It generates more .master scripts in each organism's directory in pgdb-masters; creates the Author and Organization frames in each PGDB according to authors.txt, organizations.txt, and pgdb-table.txt; creates Publication frames in each one for the citations of E2P2 and SAVI, generates files common to all the organisms in the common directory, and copies other needed files into each organism's master directory.

##### refine-a

The refine-a stage performs a number of actions. It applies the recommendations made by SAVI for pathways to add and remove from each PGDB; it credits the PMN group as curator for all predictions made by E2P2 and adds a citation to the latest E2P2 publication; it adds appropriate SAVI citations to all pathways added or validated by SAVI; It adds in info on some external databases like Phytozome and Ensembl so that proteins can be linked to them and attempts to do so when e.g. PHYTOZOME was specified as the sequence source in pgdb-table.txt; it fixes the enzymatic reactions common names, and it adds in links to PlantCyc and/or MetaCyc for each pathway, as appropriate.

##### refine-b

The refine-b stage adds experimental pathways and enzymes from the reference database(s) (usually PlantCyc and MetaCyc; see the reference-dbs option in pgdb-pipeline.txt to change the reference databases used) into the newly-created databases. Any enzyme or pathway in one of the reference databases that is annotated as belonging to the species in question based on experimental evidence will be imported into the new species database if it was not already predicted; if it was already predicted then just the experimental citation will be imported and added to the database.

##### refine-c

The refine-c stage performs several final finishing touches on the new databases. Specifically, it performs the following actions:

- Runs all of Pathway Tools' built in checks. Pathway Tools will look for inconsistencies and correct those that it can programmatically
- Generates the Cellular Overview diagram for each species
- Assigns the specified authors as authors for each PGDB as per the Authors column in pgdb-table.txt

At the conclusion of this stage, you have a full, working PGDB that can be published on the web or queried using Pathway Tools! The remaining stages are optional and export the data in the new databases in various formats for use with external programs

##### final-dump

The final-dump stage generates text-based flat files in various formats of the completed PGDBs, for use in external programs. It generates tabular .col and  attr-value .dat files (see URL for the formats), as well as BioPax level 2 and 3 dumps of each database. These files all go in pgdbs/&lt;database-name&gt;cyc/&lt;version&gt;/data. The final-dump stage is an alias for running dump and dump-biopax.

##### blastsets

The blastsets stage generates BLAST databases for all organisms in the blastsets folder. These can be used with the NCBI BLAST suite of programs to query a protein or nucleic acid sequence and find matches that link to enzyme frames in the database. These are the basis of PMN's BLAST server at https://blast.plantcyc.org that allows for the databases to be queried for enzymes that match an input sequence.

##### custom-dumps

The custom-dumps stages produces tabular output files like those linked to in the "tab-delimited text files" section of https://plantcyc.org/downloads. The files will be placed in the custom-dumps directory.

#### Other stages

##### list

This lists out the organisms in the input table file, along with their index (order within the file); this index can be used if you want to request organisms to run numerically, which can be useful when doing parallelism via SLURM (e.g. you can run a job array and use the array task ID as the organism to run, to run many organisms in parallel).

##### list-stages

This lists the stages you can run with a brief description of each.

##### savi-check

This stage can be run after savi-dump and before savi. It looks through the generated databases and lists out any pathways that do not have a rule in the SAVI input files. If you are using a copy of SAVI external to the Singularity container, you can write your own rules for these pathways if desired; otherwise the prediction of PathoLogic will be kept as-is.

##### delete

This stage deletes already-generated PGDBs. It can be used if the databases are found to have been generated incorrectly and you want to clear them and start over. It will ask for confirmation unless run with -y.

##### clean

This stage deletes various temporary files and logs. Currently it deletes:

* all log files in logs/
* any slurm log files in the main project directory

It will ask for confirmation unless run with -y.

##### shell

Opens a bash shell inside the PMN container. Only available for the container version of the pipeline when run via the pmn.sh script.

##### lisp

Launches Pathway Tools inside the PMN container with an interactive Lisp prompt. Only available for the container version of the pipeline when run via the pmn.sh script.

## Parallelism and SLURM

By default, the pipeline will run sequentially. If multiple stages are requested, each stage will be run for each organism, one organism at a time, the the next stage for each organism, and so on. The pipeline can, however, be run in parallel instead, at the organism level; that is to say, each organism will be run through the requested stages sequentially, but all organisms are run at once. 

As far as mechanisms for parallelism, the pipeline has built-in support for parallelism using the shell. There is some consideration for using SLURM, such as using SLURM job IDs to differentiate process-specific files so that multiple SLRUM jobs won't interfere with one another.

### Built-in parallelism

The built-in parallelism simply runs the requested organisms each in their own process. It is best for workstations and desktops that have enough cores and RAM to run the pipeline in parallel but do not have HPC-class job management software like SLURM installed.

When you call the main pipeline script with `-l` / `--parallel` and request one or more stages to be run, the script will spawn subprocesses for each organism. Each subprocess will then run through the requested stages for its assigned organism. The main process will block until all the subprocesses are finished. All output of the subprocesses will be saved to logs/out.&lt;pid&gt; where &lt;pid&gt; is the process ID.

You can specify the option `max-parallel-cpus` in pgdb-pipeline.txt to limit the number of these subprocesses that will run at one time. In picking this value, you should make sure to consider the available RAM as well as CPU cores.

## Customizing the pipeline

### Container-external software and data

Once fully-built (including providing a Pathway Tools installer), the PMN pipeline container will include all software and data it needs to run. You may, however, substitute your own external installation of any of these tools or data if desired.

## Tips and Troubleshooting

### E2P2 finds no or almost no enzymes

Symptoms: the .e2p2.orxn.pf file is very short or empy; the final PGDB contains no or very few pathways (for reference, a typical correct number of pathways is in the 100s-500s range)

Cause 1: E2P2 failed partway through creating the output file. Check the pipeline output or, if running in parallel, the log files in the logs directory, to see if E2P2 might have generated an error. Most E2P2 errors will result in no output file being generated at all but it is not impossible that something occurred while writing the file. Not likely, but worth checking.

Cause 2: The amino acid sequences were truncated. Your FASTA input file may contain amino acid sequences translated from expressed sequence tags (ESTs) or other truncated sequences. You can run the fa-stats pipeline stage to generate a file of statistics for all the input fasta files; look in the "Average Length" column for the average length of sequences in the file. Typical values for plants are in the 200s to 500s range; green algae can range a bit higher. Much lower and you may have truncated sequences; orders of magnitude higher and you probably have a genomic DNA fasta file.

Cause 3: Garbled protein sequences. It's possible the in silico translation that produced the amino acid sequences was done incorrectly, resulting in garbled sequences that start or stop in the wrong place, have introns still in, or otherwise have frameshifts that destroy the sequences. The fa-stats command will also help diagnose this; look at the "Good Seqs" column of the output file. This column gives the number and percentage of sequences in the file that "look good", meaning they start with M and have no stops except at the end; if more than a small percentage aren't good then you may have garbled sequences.

The percentage you get may also be a clue to what has gone wrong; ~50% good may indicate a problem determining strand; ~33% good may be a problem with initial reading frame; ~15% good may be a problem with both; ~0% good can be any number of problems, from failure to remove introns to problems with the reading frame between exons. Of course, a simple problem of low-qualilty sequencing can produce any percantage.

Using software like BUSCO to assess the genome quality may help in determining if this caused by low-quality sequencing. PMN uses a minimum score of 75% from BUSCO as a cutoff for producing a PGDB from a genome.

Cause 4: Nucleic acid FASTA input. It's possible you accidentally provided an input FASTA file with mRNA, cDNA, or gDNA sequences instead of the required amino acid sequences. Manually check the file to see if it contains only A, T, C, G, and U.

Cause 5: Invalid FASTA formatting. A few FASTA files available "out in the wild" break with the FASTA specification in ways that cause errors for some bioinformatics tools. For example, some files use a . character to mean a translation stop, rather than the * that the spec says they should use, which causes DeepEC to crash. Check for . characters in your fasta sequences. You can write a script or use an advanced text editor to replace . with *, but do so only in lines that don't start with > so that the FASTA headers aren't corrupted in the process. For example the vim command :v/>/s/\./*/g will do this operation.

Another possible problem could be that there is "human-readable formatting" like base numbers at the start of lines and spaces every however-many bases. NCBI puts these into some displays of sequences; these are not intended to be valid FASTA even though they use the FASTA letters. If you have sequences like this, you will have to remove the numbers and spaces, again using a script or advanced text editor to only operate on lines that don't start with &gt;.

Cause 6: Non-FASTA input. Make sure the input file is in FASTA format. Most other formats would generate errors that prevent the pipeline from producing any output PGDB at all but it's not impossible that you have something that looks enough like FASTA to confuse it into trying to run.

### E2P2 generates many warnings, including that the input file has an invalid format

Usually this is caused by a problem with DeepEC when running a very small test genome. It shouldn't affect the results.

### Out of disk space errors while building the container

This may be caused by simply being out of disk space &mdash; the container requires 16 - 18 GB of space during the build &mdash; but it could also be a lack of space in the /tmp directory if this has been set up to use tmpfs. Building the container via pmn.sh should detect this and change to another temp directory automatically; in the event it doesn't, you can manually create a `tmp` directory within the container build directory, and set the environment variable `TMPDIR=$PWD/tmp` before building.

### Precheck says that /pgdbs isn't writeable

This likely means that something went wrong binding the project pgdbs directory to /pgdbs directory within the container. This should happen automatically when you run the container via pmn.sh, but it may fail if you have changed the location of the pgdbs directory in your project. In this case, set the environment variable `SINGULARITY_BIND=/PATH/TO/pgdbs:/pgdbs` (replacing `/PATH/TO/pgdbs` with wherever your pgdbs directory is) before running the pipeline.

### My fasta or GFF file is inconsistent in terms of how proteins map to genes

PMN curators have encountered cases where a GFF file is inconsistent in terms of where protein IDs are; for example the annotation for Persea americana drymifolia (ref) has some proteins in the fasta file correspond to mRNA entries in the GFF file (parented to gene entries) and others to CDNA entries (parented to mRNA entries parented to gene entries). We solved this in the following way:
* Manually call the revise_pf.py script, manually type in the GFF parameters to read the mRNA-based mappings (including -gpf mRNA -gg Parent.Name), and use the --map-only option to save the full mapping to a file
* Do the same but with parameters to read the CDS-base mappings (-gpf CDS -gg Parent.Parent.Name)
* Concatenate the two maps together and use this as the input map for the organism

### PathoLogic reports a "critical error" when creating the protein BLAST DB

This message is normal when running the PMN pipeline. It means that Pathway Tools could not create a blastset for the organism, because the protein sequences are not made available to it. This is not a problem because the PMN pipeline will create the blastsets later using other tools.

### Case-sensitivity

The pipeline is intended to be run on a Linux system and with *very* rare exceptions Linux filesystems are case-sensitive, so if a file is called "file.txt" then you cannot refer to it as "File.txt", or you will not successfully find the file. It also means that it is possible to create, e.g., "file.txt", "File.txt", and "FILE.TXT" in the same directory, though this is not good practice in most cases as it can easily lead to confusion.

More tricky is the case-sensitivity of the Frame IDs used by pathway tools. The organism IDs (Database ID in the PGDB table), pathway IDs in the SAVI files (e.g. PWY-1234), and the Frame ID columns in authors.txt and organizations.txt are all examples of frame IDs. Frame IDs are used by the Pathway Tools software which is written in Common Lisp, and thus Frame IDs are Lisp symbols. Symbols in Lisp are used as funciton names, variable names, and simply as general-purpose identifiers. Lisp is best described as a case-sensitive language that pretends to be case-insensitive in most instances. Specifically, any symbol that is given to Lisp's parser will be converted to upper-case before it is interpreted, unless it is enclosed in vertical bar characters like |this|. 

Finally, most of the rest of the pipeline is written in Python and a small amount in Perl, Bash, and Java, and all of these are case-sensitive languages. And more to the point, all filenames and data, including those used as Lisp symbols in the Lisp / Pathway Tools code, are treated as strings in the Python/Perl/etc. code, making them case-sensitive regardless.

What all this means is that Database IDs (a.k.a. OrgIDs), pathway/reaction/compound names, and author / organization frame IDs will be treated case-sensitively *sometimes*. So when dealing with them, you should:
- Be consistent in referring to them by case; i.e. if you call your organism MyAwesomePlantCyc in the PGDB table, don't refer to it as mYaWesOMEpLanTcYC elsewhere.
- Don't give different PGDBs the same database ID differentiated only by case; so don't try to create MyAwesomePlantCyc and also myawesomeplantcyc as a separate databse
- Be aware that the pipeline may output MYAWESOMEPLANTCYC at times, and MyAwesomePlantCyc at others. Also the directory where your PGDB lives will be called myawesomeplantcyc. Just because.
- Be aware that by convention we *make* author IDs in particular case-sensitive and lower-case by enclosing them in vertical bars, because that's how Pathway Tools assigns these IDs by default when you carete them using its GUI. So you'll see author IDs as |hawkins| |rhee| etc. in the default config files. You aren't required to follow this convention if you don't want to; you can put e.g. Smith in the Frame ID column of authors.txt and the Authors column of the PGDB table and both will be interpreted as SMITH internally but it will all still work.

## Glossary

#### E2P2 (Ensemble Enzyme Prediction Pipeline)

Software developed by Rhee lab that predicts enzyme function from amino acid sequence data by combinding other predictors. E2P2 version 5 uses BLAST and DeepEC by default, but can be configured to use other predictors. E2P2 is the first step in the PMN pipeline.

#### Frame ID

A unique identifier for objects in the PGDB-type databases built and accessed with the Pathway Tools software (such database objects are termed "frames" in the parlance of Pathway Tools). Each pathway, protein, gene, reaction, compound, enzyme-reaction catalysis relationship, author, cited publication, and more have their own unique Frame ID within the database.

Objects that are common between databases (such as reactions, pathways, and compounds found in multiple organisms) should have the same Frame IDs in all databases in which they occur. Enzymes and genes are considered unique to their organism, however, and do not share Frame IDs with enzymes or genes from other organisms regardless of homology.

FrameIDs typically start with a letter, and may contain letters, digits, dashes, plus-signs, and underscores. Some frame IDs are human-assigned and typically descriptive in some way, such as VALDEG-PWY for the L-valine degradation pathway or RIBULOSE-5P for the compound D-ribulose 5-phosphate, but most are automatically assigned and are of the form PWY-1234 for pathways, RXN-1234 for reactions, etc. Frames that were originally created in an organism-specific database will have that organism's unique ID in the name; for example AraCyc's unique ID is "QT" so you might have RXNQT-1234 for a reaction originating from AraCyc.

There is a bit of subtlety regarding case-sensitivity of Frame IDs. Internally, the frame IDs are case-sensitive. However in most circumstances, frame IDs entered into Pathway Tools are internally converted to upper-case before being dereferenced, and thus mostly behave *as if* they were case-insensitive. So while internally "rxn-1234" and "RXN-1234" are different frame IDs, entering "rxn-1234" will in most circumstances be interpreted as "RXN-1234". This behavior can be suppressed, however, by enclosing the frame ID in vertical bars, so |rxn-1234| would be a reference to the lower-case-named frame if such a frame existed in the database.

When browsing a Pathway Genome Database via the Pathway Tools web interface, the Frame ID of the current object is usually present as part of the URL, in most cases as a URL parameter, either id= or object= depending on the type if object.

#### MetaCyc

A general reference database (PGDB) built, maintained, and distributed by SRI International. It contains pathways, enzymes, reactions, and compounds regardless of what organism they are from. Starting January 2024, access to MetaCyc requires a paid subscription.

#### OrgID (Organism ID)

A unique identifier for a pathway genome database. It is the 'cyc name without "cyc" at the end; so for AraCyc the OrgID is Ara, or for CornCyc the OrgID is Corn. OrgIDs are not case-sensitive (Corn and CORN are equivalent), but the case with which you enter them into the organism table is preserved in some instances.

#### PGDB (Pathway Genome Database)

The final output of the pipeline. These databases contain a representation of the metbolism of one<sup>\*</sup> type of organism (a species or subspecies, typically). It contains representations of all the pathways, enzymes, reactions, and compounds that are (believed to be) present in the organism according to the computational predictions made by the pipeline, as well as any pathways and enzymes from the reference database that were annotated to the organism with experimental evidence.

<sup>\*</sup> Pathway Tools does support multi-organism databases, and the reference databases PlantCyc and MetaCyc are examples of multi-organism databases. The PMN pipeline, however, is intended to produce only single-organism databases at present.

#### PlantCyc

A plant-specific reference database (PGDB) built, maintained, and distributed by the Rhee lab at Michigan State University as part of the Plant Metabolic Network (PMN) project. It contains pathways, enzymes, reactions, and compounds from species within Viridiplantae (land plants and green algae). It is used as the default reference database when creating PGDBs using the PMN pipeline.

#### RPSD (Reference Protein Sequence Dataset)

A collection of enzymes, represented as amino acid sequences and their catalyzed reactions (in the form of MetaCyc Frame IDs or EC numbers), used by E2P2 as the basis of its enzyme predictions. The RPSD has been assembled by the E2P2 developers at Rhee lab from a variety of sources, including PlantCyc, MetaCyc, BRENDA, and SwissProt. The RPSD includes only enzymes with direct experimental evidence; computationally-predicted enzymes are not included.

#### Unique ID

Each organism database has an internal "unique ID", separate from and unrelated to its Organism ID (OrgID). These are alphanumeric IDs auto-assigned by SRI International; for example AraCyc's unique ID is "QT", while ChlamyCyc's is "IO2". This ID is used when a frame (database object) is first created in an organism-specific database (rather than being first created in one of the reference databases PlantCyc and MetaCyc) as part of the frame's frame ID. So for example a reaction created in ChlamyCyc might get a frame ID like "RXNIO2-1234" while one created in AraCyc might get one like "PWYQT-4321". In contrast, frames created in the reference databases do not get a unique ID incorporated into the frame ID, such as "RXN-9876".

The PMN pipeline gets auto-assigned IDs from SRI Internaltional's server when it is run, saving them in uids.txt. Running the same step again on the same organisms ("same organisms" is determined based on the Organism IDs), it will find this file and not query the SRI server again (as doing so would generate a new set of unique IDs). You can also specify unique IDs by including the Unique IDs column in the pgdb-table.txt input file.
