## Protein-to-gene mapping from FASTA

* **FASTA Sep**: The separator that separates fields in the fasta header; such as a space, tab (enter as \t), or vertical bar. Default is space
* **FASTA Field**: Which field contains the gene ID. Can be a named field, e.g. locus for locus=gene12345, or a number for non-named fields, in which case the fields will be counted from 0 (the protein accession). Default is gene
* **FASTA KV**: The key-value separator for named fields. E.g. for gene=gene12345 you'd enter "=", or for locus:gene12345 you'd enter ":". Default is =

## Protein-to-gene mapping from GFF

* **GFF File**:  The file name of the GFF file to use; put it in the gff/ directory
* **GFF Prot Feature**: What feature type refers to a protein, looking at the third column in the GFF file. Default is "mRNA"
* **GFF Prot Name**: What field in the attributes column contains the protein accession. Should match the protein names found in the fasta file. Default is "Name"
* **GFF Path**: How to get from the protein feature to the corresponding gene feature and find its accession. It is a dot-separated list of attribute names. All but the last are treated as references to other features in the file, while the last has its value used as-is as the accession. Example Parent.Name
* **GFF Key**: What field is used for references like Parent. It should be ID (the default here) according to the GFF spec but some files ignore this and use something else, hence this option
