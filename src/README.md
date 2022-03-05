# Fasta utilities
These are a few small utilities written in c for getting some stats on protein fasta files. Pre-compiled Linux binaries of all three are provided in the `bin` directory but if you want to compile them yourself you can just do `cc fa-countseqs.c -o fa-countseqs` and likewise for the other two. None of these are required to use the pipeline but they can be useful sanity checks to run, especially after using extract-proteins.py or doing any custom processing on your fasta file.

The three utilities are:

* fa-countseqs: Counts the number of sequences in one or more fasta files (a separate total for each file). This task could be accomplished with grep -c but fa-countseqs has a few advantages:
	* It is about 8x faster than grep
	* It avoids the possibility of a catastrophic mistake with grep (that mistake being `grep -c > proteins.fa`)
	* It correctly supports multi-line fasta headers that start with ";" (admittedly this is extremely rare fasta syntax)
* fa-avg: Gives the average length in amino acids of the sequences in one or more fasta files (a separate average for each file). Values in the 300-500 range are reasonable for plants; green algae can get a bit higher. Much smaller values may suggest the file does not have whole proteins
* fa-goodseqs: Gives the number and percent of amino acid fasta sequences in one or more fasta files that are "good". A good sequence starts with methionine ("M" or "m"), ends with a stop ("*") and has no internal stops. You can turn off these checks individually. Run it with `-h` to see the options.
