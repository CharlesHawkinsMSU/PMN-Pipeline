/* This utility gives the percentage of sequences in the given protein fasta file(s) that are "good" - i.e. that start with M, end with *, and have no internal *'s. It does not produce useful results on nucleic acid fasta files. It does not check for invalid fasta letters or other invalid characters in the sequence */
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>
#include <getopt.h>

const char *usage = "Usage: fa-goodseqs [-s] [-e] [-m] [-i] [-v] seq1.fa [seq2.fa ...] | fa-goodseqs -h\n";
const char *help = "This utility gives the percentage of sequences in the given protein fasta file(s) (give it \"-\" to read from stdin) that are \"good\" - i.e. that start with M, end with *, and have no internal *'s. It does not produce useful results on nucleic acid fasta files. It does not check for invalid fasta letters or other invalid characters in the sequence.\nOptions:\n-s\tDo not check that the sequence ends with a stop codon (*)\n-e\tTreat empty sequences as good (even if -m and -s are unset)\n-m\tDo not check that the sequence starts with methionine (M)\n-i\tDo not check the sequence for internal stop codons (*)\n-v\tVerbose output - print each sequence header and then a 1 or 0 if it passed or failed, respectively, before printing the grand total as usual\n-h\tPrint this help and exit\n";

int main(int argc, char **argv)
{
	/* Parse the arguments and set flags */
	int flag_ign_stop = 0;
	int flag_ign_empty = 0;
	int flag_ign_m = 0;
	int flag_ign_istop = 0;
	int flag_verbose = 0;

	int opt = -1;
	while ((opt = getopt(argc, argv, "semivh")) != -1)
	{
		switch(opt)
		{
			case 's': flag_ign_stop = 1; break;
			case 'e': flag_ign_empty = 1; break;
			case 'm': flag_ign_m = 1; break;
			case 'i': flag_ign_istop = 1; break;
			case 'v': flag_verbose = 1; break;
			case 'h':
					  printf(usage);
					  printf(help);
					  exit(0);
			default: printf(usage); exit(1);
		}
	}
	
	for(; optind<argc; optind++)
	{
		FILE *infile;
		if(!strcmp(argv[optind], "-"))
		{
			infile = stdin;
		}
		else
		{
			infile = fopen(argv[optind], "r");
		}
		if(!infile)
		{
			perror(argv[optind]);
			continue;
		}
		size_t linecap = 0;	/* Buffer capacity for getline() */
		char *line = NULL;	/* The current line */
		int len = 0;	/* Current line length */
		int count = 0;	/* Running total number of sequences */
		int count_good = 0; /* Running total number of good sequences */
		int after_seqname = 0; /* True when we've just seen a sequence name */
		int is_good = 0; /* True if the sequence we're in is still good */
		int end_stop = 0; /* True if the line ends with a stop codon */
		errno = 0;
		while((len = getline(&line, &linecap, infile)) > 0)
		{
			if(line[0] == '>')
			{
				/* If the previous sequence was empty or didn't end with a stop codon, it was bad */
				if ((after_seqname && (! flag_ign_empty)) || ! (end_stop || flag_ign_stop || (after_seqname && flag_ign_empty) ))
				{
					//printf("as: %i; fie: %i; es: %i; fis: %i\n", after_seqname, flag_ign_empty, end_stop, flag_ign_stop);
					is_good = 0;
				}
				/* Increment good count if the previous sequence was good */
				count_good += is_good;
				if(flag_verbose)
					printf("%i\n%s", is_good, line);
				//printf("%i\n", is_good);
				is_good = 1;
				after_seqname = 1;
				end_stop = 0;
				count++;
				//printf("%s", line);
			}
			else
			{
				if(len > 1)
				{
					/* To do on the first line of a sequence */
					if (after_seqname)
					{
						/* A sequence that doesn't start with M is bad */
						is_good &= (flag_ign_m || toupper(line[0]) == 'M');
						after_seqname = 0;
					}
					/* If the previous line ended in a stop but we're still in the same sequence, then that stop was an internal stop and the sequence is bad */
					is_good &= flag_ign_istop || !end_stop;
					/* Look for internal stops */
					if (! flag_ign_istop)
					{
						int l2 = len-2;
						int i;
						for (i=0; i<l2; i++)
						{
							if (line[i] == '*')
							{
								is_good = 0;
								break;
							}
						}
					}
					/* Set flag for the line ending in a stop */
					end_stop = (line[len-2] == '*');
				}
			}
		}
		/* If the previous sequence was empty or didn't end with a stop codon, it was bad */
		if ((after_seqname && (! flag_ign_empty)) || ! (end_stop || flag_ign_stop || (after_seqname && flag_ign_empty) ))
			is_good = 0;
		/* Increment good count if the previous sequence was good */
		count_good += is_good;
		if(flag_verbose)
			printf("%i\n", is_good);
		//printf("%i\n", is_good);
		is_good = 1;
		after_seqname = 1;
		end_stop = 0;
		//printf("%s", line);
		if(errno)
		{
			perror(argv[optind]);
		}
		else
		{
			/* This next part here gets the percentage and trims off trailing 0's */
			char *pct = NULL;
			asprintf(&pct, "%f", (100*(float)count_good)/((float)count));	/* Average as a string, may have trailing 0's */
			char *j=pct;	/* Our pointer. We'll find the end of the string with it, then backtrack, "deleting" the trailing 0's by making them null chars */
			while(*j) j++;	/* Find the end of the string */
			/* Backtrack through the string, while j is a '0' overwrite with null ('\0') */
			do {
				*j = '\0';
			} while (j != pct && *(--j) == '0');
			if (*j == '.') *j = '\0';	/* If we're left with a trailing '.', delete that too */
			if (argc > 2)
				printf("%s: ", argv[optind]);
			printf("%s%% (%i/%i)\n", pct, count_good, count);
			free(pct);
			fclose(infile);
		}
	}
	return 0;
}
