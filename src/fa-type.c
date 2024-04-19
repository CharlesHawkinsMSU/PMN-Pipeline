/* This utility tries to classify an input FASTA file as nucleic acid or amino acid, and can say whether there are any ambiguous monomers in it (i.e. N, Y, etc. for nucleic acid, X for amino acid) */
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>
#include <getopt.h>

const char *usage = "Usage: fa-type [-n] seq1.fa [seq2.fa ...] | fa-type -h\n";
const char *help = "This utility tries to classify an input FASTA file as nucleic acid or amino acid, and can say whether there are any ambiguous monomers in it (i.e. N, Y, etc. for nucleic acid, X/Z/B/J for amino acid).\nOptions:\n\t-h  Display this help message and exit\n\t-n  Do not report on whether ther are wildcard/ambiguous bases/amino acids such as N or X, only report NA or AA\n\t-v  Make a separate determination for each sequence in the file\n";

const char *na_chars = "ATUCGRYKMSWBDHVN";
const int na_ambig = 5; /* NA's at and after this position in na_chars are ambiguous, so by searching for a char in na_chars, the position found at will tell us if it's an ambiguous/wildcard NA or not */
/* The set of valid AA chars is all alphabetic chars plus *, so we can test this with isalpha(c) || c == '*' and don't need an aa_chars variable */
const char *aa_ambig = "JZXB";

#define YN(x) (x)?'Y':'N'

int main(int argc, char **argv)
{
	/* Parse the arguments and set flags */
	int flag_ign_amb = 0;
	int flag_verbose = 0;

	int opt = -1;
	while ((opt = getopt(argc, argv, "nvh")) != -1)
	{
		switch(opt)
		{
			case 'n': flag_ign_amb = 1; break;
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
		printf("%s\n", argv[optind]);
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
		int seq_has_aa = 0; /* True if the sequence we're in has amino-acid-only characters */
		int seq_has_amb_aa = 0; /* True if the sequence we're in has X (unknown AA) characters */
		int seq_has_amb_na = 0; /* True if the sequence we're in has characters that may indicate ambiguous nucleic acids (N, Y, G, etc.) */
		int seq_has_gaps = 0; /* True if the sequence we're in has -, or gaps */
		int seq_has_invalid = 0; /* True if the sequence we're in has characters that aren't valid FASTA chars */
		
		int file_has_aa = 0; /* True if the file we're in has amino-acid-only characters */
		int file_has_amb_aa = 0; /* True if the file we're in has X (unknown AA) characters */
		int file_has_amb_na = 0; /* True if the file we're in has characters that may indicate ambiguous nucleic acids (N, Y, G, etc.) */
		int file_has_gaps = 0; /* True if the file we're in has -, or gaps */
		int file_has_invalid = 0; /* True if the file we're in has characters that aren't valid FASTA chars */
		char aa_char = '\0';
		char amb_aa_char = '\0';
		char amb_na_char = '\0';
		char invalid_char = '\0';
		errno = 0;
		int notfirstseq = 0;
		while((len = getline(&line, &linecap, infile)) > 0)
		{
			if(line[0] == '>')
			{
				if(flag_verbose)
				{
					if (notfirstseq)
					{
						if (seq_has_invalid)
							printf("    Invalid\n");
						else
						{
							char *amb_str;
							if (flag_ign_amb)
								printf("    %s\n", seq_has_aa?"AA":"NA");
							else if (seq_has_aa)
							{
								if (seq_has_amb_aa)
									printf("    AA (%c), ambiguous (%c)%s\n", aa_char, amb_aa_char, seq_has_gaps?", with gaps":"");
								else
									printf("    AA (%c)%s\n", aa_char, seq_has_gaps?", with gaps":"");
							}
							else
								printf("    NA%s%s\n", seq_has_amb_na?", ambiguous":"", seq_has_gaps?", with gaps":"");
						}
					}
					else
						notfirstseq = 1;
					printf("  ");
					printf(line);
				}
				file_has_aa |= seq_has_aa;
				file_has_amb_aa |= seq_has_amb_aa;
				file_has_amb_na |= seq_has_amb_na;
				file_has_gaps |= seq_has_gaps;
				file_has_invalid |= seq_has_invalid;
				seq_has_aa = seq_has_amb_aa = seq_has_amb_na = seq_has_invalid = 0;
			}
			else
			{
				if(len > 1)
				{
					/* Go through the sequence and classify all chars */
					int l2 = len-1; /* Length of line excluding \n */
					for (int i = 0; i < l2; i++)
					{
						char c = toupper(line[i]);
						char *ptr_in_na_chars = strchr(na_chars, c);
						if (ptr_in_na_chars)
						{
							if (ptr_in_na_chars-na_chars > na_ambig)
							{
								seq_has_amb_na = 1;
								amb_na_char = c;
							}
						}
						else if (isalpha(c) || c == '*')
						{
							aa_char = c;
							seq_has_aa = 1;
							if (strchr(aa_ambig, c))
							{
								seq_has_amb_aa = 1;
								amb_aa_char = c;
							}
						}
						else if (c == '-')
							seq_has_gaps = 1;
						else
						{
							seq_has_invalid = 1;
							invalid_char = c;
						}
					}
				}
			}
		}
		if(errno)
		{
			perror(argv[optind]);
		}
		else
		{
			if (file_has_invalid)
				printf("  Invalid (%c)\n", invalid_char);
			else
			{
				if (flag_ign_amb)
					printf("  %s\n", file_has_aa?"AA":"NA");
				else if (file_has_aa)
					printf("  AA (%c)%s%s\n", aa_char, file_has_amb_aa?", ambiguous":"", file_has_gaps?", with gaps":"");
				else
					printf("  NA%s%s\n", file_has_amb_na?", ambiguous":"", file_has_gaps?", with gaps":"");
			}
			fclose(infile);
		}
	}
	return 0;
}
