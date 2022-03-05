/* This utility gives the average sequence length in one or more fasta files, or stdin if given "-" as an argument */
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>

int main(int argc, char **argv)
{
	for(int i=1; i<argc; i++)
	{
		FILE *infile;
		if(!strcmp(argv[i], "-"))
		{
			infile = stdin;
		}
		else
		{
			infile = fopen(argv[i], "r");
		}
		if(!infile)
		{
			perror(argv[i]);
			continue;
		}
		size_t linecap = 0;	/* Buffer capacity for getline() */
		char *line = NULL;	/* The current line */
		int len = 0;	/* Current line length */
		int count = 0;	/* Running total number of sequences */
		int after_semi = 0;	/* True if the previous line was semicoloned - used because sequential semicolon lines only count as one */
		errno = 0;
		while((len = getline(&line, &linecap, infile)) > 0)
		{
			if(line[0] == '>')
			{
				count++;
				after_semi = 0;
			}
			else if(line[0] == ';')
			{
				if (!after_semi)
				{
					count++;
					after_semi = 1;
				}
			}
			else
				after_semi = 0;
		}
		if(errno)
		{
			perror(argv[i]);
		}
		else
		{
			if (argc > 2)
				printf("%s: ", argv[i]);
			printf("%i\n", count);
			fclose(infile);
		}
	}
	return 0;
}
