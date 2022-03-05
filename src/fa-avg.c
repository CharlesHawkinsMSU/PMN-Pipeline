/* This utility counts the number of sequences in one or more fasta files, or stdin if given "-" as an argument */
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
		long total = 0;	/* Total number of bases or amino acids */
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
			{
				total += strlen(line)-1;
				after_semi = 0;
			}
		}
		if(errno)
		{
			perror(argv[i]);
		}
		else
		{
			/* This next part here gets the average and trims off trailing 0's */
			char *avg = NULL;
			asprintf(&avg, "%f", ((float)total)/((float)count));	/* Average as a string, may have trailing 0's */
			char *j=avg;	/* Our pointer. We'll find the end of the string with it, then backtrack, "deleting" the trailing 0's by making them null chars */
			while(*j) j++;	/* Find the end of the string */
			/* Backtrack through the string, while j is a '0' overwrite with null ('\0') */
			do {
				*j = '\0';
			} while (j != avg && *(--j) == '0');
			if (*j == '.') *j = '\0';	/* If we're left with a trailing '.', delete that too */
			if (argc > 2)
				printf("%s: ", argv[i]);
			printf("%s\n", avg);
			free(avg);
			fclose(infile);
		}
	}
	return 0;
}
