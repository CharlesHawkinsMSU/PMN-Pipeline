; Rewrite of SAVI in lisp, to be executed directly from within Pathway Tools
; This version relies on a single input file, savi.txt
; The file is broken up into two sections, declarations and the pathway table
; The sections are separated by a // line
; The declarations section is a place to declare names for taxonomic groups to be referenced later, such as
; Viridiplantae="TAX-33090"
; Full taxonomic group syntax is supported (see below), including references to other variables, however a variable must be defined before it is referenced
; The pathway table contains all savitized pathways along with their calls, as a tab-delimited table
; The columns are:
; Pathway - The frame ID of the pathway
; Universal-within - Pathway is universal within this taxonomic group, and should be added if not predicted. NA is interpreted as no taxons
; Exclusive-to - Pathway is only found within this taxonomic group, and should be removed if predicted outside that group. NA is interpreted as all taxons
; Not-found-in - Pathway is not found within this taxonomic group, and should be removed if predicted within that group. NA is interpreted as no taxons
; Required-rxns - Pathway will be removed if any of these reactions do not have enzymes. Does not check if the reactions are spontaneous. Does not check that these reactions are actually in the pathway
; Citation - Citation for the version of SAVI in which this entry was added
;
; The three removal columns take precedence over Universal-within, and a pathway will be removed if any of the three indicate it should be removed
;
; Taxonomic group definitions
; These can be assigned to aliases in the declarations section or given as-is in the three taxon columns
; A taxon spec can be a taxon frame ID in quotes, e.g. "TAX-33090", a previously-defined alias, or multiple taxon specs combined with parentheses, + and - symbols
; + means set-union; a taxon matches if it is in either or both of the given groups
; - means set-difference; a taxon matches if it is in the first group but is not in the second
; + and - group from left to right, so A-B+C means (A-B)+C

(defun check-savi-input-file (savi-file &key log-file (ref-dbs '(plant meta)))
  "Does a few checks on the given savi.txt input file, and flags anything that looks wrong for any pathway. The things it currently checks for are: (1) That the file is generally formatted correctly, (2) that all reactions in Required-rxns are in the pathway or its subpathways, (3) that no reactions in Required-rxns are spontaneous, (4) that the pathway appears in at least one of the ref-dbs, (5) that none of the taxon specs refer to non-existent taxons or undefined taxon aliases"
  ; todo
  )
