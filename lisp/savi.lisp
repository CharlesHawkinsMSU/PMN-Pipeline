; Rewrite of SAVI in lisp, to be executed directly from within Pathway Tools
; This version relies on a single input file, savi.txt
; The file is broken up into two sections, declarations and the pathway table
; The sections are separated by a // line
; The declarations section is a place to declare names for taxonomic groups to be referenced later, such as
; Viridiplantae="TAX-33090"
; Full taxonomic group syntax is supported (see below), including references to other variables, however a variable must be defined before it is referenced
; The pathway table contains all savitized pathways along with their calls, as a tab-delimited table
; The columns are:
; Pathway - The pathway to apply this rule to
; Add - The conditions under which to add this pathway if it was not predicted
; Remove - The conditions under which to remove this pathway if it was predicred
; Citation - Citation for the version of SAVI in which this entry was added
;
; The Remove column takes precedence over Add, though allowing them to overlap such that it would matter is not good practice
; 
; The "conditions" mentioned above are specifiers for sets of PGDBs. PGDBs may be selected on the basis of taxonomy or on the presence of enzymes for particular reactions. Multiple conditions may be combined with + and / to add or subtract the databases matching the specifier, and specifiers may be negated with ! in front of them. The specifier * means all databases.
; 
; Examples: 
;   * To add a pathway to all databases put * in the Add column
;   * To remove a pathway from Solanaceae, put TAX-4070 in Remove
;   * To remove a pathway from everything except Solanaceae, put either */TAX-4070 or !TAX-4070 in Remove
;   * To add a pathway to all databases except Cuscuta and , put */TAX-4128/TAX-1234 in Add
;   * To add a pathway to Rosids except Rosaceae, put TAX-3333/TAX-4444 in Add
;   * To remove a pathway from all species outside Lamiaceae, unless they have an enzyme for either RXN-1234 or RXN-4567, put */TAX-4321/RXN-1234/RXN-4567 in Remove 
;   * To remove a pathway from all species outside Lamiaceae and Fabaceae, unless they have enzymes for both RXN-1234 and RXN-4567, put !RXN-4567+!RXN-1234/TAX-4321/TAX-3456 in Remove 
;
; - To replicate UPP, put TAX-<embryophyta> in Add
; - To replicate CVP, put *-TAX-<embryophyta> in Add
; - To replicate NPP, put * in Remove
; - To replicate AIPP, leave Add and Remove blank
; - To replicate CAPP TAXON-INCLUDE-WITHIN for e.g. taxon 2345, put !TAX-2345 in Remove
; - To replicate CAPP RXN-SAFE for e.g. rxn-999, put !RXN-999 in Remove
; - To replicate both of the above, put !TAX-2345/RXN-999 in Remove

(defun check-savi-input-file (savi-file &key log-file (ref-dbs '(plant meta)))
  "Does a few checks on the given savi.txt input file, and flags anything that looks wrong for any pathway. The things it currently checks for are: (1) That the file is generally formatted correctly, (2) that all reactions in Required-rxns are in the pathway or its subpathways, (3) that no reactions in Required-rxns are spontaneous, (4) that the pathway appears in at least one of the ref-dbs, (5) that none of the taxon specs refer to non-existent taxons or undefined taxon aliases"
  ; todo
  )

(defun read-savi-input (path)
  "Reads the savi file from the given path"
  (read-table-with-header-as-list path))

(defun run-savi (input-table &key (kb (current-kb)) dry-run? (ref-dbs '(plant meta)))
  "Runs SAVI on the current PGDB (or another specified with :kb). Returns a plist, with :ADD as the list of pathways added and :REMOVE as the list of pathways removed (both as lists of frame IDs). If :dry-run? is t, will only return the lists but will not make the changes. If a pathway appears in both lists it will be REMOVED. Each added pathway will be imported from the first db in ref-dbs in which it is found"
  (let ((prev-org (current-orgid)))
    (so (as-orgid kb))
    (prog1
      (setq specs
            (loop for line in input-table
                  for pwy = (getf line :PATHWAY)
                  for add-spec = (getf line :ADD)
                  for remove-spec = (getf line :REMOVE)
                  for citation = (getf line :CITATION)
                  when (match-savi-spec remove-spec)
                  collect pwy into to-remove
                  else when (match-savi-spec add-spec)
                  collect pwy into to-add
                  finally (return (list :add to-add :remove to-remove))))
      (unless dry-run?
        (loop for pwy-add in (getf specs :add)
              unless (ctfp pwy-add)
              do (add-frame-from-refs pwy-add ref-dbs))
        (loop for pwy-remove in (getf specs :remove)
              when (ctfp pwy-remove)
              do (remove-frame-and-dependents pwy-remove)))
      (so prev-org))))

(defun match-savi-spec (spec)
  (loop for (minus negated frame) in (parse-savi-spec spec)
        with matched = nil
        when (xor negated (match-savi-frame frame))
        do (setq matched (not minus))
        finally (return matched)))

(defun match-savi-frame (frame)
  (when (setq frame (coerce-to-frame frame))
    (if (eq (get-frame-type frame) :instance)
      (if (instance-all-instance-of-p frame '|Reactions|)
        (or (proteins-of-reaction frame) (genes-of-reaction frame))
        t)
      (if (class-all-sub-of-p frame '|Organisms|)
        (instance-all-instance-of-p (current-orgid) frame)
        t))))

(defparameter savi-spec-component-re (excl:compile-re "([+/])?(!)?([^+/]+)"))

(defun parse-savi-spec (spec)
  (loop with start = 0
        do (multiple-value-setq (found span set-op not-op frame)
             (excl:match-re savi-spec-component-re spec :return :index :start start))
        do (setq start (rest span))
        while found
        collect (list
                  (string-equal (subseq spec (first set-op) (rest set-op)) "/")
                  not-op
                  (subseq spec (first frame) (rest frame)))))

(defun import-frame-from-refs (frame refs)
  (loop for ref in refs
        for ref-kb = (find-kb ref)
        when (ctfp frame :kb ref-kb)
        do (copy-frame frame frame :kb ref :new-kb (current-kb))
        and do (return t)
        finally (return nil)))
