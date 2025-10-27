; Functions that help curate information into the databases

; Functions to import the compound table to compounds. They are used in curating the PMN Curation Form google doc into PlantCyc

(defparameter *smiles-filename* "tmp.cpds")
(defparameter *mol-filename* "tmp.mol")
; import-smiles-table
; 
; This function imports a compounds table file.
;
; The table should be a two-column tsv with cpd names in the first
; column and cpd structures in the second. The structures can be in
; smiles or inchi format, or anything else Open Babel can read and
; will fit in a tsv cell (specify with :chemformat; see the output
; of 'obabel -L formats read' for a list of them)
; 
; Open Babel (https://openbabel.org) must be installed and runnable as obabel
;
; This function may not work on Windows

(defun import-cpd-table (filename &key (chemformat "smiles"))
  (format t "Looking for external command obabel~%")
  (if (= 0 (excl:run-shell-command "which obabel")) ; 0 returned mean success; i.e. the command obabel exists
    (loop for (molname smiles) in (read-alist filename)
	  do (write-list (list smiles) *smiles-filename*)
	  do (excl:run-shell-command (format nil "rm -f ~A" *mol-filename*))
	  when (= 0 (excl:run-shell-command (format nil "obabel -i~A ~A -omol -O~A --gen2D" chemformat *smiles-filename* *mol-filename*)))
	  do (setq newmol (create-instance-w-generated-id "Compounds"))
	  and do (read-mdl-molfile newmol :filename *mol-filename*)
	  and do (put-slot-value newmol 'common-name molname)
	  collect newmol)
    (format t "Suggestion: Install obabel from package manager or https://openbabel.org~%")))

(defun rxn-from-formula (formula)
  (loop for tok in (excl:split-re whitespace-re formula)
	with state = :expect-stoich
	with side = :left
	with left = nil
	with right = nil
	with dir = nil
	with stoi = nil
	do (cond
	     ((eq state :expect-stoich)
	      (cond
		((setq stoi (handler-case
			      (parse-integer tok)
			      (parse-error nil))) 
		 (setq state :expect-cpd)
		 (when (< stoi 1)
		   (error (format nil "Stoichiometry must be an integer greater than or equal to 1; got ~A instead" stoi))))
		(t (error (format nil "Expected stoichiometry, which should be an integer greater than or equal to 1; got ~A instead" tok)))))
	     ((eq state :expect-cpd)
	      (if (coercible-to-frame-p tok)
		(if (eq side :left)
			(setq left (append left (list (list stoi tok))))
			(setq right (append right (list (list stoi tok)))))
		(error (format nil "Expected compound frame; ~A is not a compound in ~ACYC" tok (current-orgid))))
	      (setq state :expect-op))
	     ((eq state :expect-op)
	      (cond
		((equal tok "->")
		 (when (eq side :right)
		   (error "Multiple reaction-side-delimiting tokens (<-, ->, =)"))
		 (setq dir :left-to-right)
		 (setq side :right))
		((equal tok "<-")
		 (when (eq side :right)
		   (error "Multiple reaction-side-delimiting tokens (<-, ->, =)"))
		 (setq dir :right-to-left)
		 (setq side :right))
		((equal tok "=")
		 (when (eq side :right)
		   (error "Multiple reaction-side-delimiting tokens (<-, ->, =)"))
		 (setq dir :reversible)
		 (setq side :right))
		((equal tok "+") t)
		(t
		  (error (format nil "Expected + or reaction-side delimiter; got \"~A\" instead" tok))))
	      (setq state :expect-stoich)))
	finally (return (list left right dir))
	))
