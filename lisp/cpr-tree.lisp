; Functions that output various trees and tables of information about frames and classes

(defun cpr-tree (frame &key (tabs 0) (out-stream *standard-output*) max-depth (min-inst 1))
	"Get the full class tree of the given class, down to a depth of max-depth, printing only classes with at least min-inst instances. Output is printed to out-stream"
	(setq num-cprs (length (get-class-all-instances frame)))
	(if (and (>= num-cprs min-inst) (or (not max-depth) (> max-depth 0))) (progn
		(indent tabs out-stream)
		(format out-stream "~A: ~D~%" (get-name-string frame) num-cprs)
		(loop for sub-class in (get-class-direct-subs frame) do
			(cpr-tree sub-class :tabs (+ 1 tabs) :out-stream out-stream :max-depth (if max-depth (- max-depth 1) NIL) :min-inst min-inst)))))

; Old cpr-cats, based on a misunderstanding of how get-class-all-instances worked (this version did actually work but did a lot of unneccessary work to get there)
; Replace with (frames-of-each-class)
;(defun cpr-cats (frame stream &key (already-have NIL) (min-inst 1) (inst-print-fn #'get-frame-handle))
;	"List all classes that are direct or indirect subclasses of frame, and give a list of all instances of each"
;	; (format t "~A~%" min-inst)
;	(if (member frame already-have) already-have (progn
;		(if (>= (length (setq instances (get-class-all-instances frame))) min-inst) (progn 
;			(format stream "~A	~A	~{~A~^,~}~%"  (get-frame-handle frame) (get-name-string frame) (mapcar inst-print-fn instances))
;			(loop for sub-class in (get-class-all-subs frame) do
;				(setq already-have (cpr-cats sub-class stream :already-have already-have :min-inst min-inst :inst-print-fn inst-print-fn)))))
;		(cons frame already-have))))

(defun frames-of-each-class (class)
  "Gets a list of all subclasses of class (direct or indirect), the common name of that class, and a list of all members of each subclass (again, direct or indirect)"
  (loop for subclass in (get-class-all-subs class)
		for instances = (get-frame-handles (get-class-all-instances subclass))
		when (> (length instances) 0)
		collect (list (get-frame-handle subclass)
					  (get-slot-value subclass 'common-name)
					  instances)))



(defun pwy-table-file (filename) 
  (with-open-file (f filename :direction :output)
    (write-table
      (loop for p in (all-pathways) collect
            (cons (get-frame-handle p)
                  (mapcar #'get-frame-handle (mapcar #'first (get-slot-values p 'reaction-layout))))) f)))

(defun cpr-cats-file (frame filename &rest rest)
	"Wrapper for cpr-cats that opens a file and saves to it"
	(with-open-file (stream filename :direction :output :if-exists :supersede)
		(print (append (list frame stream) rest))
		(apply #'cpr-cats (append (list frame stream) rest))))

(defun cpr-tree-file (frame filename &rest rest)
	"Wrapper for cpr-tree that opens a file and saves to it"
	(with-open-file (stream filename :direction :output :if-exists :supersede)
		(apply #'cpr-tree (append (list frame :out-stream stream) rest))))

(defun inchi-table (filename)
    "Export a tab-delimited table with the InChI and InChI-Key identifiers for all compounds"
        (with-open-file (stream filename :direction :output :if-exists :supersede)
                (loop for cpd in (all-cpds) do (format stream "~A   ~A  ~A~%" (get-frame-handle cpd) (get-slot-value cpd 'inchi) (get-slot-value cpd 'inchi-key)))))

;(defun write-class-tree (class filename)
;  "Writes the class hierarchy starting with the given class to the given file in the format class(subclass1(sub-subclass1.1,sub-subclass1.2),subclass2,subclass3)"
;  (with-open-file (f filename :direction :output :if-exists :supersede)
;    (format f "~A~%" (class-tree class))))
;(defun class-tree (class &key name-slot)
;  (setq outstr (format NIL "~A" (or (get-slot-value class name-slot) (get-frame-handle class))))
;  (let ((subs (get-class-direct-subs class)))
;    (when (> (length subs) 0)
;      (setq outstr (concatenate 'string outstr (format NIL "(~{~A~^,~})" (loop for sub in subs collect (class-tree sub))))))) outstr)

(defun get-class-tree (class)
  "Returns the tree of classes rooted at 'class' as nested alists"
  (let ((subs (get-class-direct-subs class))
		(handle (get-frame-handle class)))
	  (cons handle (loop for sub in subs collect (get-class-tree sub)))))

(defun write-class-tree (class where)
  "Writes tabbed tree of classes rooted at 'class' to 'where' ('where' can be a filename, stream, or t for stdout)"
  (labels ((write-class-tree-internal (tree stream tabs)
									  (indent tabs stream)
									  (format stream "~A~%" (first? tree))
									  (loop for subtree in (rest? tree)
											do (write-class-tree-internal subtree stream (1+ tabs)))))
		  (to-file-or-stream where
							 (write-class-tree-internal (get-class-tree class) stream 0))))

(defun class-tree-with-frames (class frames &key name-slot)
  "Generates a class tree like write-class-tree, but only includes the given frames and superclasses thereof"
  (let (
        (outstr (format NIL "~A" (or (get-slot-value class name-slot) (get-frame-handle class))))
        (found (find class frames))
        (subtrees NIL)
        )
    (let ((subs (get-class-direct-subs class)))
      (setq subtrees (loop for (subanswer subfound) in (mapcar (lx (class-tree-with-frames x frames :name-slot name-slot)) subs) when subfound collect subanswer))
      (when (> (length subtrees) 0)
        (setq outstr (concatenate 'string outstr (format NIL "(~{~A~^,~})" subtrees)))
        (setq found t)
        )) (list outstr found)))

(defun find-parent-below (frame classes)
  "Finds the parent class to which 'frame' belongs that is directly below one of the classes in 'classes', where 'classes' is a list of classes"
  (labels ((find-parent-below-r (frame classes)
             (if (find frame classes)
                 t
                 (flatten (loop for p in (get-frame-direct-parents frame) with fp do
                       (setq fp (find-parent-below-r p classes))
                       (when (eq fp t) (return (list frame)))
                       when fp collect fp)))))
    (mapcar #'get-frame-handle (remove-duplicates (find-parent-below-r frame (mapcar #'coerce-to-frame classes))))))

(defun reaction-matrix (orgids &optional filename rxnlist)
  "Outputs a matrix of all reactions in the given orgids. If filename is given the list will be written to it as a tab-delimited table; otherwise it will be returned as an alist. The matrix has organsims as rows and reactions as columns. The value is 0 if the reaction is absent, 1 if it is present and has an enzyme, 2 if it is present and has no enzyme, 3 if it is spontaneous"
  (so 'meta)
  (let* ((rxn-list (if rxnlist rxnlist (mapcar #'get-frame-handle (get-class-all-instances "Reactions"))))
         (table (cons (cons "" rxn-list)
                      (for-orgids orgids
                            (cons org 
                                  (loop for rxn in rxn-list collect 
                                        (cond ((not (coercible-to-frame-p rxn)) 0)
                                              ((get-slot-values rxn 'enzymatic-reaction) 1)
                                              ((get-slot-value rxn 'spontaneous?) 0)
                                              (t 0))))))))
    (if filename
        (print-alist filename table)
        table)))
(defun frame-matrix (class orgids &optional filename)
  "Outputs a matrix of all instances of the given class in the given orgids. Alternatively you may give a list of frame handles (as symbols or strings) instead of a class. If filename is given the list will be written to it as a tab-delimited table; otherwise it will be returned as an alist. The matrix has organsims as rows and frames as columns. The value is 0 if a frame with the given name is absent from the pgdb, 1 if it is present"
  (so 'meta)
  (let* ((frame-list (if (listp class) class (mapcar #'get-frame-handle (get-class-all-instances class))))
         (table (cons (cons "" frame-list)
                      (for-orgids orgids
                            (cons org 
                                  (loop for frame in frame-list collect 
                                        (cond ((coercible-to-frame-p frame) 1)
                                              (t 0))))))))
    (if filename
        (print-alist filename table)
        table)))

(defun atted-table (filename &optional orgid)
  "Outputs a table of proteins, genes, and accessions"
  (when orgid (so orgid))
  (print-alist filename
			   (cons '("Protein-PMNID" "Gene-PMNID" "Protein-Accession" "Gene-Accession")
				 (loop for pro in (get-class-all-instances "Proteins")
					   collect (list (get-frame-handle pro)
									 (setq g (get-frame-handle (get-slot-value pro 'gene)))
									 (get-slot-value pro 'accession-1) 
									 (if g (get-slot-value g 'accession-1) NIL))))))
(defun rxn-of-pwy ()
  "Outputs the reactions of all pathways"
  (setq table (make-hash-table))
  (so 'meta)
  (loop for p in (all-pathways)
		for n = (get-frame-handle p)
		do (puthash n (get-frame-handles (get-slot-values p 'reaction-list)) table))
  (so 'plant)
  (loop for p in (all-pathways)
		for n = (get-frame-handle p)
		unless (gethash n table)
		do (puthash n (get-frame-handles (get-slot-values p 'reaction-list)) table))
  (hash-to-alist table))
  
