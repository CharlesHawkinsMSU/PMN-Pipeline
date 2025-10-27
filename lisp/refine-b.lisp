; Functions for running refine-b for the PMN pipeline. Refine-b is the step that pulls proteins and pathways from reference databases into a newly-created database if they are annotated to the species, where the reference databases are metacyc, plantcyc, and, in the case of the incrementally-updated pgdbs, the previous version of the same pgdb

(defparameter *secondary-ref-pwy-tmp-file* "/tmp/sec-ref-pwys.lisp")
(defparameter *secondary-ref-enz-tmp-file* "/tmp/sec-ref-enzs.lisp")
(defun update-secondary-refdb (tax-range dst-db &optional (src-db 'meta))
  "Updates a secondary ref db (such as PlantCyc) using a primary ref db (usually MetaCyc). This function should be run after propagating metacyc updates. This function will copy over any pathways and enzymes from the src-db that are within tax-range to the dst-db, but only if they do not already exist in dst-db. The list of species for each pathway will be trimmed to only include taxa that fall within tax-range after import. This function does not propagate changes to any frames that are already in dst-db, or look for new enzrxns for enzymes that are already in dst-db, because propagate metacyc updates should have already done these things. Enzymes and pathways are exported to separate files because mixing them together makes pathway tools sad (it causes pathway tools to try and add multiple enzymes to the same enzrxn, which is an error)"
  (let ((src-kb (as-kb src-db))
	(dst-kb (as-kb dst-db))
	(src-orgid (as-orgid src-db))
	(dst-orgid (as-orgid dst-db)))
    (so dst-orgid)
    ; Get list of pathways to propagate
    (setq export-pwys
	  (loop for pwy in (get-class-all-instances '|Pathways| :kb src-kb)
		when (loop for species in (get-slot-values pwy 'species)
			   thereis (class-all-sub-of-p species tax-range))
		unless (coercible-to-frame-p (get-frame-handle pwy) :kb dst-kb)
		collect pwy))
    ; Get list of enzymes to propagate
    (setq export-enz
	  (loop for enz in (get-class-all-instances '|Proteins| :kb src-kb)
		for species = (get-slot-value enz 'species)
		when species
		when (class-all-sub-of-p species tax-range)
		unless (coercible-to-frame-p (get-frame-handle enz) :kb dst-kb)
		collect enz))
    ; Export from src db
    (so src-orgid)
    (format t "Exporting ~A pathways and supporting frames from ~ACYC to ~A~%" (length export-pwys) src-orgid *secondary-ref-pwy-tmp-file*)
    (to-file-or-stream *secondary-ref-pwy-tmp-file*
		       (export-frames-to-file (append export-pwys)
					      stream
					      nil))
    (format t "Exporting ~A enzymes and supporting frames from ~ACYC to ~A~%" (length export-enz) src-orgid *secondary-ref-enz-tmp-file*)
    (to-file-or-stream *secondary-ref-enz-tmp-file*
		       (export-frames-to-file (append export-enz)
					      stream
					      nil))
    ; Import into dst db
    (so dst-orgid)
    (format t "Importing the pathway frames back into ~ACYC~%" dst-orgid)
    (import-pathway-frames-from-file *secondary-ref-pwy-tmp-file*)
    (format t "Importing the enzyme frames back into ~ACYC~%" dst-orgid)
    (import-pathway-frames-from-file *secondary-ref-enz-tmp-file*)
    (format t "Removing species annotations from imported pathways that are not within ~A~%" (or (gsv tax-range 'common-name) (gfh tax-range)))
    ; Delete species annotations that are out of scope for dst db
    (loop for pwy in export-pwys
	  do (loop for species in (get-slot-values pwy 'species)
		   unless (class-all-sub-of-p species tax-range)
		   do (remove-slot-value pwy 'species species)))
    (list (to-handles export-pwys) (to-handles export-enz))))


(defun frame-is-comp (frame)
  "Returns t iff the given frame has only computational evidence (EV-COMP)"
  (loop for c in (gsvs frame 'citations) every (search "EV-COMP" c)))

(defmacro find-frames (class form)
  "Finds all frames in class for which form evaluates to true; the varuable FRAME is the frame under examination"
  `(loop for frame in ,class when ,form collect frame))

(defparameter *console-update-interval* 500)
(defun delete-old-e2p2-citations (cur-re)
  (format t "Removing old e2p2 citations from enzrxns~%")
  (loop for ezr in (all-enzrxns)
	with n = (length (all-enzrxns))
	for i from 1 to n
	when (= 0 (mod i *console-update-interval*))
	do (format t "Checking enzrxn ~A / ~A~C" i n #\return)
	sum (loop for cit in (gsvs ezr 'citations)
		    when (excl:match-re e2p2-re cit)
		    unless (excl:match-re cur-re cit)
		    do (remove-slot-value ezr 'citations cit)
		    and count t)))

(defun delete-unsupported-enzrs ()
  (format t "Removing enzrxns that are now unsupported by any citations~%")
  (loop for ezr in (all-enzrxns)
	with n = (length (all-enzrxns))
	for i from 1 to n
	when (= 0 (mod i *console-update-interval*))
	do (format t "Checking enzrxn ~A / ~A~C" i n #\return)
	when (= 0 (length (gsvs ezr 'citations)))
	do (delete-frame-and-dependents ezr)
	and count t))

(defun delete-unsupported-enzymes ()
  (format t "Removing enzymatic proteins that no longer have any enzrxns~%")
  (loop for p in (gcai '|Proteins|)
	with n = (length (gcai '|Proteins|))
	for i from 1 to n
	when (= 0 (mod i *console-update-interval*))
	do (format t "Checking protein ~A / ~A~C" i n #\return)
	unless (or (gsv p 'component-of)
		   (gsv p 'catalyzes)
		   (gsv p 'citations)
		   (gsv p 'comment)
		   (gsv p 'appears-in-left-side-of)
		   (gsv p 'appears-in-right-side-of))
	do (delete-frame-and-dependents p)
	and count t))

(defun delete-orphan-genes ()
  (format t "Removing genes that no longer have any proteins~%")
  (loop for g in (orphan-genes)
	with n = (length (orphan-genes))
	for i from 1 to n
	when (= 0 (mod i *console-update-interval*))
	do (format t "Deleting gene~A / ~A~C" i n #\return)
	do (delete-frame-and-dependents g)
	count t))

; Run this one for incrementally-updated PGDBs before the incremental upgrade
(defun delete-old-e2p2-predictions (current-year)
  (format t "Deleted ~A citations~%Deleted ~A enzymatic-reactions~%Deleted ~A enzymes~%Deleted ~A genes~%"
	  (delete-old-e2p2-citations (excl:compile-re (format nil "^E2P2PMN~A" current-year)))
	  (delete-unsupported-enzrs)
	  (delete-unsupported-enzymes)
	  (delete-orphan-genes)))

(defun enz-is-e2p2 (enz cur-re)
  (loop for ezr in (gsvs enz 'catalyzes)
	always (loop for cit in (citations-for-enz-and-enzrs-and-gene enz)
		     always (and (excl:match-re e2p2-re cit)
				 (not (excl:match-re cur-re cit))))))

(defun get-e2p2-ezrs (cur-re)
  (loop for ezr in (gcai '|Enzymatic-Reactions|)
	when (loop for cit in (citations-for-enz-and-enzrs-and-gene ezr)
		     always (and (excl:match-re e2p2-re cit)
				 (not (excl:match-re cur-re cit))))
	collect ezr))

(defun get-e2p2-enz (cur-re)
  (loop for enz in (all-enzymes)
	unless (gsv enz 'component-of)
	unless (gsv enz 'comment)
	unless (gsv enz 'components)
	unless (and (setq g (gsv enz 'gene)) (or (gsv g 'comment)))
	when (enz-is-e2p2 enz cur-re)
	collect enz))

(defun orphan-genes ()
  (loop for g in (all-genes) unless (gsv g 'product) unless (gsv g 'comment) collect g))

;(defun delete-old-e2p2-predictions (current-year &key orphan-genes-okay?)
;  (let* ((e2p2-cur-re (excl:compile-re (format nil "^E2P2PMN~A" current-year)))
;	(e2 (get-e2p2-enz e2p2-cur-re))
;	(e2z (get-e2p2-ezrs e2p2-cur-re)))
;    (if (and (not orphan-genes-okay?) (> (length (orphan-genes)) 0))
;      (format t "Orphan genes before start~%")
;      (progn
;	(loop for ez in e2z
;	      do (format t "Deleting frame ~A~%" (gfh ez))
;	      do (delete-frame-and-dependents ez))
;	(loop for e in e2
;	      do (format t "Deleting frame ~A~%" (gfh e))
;	      do (delete-frame-and-dependents e))
;	(loop for g in (orphan-genes)
;	      do (format t "Deleting frame ~A~%" (gfh g))
;	      do (delete-frame-and-dependents g))))))


(defun get-old-comp-enz (match-new)
  "Gets the set of enzymes that don't match the given string match-new anywhere in 'NAMES (should be a substring that will be found in all new frame names) and have no experimental evidence (in the frame or its enzrs) and isn't part of a complex"
  (loop for enz in (all-enzymes)
		unless (loop for name in (gsvs enz 'names)
					 thereis (search match-new name))
		unless (gsv enz 'component-of)
		unless (enz-has-ev enz)
		collect enz))
(defun get-old-exp-enz (match-new)
  "Gets the set of enzymes that don't match the given string match-new anywhere in 'NAMES (should be a substring that will be found in all new frame names) and have experimental evidence (in the frame or its enzrs) or is part of a complex"
  (loop for enz in (all-enzymes)
		unless (loop for name in (gsvs enz 'names)
					 thereis (search match-new name))
		when (or (gsv enz 'component-of)
				 (enz-has-ev enz))
		collect enz))

(defun find-duplicate-frames (frameset)
  "Finds frames with the same accession-1. Give it '|Genes| to find duplicate genes or '|Proteins| for duplicate proteins"
  (loop for acc being the hash-keys
		in (loop for g in (expand-frameset frameset)
				 with acc_to_gene = (make-hash-table :test 'equal)
				 for acc = (gsv g 'accession-1)
				 do (append-hash-list acc (gfh g) acc_to_gene)
				 finally (return acc_to_gene))
		using (hash-value g-list)
		when (< 1 (length g-list))
		collect (cons acc g-list)))

(defun rename-enz-with-map (enz mapfile &key dry-run? manual-mappings)
  "Loads the given tab-separated file mapping from old accessions to new accessions, and renames all enzymes in the given set enz using the mapping. If there were pre-existing enz frames with the accession, the frames will be merged. Multiple mappings in the input file for the same old accession generate an error; you will have to provide entries in :manual-mappings in the form of '((oldacc1 newacc1) (oldacc2 newacc2))"
  (make-indexes-if-needed)
  (let ((enzmap (loop for entry in (read-alist mapfile)
					  with mapping = (make-hash-table :test 'equal)
					  do (append-hash-list (first entry) (second entry) mapping)
					  finally (return mapping)))
		(prot-index (gethash (as-orgid (current-kb)) *prot-index*))
		(gene-index (gethash (as-orgid (current-kb)) *gene-index*))
		(enz (get-frame-handles (expand-frameset enz))))
	(loop for e in enz
		  for eh = (gfh e)
		  for acc = (gsv e 'accession-1)
		  for is-gene = (instance-all-instance-of-p e '|Genes|)
		  for index = (if is-gene gene-index prot-index)
		  for new-name = (or (rest (assoc acc manual-mappings :test 'equal))
							 (gethash acc enzmap))
		  do (format t "~A: ~A -> ~A~%" eh acc new-name)
		  when (and (< 1 (length new-name)) (not dry-run?))
		  do (error (format nil "~A: Multiple mappings: ~A. Please resolve with, e.g. :manual-mappings '~A~%" e new-name (append manual-mappings `(,e ,(first new-name)))))
		  when new-name
		  do (setq merge-with (gethash (first new-name) index))
		  and do (setq merge-with-h (to-handles merge-with))
		  and do (unless dry-run?
				   (put-slot-value e 'accession-1 (first new-name)))
		  and do (if merge-with
				   (progn
					 (format t "~A will be merged with ~A~%" eh merge-with-h)
					 (when (and (coercible-to-frame-p e) (not dry-run?))
					   (if is-gene
						 (merge-gene-frames merge-with e :interactive? nil)
						 (merge-protein-frames merge-with e :interactive? nil))))
				   (if (< 1 (length new-name))
					 (format t "~A: Multiple mappings: ~A~%" eh new-name)
					 (format t "~A does not need to be merged~%" eh))))))




(defun enzrxn-with-ev-not-in-ref (&key (kb (current-kb)) (refs '(meta plant)))
  "Returns a list of enzrxns in the kb given by :kb (default is the curent kb) with experimental evidence that are not found in the reference kbs given by :refs (default '(plant meta))"
  (let ((prev-org (as-orgid (current-kb))))
	(loop for ref in refs do (so (as-orgid ref)))
	(so (as-orgid kb))
	(prog1
	  (let ((kb (as-kb kb))
			(refs (loop for ref in refs collect (as-kb ref))))
		(loop for ez in (all-enzrxns)
			  when (loop for c in (gsvs ez 'citations)
						 thereis (or (search "EV-EXP" c)
									 (search "EV-AS" c)))
			  unless (loop for ref-kb in refs thereis (coercible-to-frame-p (gfh ez :kb kb) :kb ref-kb) )
			  collect ez))
	  (so prev-org))))

(defvar *gene-index* (make-hash-table) "Genes indexed by accession-1 and accession-2. Maps from orgid to the actual hash tables")
(defvar *prot-index* (make-hash-table) "Proteins indexed by accession-1 and accession-2. Maps from orgid to the actual hash tables")
(defparameter *refine-b-tmp-file* "/tmp/refine-b-tmp.lisp")

(defun refine-b-check (&optional (kb (current-kb)) &key (ref-kbs '(plant meta)))
  "Checks if the current organism (or the one given in :kb) could have any experimental evidence in the reference kbs (by default, metacyc and plantcyc. Errs on the side of false-positives"
  (let ((kb (as-kb kb))
		(orgid (as-orgid kb))
		(ref-kbs (mapcar #'as-kb ref-kbs))
		(ref-orgids (mapcar #'as-orgid ref-kbs)))
	(loop for ref-orgid in ref-orgids do (so ref-orgid))
	(so orgid)
	(let ((taxa (up-to-species (get-taxon orgid))))
	  (if taxa
		(loop for ref-kb in ref-kbs
			  thereis (loop for taxon in taxa
							thereis (coercible-to-frame-p taxon :kb ref-kb)))
		t))))

(defun refine-b (&key (kb (current-kb)) (ref-kbs '(plant meta)) choices dry-run? incremental?)
  "Runs refine-b on the given orgid. Searches for enzymes and pathways in the reference kbs in :refs (by default metacyc and plantcyc) that are annotated to the organism of the pgdb :kb (default is the currently-selected organism). It then imports all of these frames from the reference kbs into the current kb. If a frame is in multiple of the reference kbs and is annotated to the species of :kb in all of them, then reference kb's toward the front of the :refs list are preferred as sources for import. Refine-b will attempt to find existing frames in :kb that match the imports and replace them with the improted frames. For pathways only a matching frame ID is checked. For enzymes, accession-1, accession-2, and synonyms in :kb are checked against accession-1 and accession-2 in :refs if a matching frame-ID is not found. Both the enzyme and its gene are checked for matches. The :choices argument lets you manually specify matching frames between :kb and :refs. It is commonly used when a gene can be matched between :kb and :refs but one or more of its proteins cannot or vice-versa (which would cause an error). It should be an alist from orgids to a list of pairs (or triplets) where the first value is the frame ID in the orgid and the second is the corresponding frame ID in the reference kb. A third value, if present, is the reference kb to draw from; otherwise the first kb in :refs that has the given frame ID will be used. Example: '((myorg (myorgg-22432 g-1391) (myorgg-3481-MONOMER g-1925-MONOMER plant) (myorgg-1119 g-5481 meta)) (someotherorg (soo-5879 g-3911))). Only frame-ids are accepted, so it is not necessary to specify if the mapping is for a gene or an enzyme. Entries in :choices override matches made based on accessions or synonyms, but not frame-id since that could create a conflict"
  (when (eq '|Multi-Organism-Groupings| (get-frame-handle (first (get-frame-direct-parents (as-orgid kb) :kb (as-kb kb)))))
    (error "Refine-b should be run on a single-organism database; ~ACYC seems to be a multi-organism database~%" (as-orgid kb)))
  (let* ((taken-frames (empty-set))
	 (org (as-orgid kb))
	 (kb (as-kb kb))
	 (matching-pathways
	   (loop for ref in ref-kbs
		 collect (list (as-orgid ref)
			       (loop for frame in (get-frames-for-taxon kb :ref ref :class "Pathways")
				     unless (set-member frame taken-frames)
				     do (add-to-set frame taken-frames)
				     and collect frame))))
	 (matching-proteins
	   (unless dry-run?(loop for ref in ref-kbs
		 collect (list (as-orgid ref)
			       (loop for frame in (get-frames-for-taxon kb :ref ref :class "Proteins")
				     unless (set-member frame taken-frames)
				     do (add-to-set frame taken-frames)
				     and collect frame))))))
    (unless dry-run?
      (if incremental?
	(loop for (ref pwys) in matching-pathways
	      when pwys
	      do (import-pwys-from-ref-incremental pwys :kb kb :ref ref))
	(loop for (ref pwys) in matching-pathways
	      when pwys
	      do (import-pwys-from-ref pwys :kb kb :ref ref))))
    (unless incremental? (loop for (ref prots) in matching-proteins
			       when prots
			       do (import-prots-from-ref prots :kb kb :ref ref :choices choices :dry-run? dry-run? )))
    (so org)
    (values matching-pathways matching-proteins)))

(defun import-pwys-from-ref (pwys &key kb ref)
  "Exports the given pathways from the given ref to a temp file and reimports into kb"
  (let ((ref-org (as-orgid ref))
		(kb-org (as-orgid kb)))
	(so ref-org)
	(with-open-file (tmpfile *refine-b-tmp-file* :direction :output :if-exists :supersede)
	  (format t "Exporting ~{~A~^, ~} with taxon ~A~%" (to-handles pwys) (get-taxon kb))
	  (export-frames-to-file pwys tmpfile nil))
	(so kb-org)
	(let ((*warn-interactive?* NIL))
	  (import-pathway-frames-from-file *refine-b-tmp-file*))
	(delete-file (probe-file *refine-b-tmp-file*))))
(defun import-pwys-from-ref-incremental (pwys &key kb ref)
  "If a given pathway is not present in kb, exports it from the given ref to a temp file and reimports into kb; it it is present, imports any experimental citations from ref kb that aren't in the current kb"
  (let ((ref-org (as-orgid ref))
	(kb-org (as-orgid kb)))
    (so kb-org)
    (setq pwys-to-export
	  (loop for p in pwys
		when (coercible-to-frame-p p)  ; Pathway is present in dest kb; import citations
		do (format t "Importing citations from ~A~%" p)
		and do (loop for cit in (get-pwy-citations-for-taxon p (first (get-frame-direct-parents (as-orgid kb) :kb (as-kb kb))))
			     for cit-cpts = (excl:split-re colon-re cit)
			     unless (loop for existing-cit in (gsvs p 'citations)
					  for ex-cit-cpts = (excl:split-re colon-re existing-cit)
					  thereis (equal (first cit-cpts) (first ex-cit-cpts)))
			     do (add-slot-value p 'citations cit))
		else collect p))

    ; The rest are not present in dest kb; import them from ref
    (so ref-org)
    (format t "Importing new pathways ~A~%" pwys-to-export)
    (with-open-file (tmpfile *refine-b-tmp-file* :direction :output :if-exists :supersede)
      (format t "Exporting ~{~A~^, ~} with taxon ~A~%" (to-handles pwys-to-export) (get-taxon kb))
      (export-frames-to-file pwys-to-export tmpfile nil))
    (so kb-org)
    (let ((*warn-interactive?* NIL))
      (import-pathway-frames-from-file *refine-b-tmp-file*))
    (delete-file (probe-file *refine-b-tmp-file*))))

(defun import-prots-from-ref (prots &key kb ref choices dry-run?)
  "Exports the given proteins and their genes from the given ref kb and reimports them into kb. Tries to match them with existing genes and proteins in the given kb"
  (make-indexes-if-needed :kb kb)
  (let* ((ref-org (as-orgid ref))
		 (dest-org (as-orgid kb))
		 (ref-kb (as-kb ref))
		 (dest-kb (as-kb kb))
		 (genes-from-ref (empty-set))        ; Set of genes that will be imported from the ref kb
		 (prots-from-ref (empty-set))        ; Set of proteins that will be imported from the ref kb
		 (genes-from-kb (empty-set))         ; Set of genes from the dest kb that will be deleted to be replaced by genes-from-ref
		 (prots-from-kb (empty-set))         ; Set of prots from the dest kb that will be deleted to be replaced by prots-from-ref
		 (kb-gene-to-ref (make-hash-table))  ; Map from gene frames in the dest kb to the same gene in the ref kb. Used when handling "orphan" genes
		 (orphaned-prots (make-hash-table))  ; Set of proteins in the dest kb that will have their genes replaced but will not be replaced themselves. Will have their gene set to a newly-imported gene using kb-gene-to-ref and genes-for-prots
		 (kb-choices (rest (assoc dest-org choices)))
		 (genes-for-prots (make-hash-table)) ; Map from ref prots to genes. Used in handling "orphan" genes
		 (prev-kb (current-kb)))
	(format t "Importing prots from ~ACYC to ~ACYC; choices is ~A~%" ref-org dest-org kb-choices)
	; Match genes and proteins in reference kb to the dest kb, error out if any are contradictory (e.g. genes match between ref and dest but their proteins do not)
	(loop for ref-prot in prots
		  for ref-gene = (get-frame-handle (get-slot-value ref-prot 'gene :kb ref-kb) :kb ref-kb)
		  for (kb-prot kb-gene good?) = (prot-and-gene-match ref-prot :kb dest-kb :ref ref :choices kb-choices)
		  unless good? do (error (format nil "Cannot resolve protein -> gene matching: ~A -> ~A in ~ACYC but ~A -> ~A in ~ACYC. Please resolve manually using :choices" ref-prot ref-gene ref-org (get-frame-handle kb-prot) (get-frame-handle kb-gene) dest-org))
		  do (add-to-set ref-prot prots-from-ref)
		  when ref-gene do (add-to-set ref-gene genes-from-ref)
		  and do (puthash kb-gene ref-gene kb-gene-to-ref)
		  and do (puthash (gfh ref-prot :kb ref-kb) ref-gene genes-for-prots)
		  when kb-prot do (add-to-set (gfh kb-prot) prots-from-kb)
		  when kb-gene do (add-to-set kb-gene genes-from-kb))
	; Check for any "orphaned" proteins in dest-kb - their gene is set to be
	; replaced with one from ref-kb but they are not. In these cases the
	; orphaned protein will be kept and its gene set to the newly-imported gene
	; from ref-kb
	(loop for gene being the hash-keys in genes-from-kb
		  do (loop for prot in (get-slot-values gene 'product :kb dest-kb)
				   for prothdl = (get-frame-handle prot :kb dest-kb)
				   unless (set-member prothdl prots-from-kb)
				   do (puthash prothdl
					       (gethash (get-frame-handle (get-slot-value prot 'gene :kb dest-kb) :kb dest-kb) kb-gene-to-ref)
					       orphaned-prots)))
	; Check for enzrxn collisions, cases where different enzrxns have been
	; autoassigned the same ID in the ref and current kb. Colliding enzrxns
	; will be renamed
	(so dest-org)
	(setq ezrs-with-collisions (check-for-enzrxn-collisions :kb dest-kb :ref ref))
	(rename-enzrxns ezrs-with-collisions :dry-run? dry-run?)
	; Delete genes and proteins from the dest kb
	(so ref-org)
	(format t "Deleting existing frames from ~ACYC: ~{~A~^, ~}~%" dest-org (set-to-list (set-union prots-from-kb genes-from-kb)))
	(unless dry-run?
	  (loop for prot being the hash-keys in prots-from-kb
			when (valid-frame-p prot)
			do (format t "Deleting prot ~A~%" prot)
			and do (delete-frame-and-dependents prot))
	  (loop for gene being the hash-keys in genes-from-kb
			when (valid-frame-p gene)
			do (format t "Deleting gene ~A~%" gene)
			and do (delete-frame-and-dependents gene)))
	(unless dry-run?
	  ; Export genes and proteins from the ref kb
	  (with-open-file (tmpfile *refine-b-tmp-file* :direction :output :if-exists :supersede)
		(format t "Exporting from ~ACYC ~{~A~^, ~}~%" ref-org (set-to-list (set-union genes-from-ref prots-from-ref)))
		(export-frames-to-file (set-to-list (set-union genes-from-ref prots-from-ref)) tmpfile (list (get-taxon kb) 'ev-as 'ev-exp 'ev-ic)))
	  ; Import the frames into the dest kb
	  (so dest-org)
	  (import-pathway-frames-from-file (probe-file *refine-b-tmp-file*))
	  (delete-file (probe-file *refine-b-tmp-file*))
	  ; Restore orphaned proteins in the dest kb
	  (loop for prot being the hash-keys in orphaned-prots using (hash-value gene)
		do (format t "Restoring orphan ~A~%" prot)
			do (put-slot-value prot 'gene gene))
	  (loop for prot being the hash-keys in genes-for-prots using (hash-value gene)
			unless (gsv prot 'gene)
			do (format t "Restoring gene to ~A~%" prot)
			and do (put-slot-value prot 'gene gene)))))

(defun prot-and-gene-match (ref-prot &key kb (ref 'meta) choices)
  "Takes a protein frame from :ref. Gets the corresponding gene, then finds the same gene and protein in :kb, searching based on the frame ID, then :choices, then accession-1 and accession-2 to find matches. Returns a triple of the matching protein, the matching gene, and a boolean indicating whether the match was okay. The match is considered okay if either (1) ref-prot and its gene both match to frames in :kb, and the matching protein frame has the matching gene frame as its gene, or (2) neither ref-prot nor its gene match any frames in :kb. The matching protein and gene returned can be NIL if no matches were found. :choices should be a list of doubles, each with a frame ID in :ref as the first element and matching frame from :kb as the second. :choices can override matches based on the accession slots, but cannot override matches based on frame ID, because doing so would cause multiple frames with the same ID in :kb"
  (let* ((ref-kb (as-kb ref))
		 (dest-kb (as-kb kb))
		 (ref-gene (get-frame-handle (get-slot-value ref-prot 'gene :kb ref-kb) :kb ref-kb))
		 (dest-prot (find-prot-in-pgdb (coerce-to-frame ref-prot :kb ref-kb) :kb kb :choices choices))
		 (dest-gene (and ref-gene (find-gene-in-pgdb (coerce-to-frame ref-gene :kb ref-kb) :kb kb :choices choices)))
		 (dest-prots-gene (and dest-prot (get-slot-value dest-prot 'gene))))
	(list dest-prot dest-gene (or (not (and dest-prot dest-gene)) (equal dest-gene dest-prots-gene)))))



(defun get-frames-for-taxon (taxon &key (ref 'meta) (class "Pathways"))
  "Gets all frames annotated to the taxon or any sub- or super-taxa up to the level of species in the given reference pgdb"
  (let ((ref-org (as-orgid ref))
		(taxon (get-taxon taxon)))
	(so ref-org)
	(format t "Getting ~A for ~A in ~ACYC~%" class (to-handles taxon) ref)
	(when (coercible-to-frame-p taxon)
	  (let ((taxa (up-to-species taxon)))
		(loop for p in (get-class-all-instances class)
			  when (loop for pwy-taxon in (get-frame-handles (get-slot-values p 'species))
						 thereis (find pwy-taxon taxa))
			  collect (get-frame-handle p))))))
(defun get-pwy-citations-for-taxon (pwy taxon &key (ref 'meta))
  "Gets all citations that the given pathway is annotated to the taxon or any sub- or super-taxa up to the level of species in the given reference pgdb"
  (let ((ref-org (as-orgid ref))
	(taxon (get-taxon taxon)))
    (so ref-org)
    (format t "Getting citations for ~A in ~ACYC~%" (to-handles taxon) ref)
    (when (coercible-to-frame-p taxon)
      (let ((taxa (up-to-species taxon)))
	(loop for pwy-species in  (get-slot-values pwy 'species)
	      for pwy-taxon = (as-handle pwy-species)
	      when (find pwy-taxon taxa)
	      append (get-value-annots pwy 'species pwy-species 'citations))))))


(defun find-gene-in-pgdb (gene &key (kb (current-kb)) choices)
  (find-frame-in-pgdb gene :kb kb :choices choices :index *gene-index*))
(defun find-prot-in-pgdb (prot &key (kb (current-kb)) choices)
  (find-frame-in-pgdb prot :kb kb :choices choices :index *prot-index*))

(defun find-frame-in-pgdb (frame &key (kb (current-kb)) choices index)
  "Finds the given gene or protein in the given PGDB. The frame argument should be a gene or protein frame in any kb. The given kb will be searched first for a matching frame ID, then for a match in :choices, then for a matching accession-1, then accession-2, then anything in the synonyms slot. The matching gene will be returned if found, or NIL if no match was found. :Index should be either *gene-index* or *prot-index*"
  (let* ((org (as-orgid kb))
		 (frame-hash (gethash org index)))
	(so org)
	(format t "Finding frame ~A in ~ACYC with choices ~A~%Choices returns ~A~%" frame org choices (assoc (gfh frame) choices))
	(or (coerce-to-frame (get-frame-handle frame))
		(second (assoc (get-frame-handle frame) choices))
		(gethash (get-slot-value frame 'accession-1) frame-hash)
		(gethash (get-slot-value frame 'accession-2) frame-hash)
		(loop for syn in (get-slot-values frame 'synonyms) when (gethash syn frame-hash) return it))))

(defun make-indexes-if-needed (&key (kb (current-kb)) ignore-dups)
  "Creates the gene and protein indexes for the given kb if they have not already been created"
  (let (org (as-orgid kb))
	(when (not (gethash org *gene-index*))
	  (make-index :class "Genes" :table *gene-index* :kb kb :ignore-dups ignore-dups))
	(when (not (gethash org *prot-index*))
	  (make-index :class "Proteins" :table *prot-index* :kb kb :ignore-dups ignore-dups))))

(defun make-index (&key class table (kb (current-kb)) ignore-dups)
  "Creates an index from accession-1 or accession-2 values to frames of the given class for the given kb and puts it in the given hash-table under the current orgid"
  (let ((org (as-orgid kb))
		(frame-hash (make-hash-table :test 'equal)))
	(format t "Indexing ~A in ~A~%" class org)
	(so org)
	(loop for g in (get-class-all-instances class)
		  do (loop for slot in '(accession-1 accession-2)
				   when (setq slotval (get-slot-value g slot))
				   when (not (string-equal slotval "NIL"))
				   when (setq conflicting-gene (gethash slotval frame-hash))
				   when (not ignore-dups)
				   do (finish-output)
				      (error (format nil "~A and ~A in ~A have the same accession, ~A"
									 (get-frame-handle g)
									 (get-frame-handle conflicting-gene)
									 org
									 (get-slot-value g slot))))
		     (puthash (get-slot-value g 'accession-1) g frame-hash)
		     (puthash (get-slot-value g 'accession-1) g frame-hash))
	(puthash NIL NIL frame-hash)
	(puthash org frame-hash table)))

; The following three functions, (enzrxns-in-file), (rename-enzrxns), and
; (merge-renamed-enzrxns), are used by refine-b to deal with the problem of
; name-collisions between the enzrxns to be imported and existing enzrxns in
; the PGDB. This is mostly a danger for the incrementally-updated PGDBs which
; need to have enzymes with attached enzrxns imported from the reference KBs
; that may have come from a previous version of the same PGDB, because the
; template for auto-assigning enzrxn names is based on the PGDB unique ID (e.g.
; QT for AraCyc) which does not change from one PGDB version to the next
;
; They way to use these is: 
; 	1. Export the enzymes from the reference PGDB with 'EV-AS and 'EV-EXP (or whatever
; evidence codes you want)
; 	2. In the new PGDB, call (enzrxns-in-file) on the exported lisp file to get a
; list of all enzrxns in that file
; 	3. Call (rename-enzymes) on the list you just got. This will find enzrxns
; 	in the current kb that have the same ID as enzrxns in the list, and renames
; 	the ones in the current kb. Save the list it returns
; 	4. Import the lisp file
; 	5. Call merge-renamed-enzrxns on the list returned by (enzrxns-in-file). This
; will deal with the case that the existing and newly-imported enzrxn were
; actually the same after all and now you have duplicate enzrxns. It will merge
; them (under the old frame ID) if both the enzyme and reaction are the same

(defun enzrxns-in-file (file)
  "Returns a list of all frame IDs from the given lisp file (such as is produced by export-frames-to-file) that are enzymatic-reactions. Called as part of refine-b"
  (from-file-or-stream file
					   (loop for frame = (read stream nil nil)
							 while frame
							 for classes = (first (last frame))
							 when (loop for class in classes
										thereis (equal class "Enzymatic-Reactions"))  ; This test works because Enzymatic-Reactions has no subclasses; otherwise we'd need a more complicated test
							 collect (symbol (first frame)))))

(defun rename-enzrxns (enzrxns &key dry-run?)
  "Given a list of enzrxns, renames all of them to new names that are unoccupied (and not found in the list given as argument). Renamed enzrxns get a slot called 'OLD-NAME that gives what they were renamed from; this slot should be cleared afterward once it is decided to merge with the original or not. Called as part of refine-b"
  (loop for enzrxn in enzrxns
		when (valid-frame-p enzrxn)
		do (unless (instance-all-instance-of-p enzrxn '|Enzymatic-Reactions|)
			 (error (format nil "Frame ~A from the set of enzrxns to be imported already exists, but the existing frame is not an enzrxn~%" (gfh enzrxn))))
		and do (setq new-name (loop for new-name-candidate = (generate-instance-name '|Enzymatic-Reactions|)
									while (find new-name-candidate enzrxns)
									finally (return new-name-candidate)))
		and do (unless dry-run? (put-slot-value enzrxn 'old-name enzrxn))
		and do (unless dry-run? (rename-frame enzrxn new-name))
		and do (format t "Rename Enzrxn ~A to ~A~%" enzrxn new-name)
		and collect new-name))

(defun merge-renamed-enzrxns (enzrxns &key dry-run?)
  "Given a list of enzrxns that have been renamed by (rename-enzrxns) after the lisp file has been imported, this function checks to see which of them are actually the same enzrxn as the imported one and therefore sholud be merged. The slot 'OLD-NAME (added by rename-enzrxns) is used to find the corresponding imported enzrxn. The 'OLD-NAME slot is removed whether or not a merge will occur"
  (loop for moved-ezr in enzrxns
		for imported-ezr = (gsv moved-ezr 'old-name)
		unless imported-ezr
		do (error (format nil "Enzrxn ~A is missing the 'OLD-NAME slot. The rename-enzrxns function should have added this slot~%" moved-ezr))
		unless (valid-frame-p imported-ezr)
		do (error (format nil "Enzrxn ~A's 'OLD-NAME slot refers to ~A, but no such frame exists~%" moved-ezr imported-ezr))
		do (unless dry-run? (put-slot-values moved-ezr 'old-name nil))
		when (and (eq (to-handles (gsv moved-ezr 'enzyme)) (to-handles (gsv imported-ezr 'enzyme)))
				  (eq (to-handles (gsv moved-ezr 'reaction)) (to-handles (gsv imported-ezr 'reaction))))
		do (unless dry-run? (merge-frames moved-ezr imported-ezr))
		and do (format t "Merge moved enzrxn ~A back into imported enzrxn ~A~%" moved-ezr imported-ezr)))
			   
(defun check-for-enzrxn-collisions (&key (kb (current-kb)) (ref 'meta))
  "Finds all enzrxns in the given ref kb that refer to the species of kb and would therefore be imported during refine-b, then finds those for which an enzrxn appears in kb with the same frame id, then checks that the enzyme and reaction of the two are different. A list of such enzrxns are returned. Generally this indicates name collisions between the ref kb and the kb"
  (let ((org (as-orgid kb))
		(kb (as-kb kb))
		(ref-org (as-orgid ref))
		(ref-kb (as-kb ref)))
	(so org)
	(so ref-org)
	(loop for enz in (get-frames-for-taxon org :ref ref :class "Proteins")
		  append (loop for ezr in (get-frame-handles (get-slot-values enz 'catalyzes :kb ref-kb))
					   when (coercible-to-frame-p ezr :kb kb)
					   unless (and (eq (get-frame-handle (get-slot-value ezr 'enzyme :kb ref-kb))
									   (get-frame-handle (get-slot-value ezr 'enzyme :kb kb)))
								   (eq (get-frame-handle (get-slot-value ezr 'reaction :kb ref-kb))
									   (get-frame-handle (get-slot-value ezr 'reaction :kb kb))))
					   collect ezr))))

; Old functions for semi-automated refine-b

(defun frames-with-exp-ev (&key (accept-codes '("EV-AS" "EV-EXP")) species exclude (class "Pathways"))
  "Returns a list of frame IDs of the given class that contain pathways with experimental evidence for the given species, or regardless of species if :species is not given. If :exclude is given, pathways in it will not be included in the returned list."
  (loop for p in (get-class-all-instances class)
		when (and (or (null species) (eq (get-slot-value p 'species) species))
				  (loop for cit in (get-slot-values p 'citations) thereis
						(loop for code in accept-codes thereis (search code cit)))
				  (not (find (get-frame-handle p) exclude)))
		collect (get-frame-handle p)))

(defun refine-b-export (species &key metacyc)
  "Exports pathways for the given species from either the old version of the same species (refine-b first step) or from metacyc (refine-b second step) depending on the value of :metacyc. There should be files called <species>.pathways.export.txt in the current directory for all the given species. Each one's pathways will be exported to <species>.pwydata.lisp"
  (loop for s in species do
		(so s)
		(let* ((cyc (concatenate 'string (string-downcase (symbol-name s)) "cyc"))
			   (pwylist-file (concatenate 'string cyc ".pathways.export.txt"))
			   (pwydata-file (concatenate 'string cyc ".pwydata.lisp"))
			   (taxid (get-frame-handle (first (get-frame-direct-parents s)))))
		  (if (probe-file pwylist-file)
			(progn
			  (load pwylist-file)
			  (when metacyc (so 'meta))
			  (format t "Exporting frames for ~A pathways from ~A for ~A~%"  (length pwys) (if metacyc "metacyc" cyc) s)
			  (with-open-file (f pwydata-file :direction :output :if-exists :supersede)
				(export-frames-to-file pwys f `(,taxid EV-EXP EV-AS EV-IC))))
			(format t "No pathways to export for ~A~%" cyc)))))

(defun refine-b-delete-and-import (species)
  "For each species, loads <species>.pathways.delete.txt, deleting some pathways, and then imports the data in <species>.pwydata.lisp. This should be run using the ptools that you want to update, and it should be run from the directory containing these files"
  (for-orgids
	species
	(progn
	  (let* ((cyc (concatenate 'string (string-downcase (symbol-name org)) "cyc"))
			 (pwydel-file (concatenate 'string cyc ".pathways.delete.txt"))
			 (pwydata-file (concatenate 'string cyc ".pwydata.lisp")))
		(format t "Import and delete for ~A~%" cyc)
		(if (probe-file pwydel-file)
		  (progn (format t "Deleting pathways from ~A~%" cyc)
				 (load pwydel-file))
		  (format t "No pathways to delete for %A~%" cyc))
		(if (probe-file pwydata-file)
		  (progn (format t "Importing pathways for ~A~%" cyc)
				 (import-pathway-frames-from-file pwydata-file))
		  (format t "No pathways to import for ~A~%" cyc))
		(save-kb)))))
