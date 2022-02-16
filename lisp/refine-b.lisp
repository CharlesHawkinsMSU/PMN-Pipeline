; Functions for running refine-b for the PMN pipeline. Refine-b is the step that pulls proteins and pathways from reference databases into a newly-created database if they are annotated to the species, where the reference databases are metacyc, plantcyc, and, in the case of the incrementally-updated pgdbs, the previous version of the same pgdb

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

(defun refine-b (&key (kb (current-kb)) (refs '(meta plant)) choices dry-run?)
  "Runs refine-b on the given orgid. Searches for enzymes and pathways in the reference kbs in :refs (by default metacyc and plantcyc) that are annotated to the organism of the pgdb :kb (default is the currently-selected organism). It then imports all of these frames from the reference kbs into the current kb. If a frame is in multiple of the reference kbs and is annotated to the species of :kb in all of them, then reference kb's toward the front of the :refs list are preferred as sources for import. Refine-b will attempt to find existing frames in :kb that match the imports and replace them with the improted frames. For pathways only a matching frame ID is checked. For enzymes, accession-1, accession-2, and synonyms in :kb are checked against accession-1 and accession-2 in :refs if a matching frame-ID is not found. Both the enzyme and its gene are checked for matches. The :choices argument lets you manually specify matching frames between :kb and :refs. It is commonly used when a gene can be matched between :kb and :refs but one or more of its proteins cannot or vice-versa (which would cause an error). It should be an alist from orgids to a list of pairs (or triplets) where the first value is the frame ID in the orgid and the second is the corresponding frame ID in the reference kb. A third value, if present, is the reference kb to draw from; otherwise the first kb in :refs that has the given frame ID will be used. Example: '((myorg (myorgg-22432 g-1391) (myorgg-3481-MONOMER g-1925-MONOMER plant) (myorgg-1119 g-5481 meta)) (someotherorg (soo-5879 g-3911))). Only frame-ids are accepted, so it is not necessary to specify if the mapping is for a gene or an enzyme. Entries in :choices override matches made based on accessions or synonyms, but not frame-id since that could create a conflict"
  (let* ((taken-frames (empty-set))
		 (org (as-orgid kb))
		 (kb (as-kb kb))
		 (matching-pathways
		   (loop for ref in refs
				 collect (list (as-orgid ref)
							   (loop for frame in (get-frames-for-taxon kb :ref ref :class "Pathways")
									 unless (set-member frame taken-frames)
									 collect (progn (add-to-set frame taken-frames) frame)))))
		 (matching-proteins
		   (loop for ref in refs
				 collect (list (as-orgid ref)
							   (loop for frame in (get-frames-for-taxon kb :ref ref :class "Proteins")
									 unless (set-member frame taken-frames)
									 collect (progn (add-to-set frame taken-frames) frame))))))
	(if dry-run?
	  (values matching-pathways matching-proteins)
	  (progn
		(loop for (ref pwys) in matching-pathways
			  when pwys
			  do (import-pwys-from-ref pwys :kb kb :ref ref))
		(loop for (ref prots) in matching-proteins
			  when prots
			  do (import-prots-from-ref prots :kb kb :ref ref :choices choices))))))

(defun import-pwys-from-ref (pwys &key kb ref)
  "Exports the given pathway from the given ref to a temp file and reimports into kb"
  (let ((ref-org (as-orgid ref))
		(kb-org (as-orgid kb)))
	(so ref-org)
	(with-open-file (tmpfile *refine-b-tmp-file* :direction :output :if-exists :supersede)
	  (format t "Exporting ~{~A~^, ~} with taxon ~A~%" (to-handles pwys) (get-taxon kb))
	  (export-frames-to-file pwys tmpfile (list (get-taxon kb) 'ev-as 'ev-exp 'ev-ic)))
	(so kb-org)
	(let ((*warn-interactive?* NIL))
	  (import-pathway-frames-from-file *refine-b-tmp-file*))
	(delete-file (probe-file *refine-b-tmp-file*))))

(defun import-prots-from-ref (prots &key kb ref choices)
  "Exports the given proteins and their genes from the given ref kb and reimports them into kb. Tries to match them with existing genes and proteins in the given kb"
  (make-indexes-if-needed :kb kb)
  (let* ((ref-org (as-orgid ref))
		 (dest-org (as-orgid kb))
		 (ref-kb (as-kb ref))
		 (dest-kb (as-kb kb))
		 (genes-from-ref (empty-set))
		 (prots-from-ref (empty-set))
		 (genes-from-kb (empty-set))
		 (prots-from-kb (empty-set))
		 (kb-gene-to-ref (make-hash-table))
		 (orphaned-prots (make-hash-table))
		 (kb-choices (rest (assoc dest-org choices)))
		 (prev-kb (current-kb)))
	; Match genes and proteins in reference kb to the dest kb, error out if any are contradictory (e.g. genes match between ref and dest but their proteins do not)
	(loop for ref-prot in prots
		  for ref-gene = (get-frame-handle (get-slot-value ref-prot 'gene :kb ref-kb) :kb ref-kb)
		  for (kb-prot kb-gene good?) = (prot-and-gene-match ref-prot :kb dest-kb :ref ref :choices choices)
		  unless good? do (error (format nil "Cannot resolve protein -> gene matching: ~A -> ~A in ~ACYC but ~A -> ~A in ~ACYC. Please resolve manually using :choices" ref-prot ref-gene ref-org (get-frame-handle kb-prot) (get-frame-handle kb-gene) dest-org))
		  do (add-to-set ref-prot prots-from-ref)
		  when ref-gene do (add-to-set ref-gene genes-from-ref)
		  (puthash kb-gene ref-gene kb-gene-to-ref)
		  when kb-prot do (add-to-set kb-prot prots-from-kb)
		  when kb-gene do (add-to-set kb-gene genes-from-kb))
	; Check for any "orphaned" proteins in dest-kb - their gene is set to be replaced with one from ref-kb but they are not. In these cases the orphaned protein will be kept and its gene set to the newly-imported gene from ref-kb
	(loop for gene being the hash-keys in genes-from-kb
		  do (loop for prot in (get-slot-values gene 'product :kb dest-kb)
				   for prothdl = (get-frame-handle prot :kb dest-kb)
				   unless (set-member prothdl prots-from-kb)
				   do (puthash prothdl (gethash (get-frame-handle (get-slot-value prot 'gene :kb dest-kb) :kb dest-kb) kb-gene-to-ref) orphaned-prots)))
	; Delete genes and proteins from the dest kb
	(so ref-org)
	(format t "Deleting existing frames from ~ACYC: ~{~A~^, ~}~%" dest-org (set-to-list (set-union prots-from-kb genes-from-kb)))
	(loop for prot being the hash-keys in prots-from-kb
		  when (coercible-to-frame-p prot)
		  do (delete-frame-and-dependents prot))
	(loop for gene being the hash-keys in genes-from-kb
		  when (coercible-to-frame-p gene)
		  do (delete-frame-and-dependents gene))
	; Export genes and proteins from the ref kb
	(with-open-file (tmpfile *refine-b-tmp-file* :direction :output :if-exists :supersede)
	  (format t "Exporting from ~ACYC ~{~A~^, ~}~%" ref-org (set-to-list (set-union genes-from-ref prots-from-ref)))
	  (export-frames-to-file (set-to-list (set-union genes-from-ref prots-from-ref)) tmpfile (list (get-taxon kb) 'ev-as 'ev-exp 'ev-ic)))
	; Import them into the dest kb
	(so dest-org)
	(import-pathway-frames-from-file (probe-file *refine-b-tmp-file*))
	(delete-file (probe-file *refine-b-tmp-file*))
	; Restore orphaned proteins in the dest kb
	(loop for prot being the hash-keys in orphaned-prots using (hash-value gene)
		  do (put-slot-value prot 'gene gene))))

(defun prot-and-gene-match (ref-prot &key kb (ref 'meta) choices)
  "Takes a protein frame from :ref. Gets the corresponding gene, then finds the same gene and protein in :kb, searching based on the frame ID, then :choices, then accession-1 and accession-2 to find matches. Returns a triple of the matching protein, the matching gene, and a boolean indicating whether the match was okay. The match is considered okay if either (1) ref-prot and its gene both match to frames in :kb, and the matching protein frame has the matching gene frame as its gene, or (2) neither ref-prot nor its gene match any frames in :kb. The matching protein and gene returned can be NIL if no matches were found. :choices should be a list of doubles, each with a frame ID in :ref as the first element and matching frame from :kb as the second. :choices can override matches based on the accession slots, but cannot override matches based on frame ID, because doing so would cause multiple frames with the same ID in :kb"
  (let* ((ref-kb (as-kb ref))
		 (dest-kb (as-kb kb))
		 (ref-gene (get-frame-handle (get-slot-value ref-prot 'gene :kb ref-kb) :kb ref-kb))
		 (dest-prot (find-prot-in-pgdb (coerce-to-frame ref-prot :kb ref-kb) :kb kb :choices choices))
		 (dest-gene (and ref-gene (find-gene-in-pgdb (coerce-to-frame ref-gene :kb ref-kb) :kb kb :choices choices)))
		 (dest-prots-gene (and dest-prot (get-slot-value dest-prot 'gene))))
	(list dest-prot dest-gene (or (not (or dest-prot dest-gene)) (equal dest-gene dest-prots-gene)))))



(defun get-frames-for-taxon (taxon &key (ref 'meta) (class "Pathways"))
  "Gets all frames annotated to the taxon or any sub- or super-taxa up to the level of species in the given reference pgdb"
  (let ((ref-org (as-orgid ref))
		(taxon (get-taxon taxon)))
	(so ref-org)
	(format t "Getting ~A for ~A in ~ACYC~%" class (get-frame-handle taxon) ref)
	(when (coercible-to-frame-p taxon)
	  (let ((taxa (up-to-species taxon)))
		(loop for p in (get-class-all-instances class)
			  when (loop for pwy-taxon in (get-frame-handles (get-slot-values p 'species))
						 thereis (find pwy-taxon taxa))
			  collect (get-frame-handle p))))))


(defun find-gene-in-pgdb (gene &key (kb (current-kb)) choices)
  (find-frame-in-pgdb gene :kb kb :choices choices :index *gene-index*))
(defun find-prot-in-pgdb (prot &key (kb (current-kb)) choices)
  (find-frame-in-pgdb prot :kb kb :choices choices :index *prot-index*))

(defun find-frame-in-pgdb (frame &key (kb (current-kb)) choices index)
  "Finds the given gene or protein in the given PGDB. The frame argument should be a gene or protein frame in any kb. The given kb will be searched first for a matching frame ID, then for a match in :choices, then for a matching accession-1, then accession-2, then anything in the synonyms slot. The matching gene will be returned if found, or NIL if no match was found. :Index should be either *gene-index* or *prot-index*"
  (let* ((org (as-orgid kb))
		 (frame-hash (gethash org index)))
	(so org)
	(or (coerce-to-frame (get-frame-handle frame))
		(second (assoc (get-frame-handle frame) choices))
		(gethash (get-slot-value frame 'accession-1) frame-hash)
		(gethash (get-slot-value frame 'accession-2) frame-hash)
		(loop for syn in (get-slot-values frame 'synonyms) when (gethash syn frame-hash) return it))))

(defun make-indexes-if-needed (&key (kb (current-kb)))
  "Creates the gene and protein indexes for the given kb if they have not already been created"
  (let (org (as-orgid kb))
	(when (not (gethash org *gene-index*))
	  (make-index :class "Genes" :table *gene-index* :kb kb))
	(when (not (gethash org *prot-index*))
	  (make-index :class "Proteins" :table *prot-index* :kb kb))))

(defun make-index (&key class table (kb (current-kb)))
  "Creates an index from accession-1 or accession-2 values to frames of the given class for the given kb and puts it in the given hash-table under the current orgid"
  (let ((org (as-orgid kb))
		(frame-hash (make-hash-table :test 'equal)))
	(format t "Indexing ~A in ~A~%" class org)
	(so org)
	(loop for g in (get-class-all-instances class)
		  do (loop for slot in '(accession-1 accession-2)
				   when (setq slotval (get-slot-value g slot))
				   when (setq conflicting-gene (gethash slotval frame-hash))
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

; Old functions for semi-automated refine-b

(defun frames-with-exp-ev (&key (accept-codes '("EV-AS" "EV-EXP" "EV-IC")) species exclude (class "Pathways"))
  "Returns a list of frame IDs of the given class that contain pathways with experimental evidence for the given species, or regardless of species if :species is not given. If :exclude is given, pathways in it will not be included in the returned list."
  (loop for p in (get-class-all-instances class)
		when (and (or (null species) (eq (get-slot-value p 'species) species))
				  (loop for cit in (get-slot-values p 'citations) thereis
						(loop for code in accept-codes thereis (search code cit)))
				  (not (find (get-frame-handle p) exclude))
				  collect (get-frame-handle p))))

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
