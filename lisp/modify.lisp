; Lisp functions that modify things in the kb

(defparameter citation-re (excl:compile-re "^[ \\t\"[]*([^ \\t\"\\]:]+)") "For extracting a citation from 'CITATION slots and annotations. The first group is the citation. To find the frame you should string-upcase the result and concatenate PUB- in front of it (accomplished with the (pub-frame) function)")
(defparameter pub-re (excl:compile-re "^PUB-"))
(defparameter cits-re (excl:compile-re "\\| *CITS *:.*?\\|") "Matches a CITS citation block from within a 'comment")
(defparameter citspub-re (excl:compile-re "(?<=\\[)\\s*.*?\\s*(?=\\])") "Matches one citatio within a CITS citation block")

(defun replace-indirect-citations (&optional (classes '(|Pathways| |Genes| |Proteins| |Reactions| |Compounds| |Enzymatic-Reactions|)))
  "Replaces indirect citations with direct citations in all members of the given list of classes"
  (loop for class in classes
		append (loop for frame in (gcai class)
				 for rcits = (loop for cit in (gsvs frame 'citations)
						  for cit-name = (extract-citation-from-string cit)
						  for cit-frame = (pub-frame cit-name)
						  when (publication-is-indirect cit-frame)
						  do (replace-slot-value frame 'citations cit
												 (get-direct-citation cit))
						  and collect cit-name)
				 for arcits = (loop for slot in (get-frame-slots frame)
									append (loop for val in (gsvs frame slot)
												 append (loop for cit in (get-value-annots frame slot val 'citations)
															  for cit-name = (extract-citation-from-string cit)
															  for cit-frame = (pub-frame cit-name)
															  when (publication-is-indirect cit-frame)
															  do (replace-value-annot frame slot val 'citations cit (get-direct-citation cit))
															  and collect cit-name)))
				 do (replace-indirect-comment-citations frame)
				 when (or rcits arcits) collect (cons frame (append rcits arcits)))))
(defun replace-indirect-comment-citations (frame &key (dry-run? nil))
  "Finds all CITS citations in the given frame's 'COMMENT and replaces all citations to indirect publication frames with direct ones. Also returns the modified 'COMMENT. If :dry-run? is t, then does not change the 'COMMENT but still returns what the 'COMMENT would have been changed to"
  (let ((cmt (gsv frame 'comment)))
	(when cmt
	  (let ((replacement 
			  (replace-re-using cits-re cmt
								(lx (replace-re-using citspub-re x
													  (lx (let* ((pubn (extract-citation-from-string x)) (pubf (pub-frame pubn)))
															(when (publication-is-indirect pubf)
															  (get-direct-citation pubn)))))))))
		(unless dry-run?
		  (put-slot-value frame 'comment replacement))
		replacement))))
(defun find-indirect-comment-citations (class)
  "Finds all members of class that cite indirect pub frames in their 'comment"
  (loop for f in (gcai class)
		for cmt = (gsv f 'comment)
		do (setq iflist (loop for cits in (match-re-all cits-re (or cmt ""))
							  append (loop for pub in (match-re-all citspub-re cits)
										   for just-pub = (extract-citation-from-string pub)
										   when (publication-is-indirect (pub-frame just-pub))
										   collect just-pub)))
		when iflist collect (cons (gfh f) iflist)))

(defun publication-is-indirect (publication)
  "Determines whether the given pub frame is a purely indirect publication. That is, it (1) exists; (2) has no info in any of the slots enumerated in *pub-slots-to-check*; and (3) has a value in 'referent-frame that is not itself"
	(and (coercible-to-frame-p publication)
		 (setq ref-frame (gsv publication 'referent-frame))
		 (not (equal publication ref-frame))
		 (loop for slot in *pub-slots-to-check* never (gsv publication slot))))
(defun get-direct-citation (citation-text)
  "When passed the citation text as appears in a 'CITATIONS slot or annotation, returns the same text with the citation replaced with a direct citation if the existing one is an indirect citation; else returns NIL as no replacement is needed. Second return value is a symbol explaining the result: 'REPLACE means a replacement was issued. 'DIRECT means the citation was already direct. 'NOT-FOUND means no citation with the given citation ID could be found in the current kb. 'NOT-RECOGNIZED means that the text given didn't have anything in a format we could recognize as a citatation ID"
  (multiple-value-bind (citation span) (extract-citation-from-string citation-text)
	(if citation
		(let ((citation-frm (format nil "PUB-~A" (string-upcase citation))))
		  (if (coercible-to-frame-p citation-frm)
			(let ((referent (gsv citation-frm 'referent-frame)))
			  (if referent
				(values (format nil "~A~A~A"
								(subseq citation-text 0 (first span))
								(excl:replace-re (symbol-name (get-frame-handle referent)) pub-re "")
								(subseq citation-text (rest span)))
						'replace)
				(values nil 'direct)))
			(values nil 'not-found)))
		(values nil 'not-recognized))))

(defun extract-citation-from-string (citation-text)
  "Passed citation text as appears in a 'CITATIONS slot or annotation, returns the actual citation from it, with quotes, brackets, and trailing information after the : stripped off. Returns NIL if nothing that looks like a citation can be found in the given text. Does not check that such a citation actually exists in the current kb. Second return value is the span of the citation as an improper list (e.g. '(0 . 11))"
  (multiple-value-bind (matched full grp) (excl:match-re citation-re citation-text :return :index)
	(values (when grp (subseq citation-text (first grp) (rest grp))) grp)))
; Functions to add ChEBI links to compounds using chebi_core.obo
(defvar *inchi-to-chebi* (make-hash-table :test 'equal))
(defvar *inchikey-to-chebi* (make-hash-table :test 'equal))
(defvar *smiles-to-chebi* (make-hash-table :test 'equal))
(defvar *mol-to-chebi* (make-hash-table :test 'equal))

(defun read-chebi-obo (chfile)
  "Reads in chebi_core.obo and generates hashes mapping from inchi, inchikey, and SMILES to ChEBI IDs"
  (from-file-or-stream chfile
					   (loop while (next-term stream)
							 for n from 1
							 when (= 0 (mod n 5000))
							 do (format t "Read ~A ChEBIs~%" n)
							 do (let (inchi inchikey smiles chebi)
								  (loop for line = (read-line stream nil nil)
										while (> (length line) 0)
										for (key . vals) = (excl:split-re ": " line)
										do (setq val (string-trim " " (first vals)))
										when (string-equal key "id")
										do (setf chebi (second (excl:split-re ":" val)))
										when (string-equal key "property_value")
										do (let* ((properties (excl:split-re " " val))
												  (property (first properties))
												  (property-val (string-trim "\"" (second properties))))
											 (cond ((string-equal property "http://purl.obolibrary.org/obo/chebi/inchi") (setf inchi property-val))
												   ((string-equal property "http://purl.obolibrary.org/obo/chebi/inchikey") (setf inchikey property-val))
												   ((string-equal property "http://purl.obolibrary.org/obo/chebi/smiles") (setf smiles property-val))
												   ;(t (format t "(~A: ~A)~%" property property-val))
												   )))
								  (when chebi
									(when inchi (puthash inchi chebi *inchi-to-chebi*))
									(when inchikey (puthash inchikey chebi *inchikey-to-chebi*))
									(when smiles
									  (puthash smiles chebi *smiles-to-chebi*)
									  (handler-case (puthash (parse-smiles smiles) chebi *mol-to-chebi*) (smiles-error () nil))))))))


(defun next-term (chfile)
  "Advances the stream chfile to the line following the next [term] line"
  (loop for line = (read-line chfile nil nil) while line when (string-equal line "[term]") return t))

(defun suggest-chebi-links (filename)
  "Creates a table of suggested chebi links for compounds in the current pgdb"
	(to-file-or-stream filename
					   (loop for cpd in (all-cpds)
							 for options = (empty-set)
							 unless (assoc 'chebi (get-slot-values cpd 'dblinks))
							 do (when (setq smiles (gethash (get-slot-value cpd 'smiles) *smiles-to-chebi*))
								  (add-to-set smiles options))
							 and do (when (setq inchi (gethash (get-slot-value cpd 'inchi) *smiles-to-chebi*))
								  (add-to-set smiles options))
							 and do (when (setq inchi-key (gethash (get-slot-value cpd 'inchi-key) *smiles-to-chebi*))
								  (add-to-set smiles options))
							 and do (handler-case
									  (when (setq mol (when (setq csmiles (get-slot-value cpd 'smiles))
														(gethash (parse-smiles csmiles) *mol-to-chebi*)))
										(add-to-set mol options))
									  (smiles-error () nil))
							 ;and when (> (set-length options) 0)
							 and do (format stream "~A	~{~A~^	~}~%" (get-frame-handle cpd) (set-to-list options)))))

(defun find-smiles-match (cpd)
  (handler-case
	(let* ((cpd-al (->adj-list (parse-smiles (get-slot-value cpd 'smiles))))
		   (total-chebis (hash-table-count *smiles-to-chebi*)))
	  (loop for cs being the hash-keys in *smiles-to-chebi*
			for n from 1
			when (= 0 (mod n 5000))
			do (format t "Checked ~A / ~A ChEBIs~%" n total-chebis)
			when (handler-case 
				   (let ((chebi-al (->adj-list (parse-smiles cs))))
					 (adj-lists-equal? cpd-al chebi-al))
				   (smiles-error () nil)
				   (program-error () nil))
			collect (gethash cs *smiles-to-chebi*)))
	(smiles-error () nil)
	((program-error () nil))))
(defun find-inchi-match (cpd)
  (let ((cpd-inchi (get-slot-value cpd 'inchi-key))
		(total-chebis (hash-table-count *inchikey-to-chebi*)))
	(when cpd-inchi
	  (loop for chebi-inchi being the hash-keys of *inchikey-to-chebi*
			for n from 1
			when (= 0 (mod n 5000))
			do (format t "Checked ~A / ~A ChEBIs~%" n total-chebis)
			when (inchi-key-match-p chebi-inchi cpd-inchi)
			collect (gethash chebi-inchi *inchikey-to-chebi*)))))

; Functions to add PhylomeDB links to proteins using dupmed PhylomeDB files
(defparameter *tair-phylome-table* (make-hash-table :test 'equal))
(defparameter *uniprot-phylome-table* (make-hash-table :test 'equal))
(defun read-phylome-table (filename)
  "Reads a three-column phylomedb all-id-conversion.txt file (with header line removed), and creates two hash tables, *tair-phylome-table* and *uniprot-phylome-table*, that map from tair protein IDs to phylomedb ids and uniprot ids to phylomedb ids, respectively"
  (from-file-or-stream filename
					   (loop for n from 1
							 for line = (read-line stream nil) while line
							 for (pdb-id db id) = (excl::split-re "\\t" line)
							 when (= 0 (mod n 5000))
							 do (format t "Read ~A PhylomeDB Entries~%" n)
							 when (equal db "TAIR") do (puthash id pdb-id *tair-phylome-table*)
							 when (or (equal db "swissprot") (equal db "trembl")) do (puthash id pdb-id *uniprot-phylome-table*))))
(defun delete-dblink (frames db)
  "Deletes all dblinks to the given db from the given frames"
  (loop for frame in frames
		do (loop for dblink = (assoc db (get-slot-values frame 'dblinks))
				 while dblink
				 do (remove-slot-value frame 'dblinks dblink))))

(defun add-phylomedb-from-tair ()
	(loop for prot in (get-class-all-instances "Proteins")
		  for pdb-id = (gethash (get-slot-value prot 'acccession-1) *tair-phylome-table*)
		  when pdb-id
		  do (add-slot-value prot 'dblinks (list 'phylomedb pdb-id))))
(defun add-phylomedb-from-uniprot ()
  (loop for prot in (get-class-all-instances "Proteins")
		do (loop for uniprot in (assoc-every 'uniprot (get-slot-values prot 'dblinks))
				 for phylome = (gethash uniprot *uniprot-phylome-table*)
				 when phylome
				 do (add-slot-value prot 'dblinks (list 'phylomedb phylome)))))

(defun replace-curator (replace replace-with &optional (frames (get-slot-values replace 'credited-for)))
  "Replace the given curator in the given frames with the curator replace-with"
  (loop for p in frames when (replace-slot-value p 'credits replace replace-with) collect p))


(defun add-accessions (file)
  (let ((ct (read-alist file)))
    (loop for (frame protein gene) in ct do (put-slot-value frame 'accession-1 protein))
    (loop for (frame protein gene) in ct when (setq g (get-slot-value frame 'gene)) do (put-slot-value g 'accession-1 gene))))

(defun replace-first-db-link (frames dbl-from dbl-to)
  "Replace the db-link dbl-from with dbl-to in all the given frames"
  (loop for f in frames
		when (setq old (assoc dbl-from (get-slot-values f 'dblinks)))
		do (setq new (cons dbl-to (rest old)))
		(replace-slot-value f 'dblinks old new)))

(defun replace-db-link (frames dbl-from dbl-to &optional (pattern ""))
  "Replace the db-link dbl-from with dbl-to in all the given frames"
  (loop for f in frames
		do (loop for old in (get-slot-values f 'dblinks)
				 when (eq dbl-from (first old))
				 when (search pattern (second old))
				 do (setq new (cons dbl-to (rest old)))
				 (replace-slot-value f 'dblinks old new)
				 (format t "~A -> ~A~%" old new)
				 )))

;(defun merge-duplicate-proteins (prefix)
;  "Merges duplicated proteins, discarding the ones whose frame-ids start with the given prefix. Duplicates are determined because they have the same enzrxns, which shouldn't normally happen."
;  (let ((pre-l (length prefix)))
;	(loop for prot in (get-class-all-instances "Proteins")
;		  for name = (symbol-name (get-frame-handle p))
;		  when (equal prefix (subseq name 0 pre-l)))))

(defun fix-enzrxn-names (&key orgid overwrite?)
  "Imports enzrxn names from metacyc or, if necessary, plantcyc, into :orgid, or the current orgid if :orgid is not specified. If :overwrite? is t then all enzrxn names will be imported, if it is NIL then only enzrxns whose names are currently NIL wil have names imported"
  (if orgid
	(so orgid)
	(setq orgid (kb-orgid (current-kb))))
  (so 'meta)
  (so 'plant)
  (so orgid)
  (loop for r in (get-class-all-instances "Reactions")
		for name = (get-ref-rxn-name r)
		do (loop for ezr in (get-slot-values r 'enzymatic-reaction)
				 for cur-name = (get-slot-value ezr 'common-name)
				 when (or overwrite? (null cur-name) (equal cur-name "NIL")) ; don't ask
				 do (put-slot-value ezr 'common-name (or name (or (get-slot-value (get-slot-value (get-slot-value ezr 'enzyme) 'gene) 'accession-1) (get-frame-handle (get-slot-value ezr 'enzyme)))))
				 do (format t "~A renamed to ~A~%" (get-frame-handle ezr) (or name (or (get-slot-value (get-slot-value (get-slot-value ezr 'enzyme) 'gene) 'accession-1) (get-frame-handle (get-slot-value ezr 'enzyme))))))))

(defun get-ref-rxn-name (r)
  "Gets the name for the first enzrxn of r in metacyc or plantcyc. Returns NIL if the frame is not in either metacyc or plantcyc, or there are no enzrxns, or the first enzrxn has no name"
  (let* ((metacyc (find-kb 'meta))
		 (plantcyc (find-kb 'plant))
		 (frame-id (get-frame-handle r))
		 (ref-kb (if (coercible-to-frame-p frame-id :kb metacyc) metacyc (if (coercible-to-frame-p frame-id :kb plantcyc) plantcyc NIL))))
	(when ref-kb
	  (let* ((ref-r (coerce-to-frame frame-id :kb ref-kb))
			 (ezrs (get-slot-values ref-r 'enzymatic-reaction :kb ref-kb)))
		(when ezrs
		  (get-slot-value (first ezrs) 'common-name :kb ref-kb))))))

(defun copy-gene-accs-to-proteins (&key overwrite?)
  "Copies the accession-1 field of all genes to their gene-products. if :overwreite? is false, only copies if the products' current accession-1 is NIL; otherwise overwrites existing accession-1's"
  (loop for g in (all-genes)
		for acc = (get-slot-value g 'accession-1)
		do (loop for p in (get-slot-values g 'product)
				 when (or overwrite? (null (get-slot-value p 'accession-1)))
				 do (put-slot-value p 'accession-1 acc)
				 when (or overwrite? (null (get-slot-value p 'common-name)))
				 do (put-slot-value p 'common-name acc)
				 do (format t "Accession of ~A changed to ~A~%" (get-frame-handle p) acc))))

(defun list-enzr-names (rxn)
  "Lists the names of the enzrxns that catalyze rxn"
  (loop for e in (get-slot-values rxn 'enzymatic-reaction) collect (get-slot-value e 'common-name)))

(defun rename-enzyme (enzyme new-name &optional (gene-name (first (excl:split-re "-" new-name)))) 
  "Renames the given enzyme. The old 'accession-1 is moved to 'synonyms and new-name is placed into 'accession-1. 'Dblinks is unaltered. If 'common-name is equal to the old 'accession-1 then the new one is put there; otherwise the old value is kept. For the gene, 'accession-1 is changed to gene-name, 'common-name is set to new-name, and the old 'accession-1 is moved to 'synonyms."
  (let* ((old-name (get-slot-value enzyme 'accession-1))
		(gene (get-slot-value enzyme 'gene))
		(old-gene-name (get-slot-value gene 'accession-1)))
	(add-slot-value enzyme 'synonyms old-name)
	(put-slot-value enzyme 'accession-1 new-name)
	(replace-slot-value enzyme 'common-name old-name new-name)
	(remove-slot-value enzyme 'synonyms new-name)
	(add-slot-value gene 'synonyms old-gene-name)
	(put-slot-value gene 'accession-1 gene-name)
	(put-slot-value gene 'common-name gene-name)
	(remove-slot-value gene 'synonyms gene-name)))


(defun copy-slot-values-with-annots-to-kb (frame slot from-kb to-kb)
  "Copies all slot values from frame in from-kb to the frame with the same id in to-kb, along with all annotations. Prints a warning but has no effect if frame does not exist in to-kb. Any and all existing values in the slot in the frame in to-kb will be deleted. The frame, slot, and to-kb arguments can also be lists of frames, slots, and kbs, respectively. All kbs in to-kb will be saved and closed."
  (let* ((from-orgid (as-orgid from-kb))
		 (from-kb (as-kb from-kb))
		 (frame-list (loop for f in (as-list frame) collect (get-frame-handle f)))
		 (slot-list (as-list slot))
		 (to-orgid-list (loop for k in (as-list to-kb) unless (eq (setq org (as-orgid k)) from-orgid) collect org)))
	(so from-orgid)
	(loop-orgids to-orgid-list saving closing
				 do (loop for frame in frame-list
						  do (if (coercible-to-frame-p frame)
							   (loop for slot in slot-list
									 do (put-slot-values frame slot (to-handles (get-slot-values frame slot :kb from-kb)))
									 do (loop for val in (get-slot-values frame slot :kb from-kb)
											  do (loop for label in (get-all-annots frame slot val :kb from-kb)
													   do (put-value-annots frame slot val label (get-value-annots frame slot (as-handle val) label :kb from-kb)))))
							   (format t "Warning: ~ACYC has no frame '~A; skipping~%" org frame))))))

(defun copy-slot-values-with-annots-to-frame (from-frame to-frame slot &key (kb (current-kb)))
  "Copies all slot values of the given slot from from-frame to to-frame in the same kb, given by the optional :kb argument. Any existing values in the slot in to-frame will be deleted. Slot can be a list of slots and to-frame can be a list of frames"
  (let* ((org (as-orgid kb))
		 (slot-list (as-list slot))
		 (to-frame-list (as-list to-frame))
		 (prev-org (as-orgid (current-kb))))
	(so org)
	(get-frame-handle from-frame) ; To cause an error if from-frame doesn't exist, so we don't clobber to-frame's slot if that's the case
	(loop for to-frame in to-frame-list do
		  (loop for slot in slot-list
				do (put-slot-values to-frame slot nil)
				do (loop for val in (get-slot-values from-frame slot)
						 do (put-slot-value to-frame slot val)
						 do (loop for label in (get-all-annots from-frame slot val)
								  do (put-value-annots to-frame slot val label (get-value-annots from-frame slot val label))))))
	(so prev-org)))

(defun swap-compartment (rxn from-cmp to-cmp &key (side :both) (kb (current-kb)))
  "Changes all instances of the compartment from-cmp to to-cmp in the given reaction frame. The :side argument controls which side(s) of the reaction are affected, and can be :left, :right, or :both (the default)"
  (unless (find side '(:left :right :both))
	(error ":side should be :left, :right, or :both"))
  (let ((kb (as-kb kb)))
	(when (find side '(:left :both))
	  (loop for val in (get-slot-values rxn 'left :kb kb)
			do (replace-value-annot rxn 'left val 'compartment from-cmp to-cmp :kb kb)))
	(when (find side '(:right :both))
	  (loop for val in (get-slot-values rxn 'right :kb kb)
			do (replace-value-annot rxn 'right val 'compartment from-cmp to-cmp :kb kb)))))

; Functions to find the origin of a frame by looking at the flatfiles in previous versions of the pgdb. All previous versions should be put into the pgdb folder (the will not be loaded or upgraded). They will need to have their flatfiles dumped, however.

(defun versions-with-enzrxn (ezr dir)
  (let* ((vers-list (sort (get-org-versions :dir (probe-file (format nil "~A/" dir))) #'compare-versions)))
	(loop for v in vers-list
		  for ezrfn = (make-pathname :directory `(:absolute ,dir ,v "data") :name  "enzrxns.dat")
		  for ezrfile = (probe-file ezrfn)
		  ;do (format t "~A~%~A~%" ezrfn ezrfile)
		  collect (list v (if ezrfile (flatfile-has-frame ezr ezrfile) 'nf)))))

(defun sorted-versions-of-pgdb (&optional (org (current-orgid)))
  (sort (get-org-versions org) #'compare-versions))

(defun get-org-versions (&key (org (current-orgid))(dir (make-pathname :directory (drop-last-n 1 (pathname-directory (full-org-system-directory org))))))
  (mapcar #'file-namestring
		  (remove-if-not
			#'dir-p (directory (format nil "~A/" dir)))))

(defun compare-versions (v1 v2)
  "Returns t iff v1 is an earlier version than v2, assuming they are of the form 1.2.3; so 1.9.3 is an earlier version than 1.12.1. Returns NIL if the versions are the same or v2 is the earlier version. If one version has fewer segments than the other than the missing segments are treated as 0; so 3.2 is an earlier version than 3.2.1, but 3.2 and 3.2.0 are the same version. Versions that do not consist of strings of digits separated by dots will generate an error"
  (let ((v1s (loop for s in (excl::split-re "\\." v1) collect (parse-integer s)))
		(v2s (loop for s in (excl::split-re "\\." v2) collect (parse-integer s))))
	(when (< (setq l1 (length v1s)) (setq l2 (length v2s)))
	  (nconc v1s (make-list (- l2 l1) :initial-element 0)))  ; pad out v1 to the length of v2, if needed
	(loop for n1 in v1s for n2 in v2s
		  when (< n1 n2) do (return t)
		  when (> n1 n2) do (return nil))))



(require :sax)
(defstruct pubchem-state (id "") (cur-attr nil) (look-for :none) (inchi nil) (inchi-key nil) (smiles nil))
(defstruct pubchem-attr (name "") (label "") (value ""))

(defun print-pubchem-cpd (state)
  (format t "ID: ~A~%" (pubchem-state-id state)))

(defclass pubchem-xml-parser (net.xml.sax:sax-parser)
  ((state :initform (make-pubchem-state) :reader state)
   (recs :initform (make-hash-table) :reader recs)))

(defmethod make-pc-recs ((parser pubchem-xml-parser))
  "Makes recommendations for the current pubchem record, finding compounds that match it for InChI or SMILES and putting those into the recs field of the class. Should be called when the end of a pubchem record is reached"
  (let* ((state (state parser))
		 (id (pubchem-state-id state))
		 (inchi (pubchem-state-inchi state))
		 (inchi-key (pubchem-state-inchi-key state))
		 (smiles (pubchem-state-smiles state))
		 (rec '(nil nil nil)))
	(when inchi
	  (multiple-value-bind (cpd status) (get-cpd-id-by-inchi inchi)
		(when cpd
		  (setf (first rec) cpd))))
	(when inchi-key
	  (multiple-value-bind (cpd status) (get-cpd-id-by-inchi inchi)
		(when cpd
		  (setf (second rec) cpd))))
	(when smiles
	  (multiple-value-bind (cpd status) (get-cpd-id-by-inchi inchi)
		(when cpd
		  (setf (third rec) cpd))))
	(when (or (first rec) (second rec) (third rec) (fourth rec))
	  (puthash id rec (recs parser)))))

(defmethod net.xml.sax:start-document ((parser pubchem-xml-parser))
  (format t "Beginning parse of PubChem XML~%"))

(defmethod net.xml.sax:end-document ((parser pubchem-xml-parser))
  (format t  "Finished parsing PubChem XML~%")
  (recs parser)
  )

(defmethod net.xml.sax:start-element ((parser pubchem-xml-parser) iri localname qname attrs)
  (declare (ignore iri qname attrs))
  (let ((state (state parser)))
	(cond ((string-equal localname "PC-Compound")
		   (setf (pubchem-state-id state) "")
		   (setf (pubchem-state-cur-attr state) nil)
		   (setf (pubchem-state-look-for state) nil))
		  ((string-equal localname "PC-CompoundType_id_cid")
		   (setf (pubchem-state-look-for state) :id))
		  ((string-equal localname "PC-InfoData")
		   (setf (pubchem-state-cur-attr state) (make-pubchem-attr)))
		  ((string-equal localname "PC-Urn_label")
		   (setf (pubchem-state-look-for state) :label))
		  ((string-equal localname "PC-Urn_name")
		   (setf (pubchem-state-look-for state) :name))
		  ((string-equal localname "PC-InfoData_value_sval")
		   (setf (pubchem-state-look-for state) :value))))
  nil)

(defmethod net.xml.sax:end-element ((parser pubchem-xml-parser) iri localname qname)
  (declare (ignore iri qname attrs))
  (let ((state (state parser)))
	(cond ((string-equal localname "PC-Compound")
		   (print-pubchem-cpd state))
		  ((string-equal localname "PC-InfoData")
		   (make-pc-recs parser)
		   (setf (pubchem-state-cur-attr state) (make-pubchem-attr))))
	(setf (pubchem-state-look-for state) :none))
  nil)

(defmethod net.xml.sax:content ((parser pubchem-xml-parser) content start end ignorable)
  (let* ((state (state parser))
		 (sattr (pubchem-state-cur-attr state))
		 (look-for (pubchem-state-look-for state))
		 (content (subseq content start end))
		 )
	(cond ((eq look-for :none))
		  ((eq look-for :id)
		   (setf (pubchem-state-id state) (concatenate 'string (pubchem-state-id state) content)))
		  ((eq look-for :label)
		   (setf (pubchem-attr-label sattr) (concatenate 'string (pubchem-attr-label sattr) content)))
		  ((eq look-for :name)
		   (setf (pubchem-attr-name sattr) (concatenate 'string (pubchem-attr-name sattr) content)))
		  ((eq look-for :value)
		   (setf (pubchem-attr-value sattr) (concatenate 'string (pubchem-attr-value sattr) content)))))
  nil)
