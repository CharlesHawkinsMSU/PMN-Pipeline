; Misc utility functions that are specific to pathway tools (for non-ptools-specific utulity functions, see utils.lisp)

(defun make-frame-index (class slots &key include-subclasses? downcase?)
  "Creates an index of all members of the given frame class using the given list of slots. Returns a hash from slot-values to a list of frames with that value in any of the given slots"
  (loop for frame in (append (when include-subclasses? (get-class-all-subs class))
			     (get-class-all-instances class))
	with table = (make-hash-table :test 'equal)
	do (loop for slot in (as-list slots)
		 do (loop for val in (if (eq slot t)
				       (list (get-frame-handle frame))
				       (get-slot-values frame slot))
			  for val-str = (format nil "~A" val)
			  when downcase?
			  do (setq val-str (string-downcase val-str))
			  do (puthash val-str (union (list (gfh frame)) (gethash val-str table)) table)))
	finally (return table)))



(defun print-handles (framestruct &optional (seps '(";" "," "/")))
  (cond ((listp framestruct)
	 (format nil (format nil "~~{~~A~~^~A~~}" (or (first seps) ""))
		 (loop for el in framestruct
		       collect (print-handles el (rest seps)))))
	((coercible-to-frame-p framestruct)
	 (gfh framestruct))
	(t (format nil "~A" framestruct))))
(defun print-frames-nicely (framestruct &optional (seps '(";" "," "/")))
  (cond ((listp framestruct)
	 (format nil (format nil "~~{~~A~~^~A~~}" (or (first seps) ""))
		 (loop for el in framestruct
		       collect (print-frames-nicely el (rest seps)))))
	((coercible-to-frame-p framestruct)
	 (or (gsv framestruct 'accession-1)
	     (gsv framestruct 'common-name)
	     (gsv framestruct 'synonyms)
	     (gfh framestruct)))
	(t (format nil "~A" framestruct))))

(defun read-flatfile (file)
  "Reads a flat-file and returns (as its first value) a hash of hashes for all frames in the file; outer hash maps frame IDs to frame hahses, frame hash maps key to list of values. Also returns two additional values: a list of frames with no UNIQUE-ID, and a list of lines that couldn't be interpreted (i.e. they did not look like comments, blank lines, /continuations, // delimiters, or key-value pairs)"
  (let ((dat-hash (make-hash-table))
		(frame-hash (make-hash-table))
		(last-key nil)
		(nameless-frames nil)
		(unreadable-lines nil))
	(for-lines-in-file file
					   do (cond ((starts-with-letter #\# line) nil)   ; Ignore comments
								((string-equal "//" line)                 ; A // delimits frames; put the current frame into the hash table (or nameless-frames list). Dat files are also supposed to end with // so this should still work for the last frame in the file
								 (if (setq frame-id (symbol (gethash 'unique-id frame-hash)))
								   (puthash frame-id frame-hash dat-hash)
								   (push frame-hash nameless-frames))
								 (setq frame-hash (make-hash-table))
								 (setq last-key nil))
								((starts-with-letter #\/ line)            ; Starting with a / means continue value from previous line
								 (puthash last-key
										  (concatenate 'string
													   (gethash last-key frame-hash)
													   " "
													   (subseq line 1))
										  frame-hash))
								((= 2 (length (setq kv (excl::split-re " - " line :limit 2))))  ; Standard key-value line
								 (let ((key (symbol (first kv)))
									   (val (second kv)))
								   (puthash key val frame-hash)
								   (setq last-key key)))
								(t (push line unreadable-lines))))       ; Couldn't interpret line
	(values dat-hash nameless-frames unreadable-lines)))

(defun flatfile-has-frame (frame file)
  "Reads the given attribute-value flatfile and determines whether it contains the given frame ID"
  (let ((frame-id-str (symbol-name (to-handles frame))))
	(for-lines-in-file file
					   thereis (string-equal line (format nil "UNIQUE-ID - ~A" frame-id-str)))))

(defmacro ctfp (&rest args)
  "coercible-to-frame-p"
  (cons 'coercible-to-frame-p args))

(defun current-orgid ()
  "As (current-kb) but returns an orgid"
  (kb-orgid (current-kb)))

; It's expand-frameset, not resolve-frame-spec or resolve-framespec
(defun expand-frameset (frameset)
  "Expands the given set of frames. A frame expands to itself. A class expands to all class members. A list expands to the union of the expansions of all list members."
  (labels ((expand-frameset-set (frameset)
							  (setq exset (empty-set))
							  (if (listp frameset)
								(progn (loop for f in frameset do (setq exset (nset-union exset (expand-frameset-set f))))
									   exset)
								(if (setq frame (coerce-to-frame frameset))
								  (if (class-p frame) (set-from-list (gcai frame)) (set-from-list (list frame))) (empty-set)))))
	(set-to-list (expand-frameset-set frameset))))
(defun as-kb (kb)
  "Returns kb as a kb. If it is a kb, returns it as-is. If it is not, looks up the kb with (find-kb)"
  (if (kb-p kb)
	kb
	(find-kb kb)))

(defun try-as-kb (kb)
  (if (kb-p kb)
    kb
    (ignore-errors (find-kb kb))))

(defun as-orgid (orgid)
  "Returns orgid as a orgid. If orgid is a kb, looks up the orgid for it with (kb-orgid); otherwise returns it as-is"
  (if (kb-p orgid)
	(kb-orgid orgid)
	orgid))

(defun get-taxon (orgid)
  "Returns the taxon to which orgid belongs if orgid is a PGDB or KB; otherwise returns orgid as-is"
  (as-handle
	(let ((orgid (as-orgid orgid)))
	  (if (find orgid (all-orgids))
		(let ((current (kb-orgid (current-kb))))
		  (so orgid)
		  (let ((taxon (first (get-frame-direct-parents orgid :kb (find-kb orgid)))))
			(so current) taxon))
		orgid))))

(defun as-handle (frame)
  "Gets the frame handle for frame if it is a frame, otherwise returns as-is"
  (if (coercible-to-frame-p frame)
	(get-frame-handle frame)
	frame))

(defun get-class-schema (class)
  "See the 'schema' (or whatever it is, the list of frame slots I mean) for the given class (e.g. 'Reactions')"
  (list (if (class-p class) (get-frame-named class) class) (get-frame-slots (first (get-class-all-instances class)))))

(defun export-dblinks-table (class dbs filename)
  "Saves a tab-delimited table of the unification links (dblinks) for the list of databases given by db"
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (format stream "Frame	Name	~{~A~^	~}~%" dbs)
    (loop for frame in (get-class-all-instances class) do
          (format stream "~A	~A	" (get-frame-handle frame) (get-slot-value frame 'common-name))
          (let ((dblinks (get-slot-values frame 'dblinks)))
            (format stream "~{~A~^	~}~%" (mapcar #'(lambda (db) (first (find-in-sublists db dblinks))) dbs))))))

(defun print-frame-handle-and-name (frame &key (indent 0) (stream *standard-output*) (sep ": "))
  (indent indent stream)
  (format stream "~A" (get-frame-handle frame))
  (when (setq names (get-slot-values frame 'names)) 
    (format stream "~A~{~A~^, ~}" sep names))
  (terpri stream))

(defun write-frames-html (frames htmlfile &key (site "pmn.plantcyc.org"))
  "Write an html file with a list of frames, including plantcyc links"
  (with-open-file (stream htmlfile :direction :output :if-exists :supersede)
    (format stream "<html><body>")
    (let ((kb-name (current-kb)))
      (loop for frame in frames do 
            (format stream "<p><a href=\"https://~A/~A/NEW-IMAGE?type=NIL&object=~A&redirect=T\">~A</a></p>" site (frame->orgid frame) (get-frame-handle frame) (get-frame-handle frame)))
      (format stream "</body></html>"))))

(defun concat-slot-values (frame slots)
  "Returns a list containing all values of the given slots for the given frame"
  (reduce #'append (loop for slot in slots collect (get-slot-values frame slot))))

(defun open-pgdb (dirname &key version orgid)
  "Opens a PGDB whose directory is dirname (the dir that contains default-version). If :version is given, load that version; otherwise load the version given by the default-version file. If orgid is given, use the given orgid for the PGDB; otherwise chop 'cyc' off of the directory's name."
  (if (probe-file (concatenate 'string dirname "/")) 
      (let* ((name (if orgid orgid (remove-cyc-from-name (pathname-name dirname))))
            (version (if version version (get-default-version dirname)))
            (kb-file (concatenate 'string dirname "/" version "/kb/" name "base.ocelot"))
            )
        (if (probe-file kb-file)
            (open-kb :filename kb-file :kb (create-kb (intern (string-upcase name))))
            (error "Could not find ~A" kb-file))
        
        )
      (error "~A either does not exist or is not a directory~%" dirname)))

(defun remove-cyc-from-name (name)
  "Removes \"cyc\" from the end of the given name, if it exists. Case-insensitive"
  (let* ((cyc-pos (- (length name) 3))
         (last-three (subseq name cyc-pos)))
    (if (equal (string-downcase last-three) "cyc")
        (subseq name 0 cyc-pos)
        name)))

(defun get-default-version (dirname)
  "Given a pgdb directory, get the default version"
  (let ((df-file (concatenate 'string dirname "/default-version")))
    (if (probe-file df-file)
        (first (with-open-file (df-stream df-file :direction :input) (read-list df-stream)))
        (error "Could not find ~A~%" df-file))))

(defun check-pwys-vs-metacyc (pgdb &optional (slots '(REACTION-LAYOUT REACTION-LIST PREDECESSORS PATHWAY-LINKS)))
  "Goes through all pathways in the given PGDB and checks that the given slots are the same as for that pathway in MetaCyc"
  (so pgdb)
  (let ((org-kb (find-org pgdb))
		(metacyc (find-org 'meta))
		(p NIL) (m NIL) (problem NIL))
	(loop for pwy in (to-handles (get-class-all-instances "Pathways" :kb org-kb))
		  when (setq problem (and (coercible-to-frame-p pwy :kb metacyc)
								  (loop for slot in slots
										do (setq m (to-handles (get-slot-values pwy slot :kb metacyc)))
										(setq p (to-handles (get-slot-values pwy slot :kb org-kb)))
										when (set-exclusive-or m p :test 'equal) collect slot)))
		  collect (cons pwy problem))))

(defun check-pwy-links-vs-metacyc (pgdb &key (reference 'meta) (both-ways NIL)) 
  "Goes through all pathways in the given PGDB and checks that any pathway links missing vs metacyc are to pathways that are not in this pgdb. Gives a list of pathways for which this is not the case; ones that are missing links that should be there."
  (so reference)
  (so pgdb)
  (let* ((pgdb (find-kb pgdb))
		 (metacyc (find-kb reference)))
	(loop for pwy in (get-frame-handles (get-class-all-instances "Pathways" :kb pgdb))
		  for this-pgdb-links = (get-slot-values pwy 'pathway-links :kb pgdb)
		  when (coercible-to-frame-p pwy :kb metacyc)
		  when (or (null both-ways) (missing-links (get-slot-values pwy 'pathway-links :kb metacyc) this-pgdb-links metacyc))
		  when (setq missing (missing-links this-pgdb-links (get-slot-values pwy 'pathway-links :kb metacyc) pgdb))
		  collect (cons pwy missing))))

(defun missing-links (pgdb-links metacyc-links pgdb)
  "Returns a list of pathway links that are \"missing\" from the given pgdb. Goes through each compound in metacyc-links and makes sure all its links are either present in pgdb-links' list for the same compound or refer to pathways that are absent from pgdb"
  (loop for cpd-list in metacyc-links
		nconcing (let ((pgdb-entry (assoc (car cpd-list) pgdb-links)))
					(loop for mc-pw in (cdr cpd-list)
						  for pw = (get-link mc-pw)
						  unless (find mc-pw pgdb-entry :test 'equal)
						  when (coercible-to-frame-p pw :kb pgdb)
						  collect pw))))

(defun fix-all-pwy-links (pgdbs &key (ref 'meta) (log "~/fix-pwy-links.log"))
  "Fixes all pathway links in the given pgdbs using :ref as a reference (default Metacyc)"
  (with-open-file (f log :direction :output :if-exists :supersede)
	  (let ((metacyc (find-kb ref)))
		  (for-orgids pgdbs
					  (let* ((org-kb (find-kb org))
							 (incorrect-pwys (mapcar #'first (check-pwy-links-vs-metacyc org :reference ref))))
						(format f "~A: ~A~%" org incorrect-pwys)
						(loop for pwy in incorrect-pwys do
							  (format f "~A~%" `(put-slot-values ,pwy 'pathway-links ,(pull-pwy-links-from-ref pwy org-kb metacyc)))
							  (put-slot-values pwy 'pathway-links (pull-pwy-links-from-ref pwy org-kb metacyc))))))))

(defun pull-pwy-links-from-ref (pwy pgdb ref-pgdb)
  "Gets the list of pathway links for pwy's counterpart in the given reference pgdb (usually metacyc) and merges with this pgdb's list (using combine-pwy-links), then removes links that point to pathways that are not present in the given pgdb"
	(loop for cpd-list in (combine-pwy-links pwy pgdb ref-pgdb) do (setq pruned-cpd-list (loop for link in cpd-list when (pgdb-has-linked-pwy link pgdb) collect link)) when (> (length pruned-cpd-list) 1) collect pruned-cpd-list))

(defun combine-pwy-links (pwy pgdb ref-pgdb)
  "Combines the pathway-links lists (nonredundantly) from the two pgdbs. Keeps only links to pathways in pgdb, and takes pgdb as difinitive if the two have the same link but conflict as to direction"
  (let ((pgdb-list (get-slot-values pwy 'pathway-links :kb pgdb))
		 (ref-list (get-slot-values pwy 'pathway-links :kb ref-pgdb)))
	(nconc (loop for pgdb-cpd-list in pgdb-list
				 unless (setq ref-cpd-list (assoc (car pgdb-cpd-list) ref-list))
				 collect pgdb-cpd-list
				 when ref-cpd-list
				 collect (nconc pgdb-cpd-list
								(loop for ref-link in (rest ref-cpd-list)
									  unless (has-link ref-link pgdb-cpd-list)
									  collect ref-link)))
		   (loop for cpd-list in ref-list
							when (coercible-to-frame-p (car cpd-list) :kb pgdb)
							unless (setq pgdb-cpd-list (assoc (car cpd-list) pgdb-list))
							collect cpd-list))))
(defun pgdb-has-linked-pwy (link pgdb)
  "Tests whether the given pgdb has the pathway linked to. The link should be an element of one of the sublists found in 'pathway-links; these can be either an atomic symbol name of a pathway or an improper list of the symbol name and a symbol specifying direction"
  (coercible-to-frame-p (get-link link) :kb pgdb))

(defun has-link (link list)
  "Tests if the list (a value taken from a pathway's PATHWAY-LINKS slot) contains the given pathway link"
  (let ((link (get-link link)))
	  (loop for pwy in (cdr list) thereis (equal link (get-link pwy)))))

(defun get-link (link)
  "Returns the pathway name for the given link, an element of the lists contained in a pathway's PATHWAY-LINKS slot. link can be either an improper list, in which case its first element is returned, or an atom, in which case it is returned unaltered"
  (if (listp link) (car link) link))

(defun to-handles (list)
  "Recursively goes through the list and converts all frames to frame handles. Non-frame atoms are left unchanged."
  (maptree #'(lambda (x) (if (frame-p x) (get-frame-handle x) x)) list))

(defun frame-tree-to-kb (tree kb &key (if-not-found :error))
  "Translates all frames in the given tree to the equivalent frames (same frame ID) in the given kb. Non-frame atoms are left unchanged, as is the structure of the tree"
  (cond ((listp tree) (loop for elt in tree collect (frame-tree-to-kb elt kb :if-not-found if-not-found)))
		((frame-p tree) (let ((handle (get-frame-handle tree)))
						  (if (or (coercible-to-frame-p handle :kb kb) (eq if-not-found :error))
							(get-frame-named handle :kb kb)
							NIL)))
		(t tree)))

(defun reaction-layout-to-kb (pathway kb &key (if-not-found :error))
  "Gets the reaction layout of pathway and translates the linked frames to the given kb"
  (frame-tree-to-kb (get-slot-values pathway 'reaction-layout) kb :if-not-found if-not-found))

(defun frame-to-kb (frame kb)
  "Translates one frame from one kb to another"
  (get-frame-in-kb (get-frame-handle frame) :kb kb))

(defun duplicate-pwy-links (frame)
  "Checks the given frame for duplicate pathway links"
  (let ((pwy-set (make-hash-table)))
	(loop for entry in (get-slot-values frame 'pathway-links)
		  thereis (loop for link in (rest entry) 
						for linkname = (get-link link)
						thereis (gethash linkname pwy-set)
						do (setf (gethash linkname pwy-set) t)))))
(defun check-reaction-list (pwy)
  (frame-tree-to-kb (get-slot-values pwy 'reaction-list :kb (find-org 'meta)) (current-kb)))
(defun propagate-pwy-structure (pwy)
  (put-slot-values pwy 'reaction-list (frame-tree-to-kb (get-slot-values pwy 'reaction-list :kb (find-org 'meta)) (current-kb)))
  (put-slot-values pwy 'predecessors (frame-tree-to-kb (get-slot-values pwy 'predecessors :kb (find-org 'meta)) (current-kb))))

(defmacro other-ptools (form)
  "Runs form on another ptools instance that is running in -api mode and returns the result. The form argument should be single-quoted, e.g. (other-ptools '(+ 1 2)). You can use backtick and comma to control which parts of the expression are evaluated by the local ptools instance and which by the remote one, e.g. (other-ptools `(equal ,local-var remote-var)). The macro won't work if the arguments or return value contain #<heap objects>"
  `(progn 
  (setq sock (socket:make-socket :connect :active :type :stream :address-family :file :remote-filename "/tmp/ptools-socket"))
  (format sock "~A~%" ,form)
  (force-output sock)
  (read sock)))

(defun pgdb-counts (orgids)
  "Returns an alist with counts of different classes for the given pgdbs"
  (let ((classes '("Pathways" "Reactions" "Compounds" "Proteins" "Enzymatic-Reactions")))
	(for-orgids orgids (cons org (cons
						 (get-slot-value org 'common-name)
						 (loop for c in classes collect
							   (length (get-class-all-instances c))))))))

(defun pgdb-stats (orgids)
  "Returns a plist with various stats about the given collection of pgdbs"
  (let* ((inf most-positive-fixnum)
		 (maxs `(("Pathways" ,#'true NIL 0) ("Reactions" ,#'true NIL 0) ("Compounds" ,#'true NIL 0) ("Polypeptides" ,#'true NIL 0) ("Polypeptides" ,#'enz-has-ev NIL 0) ("Publications" ,#'true NIL 0)))
		 (mins `(("Pathways" ,#'true NIL ,inf) ("Reactions" ,#'true NIL ,inf) ("Compounds" ,#'true NIL ,inf) ("Polypeptides" ,#'true NIL ,inf) ("Polypeptides" ,#'enz-has-ev NIL 0) ("Publications" ,#'true NIL ,inf))))
	(for-orgids orgids (progn (setq mins (update-classes-mins mins)) (setq maxs (update-classes-maxs maxs))))
	(list :min mins :max maxs)))


(defun update-classes-mins (currents)
  (loop for (class filter kb current) in currents collect (cons class (cons filter (update-min-of-class class filter kb current)))))

(defun update-classes-maxs (currents)
  (loop for (class filter kb current) in currents collect (cons class (cons filter (update-max-of-class class filter kb current)))))

(defun update-min-of-class (class filter kb current-min)
  (let ((l (count-if filter (get-class-all-instances class))))
	(if (<= l current-min) (list (kb-orgid (current-kb)) l) (list kb current-min))))

(defun update-max-of-class (class filter kb current-max)
  (let ((l (count-if filter (get-class-all-instances class))))
	(if (>= l current-max) (list (kb-orgid (current-kb)) l) (list kb current-max))))


(defun get-rxn-cc (&optional (rxns (get-class-all-instances "Reactions")))
  "Goes through the given list of reactions, or if none is given goes through the list of all reactions in the current pgdb, and gets all CC CO terms associated with enzymes that catalyze each reaction"
	(loop for rxn in rxns when (setq c (cc-for-rxn rxn)) collect (cons (get-frame-handle rxn) c)))
(defun get-pwy-cc (&optional (pwys (get-class-all-instances "Pathways")))
  "Goes through the given list of pathways, or if none is given goes through the list of all pathways in the current pgdb, and gets all CC CO terms associated with enzymes that catalyze reactions in each pathway"
	(loop for pwy in pwys when (setq pc (delete-duplicates (loop for rxn in (get-slot-values pwy 'reaction-list) when (cc-for-rxn rxn) nconc it))) collect (cons (get-frame-handle pwy) pc)))


;(setq d (for-orgids pmn13 (progn (other-ptools `(so ',org)) (setq p14 (mapcar #'get-frame-handle (all-pathways))) (setq p13 (other-ptools '(mapcar #'get-frame-handle (all-pathways)))) (list org (set-difference p14 p13) (set-difference p13 p14)))))
;(setq d2 (loop for (org add del) in d collect (list org (other-ptools `(loop for p in ',add when (coercible-to-frame-p p) collect p)) del)))
; (setq d3 (loop for (org add del) in d2 collect (list org add (loop for p in del when (coercible-to-frame-p p) collect p))))
;(with-open-file (f "~/changes.html" :direction :output :if-exists :supersede) (format f "<html>~%") (loop for (org add del) in d3 do (format f "<h2>~A</h2>~%<h3>Added</h3>~%<ul>~%" org) (loop for p in add do (format f "<li><a href=\"https://metacyc.org/META/NEW-IMAGE?type=PATHWAY&object=~A\">~A</a></li>~%" p p)) (format f "</ul><h3>Removed</h3><ul>") (loop for p in del do (format f "<li><a href=\"https://metacyc.org/META/NEW-IMAGE?type=PATHWAY&object=~A\">~A</a></li>~%" p p)) (format f "</ul>")) (format f "</html>"))
;(loop for (org add del) in d3 do (loop for p in d do (setf (gethash p hd)(+ 1 (gethash p hd 0)))))
;(setq hd (make-hash-table))
;(loop for (org add del) in d3 do (loop for p in del do (setf (gethash p hd)(+ 1 (gethash p hd 0)))))
;(with-open-file (f "~/pwydel.txt" :direction :output :if-exists :supersede) (loop for p being the hash-keys in hd do (format f "~A ~A~%" p (gethash p hd))))
