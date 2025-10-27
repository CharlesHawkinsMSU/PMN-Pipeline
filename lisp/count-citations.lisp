; Functions that count things in the database or pull sets of things out from the database

(defun cpd-formulas-table (orgids file)
  (to-file-or-stream file
	  (loop-orgids orgids closing
		       with found = (empty-set)
		       do (loop for c in (all-cpds)
				for h = (gfh c)
				unless (gethash h found)
				do (format stream "~A	~A~%" h (get-molecular-formula c))
				and do (add-to-set h found)))))

(defun get-molecular-formula (c)
  (let ((l (gsvs c 'chemical-formula)))
    (when l
      (loop for (e n) in l
	    when (and (ctfp e) (not (instance-all-instance-of-p e '|Elements|)))
	    do (return nil)
	    do (nstring-capitalize (symbol-name e)))
      (format nil "~{~{~A~A~}~}" l))))

; Functions for finding unbalanced reactions
(defun unbalanced-table ()
  "Returns a table (list of lists) with the output of the balance checker run on all reactions"
  (loop for r in (all-rxns)
	do (format t "~A~%" (gfh r))
	collect (list
		  (gfh r)
		  ;(yn (rxn-balanced? r))
		  (yn (reaction-balanced-p r))
		  )))

; Functions for mass-instantiating reactions
(defun write-rxn-instantiation-table (where &key existing)
  (let ((table (or existing (make-rxn-instantiation-table))))
    (write-list-n
      (cons '("Generic Rxn"
	      "Valid Instances"
	      "Invalid Instances"
	      "Non-unique Instances"
	      "Result Code")
	    table)
      '("~%" "	" "//" "=" ";")
      where)))

(defun make-rxn-instantiation-table ()
  (loop for r in (all-class-rxns)
	do (multiple-value-setq (a b c d)
	     (generic-rxn-instantiation r :force-all-instantiations t))
	collect (to-handles (list r a b c d))))

(defparameter *ec-to-rxn* (make-hash-table))
(defun rxns-with-cpds-on-opposite-sides (cpd1 cpd2)
  (let ((cpd1 (coerce-to-frame cpd1))
	(cpd2 (coerce-to-frame cpd2)))
    (when (and cpd1 cpd2)
      (loop for candidate-rxn
	    in (set-to-list
		 (set-intersect
		   (set-from-list (reactions-of-compound cpd1))
		   (set-from-list (reactions-of-compound cpd2))))
	    when (or
		   (and (find cpd1 (gsvs candidate-rxn 'left))
			(find cpd2 (gsvs candidate-rxn 'right)))
		   (and (find cpd2 (gsvs candidate-rxn 'left))
			(find cpd1 (gsvs candidate-rxn 'right))))
	    collect candidate-rxn))))

(defun index-ecs ()
  "For the current kb, creates a hash mapping from ec numbers to reactions and stores it in *ec-to-rxn* under the current orgid"
  (puthash (current-orgid)
	   (loop with d = (make-hash-table :test 'string-equal)
		 for r in (all-rxns)
		 do (loop for e in (gsvs r 'ec-number)
			  do (append-hash-list e (gfh r) d))
		 finally (return d)) *ec-to-rxn*))

(defun frames-in-one-pgdb (class kb1 kb2)
  "Returns a list of frames of the specified class that exist in kb1 but not in kb2"
  (let ((o1 (as-orgid kb1))
	(o2 (as-orgid kb2))
	(k2 (as-kb kb2)))
    (so o2)
    (so o1)
    (loop for f in (gcai class)
	  for fh = (gfh f)
	  unless (coercible-to-frame-p fh :kb k2)
	  collect fh)))

(defun terpenes ()
  (loop for c in (all-cpds)
	when (is-terpene c)
	collect c))

(defun is-terpene (cpd)
  (setq Carbon (get-frame-named 'C))
  (setq Hydrogen (get-frame-named 'H))
  (setq atoms (gsvs cpd 'structure-atoms))
  (setq num-c 0) ; number of C atoms
  (setq num-h 0) ; number of H atoms
  (and ; All conditions must be met
    ; Return false if no structure; otherwise it would appear to satisfy the requirements
    (> (length atoms) 0)
    ; This loop counts the number of C's and any explicit H's, and fails out (returns nil) if any other atoms are found
    (setq passed-a (loop for a in atoms
			 do (cond ((eq a Carbon) (setq num-c (+ 1 num-c)))
				  ((eq a Hydrogen) (setq num-h (+ 1 num-h)))
				  (t (return nil)))
			 finally (return t)))
    ; This progn counts the implicit H's and returns false if the total H's is not a multiple of 8
    (progn
      (setq nbonds (make-array (length atoms) :initial-element 0))
      (loop for (a1 a2 n) in (gsvs cpd 'structure-bonds)
	    do (setf (aref nbonds (- a1 1)) (+ (aref nbonds (- a1 1)) n))
	    do (setf (aref nbonds (- a2 1)) (+ (aref nbonds (- a2 1)) n)))
      (loop for b across nbonds
		      for i from 0
		      for a in atoms
		      when (eq a Carbon)
		      do (setq num-h (+ num-h (- 4 b))))
      (= 0 (mod num-h 8)))
    ; Return false if the number of C's is not in line with the number of H's (if C != H / 8 * 5)
    (= num-c (* (/ num-h 8) 5))))



(defun get-all-citations (classes)
  "Gets a nonredundant list of all citations from the 'citations slot in all the given classes"
  (setq cset (empty-set))
  (loop for class in classes
	do (loop for f in (gcai class)
		 do (loop for cit in (gsvs f 'citations)
			  do (add-to-set (first (excl:split-re ":" cit)) cset))))
  (set-to-list cset))


(defun frame-table (frames specs)
  "Returns a table of the specified data from the given frames, suitable for use with write-alist. Give it a framespec (a frame, class, or list of either) and a list of 2-element specs. Each spec consists of a string used as the column header and either a slot name or a function to be run on the frame. That column will be populated with either that slot for each frame (values converted to strings and multiple values separated with semicolons) or the result of running the given function on each frame"
  (cons (loop for spec in specs collect (first spec))
	  (loop for frame in (expand-frameset frames)
		collect (loop for (header to-get print-fn) in specs
			      collect (if (functionp to-get)
					(funcall (or print-fn #'print-frames-nicely) (funcall to-get frame))
					(format nil "~{~A~^;~}"
						(gsvs frame to-get)))))))


(defun rxn-enzyme-list ()
  "Create a list of reactions in the current db and all enzymes associated with each"
  (loop for r in (gcai '|Reactions|)
	collect (list (gfh r)
		      (print-frames-nicely (enzymes-of-reaction r)))))
; Functions to generate reaction formulae with compound IDs
(defun rxn-cpd-list (rxn slot)
  "For the given reaction and slot (should be 'left or 'right), gives a list of compounds (as frame IDs) and their coefficients in the reaction equation"
  (format nil "~{~A~^ + ~}"
	  (loop for cpd in (gsvs rxn slot)
		collect (format nil "~A x ~A"
				(or (get-value-annot rxn slot cpd 'coefficient) 1)
				(gfh cpd)))))

(defun dir-to-arrow (dir)
  "Given a direction as appears in the 'reaction-direction slot of a reaction, returns a string representing that direction; <- -> or ="
  (cond
    ((find dir '(physiol-left-to-right
		  irreversible left-to-right
		  left-to-right)) "->")
    ((find dir '(physiol-right-to-left
		  irreversible-right-to-left
		  right-to-left)) "<-")
    (t "=")))

(defun rxn-formula (rxn)
  (format nil "~A ~A ~A"
	  (rxn-cpd-list rxn 'left)
	  (dir-to-arrow (gsv rxn 'reaction-direction))
	  (rxn-cpd-list rxn 'right)))

(defun rxn-formula-list ()
  "Generates a list of reaction formulae for all reactions using compound IDs"
  (loop for r in (all-rxns)
	collect (rxn-formula r)))

(defun orgids-have-frame-table (orgids frames)
  "For the given list of orgids and of frames, makes a table of which PGDBs have each frame"
  (let ((frames (to-handles frames))
		(orgids (mapcar #'as-orgid orgids))
		(kbs (mapcar #'as-kb orgids)))
	(loop for orgid in orgids do (so orgid))
	(cons (cons "" orgids)
		  (loop for frame in frames
				collect (cons frame
							  (loop for kb in kbs collect (yn (coercible-to-frame-p frame :kb kb))))))))

		  
(defun make-dblink-table (class)
  "Creates a table of dblinks for the given class. Rows are instances of the class, columns are external databases"
  (let ((dbs (set-to-list (loop for frame in (gcai class)
								with db-set = (empty-set)
								do (loop for (db . rest) in (gsvs frame 'dblinks)
										 do (add-to-set db db-set))
								finally (return db-set)))))
	(append (list (append (list (gfh class)) dbs))
			(loop for frame in (gcai class)
				  for dbl = (gsvs frame 'dblinks)
				  collect (append (list (gfh frame))
								  (loop for db in dbs
										for entries = (assoc-every db dbl)
										collect (format nil "~{~A~^;~}"
														(loop for entry in entries
															  collect (second entry)))))))))
										;collect (if entry (second entry) "")))))))

(defun cyc-setops (org1 org2 class) "Does set operations on the given class of frames for the two given PGDBs. Returns a list of three lists, representing frames within <class> that are unique to org1, unique to org2, and common bwtween org1 and org2"
  (so org1)
  (setq org1s (set-from-list (mapcar #'get-frame-handle (gcai class))))
  (so org2)
  (setq org2s (set-from-list (mapcar #'get-frame-handle (gcai class))))
  (list (set-to-list (set-diff org1s org2s)) (set-to-list (set-diff org2s org1s)) (set-to-list (set-intersect org1s org2s))))
(defun cyc-setops-report (org1 org2 class file) "Writes a report on the setops to the given file. It will write a list of all frames in class that are only in org1, only in org2, and common to org1 and org2, respectively, with headers"
  (destructuring-bind (only1 only2 common) (cyc-setops org1 org2 class)
	(to-file-or-stream
	  file
	  (format stream "~A only in ~A (~A):~%" class org1 (length only1))
	  (write-list only1 stream )
	  (format stream "~A only in ~A (~A):~%" class org2 (length only2))
	  (write-list only2 stream )
	  (format stream "~A in both ~A and ~A (~A):~%" class org1 org2 (length common))
	  (write-list common stream ))))
	  

(setq tps-ec '("4.2.3.12" "4.2.3.47" "4.2.3.88" "4.2.3.101" "4.2.3.102" "4.2.3.123"))
(setq cyp-ec '("1.1.1.324" "1.11.2.4" "1.14.13.n7" "1.14.14.14" "1.14.14.16" "1.14.14.19" "1.14.14.23" "1.14.14.24" "1.14.14.25" "1.14.14.31" "1.14.14.32" "1.14.14.36" "1.14.14.37" "1.14.14.38" "1.14.14.39" "1.14.14.40" "1.14.14.41" "1.14.14.42" "1.14.14.43" "1.14.14.44" "1.14.14.45" "1.14.14.46" "1.14.14.48" "1.14.14.49" "1.14.14.50" "1.14.14.54" "1.14.14.55" "1.14.14.56" "1.14.14.57" "1.14.14.58" "1.14.14.59" "1.14.14.60" "1.14.14.61" "1.14.14.62" "1.14.14.63" "1.14.14.64" "1.14.14.65" "1.14.14.67" "1.14.14.68" "1.14.14.69" "1.14.14.70" "1.14.14.71" "1.14.14.73" "1.14.14.74" "1.14.14.75" "1.14.14.76" "1.14.14.77" "1.14.14.78" "1.14.14.79" "1.14.14.80" "1.14.14.82" "1.14.14.83" "1.14.14.84" "1.14.14.85" "1.14.14.88" "1.14.14.89" "1.14.14.90" "1.14.14.91" "1.14.14.92" "1.14.14.93" "1.14.14.94" "1.14.14.96" "1.14.14.97" "1.14.14.98" "1.14.14.99" "1.14.14.102" "1.14.14.103" "1.14.14.105" "1.14.14.106" "1.14.14.107" "1.14.14.109" "1.14.14.110" "1.14.14.111" "1.14.14.112" "1.14.14.113" "1.14.14.114" "1.14.14.115" "1.14.14.116" "1.14.14.120" "1.14.14.121" "1.14.14.122" "1.14.14.123" "1.14.14.126" "1.14.14.127" "1.14.14.128" "1.14.14.129" "1.14.14.130" "1.14.14.131" "1.14.14.132" "1.14.14.133" "1.14.14.134" "1.14.14.137" "1.14.14.139" "1.14.14.140" "1.14.14.145" "1.14.14.147" "1.14.14.148" "1.14.14.149" "1.14.14.150" "1.14.14.151" "1.14.14.152" "1.14.14.153" "1.14.14.154" "1.14.14.156" "1.14.14.157" "1.14.14.158" "1.14.14.159" "1.14.14.160" "1.14.14.161" "1.14.14.162" "1.14.14.163" "1.14.14.164" "1.14.14.165" "1.14.14.166" "1.14.14.167" "1.14.14.168" "1.14.14.169" "1.14.14.170" "1.14.14.171" "1.14.14.174" "1.14.14.175" "1.14.14.177" "1.14.14.178" "1.14.14.179" "1.14.14.180" "1.14.15.6" "1.14.15.8" "1.14.15.11" "1.14.15.13" "1.14.15.14" "1.14.15.16" "1.14.15.19" "1.14.15.22" "1.14.15.27" "1.14.15.29" "1.14.15.33" "1.14.15.35" "1.14.15.36" "1.14.15.39" "1.14.19.50" "1.14.19.51" "1.14.19.52" "1.14.19.53" "1.14.19.54" "1.14.19.65" "1.14.19.68" "1.14.19.69" "1.14.19.70" "1.14.19.72" "1.14.19.73" "1.14.19.74" "1.14.19.76" "1.14.19.79" "4.2.1.121" "1.7.1.14" "1.11.2.4" "1.14.14.1" "1.14.14.16" "1.14.14.19" "1.14.14.25" "1.14.14.37" "1.14.14.133" "1.14.14.138" "1.14.14.154" "1.14.14.184" "1.14.15.1" "1.14.15.4" "1.14.15.6" "1.14.15.10" "1.14.15.15"))

(defun pgdb-versions-table (pgdbs file)
  "Writes to FILE a tab-delimited table of the pgdbs in PGDBS and their current versions"
  (print-alist file 
			   (loop-orgids pgdbs
							collect `(,org ,(kb-version (current-kb)))
							closing)))

(defun rand-prot ()
  "Returns a randomly-selected protein"
  (rand-frame (all-proteins)))

(defun rand-gene ()
  "Returns a randomly-selected gene"
  (rand-frame (all-genes)))

(defmacro prand-prot ()
  "Prints out a randomly-selected protein"
  `(print-frame (rand-prot)))

(defun rand-frame (frameset)
  "Returns a randomly-selected frame from the given frameset"
  (pickrand (expand-frameset frameset)))

(defun prandframe (frameset)
  "Prints out a randomly-selected frame from the given frameset"
  (print-frame (rand-frame frameset)))

(defun pickrand (list)
  "Returns a randomly-selected member of the given list"
  (nth  (random (length list)) list))

(defvar *ec-to-rxns* nil)
(defun build-ec-to-rxn-table (&key force?)
  (when (or force? (null *ec-to-rxns*))
	(format t "Building EC to rxn table for plantcyc+metacyc~%")
	(setf *ec-to-rxns* (make-hash-table))
	(loop-orgids '(meta plant)
				 do (loop for r in (all-rxns)
						  do (loop for ec in (gsvs r 'ec-number)
								   do (puthash ec (add-to-or-create-set (gfh r)
																		(gethash ec *ec-to-rxns*))
											   *ec-to-rxns*))))))
(defun get-enzymes-for-ec (partial-ec &key (include-comp? t))
  (loop for ec in (if (listp partial-ec) partial-ec (list partial-ec))
		for rxns  = (get-rxns-for-ec ec)
		with eset = (empty-set)
		do (loop for r in rxns
				 when (valid-frame-p r)
				 do (loop for ezr in (gsvs r 'enzymatic-reaction)
						  when (or include-comp? (filter-exp-ev (gsvs ezr 'citations))) 
						  do (add-to-set (gsv ezr 'enzyme) eset)))
		finally (return eset)))

(defun missing-rxns-for-ec (partial-ec &key (kb (current-kb)) exclude-spontaneous? exclude-comp?)
  "Given a full or partial EC number, finds all reactions that are under that EC number and have no enzyme in the specified kb. If exclude-spontaneous? is t, spontaneous reactions will not be returned. If exclude-comp? is t, computational evidence does not count as having an enzyme; only EV-EXP and EV-AS will count"
  (so (as-orgid kb))
  (let ((kb (as-kb kb)))
	(loop for rxn in (get-rxns-for-ec partial-ec)
		  when (or (not (coercible-to-frame-p rxn :kb kb))
				   (and (not (and exclude-spontaneous? (gsv rxn 'spontaneous? :kb kb)))
						(progn (setq ezrs (gsvs rxn 'catalyzes :kb kb))
							   (not (if exclude-comp?
									  (loop for ezr in ezrs
											thereis (loop for cit in (gsvs ezr 'citations :kb kb)
														  thereis (or (search "EV-EXP" cit)
																	  (search "EV-AS" cit))))
									  ezrs)))))
		  collect rxn)))


(defun get-rxns-for-ec (partial-ec)
  "Given a partial EC number such as 1.2.3 or 4.2, finds all reactions that are associated with ECs that are part of that EC, so 1.2.3.4 or 4.2.2.6"
  (loop for ec in (get-ecs-for-partial partial-ec)
		with rxns = (empty-set)
		do (nset-union rxns (gethash ec *ec-to-rxns*))
		finally (return (set-to-list rxns))))
		

(defparameter *re-dot* (excl:compile-re "\\."))
(defun get-ecs-for-partial (partial-ec)
  "Given a partial EC number such as 1.2.3 or 4.2, finds all ECs that are part of that EC, so 1.2.3.4 or 4.2.2.6"
  (build-ec-to-rxn-table)
  (loop for ec being the hash-keys in *ec-to-rxns*
		for ecn = (symbol-name ec)
		with re = (excl:compile-re (format nil "^EC-~A(\\.|$)" (replace-re-using *re-dot* partial-ec "\\.")))
		when (excl:match-re re ecn)
		collect ec))

(defparameter *dehydrogenase-subs* '(FAD NAD NADP |Red-NADPH-Hemoprotein-Reductases| NAD-P-OR-NOP |Acceptor| |Cytochromes-C-Oxidized| |ETR-Quinones| |Ubiquinols| |ETF-Oxidized|))

(defun get-dehydrogenase-rxns ()
  (loop for r in (all-rxns) when (search "EC-1" (symbol-name (gsv r 'ec-number))) when (or (loop for c in (compounds-of-reaction r) thereis (loop for sub in *dehydrogenase-subs* thereis (eq sub (get-frame-handle c)))) (loop for ez in (gsvs r 'enzymatic-reaction) thereis (search "dehydrogenase" (gsv ez 'common-name)))) collect (get-frame-handle r)))

(defun get-enzymes-for-rxns-in-orgids (rxn-list orgid-list)
  (loop-orgids orgid-list collect (list org (loop for r in rxn-list when (coercible-to-frame-p r) collect (list r (loop for ez in (gsvs r 'enzymatic-reaction) collect (gsv ez 'enzyme))))) closing))
(defun collect-enzymes-for-rxns-in-orgids (rxn-list orgid-list)
  (loop-orgids orgid-list append (loop for r in rxn-list when (coercible-to-frame-p r) append (loop for ez in (gsvs r 'enzymatic-reaction) collect (get-frame-handle (gsv ez 'enzyme)))) closing))

(defun ecs-with-name (name)
  "Gets ec numbers of reactions with an enzrxn whose common name contains <name>"
  (setq ec-set (empty-set))
  (loop for ezr in (gcai '|Enzymatic-Reactions|)
		when (search name (gsv ezr 'common-name))
		do (when (and (setq r (gsv ezr 'reaction)) (setq ec (gsv r 'ec-number)))
			 (add-to-set ec ec-set)))
  (set-to-list ec-set))
(defun rxns-with-name (name)
  "Gets ec numbers of reactions with an enzrxn whose common name contains <name>"
  (setq rxn-set (empty-set))
  (loop for ezr in (gcai '|Enzymatic-Reactions|)
		when (search name (gsv ezr 'common-name))
		do (when (setq r (gsv ezr 'reaction))
			 (add-to-set r rxn-set)))
  (set-to-list rxn-set))


(defvar *chebi-to-cpd* nil)
(defvar *kegg-to-cpd* nil)
(defvar *knapsack-to-cpd* nil)

(defun make-dblink-tables ()
  "Creates tables mapping chebi and kegg dblinks to compounds (*chebi-to-cpd* etc.)"
  (setf *chebi-to-cpd* (make-hash-table :test 'string-equal))
  (setf *kegg-to-cpd* (make-hash-table :test 'string-equal))
  (setf *knapsack-to-cpd* (make-hash-table :test 'string-equal))
  (loop for c in (all-cpds)
		for links = (gsvs c 'dblinks)
		when (setq chebi (assoc 'chebi links))
		do (puthash (second chebi) (get-frame-handle c) *chebi-to-cpd*)
		when (setq knapsack (assoc 'knapsack links))
		do (puthash (second knapsack) (get-frame-handle c) *knapsack-to-cpd*)
		when (setq kegg (or (assoc 'ligand-cpd links) (assoc 'ligand links)))
		do (puthash (second kegg) (get-frame-handle c) *kegg-to-cpd*)))

(defun compounds-in-soycyc-table (&optional (filename "soycyc-with-ids.txt"))
  "Takes the soycyc-with-ids.txt file and resolves the frame if possible using the frame id column in the current kb then metacyc, then the chebi column, then the kegg column, then the knapsack column"
  (setq s (read-alist filename :limit 100))
  (setq prev (current-kb))
  (so 'meta)
  (so (as-orgid prev))
  (loop for (chebi soy plant name kegg knapsack cpd) in s
		for cpdid = (or (get-frame-handle (coerce-to-frame cpd))
						(get-frame-handle (coerce-to-frame cpd :kb (find-kb 'meta)))
						(gethash chebi *chebi-to-cpd* )
						(gethash kegg *kegg-to-cpd* )
						(gethash knapsack *knapsack-to-cpd* ))
		collect (list chebi soy plant name cpdid
					  (when cpdid (to-handles (reactions-of-compound
												(or (coerce-to-frame cpdid)
													(coerce-to-frame cpdid :kb (find-kb 'meta))))))
					  (when cpdid (to-handles (pathways-of-compound
												(or (coerce-to-frame cpdid)
													(coerce-to-frame cpdid :kb (find-kb 'meta))))))

					  )))

(defparameter *pub-slots-to-check* '(authors source title year url common-name agricola-id medline-uid comment))
(defun compatible-referent (pub)
  "Decides whether pub's referent frame is  likely the same publication by looking for incompatible information"
  )
(defun referent-has-more-info (pub)
  "Returns true iff pub has a referent-frame, pub is non-empty (has a value in any of the slots listed in *pub-slots-to-check*), but the referent still has info pub doesn't (there is a slot in *pub-slots-to-check* for which the referent has a value but pub does not"
  (and (setq ref (gsv pub 'referent-frame))
	   (loop for slot in *pub-slots-to-check* thereis (gsv pub slot))
	   (loop for slot in *pub-slots-to-check* thereis (and (gsv ref slot) (not (gsv pub slot))))))

(defun find-vals-with-citations (class)
  "Finds all class members with slots that have values that have a 'CITATIONS annotation"
  (loop for frame in (gcai class)
		do (setq slots
				 (loop for slot in (get-frame-slots frame)
					   do (setq vals
								(loop for val in (gsvs frame slot)
									  when (value-has-annot-p frame slot val 'citations)
									  collect (as-handle val)))
					   when vals collect (cons slot vals)))
		when slots collect (cons (get-frame-handle frame) slots)))

(defun collect-value-citations (class)
  "Returns a nonredundant set of citations associated with slot values in frames that are instances of <class>"
  (setq cset (empty-set))
  (loop for frame in (gcai class)
		do (loop for slot in (get-frame-slots frame)
				 do (loop for value in (gsvs frame slot)
						  do (loop for citation in (get-value-annots frame slot value 'citations)
								   do (add-to-set citation cset)))))
  (set-to-list cset))

(defun find-vals-with-ref-citations (class)
  "Finds all class members with slot value citations to reference frames"
  (loop for frame in (gcai class)
		when (setq slots
				   (loop for slot in (get-frame-slots frame)
						 when (setq vals
									(loop for val in (gsvs frame slot)
										  when (setq cits
													 (loop for citation in (get-value-annots frame slot val 'citations)
														   do (setq cit-frame
																	(format nil "PUB-~A" (string-upcase (multiple-value-bind (a b c) (excl:match-re "^[\\[ \"]?([^][:]*)\\]?" citation) c))))
														   when (coercible-to-frame-p cit-frame)
														   when (gsv cit-frame 'referent-frame)
														   collect citation))
										  collect (cons (as-handle val) cits)))
						 collect (cons slot vals)))
		collect (cons (get-frame-handle frame) slots)))

(defun find-vals-with-missing-citations (class)
  "Finds all class members with slot value citations to non-existant frames"
  (loop for frame in (gcai class)
		when (setq slots
				   (loop for slot in (get-frame-slots frame)
						 when (setq vals
									(loop for val in (gsvs frame slot)
										  when (setq cits
													 (loop for citation in (get-value-annots frame slot val 'citations)
														   do (setq cit-frame
																	(format nil "PUB-~A" (string-upcase (multiple-value-bind (a b c) (excl:match-re "^[\"\\t\\[ ]?([^\\]\\[: ]+)" citation) (setq found b) c))))
														   when found
														   unless (coercible-to-frame-p cit-frame)
														   collect citation
														   and do (format t "~A~%" found)))
										  collect (cons (as-handle val) cits)))
						 collect (cons slot vals)))
		collect (cons (get-frame-handle frame) slots)))


(defun citation-formats (class)
  "Goes through all members of class and enumerates the classes of citation found. It recognizes (given citation is a string of alphanumerics), \"citation\", \"[citation]\", and \"citation:things:things:...\". Anything not matching one of those patterns is listed separately."
  (setq cset (empty-set))
  (loop for frame in (get-class-all-instances class)
		do (loop for c in (gsvs frame 'citations)
				 do (cond ((string-equal "" c) (add-to-set 'blank cset))
						  ((excl:match-re "^[a-zA-Z0-9 ,_'./-]+$" c) (add-to-set 'plain cset))
						  ((excl:match-re "^\\[[a-zA-Z0-9 ,_'./-]+\\]$" c) (add-to-set 'bracket cset))
						  ((excl:match-re "^\"[a-zA-Z0-9 ,_'./-]+\"$" c) (add-to-set 'quotes cset))
						  ((excl:match-re "^[a-zA-Z0-9 ,_'./-]+:" c) (add-to-set 'colons cset))
						  ((excl:match-re "^:" c) (add-to-set 'blank-colons cset))
						  (t (add-to-set (list c frame) cset)))))
  (set-to-list cset))

(defun frames-that-cite (citation)
  "Gets a list of frames that cite one citation"
  (loop for l in (rest (first (find-frames-for-citations citation))) collect (first l)))
(defun get-referencing-frames (&optional exclude-self-refs)
  "Get a list of frames with the 'referent-frame slot; if exclude-self-refs is true, will exclude frames that reference themselves; otherwise these will be included"
  (loop for pub in (get-class-all-instances "Publications")
		when (setq ref (gsv pub 'referent-frame))
		unless (and exclude-self-refs (equal pub ref))
		collect pub))

(defun pathway-jaccard (pwy1 pwy2 &optional (slot 'reaction-list))
  "Computes the jaccard index of the reactions of pwy1 and pwy2 (intersect over union)"
  (let* ((r1 (set-from-list (get-frame-handles (get-slot-values pwy1 slot))))
		 (r2 (set-from-list (get-frame-handles (get-slot-values pwy2 slot))))
		 (n (set-intersect r1 r2))
		 (u (set-union r1 r2)))
	(/ (set-length n) (set-length u))))


(defun pathway-class-list (&optional (tlc '|Pathways|))
  (loop for pc in (get-class-all-subs tlc)
		do (setq genecol (empty-set))
		do (loop for p in (get-class-all-instances pc)
				 do (add-list-to-set (mapcar (lx (if (coercible-to-frame-p x) (or (get-slot-value x 'accession-1) (get-frame-handle x)) x))
											 (loop for r in (get-slot-values p 'reaction-list)
												   append (gene-of-rxn r)))
									 genecol))
		collect (list (or (get-slot-value pc 'common-name) (get-frame-handle pc)) (set-to-list genecol))))
;(defun pathway-class-list (&optional (tlc '|Pathways|))
;  (loop for pc in (get-class-all-subs tlc)
;		collect (list (get-frame-handle pc)
;					  (loop for p in (get-class-all-instances pc)
;							collect (mapcar (lx (if (coercible-to-frame-p x) (or (get-slot-value x 'accession-1) (get-slot-value x 'accession-2) (get-frame-handle x))x))
;											(loop for r in (get-slot-values p 'reaction-list)
;												  append (gene-of-rxn r)))))))

(defconstant *non-ribozyme-classes* '(|Proteins| |tRNAs|))
(defun belongs-to-any (frame classes)
  "Returns true iff the given frame is an instance or subclass of any of the given classes"
  (and (coercible-to-frame-p frame)
	   (or (and (class-p frame) (loop for class in classes thereis (class-all-sub-of-p frame class)))
		   (and (instance-p frame) (loop for class in classes thereis (instance-all-instance-of-p frame class))))))

(defun find-ribozymes (&optional (class "Genes") (slot 'product))
  (to-handles
	(loop for g in (get-class-all-instances class)
		  for prods = (loop for prod in (get-slot-values g slot) unless (belongs-to-any prod *non-ribozyme-classes*) collect prod)
		  when prods collect (cons g prods))))

(defun count-instances (species classes)
  "Counts non-redundant instances of all classes across all species"
	(let ((table (make-hash-table :test 'equal)))
	  (loop for c in classes do (puthash c (empty-set) table))
	  (for-orgids species
				  (loop for c in classes do
						(loop for f in (if (coercible-to-frame-p c)
										 (get-class-all-instances c)
										 (apply c '()))
							  do (add-to-set (get-frame-handle f) (gethash c table)))))
	  (loop for c in classes collect (list c (hash-table-count (gethash c table))))))

(defun all-instances (species classes)
  "Gets the counts of class instances across all the given species. Will not remove redundant instances accross species"
  (let ((table (make-hash-table)))
	(loop for c in classes do
		  (puthash c 0 table))
	(for-orgids species (loop for c in classes do
							  (setq cur (gethash c table))
							  (puthash c (+ cur (length (get-class-all-instances c))) table)))
	(hash-to-alist table)))

(defun atted-candidates (species)
  "Gives a list of pathways among the given species for which at least one species has the pathway annotated with experimental evidence and one for which it is predicted computationally"
  (so 'meta)
  (so 'plant)
  (let ((pwy-exp (make-hash-table))
		(pwy-comp (make-hash-table)))
	(for-orgids species
				(loop for pwy in (all-pathways)
					  do
					  (setq pwyn (get-frame-handle pwy))
					  (if (pwy-is-exp pwyn org)
						(append-hash-list pwyn org pwy-exp)
						(append-hash-list pwyn org pwy-comp))))
	(loop for pwy being the hash-keys in pwy-exp
		  when (setq c (gethash pwy pwy-comp))
		  collect (list pwy (gethash pwy pwy-exp) c))))
(defun pwy-is-exp (pwy org)
  "Returns t if the pathway has experimental evidence"
  (let ((metacyc (find-kb 'meta))
		(plantcyc (find-kb 'plant))
		(taxon (get-frame-handle (first (get-frame-direct-parents org)))))
	(or 
	  (and (coercible-to-frame-p pwy :kb metacyc)
		   (loop for pwy-taxon in (get-frame-handles (get-slot-values pwy 'species :kb metacyc))
				 thereis (or (equal taxon pwy-taxon)
							 (class-all-super-of-p taxon pwy-taxon :kb metacyc)
							 (class-all-sub-of-p taxon pwy-taxon :kb metacyc))))
	  (when (coercible-to-frame-p pwy :kb plantcyc)
		   (loop for pwy-taxon in (get-frame-handles (get-slot-values pwy 'species :kb plantcyc))
				 thereis (or (equal taxon pwy-taxon)
							 (class-all-super-of-p taxon pwy-taxon :kb plantcyc)
							 (class-all-sub-of-p taxon pwy-taxon :kb plantcyc)))))))
	
(defun enz-of-pwy (pwy)
  "Gets all enzymes that catalyze reactions in pwy and all sub-pathways"
  (delete-duplicates (loop for rxn in (get-slot-values pwy 'reaction-list) append (if (class-all-super-of-p "Pathways" rxn) (enz-of-pwy rxn) (enz-of-rxn rxn)))))

(defun monomer-of-pwy (pwy)
  "Gets all enzymes that catalyze reactions in pwy and all sub-pathways"
  (delete-duplicates (loop for rxn in (get-slot-values pwy 'reaction-list) append (if (class-all-super-of-p "Pathways" rxn) (monomer-of-pwy rxn) (monomer-of-rxn rxn)))))

(defun enz-of-rxn (rxn)
  "Gets all enzymes that catalyze rxn"
  (delete-duplicates (loop for ezr in (get-slot-values rxn 'enzymatic-reaction) collect (get-slot-value ezr 'enzyme))))

(defun gene-of-rxn (rxn)
  "Gets the genes for all enzymes that catalyze rxn, breaking down any complexes into monomers. No output is produced for monomers that have no gene"
  (delete-duplicates (loop for m in (monomer-of-rxn rxn) when (get-slot-value m 'gene) collect it)))

(defun monomer-of-rxn (rxn)
  "Gets all enzymes that catalyze rxn, breaking down any complexes into monomers"
  (delete-duplicates (loop for ezr in (get-slot-values rxn 'enzymatic-reaction) when (setq e (get-slot-value ezr 'enzyme)) append (monomer-of-cpx e))))

(defun monomer-of-cpx (cpx)
  "Gets the monomers of cpx. Returns a list containing only cpx itself if it is not a complex"
  (if (class-all-super-of-p "Protein-Complexes" cpx) (get-slot-values cpx 'components) (list cpx)))

(defun cc-for-rxn (rxn)
  "Gets GO CC terms for any enzyme that catalyzes rxn"
	(delete-duplicates (to-handles (append (first-n 10 (get-slot-values rxn 'rxn-locations)) (loop for e in (enz-of-rxn rxn) when e append (get-slot-values e 'locations))))))

(defun count-comments (class)
  "Counts the comments in the given class"
	(loop for e in (get-class-all-instances class) count (get-slot-value e 'comment)))

(defun get-comments (class)
  "Get all non-NIL comments for the given class"
  (loop for e in (get-class-all-instances "Chemicals") when (get-slot-value e 'comment) collect it))

(defun count-comments-of-classes (classes)
  "Counts the comments in all the given classes"
  (loop for c in classes collect (list c (count-comments c))))

(defun merge-species-counts (taxtable &key (keep-above? t))
  "Takes a hash-table mapping from taxa to some number (for example, the number of pathways curated to that taxon). Creates and returns a new hash table that is the same except: Any sub-species taxa will be merged into their respective species, with the associated numbers added. If :keep-above? is true, taxa above the rank of species will be kept in the new table as-is; otherwise they will be discarded"
  (let ((merged-table (make-hash-table)))
	(loop for taxon being the hash-keys in taxtable
		  for count = (gethash taxon taxtable)
		  for species = (get-frame-handle (get-species-above taxon))
		  do (cond (species (add-to-hashvalue species count merged-table))
				   (keep-above? (puthash taxon count merged-table))))
	merged-table))
	

(defun get-species-above (taxon)
  "Returns the species to which the given taxon belongs, if it is a taxon below the rank of species. If the taxon is a species itself, taxon is returned. If taxon is above the rank of species, NIL is returned. An error saying that Frame NIL does not reside in the current kb usually means you gave it a non-taxon frame"
  (let ((rank (symbol (get-slot-value taxon 'rank))))
	(cond ((eq rank '|species|) taxon)
		  ((eq (get-frame-handle taxon) 'tax-1) NIL)
		  (t (get-species-above (first (get-frame-direct-parents taxon)))))))

(defun up-to-species (taxon)
  "Like get-species-above, but returns a list including taxon and all supertaxa up to the species level."
  (let ((rank (symbol (get-slot-value taxon 'rank))))
	(cond ((eq rank '|species|) (list (get-frame-handle taxon)))
		  ((eq (get-frame-handle taxon) 'tax-1) NIL)
		  (t (let ((taxa-above (up-to-species (first (get-frame-direct-parents taxon)))))
			   (if taxa-above
				 (cons taxon taxa-above)
				 NIL))))))

(defun get-terminal-taxa (&key (from 'tax-1) (list? nil))
  "Gets a set of the taxon ranks of the leaf nodes of the taxon tree rooted at :from (default is the \"all\" taxon). If :list? is t, returns an alist from the ranks to all members of that rank that are leaf nodes. Otherwise returns a simple list of ranks"
  (let ((table (make-hash-table)))
	(labels ((gtt-r (taxon list? htable)
					(let ((rank (symbol (get-slot-value taxon 'rank)))
						  (subtaxa (get-class-direct-subs taxon)))
					  (if subtaxa
						(loop for subtaxon in subtaxa do
							  (gtt-r subtaxon list? htable))
						(if list?
						  (append-hash-list rank (get-frame-handle taxon) htable)
						  (puthash rank t htable))))))
	  (gtt-r from list? table)
	  (if list?
		(hash-to-alist table)
		(set-to-list table)))))


(defun count-annotated-frames-all-species (&key (class "Pathways") (ref 'plant) (species-only? nil))
  "Gets all frames of the given class in the given reference db annotated to any species, and returns an alist of the species with counts of pathways annotated to them. If :species-only? is true, sub-species taxa will be merged into their species (so frames annotated to, e.g., A. thaliana, A. thaliana col, and A. thatliana ler, would all be treated as just belonging to A. thaliana). Additionally, :species-only? causes taxa above the rank of species to be excluded from the list. 'class' can be an explicit list of frames instead of a class"
  (so ref)
  (let ((table (make-hash-table)))  ; 'table' will map from species to counts of frames annotated to that species
	(loop for frame in (if (coercible-to-frame-p class) (get-class-all-instances class) class)  ; Loop over frames in the database
		  do (loop for assigned-sp in (get-slot-values frame 'species)  ; For each frame, loop over species it's annotated to
				   for sp = (if species-only? (get-species-above assigned-sp) assigned-sp)
				   when sp
				   do (inc-hash-count (get-frame-handle sp) table)))  ; For each species, increment its count in the hash-table
	; Now we will convert to an alist and convert the taxids to species (and, if applicable, strain) names
	(so 'meta)
	(loop for (taxid count) in (hash-to-alist table)  ; Destructuring loop to get the taxid and count for each entry in the table
		  ; This do block tries to find a taxon frame that has info on the taxid. It first tries metacyc and, if it isn't there, plantcyc. If it isn't in plantcyc ether then we'll just put nil, which will result in the taxid being left as-is and not translated into a species name
		  do (setq taxframe
				   (if (coercible-to-frame-p taxid)
					 (get-frame-named taxid)
					 (when (coercible-to-frame-p taxid :kb (find-kb 'plant))
					   (get-frame-named taxid :kb (find-kb 'plant)))))
		  ; This collect block gets the taxon name and puts it together with the count as an entry in the alist. If there is a strain name we include it. If the taxon frame wasn't found in the above do block we just leave the taxid as-is
		  when taxframe
		  when (is-viridiplantae taxframe)
		  collect (list taxid
					  (let ((name (get-slot-value taxframe 'common-name))
								(strain (get-slot-value taxframe 'strain-name)))
							(if strain (format nil "~A ~A" name strain) name)) count))))
(defun count-annotated-pathways-and-enzymes (orgids &optional (ref 'plant))
  "Counts the pathways and enzymes in the given reference pgdb (default plantcyc) annotated to each organism in the list of orgids, or to any subspecies. Output is a plist of the form (:ENZ X :PWY Y) where X and Y are alists of the form ((TAXID NAME COUNT)) where TAXID is the taxon ID, NAME is the org name, and COUNT is the number of enzymes or pathways annotated to that org"
  (let ((orgs-info (for-orgids orgids (list (get-frame-handle (first (get-frame-direct-parents org))) (get-slot-value org 'common-name)))))
	(so ref)
	(list
	  :ENZ (loop for (taxid taxname) in orgs-info collect (list taxid taxname (num-enzymes-for-taxon taxid)))
	  :PWY (loop for (taxid taxname) in orgs-info collect (list taxid taxname (num-pathways-for-taxon taxid))))))

(defun exp-plant-rxns (&key (ref 'plant))
  "Gets a list of reactions with at least one experimentally-validated enzyme in plants"
  (so ref)
  (let ((rxn-set (empty-set)))
	(loop for e in (all-enzymes)
		  when (loop for s in (get-slot-values e 'species)
				   thereis (is-viridiplantae s))
		  do (loop for enzrxn in (enzrxns-for-enz e)
				   when (loop for cit in (get-slot-values enzrxn 'citations) thereis (search "EXP" cit))
				   do (add-to-set (get-frame-handle (get-slot-value enzrxn 'reaction)) rxn-set)))
	(set-to-list rxn-set)))

(defun get-taxid-strings (orgids)
  "Returns a list of taxid strings for the listed orgids"
  (for-orgids orgids (symbol-name (get-frame-handle (first (get-frame-direct-parents org))))))
(defun get-taxid-symbols (orgids)
  "Returns a list of taxid strings for the listed orgids"
  (for-orgids orgids (get-frame-handle (first (get-frame-direct-parents org)))))
(defun num-pathways-for-species (species)
  "Gets a count of all pathways annotated to the given species (does not consider a species equal to its subspecies)"
  (loop for p in (all-pathways) count (find species (mapcar #'get-frame-handle (get-slot-values p 'species)) :test 'equal)))
(defun count-annotated-frames-per-species (class &key (ref 'plant) (slot 'species))
  "Looks through the given reference database (by default plantcyc) and examines the frames of the given class for which taxids each of them is annotated to via the given slot (by default 'species). Returns an alist of all such taxids, their common name, and the number of frames annotated to them"
  (so ref)
  (let ((table (make-hash-table)))
    (loop for frame in (expand-frameset class)
	  do (loop for taxon in (get-slot-values frame slot)
		   for species = (when taxon (get-species-above taxon))
		   when species
		   do (inc-hash-count (get-species-above species) table)))
    (loop for taxid being the hash-keys in table collect (list (get-frame-handle taxid) (get-slot-value taxid 'common-name) (gethash taxid table)))))
									
(defun num-pathways-for-taxon (taxon &key (ref 'meta))
  "Gets a count of all pathways annotated to the taxon or any sub- or super-taxa"
  (let ((metacyc (find-kb ref)))
	  (and (coercible-to-frame-p taxon :kb metacyc)
		   (loop for p in (get-class-all-instances "Pathways" :kb metacyc)
				 count (loop for pwy-taxon in (get-frame-handles (get-slot-values p 'species :kb metacyc))
							 thereis (or (equal taxon pwy-taxon)
										 (class-all-super-of-p taxon pwy-taxon :kb metacyc)
										 (class-all-sub-of-p taxon pwy-taxon :kb metacyc)))))))

(defun num-enzymes-for-species (species)
  "Gets a count of all enzymes annotated to the given species (does not consider a species equal to its subspecies)"
  (loop for p in (all-enzymes) count (find species (mapcar #'get-frame-handle (get-slot-values p 'species)) :test 'equal)))

(defun num-enzymes-for-taxon (taxon)
  "Gets a count of all enzymes annotated to the taxon or any sub- or super-taxa"
  (and (coercible-to-frame-p taxon)
	   (loop for p in (all-enzymes)
			 count (loop for enz-taxon in (get-frame-handles (get-slot-values p 'species))
						 thereis (or (equal taxon enz-taxon)
									 (class-all-super-of-p taxon enz-taxon)
									 (class-all-sub-of-p taxon enz-taxon))))))

(defun frames-common-to-clade (class clade orgids)
  "Gives a list of frames of the given class that are universal among those organisms from orgids that are within the given clade"
  (let ((frame-set NIL))
	(for-orgids orgids (when (and (coercible-to-frame-p clade) (class-all-super-of-p clade org))
						 (setq frame-set (if (not frame-set)
										   (set-from-list (get-frame-handles (get-class-all-instances class)))
										   (set-intersect frame-set (set-from-list (get-frame-handles (get-class-all-instances class))))))))
	(set-to-list frame-set)))
(defun frames-present-in-clade (class clade orgids)
  "Gives a list of frames of the given class that are present in any species from orgids that are within the given clade"
  (let ((frame-set (empty-set)))
	(for-orgids orgids (when (and (coercible-to-frame-p clade) (class-all-super-of-p clade org))
						 (setq frame-set (nset-union frame-set (set-from-list (get-frame-handles (get-class-all-instances class)))))))
	(set-to-list frame-set)))

(defun frames-unique-to-clade (frames class clade orgids)
  "Gives a list of frames from the given set that are, within the organisms orgids, unique to the given clade"
  (let ((frame-set (set-from-list frames)))
	(for-orgids orgids (when (not (and (coercible-to-frame-p clade) (class-all-super-of-p clade org)))
						 (setq frame-set (nset-diff frame-set (set-from-list (get-frame-handles (get-class-all-instances class)))))))
	(set-to-list frame-set)))


(defparameter dicots 'tax-71240)
(defparameter land-plants 'tax-3193)
(defparameter viridiplantae 'tax-33090)

(defun count-class-members (orgids classes)
  "Get a non-redundant count of members of the given frame classes across the given PGDBs. \"Non-redundant\" is judged by frame ID, so PWY-1234 in AraCyc and PWY-1234 in CornCyc will only count as one pathway"
  (let ((tables
	  (loop for cl in classes collect (list cl (make-hash-table)))))
    (loop-orgids orgids do (loop for (cl table) in tables do (add-list-to-set (get-frame-handles (get-class-all-instances cl)) table)) closing)
    (loop for (cl table) in tables collect (list cl (set-length table)))))

(defun avg-class-members (orgids classes)
  "Gets the average count of the given class members across the given PGDBs"
  (loop for cl in classes
	collect (list cl
		      (loop-orgids orgids
				   closing
				   sum (length (gcai cl)) into s
				   finally (return (float (/ s (length orgids))))))))

(defun count-class-members-with-dblink (orgids class db)
  (let ((tab (make-hash-table)))
	(for-orgids orgids
				(add-list-to-set (get-frame-handles
								   (loop for f in (get-class-all-instances class) when (assoc db (get-slot-values f 'dblinks)) collect f)) tab))
	(set-length tab)))

(defun count-exp-enz (orgids)
  "Count the number of experimentally-supported enzymes across the given orgids"
  (let ((tab (make-hash-table)))
	(for-orgids orgids (add-list-to-set (get-frame-handles (enz-with-exp-ev)) tab))
	(set-length tab)))

(defun count-plants (&key (pgdb 'plant)(clade viridiplantae)(from-frame-classes '(|Pathways| |Proteins|)))
  "Counts the number of unique viridiplantae species represented by the pathways in the given org-kb"
  (so (as-orgid pgdb))
  (setq sp (make-hash-table))
  (loop for c in from-frame-classes
	do (loop for p in (get-class-all-instances c) do 
		 (loop for s in (get-slot-values p 'species)
		       do (setq rs (car (last (up-to-species s))))
		       unless rs
		       do (format t "~A~%" s)
		       do (add-to-set (gfh rs) sp))))
  (loop for s in (set-to-list sp) when s count (is-viridiplantae s clade)))

(defun is-viridiplantae (taxon &optional (clade viridiplantae))
  "Returns the given taxon if it is a member of viridiplantae (land plants and green algae), or NIL otherwise"
  (if (class-p taxon) (class-all-sub-of-p taxon clade) (class-all-type-of-p taxon clade)))
(defun rxns-for-enz (enz)
  (loop for ezr in (get-slot-values enz 'catalyzes) collect (get-slot-value ezr 'reaction)))
(defun enzrxns-for-enz (enz)
	"Get enzrxns catalyzed by an enzyme"
		(get-slot-values (get-frame-named enz) 'CATALYZES))

(defun count-all-citations (&key (class 'frames))
  "Returns a nonreduntant count of all citations actually referenced by frames in a 'CITATIONS slot"
  (set-length
    (loop for f in (frames-that-have class 'citations)
	  with s = (empty-set)
	  do (loop for c in (gsvs f 'citations)
		   do (add-to-set (extract-citation-from-string c) s))
	  finally (return s))))
(defun count-citations-for-enzrxn (enzrxn)
	"Count the number of evidence citations in a particular enzrxn"
	(loop for cit in (enzrxns-for-enz enz) count (search "EV" cit)))

(defun count-citations-each-enzrxn-of-enzyme (enz)
	"Count the number of evidence citations in each of the enzrxns of a particular enzyme"
	(loop for enzrxn in (enzrxns-for-enz enz) collect (loop for cit in (get-slot-values enzrxn 'citations) count (search "EV" cit))))

(defun count-citations-for-enzyme (enz)
	"Count the number of evidence citations in all of the enzrxns of a particular enzyme"
	(loop for enzrxn in (enzrxns-for-enz enz) sum (loop for cit in (get-slot-values enzrxn 'citations) count (search "EV" cit))))

(defun citations-for-enz-and-enzrs-and-gene (enz)
  "Gets a list of citations for the enzyme enz and its enzreactions and its gene"
  (append
         (get-slot-values enz 'citations)
         (apply #'append
                (loop for enzr in (enzrxns-for-enz enz) collect (get-slot-values enzr 'citations)))
         (when (setq gene (get-slot-value enz 'gene)) (get-slot-values gene 'citations))))

(defun enz-has-ev (enz)
  "Given an enzyme, returns t if enz has any experimental evidence for any function"
  (filter-exp-ev (citations-for-enz-and-enzrs-and-gene enz)))

(defun filter-exp-ev (ev-list)
  "Given a list of citation codes, pulls out those that are notated as experimental evidence"
  (loop for ev in ev-list when (or (search "EV-EXP" ev) (search "EV-AS" ev)) collect ev))

(defun enz-with-exp-ev ()
  "Returns a list of enzymes that have experimental evidence for themselves, their gene, or at least one enzrxn"
  (loop for enz in (get-class-all-instances "Polypeptides") when (enz-has-ev enz) collect enz))

(defun count-enz-with-ev ()
	"How many enzymes have at least one enzrxn with evidence?"
	(loop for enz in (all-enzymes) when (< 0 (loop for enzrxn in (get-slot-values enz 'CATALYZES) sum (loop for cit in (get-slot-values enzrxn 'citations) count (search "EV" cit)))) count it))

(defun enz-no-ev ()
	"Get names of enzymes that have no evidence for any reactions"
	(loop for enz in (all-enzymes) when (= 0 (loop for enzrxn in (get-slot-values enz 'CATALYZES) sum (loop for cit in (get-slot-values enzrxn 'citations) count (search "EV" cit)))) collect (get-frame-handle enz)))

(defun pathways-that-have (slot) "Get names of pathways that have any value in a particular slot" (loop for pwy in (all-pathways) when (get-slot-values pwy slot) collect pwy))
(defun rxns-that-have (slot) "Get names of reactions that have any value in a particular slot" (loop for rxn in (all-rxns) when (get-slot-values rxn slot) collect rxn))
(defun cpds-that-have (slot) "Get names of compounds that have any value in a particular slot" (loop for cpd in (all-cpds) when (get-slot-values cpd slot) collect cpd))
(defun frames-that-have (class slot) "Get names of frames of the given class that have any value in a particular slot" (loop for cpd in (get-class-all-instances class) when (get-slot-values cpd slot) collect cpd))
(defun frames-that-dont-have (class slot) "Get names of frames of the given class that don't have any value in a particular slot" (loop for cpd in (get-class-all-instances class) unless (get-slot-values cpd slot) collect cpd))

(defun rxns-for-all-pwys ()
	"Get reaction list for all pathways"
	(loop for pwy in (all-pathways) when 
		(setq c 
			(get-slot-values pwy 'REACTION-LIST))
	collect
		(cons (get-frame-handle pwy) c)))

(defun pwy-ev-quotient ()
	"Get pathways with experimental reaction evidence. If pathway PWY-1111 has 4 reactions of which 2 have exp evidence, then it returns ('PWY-1111 2 4)"
	(loop for pwy in (all-pathways) when 
		(< 0 (first (setq ev-q 
			(cons
				(loop for rxn in (get-slot-values pwy 'REACTION-LIST) when
					(< 0 (if (get-slot-value rxn 'ENZYMATIC-REACTION)
						(loop for enzrxn in (get-slot-values rxn 'ENZYMATIC-REACTION)  when
							(< 0 (loop for ev in (get-slot-values enzrxn 'CITATIONS) when
								(search "EV-EXP" ev)
							count ev))
						count enzrxn) 0))
				count rxn)
				(cons (length (get-slot-values pwy 'REACTION-LIST)) '())))))
	collect
		(cons (get-frame-handle pwy) ev-q)))

(defun file-ev-q (filename)
	"Print a table of pathways with reactions that have at least one enzrxn that has at least one citation that contains EV-EXP"
	(with-open-file (stream filename :direction :output :if-exists :supersede)
		(format stream "Pathway	Rxns with Ev	Rxns in Pwy	Percentage with Ev~%")
		(loop for ev-q in (pwy-ev-quotient) do
			(format stream "~A	~D	~D	~D%~%" (first ev-q) (second ev-q) (third ev-q) (* 100.0 (/ (second ev-q) (third ev-q)))))))

(defun pwy-with-enzrxns ()
	"Pathways with enzrxns"
	(loop for pwy in (all-pathways) when 
		(< 0 (first (setq ev-q 
			(cons
				(loop for rxn in (get-slot-values pwy 'REACTION-LIST) when
					(get-slot-value rxn 'ENZYMATIC-REACTION)
				count rxn)
				(cons (length (get-slot-values pwy 'REACTION-LIST)) '())))))
	collect
		(cons (get-frame-handle pwy) ev-q)))

(defun citations-for-all-pwys ()
	"Get a list of citations in the CITATIONS slots of pathways"
	(loop for pwy in (all-pathways) when (setq c (cons (get-frame-handle pwy) (get-slot-values pwy 'CITATIONS))) collect c))

(defun get-one-slot-values (class slot)
	"Get the slot values for slot for the first instance of class"
	(get-slot-values (first (get-class-all-instances class)) slot))

(defun dump-all (class filename)
  "Dump a newline-delimited list of all members of class to filename"
  (with-open-file (stream filename :direction :output :if-exists :supersede) (loop for inst in (get-class-all-instances class) do (format stream "~A~%" (get-frame-handle inst)))))

(defun rate-limiting-rxns ()
	"Prints a list of pathways that have rate-limiting reactions, the reaction(s) that are rate-limiting, and all enzymes that catalyze those reactions"
	(remove-if 
		(lx (null (cadr x)))
		(mapcar (l (pwy)
			(list pwy (mapcar (l (rxn)
				(list rxn (enzymes-of-reaction rxn))) (get-slot-values pwy 'Rate-Limiting-Step)))) (all-pathways))))
;(defun rate-limiting-rxns ()
; "Prints a list of pathways that have rate-limiting reactions, the reaction(s) that are rate-limiting, and all enzymes that catalyze those reactions"
; (remove-if 
;  (lx (null (cadr x)))
;  (mapcar (l (pwy)
;		   (list pwy (get-slot-values pwy 'Rate-Limiting-Step))) (all-pathways))))

(defun get-all-facet-values (frame slot)
	"Gets the values for all facets of the slot of the frame"
	(let ((facets (get-slot-facets frame slot))) 
		(loop for facet in facets collect (cons facet (get-facet-values frame slot facet)))))

(defun get-all-slot-values (frame)
	"Get the values for all slots of frame"
	(loop for slot in (get-frame-slots frame) collect (cons slot (get-slot-values frame slot))))

(defun get-present-slot-values (frame)
    "Get the values for all slots of frame that are present in the frame (i.e. that are non-NIL)"
        (loop for slot in (get-frame-slots frame) when (setq vals (get-slot-values frame slot)) collect (cons slot vals)))
(defun slot-doc (frame slot)
	"Get the documentation for a slot"
		(get-facet-value frame slot :documentation))

(defun nonmember-rxns ()
  "Finds frames that are members of class \"Reactions\" that are not returned by (all-rxns)"
  (let ((allr (set-from-list (all-rxns) :test 'equal)))
    (loop for rxn in (get-class-all-instances "Reactions") unless (set-member rxn allr) collect rxn)))

(defun write-enzymes-and-rxns (enz-set filename)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (loop for enz in enz-set do
          (format stream "~A~%" (get-frame-handle enz))
          (loop for enzr in (enzrxns-for-enz enz) do
                (let ((rxn (get-slot-value enzr 'reaction)))
                  (format stream "	~A~%" (get-frame-handle enzr)))))))

(defun write-enzymes-and-rxns-html (enz-set filename &key (org 'ara) (db "https://pmn.plantcyc.org"))
  (so org)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    ;; At some point I am going to learn a proper html library and this code shall look very silly
    (format stream "<html>~%<body>~%")
    (loop for enz in enz-set do
          (setq enz-id (get-frame-handle enz))
          (format stream "<p><a href=\"~A/gene?orgid=~A&id=~A\">~A</a>" db org enz-id enz-id)
          (when (setq enz-name (get-slot-value enz 'names)) (format stream ": ~A" enz-name))
          (format stream "</p>~%")
          (loop for enzr in (enzrxns-for-enz enz) do
                (setq rxn (get-slot-value enzr 'reaction))
                (setq rxn-id (get-frame-handle rxn))
                (format stream "<p style=\"margin-left: 40px;\"><a href=\"~A/~A/NEW-IMAGE?type=REACTION&object=~A\">~A</a>" db org rxn-id rxn-id)
                (when (setq rxn-name (get-slot-values rxn 'names)) (format stream ": ~{~A~^; ~}" rxn-name))
                (format stream "</p>~%")))))

(defun frames-with-dblink-across-orgids (class db orgids)
  "Gives a set of frames of class class that have unification links to the database db, nonredundantly across orgids. Compounds that appear in multiple orgids will be returned for the first orgid in the list orgids in which they appear"
  (setq frame-table (make-hash-table))
  (for-orgids orgids 
              (loop for frame in (get-class-all-instances class) when (find-in-sublists db (get-slot-values frame 'dblinks)) do (add-to-set (get-frame-handle frame) frame-table)))
  (set-to-list frame-table))

(defun pwys-with-trans ()
  "Returns a list of pathways that contain transport reactions"
  (remove-duplicates
    (reduce #'append
            (loop for rxn in (get-class-all-instances "Transport-Reactions")
                  when (setq p (get-slot-values rxn 'in-pathway))
                  collect p))))

(defun pwy-rxn-count-hist ()
  (let ((hist (make-hash-table)))
    (loop for p in (all-pathways) do (inc-hash-count (length (get-slot-values p 'reaction-layout)) hist))
    hist))

(defun get-frame-handles (frames)
  "Takes a list of frames and returns their frame handles. Faster but less flexible than (to-handles)"
  (mapcar #'get-frame-handle frames))

(defun lookup-protein-ids-in-reverse-mapping-file (filename protein-list) "Looks up the protein ids in the specified list of proteins in the mapping file that is a tab-delimited file with new ids in the first column and all old ids that map to it in the second column"
  (let ((h (invert-alist-to-hash (read-alist filename))))
    (mapcar #'flatten
          (loop for enz in protein-list when
                (setq ee (loop for n in
                               (concat-slot-values enz '(names synonyms accession-1))
                               when (setq nn (gethash n h)) collect nn))
                collect (list (get-frame-handle enz) (remove-duplicates ee :test 'equal))))))
(defun no-name-enzr (&optional (enzrs (get-class-all-instances "Enzymatic-Reactions")))
  "Find enzrxns in the current db with no name"
  (loop for e in enzrs when (null (get-slot-value e 'names)) collect e))

(defun write-slot-table (filename class slots)
  "Writes a tab-delimited table of the requested slots for all frames that are members of the given class, multiple values semicolon-delimited"
  (with-open-file (f filename :direction :output :if-exists :supersede)
	(format f "Frame-ID	~{~A~^	~}~%" slots)
	(loop for frame in (get-class-all-instances class) do
		  (format f "~A	~{~{~A~^;~}~^	~}~%" (get-frame-handle frame)
				  (loop for slot in slots collect (get-slot-values frame slot))))))
(defun slot-set (frameset &rest args)
  "Gives a nonredundant set of slots for the given frameset (see expand-frameset)"
  (let ((slot-set (empty-set)))
	(loop for frame in (expand-frameset frameset)
		  do (add-list-to-set (eval (append `(get-frame-slots ,frame) args)) slot-set))
	(set-to-list slot-set)))


; Some obvious missing functions

(defun all-regulation () (gcai "Regulation"))
(defun all-proteins () (gcai "Proteins"))
(defun all-enzrxns () (gcai "Enzymatic-Reactions"))
(defun all-publications () (gcai "Publications"))
(defun all-class-rxns ()
  (loop for r in (gcai "Reactions")
		when (loop for c in (append (gsvs r 'left) (gsvs r 'right))
				   thereis (class-p c))
		collect r))

; Quick access to specific slots

(defun enzyme (frame) (get-slot-values frame 'enzyme))
(defun gene (frame) (get-slot-values frame 'gene))
(defun names (frame) (get-slot-values frame 'names))
(defun synonyms (frame) (get-slot-values frame 'synonyms))
(defun catalyzes (frame) (get-slot-values frame 'catalyzes))
(defun enzyme-l (frames) (mapcar #'enzyme frames))
(defun catalyzes-l (frames) (mapcar #'catalyzes frames))

(defmacro gfh (&rest r) (cons 'get-frame-handle r))

(defun try-gsv (frame slot)
  (when (coercible-to-frame-p frame)
	(gsv frame slot)))

(defvar *species-to-cycs* nil "Hash that maps from species taxon IDs to a list of pgdb names")
(defun generate-species-to-cyc (orgids)
  "Generates the *species-to-cyc* table for the given list of orgids"
  (setq *species-to-cycs* (make-hash-table))
  (loop-orgids orgids closing
			   do (setq species (get-frame-handle (get-species-above (first (get-frame-direct-parents org)))))
			   do (puthash species (cons org (gethash species *species-to-cycs*)) *species-to-cycs*)))

(defun species-to-cyc-table (infile outfile)
  "Reads a tab-delimited table whose first column is taxon IDs of species and adds a column to it with the names of pgdbs that correspond to that species or any subspecies")

(defun rxns-with-ec (ec)
  "Finds reactions with an EC number starting with the given EC digits, for example \"1.23.1\" will find EC-1.21.1, EC-1.21.1.4, etc."
  (let ((re (excl:compile-re (format nil "^EC-~A" ec))))
	(loop for r in (all-rxns) when (loop for ec in (gsvs r 'ec-number) thereis (excl:match-re re (symbol-name ec))) collect r)))

(defun ec-table (ec)
  (let ((exp-re (excl:compile-re "EV-EXP")))
	(loop for rx in (rxns-with-ec ec)
		  for ezrs = (gsvs rx 'enzymatic-reaction)
		  collect (append (list (gfh rx) (gsvs rx 'ec-number))
						(loop for ezr in ezrs
							  for prots = (mapcar #'protein-name (monomer-of-cpx (gsv ezr 'enzyme)))
							  when (loop for cit in (gsvs ezr 'citations)
										 thereis (excl:match-re exp-re  cit))
							  collect prots into exp
							  else collect prots into comp
							  finally (return (list exp comp)))))))
(defun protein-name (p)
  "Tries to find a name for the given protein"
  (or (gsv p 'accession-1)
	  (gsv p 'accession-2)
	  (and (setq g (gsv p 'gene))
		   (or (gsv g 'accession-1)
			   (gsv g 'accession-2)))
	  (gsv p 'common-name)
	  (and g (gsv g 'common-name))
	  (symbol-name (gfh p))))


; (print-alist "plant-reg.txt" (loop for r in (all-regulation) do (setq src (gsv r 'regulator)) (setq dest (gsv r 'regulated-entity)) when src when dest collect (list (gfh src) (gsv r 'mode) (gfh (gsv dest 'enzyme)) (gfh (gsv dest 'reaction)) (gsv r 'mechanism) (try-gsv (gsv (gsv dest 'enzyme) 'species) 'common-name))))
