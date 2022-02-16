; Functions to find how many steps into the pathway each reaction in a pathway is. Takes the longest route back to an input (without going through any reaction twice in the case that part of the pathway is circular)

(defun save-rxn-depth-lists (filename)
  "Gets the reaction depths for all reactions in all pathways for the current pgdb and saves in a tabbed-tree format to file. Unindented lines are pathways with the second field being the max depth of that pathway. Indented lines under it are its reactions with the reaction depth as the second field"
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (loop for pwy in (all-pathways) do
          (format t "~A~%" pwy)
          (let ((depth-table (rxn-depth-for-pathway pwy)))
            (format stream "~A	~A~%" (get-frame-handle pwy) (loop for rxn being the hash-keys in depth-table maximizing (gethash rxn depth-table)))
            (setq sorted-keys (sort-hash depth-table))
            (loop for rxn in sorted-keys do
                  (format stream "	~A	~A~%" (get-frame-handle rxn) (gethash rxn depth-table)))))))
(defun sort-hash (table)
  "Takes a hash and returns the keys sorted according to their values"
  (sort (loop for key being the hash-keys in table collect key) (l (k1 k2) (< (gethash k1 table) (gethash k2 table)))))

(defun write-ordered-rxn-lists (filename)
  "Writes a file with the reaction lists for each pathway in the current pgdb, sorted by reaction depth"
  (write-file filename
              (loop for pwy in (all-pathways) do
                    (let* ((depth-table (rxn-depth-for-pathway pwy))
                           (sorted-rxns (sort-hash depth-table)))
                      (format file "~A	~{~A~^	~}~%" (get-frame-handle pwy) (mapcar #'get-frame-handle sorted-rxns))))))

(defun rxn-depth-for-pathway (pwy)
  "Returns a hash table of all the reactions in the pathway mapped to their depth into the pathway"
  (setq dist-table (make-hash-table))
  (loop for cpd in (pathway-inputs-w-screening pwy) do (rxn-depth-for-pathway-rec pwy cpd 1 dist-table NIL))
  dist-table)

(defun rxn-depth-for-pathway-rec (pwy cpd depth dist-table visited)
  "Recursive helper for (steps-in-for-pathway). Starting from cpd, goes down the graph and updates dist-table with the longest distance to each reaction. Depth is the current depth into the pathway, dist-table is the table to update, visited is a list of reactions upstream of this one (to avoid infinite loops in the case of circular pathways)"
  (unless (member cpd visited)
    (loop for rxn in (rxns-taking-cpd-in-pwy cpd pwy)
          unless (member rxn visited) do
          (if (or (null (gethash rxn dist-table)) (> depth (gethash rxn dist-table))) (setf (gethash rxn dist-table) depth))
          (let ((nxt-visited (cons rxn visited)))
            (loop for nxt-cpd in (non-redundant (primary-cpds-produced-by-rxn-in-pwy rxn pwy)) do
                  (rxn-depth-for-pathway-rec pwy nxt-cpd (+ 1 depth) dist-table (cons cpd nxt-visited)))))))

(defun rxn-direction (layout-entry)
  "Determines the direction of a reaction from the 'reaction-layout list of a pathway. Returns the first element of the list starting with :direction, which will usually be :L2R. Returns NIL if no :direction entry was found"
  (first (find-in-sublists :direction layout-entry)))
(defun find-in-sublists (key list)
  "Takes a list of the form ((:key1 val1 val2 val3) (:key2 val4 val5 val6) (:key3 val7) ...) and searches for key, returning the rest of key's list"
  (setq first-entry (first list))
  (cond
    ((null list) NIL) ; Reached the end of the list without finding key; return NIL
    ((and (listp first-entry) (eq (first first-entry) key)) (rest first-entry)) ; This is key's entry; return the rest of it
    (t (find-in-sublists key (rest list))))) ; This isn't it; keep going
(defun directions-for-pwy (pwy)
  "Returns a nonredundant list of reaction directions in a pathway"
  (non-redundant (mapcar #'rxn-direction (get-slot-values pwy 'reaction-layout))))
(defun get-step-inputs (layout-entry)
  "Returns a list of inputs to the given pathway step, taking into account its direction to determine which side of the reaction is the input. Unrecognized or absent reaction directions are treated as L2R"
  (find-in-sublists (if (eq (rxn-direction layout-entry) :R2L) :right-primaries :left-primaries) layout-entry))
(defun get-step-outputs (layout-entry)
  "Returns a list of outputs to the given pathway step, taking into account its direction to determine which side of the reaction is the output. Unrecognized or missing reaction directions are treated as L2R"
  (find-in-sublists (if (eq (rxn-direction layout-entry) :R2L) :left-primaries :right-primaries) layout-entry))

(defun rxns-taking-cpd-in-pwy (cpd pwy)
  "Returns a list of reactions in pwy that take cpd as a primary input"
  (let ((cpd (get-frame-named cpd)))
    (loop for rxn-step in (get-slot-values pwy 'reaction-layout) when (member cpd (get-step-inputs rxn-step)) collect (first rxn-step))))

(defun primary-cpds-produced-by-rxn-in-pwy (rxn pwy)
  "Returns a list of compounds produced by rxn in pwy as primary outputs"
  (let* ((rxn (get-frame-named rxn)) (rxn-entry (find-in-sublists rxn (get-slot-values pwy 'reaction-layout))))
    (get-step-outputs rxn-entry)))
