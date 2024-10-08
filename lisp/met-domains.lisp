; Functions to generate updated metabolic domain files for pathways, reactions, compounds, and enzymes
; The write-domain-file function writes the final table to a file
; The final domain table is stored as a hash of vectors, where the hash-key is the frame ID and the vector is a list of 1's and 0's for membership / nonmembership in each domain, in the order given in the variable domain-list

(defparameter domain-list '(("Amines and polyamines" "AMINE-DEG" "Polyamine-Biosynthesis") ("Amino acids" "Amino-Acid-Biosynthesis" "Amino-Acids-Modification" "Amino-Acid-Degradation") ("Carbohydrates" "Carbohydrates-Degradation" "Carbohydrates-Biosynthesis") ("Cofactors" "COFACTOR-DEGRADATION" "Cofactor-Biosynthesis") ("Detoxification" "Detoxification") ("Energy" "Energy-Metabolism") ("Fatty acids and lipids" "Fatty-Acid-and-Lipid-Degradation" "Lipid-Biosynthesis") ("Hormones" "HORMONE-DEG" "HORMONE-SYN") ("Inorganic nutrients" "Noncarbon-Nutrients") ("Intermediate metabolism") ("Nucleotides" "Nucleotide-Biosynthesis" "NUCLEO-DEG") ("Redox") ("Specialized metabolism")) "List of the metabolic domains and the pathway classes that map to them. First element of each sublist is the domain, and subsequent elements are the frame classes whose members should be assigned to that domain.")

(defun write-domain-file (domain-table filename)
  "Writes the finished domain table to a file"
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (format stream "Frame-ID	Common Name	")
    (loop for domain in domain-list do 
          (format stream "~A	" (first domain)))
    (terpri stream)
    (loop for frame being the hash-keys in domain-table do
          (format stream "~A	~A	" (get-frame-handle frame) (get-slot-value frame 'names))
          (loop for domain across (gethash frame domain-table) do
                (format stream "~A	" domain))
          (terpri stream))))

(defun make-blank-domain-table (class)
  "Creates a blank domain table for the given class (e.g. \"Pathways\" or \"Reactions\"), filled in with 0's"
  (let ((domain-table (make-hash-table))
        (num-domains (length domain-list)))
    (loop for frame in (get-class-all-instances class) do
          (puthash frame (make-array num-domains :initial-element 0) domain-table))
    domain-table))

(defun add-pwy-classes-to-domain-table (&optional (domain-table (make-blank-domain-table "Pathways")))
  "Adds the domains to each pathway in the table using the pathway classes in the domain-list variable. If no existing domain table is provided a blank table of Pathways will be created to start from"
  (do ((n 0 (1+ n))
       (domain-rest domain-list (rest domain-rest)))
      ((null domain-rest) domain-table)
      (loop with domain-classes = (rest (first domain-rest))
            for class in domain-classes do
            (loop for pwy in (get-class-all-instances class) do
                  (finish-output)
                  (setf (elt (gethash pwy domain-table) n) 1)))))

;(defun read-domain-file (domain-file)
;  "Reads a domain file in tab-delimited format with columns Frame ID, common name (ignored), domain 1, domain 2, etc. The resulting list has lists headed with frame IDs and with domains as the tail"
;  (with-open-file (dfile domain-file :direction :input)
;    (read-line dfile nil)
;    (loop for line = (read-line dfile nil) while line collect 
;          (let ((fields (cl-ppcre:split "	" (string-trim " " line))))
;            (cons (first fields) (remove "0" (cddr fields) :test 'equal))))))
;
(defun domain-table-from-list (domain-list)
  "Converts a domain list of the sort returned by read-domain-file to a domain table of the sort created by make-blank-domain-table"
  )

(defun set-from-domain-list (domain-list)
  "Returns a set containing the IDs from the given domain list, as returned by (read-domain-file)"
  (set-from-list (mapcar #'first domain-list) :test 'equal))

(defun set-from-class (class)
  "Constructs a set containing the frame IDs of the members of the given ontology class"
  (set-from-list (mapcar (l (frame) (string (get-frame-handle frame))) (get-class-all-instances class)) :test 'equal))
