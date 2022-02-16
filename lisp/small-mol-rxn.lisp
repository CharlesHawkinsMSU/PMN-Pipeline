; Tests whether each reaction in the given list is a small-molecule reaction
(defun separate-macromol-file (in-file out-file)
  "Reads a file with rxn IDs (one per line) and writes a similar file containing three sections; one with the small molecule reactions, one with th emacromolecule reactions, and one with reactions not in either class."
  (so 'meta)
  (with-open-file (in-str in-file :direction :input)
    (with-open-file (out-str out-file :direction :output :if-exists :supersede)
      (multiple-value-bind (smol mmol nmol) (separate-macromol (read-list in-str))
        (format out-str "Small:~%")
        (write-list out-str smol)
        (format out-str "Macro:~%")
        (write-list out-str mmol)
        (format out-str "Neither:~%")
        (write-list out-str nmol)))))
(defun get-non-small-from-file (in-file out-file)
  "Reads a file with rxn IDs (one per line) and writes a similar file containing those that aren't small molecule reactions"
  (so 'meta)
  (with-open-file (in-str in-file :direction :input)
    (with-open-file (out-str out-file :direction :output :if-exists :supersede)
      (write-list out-str (get-non-small-mol (read-list in-str))))))
(defun test-small-mol (rxn-list)
  "Tests whether each rxn is a small molecule reaction according to the reaction ontology. Returns a list like ((\"RXN-2234\" t) (\"RXN-3367\" NIL) ...)"
  (let ((smol (get-frame-named "Small-Molecule-Reactions")))
    (mapcar #'(lambda (rxn)
                (is-class-member rxn smol)) rxn-list)))
(defun is-class-member (frame class)
  "Returns t if frame is a member of class, NIL otherwise. Class should be a frame, such as from (get-frame-named)"
  (member class (get-frame-all-parents frame)))
(defun get-non-small-mol (rxn-list)
  "Tests whether each rxn is a small molecule and collects those that are not"
  (let ((smol (get-frame-named "Small-Molecule-Reactions")))
    (loop for rxn in rxn-list unless (is-class-member rxn smol) collect rxn)))
(defun separate-macromol (rxn-list)
  (let ((smol (get-frame-named "Small-Molecule-Reactions")) 
        (mmol (get-frame-named "Macromolecule-Reactions"))
        (smol-list NIL)
        (mmol-list NIL)
        (nmol-list NIL))
    (loop for rxn in rxn-list do
          (cond
            ((is-class-member rxn smol) (setq smol-list (nconc smol-list (list rxn))))
            ((is-class-member rxn mmol) (setq mmol-list (nconc mmol-list (list rxn))))
            (t (setq nmol-list (nconc nmol-list (list rxn))))))
    (values smol-list mmol-list nmol-list)))
