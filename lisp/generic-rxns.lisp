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

(defparameter *report-interval-sec* 10)
(defun make-rxn-instantiation-table ( &optional (rxnlist (all-class-rxns)))
  (setq tm (get-universal-time))
  (loop for r in (all-class-rxns)
	for i from 1
	with l = (length rxnlist)
	when (> (- (get-universal-time) tm) *report-interval-sec*)
	do (format t "~A / ~A (~A%)~%" i l (/ i l))
	and do (setq tm (get-universal-time))
	do (multiple-value-setq (a b c d)
	     (generic-rxn-instantiation r :force-all-instantiations t))
	collect (to-handles (list r a b c d))))

(defun find-original-of-cpd-inst (rxn icpd side)
  (loop for ccpd in (gsvs rxn side)
	)
  )
(defun complete-instantiated-rxn (rxn inst)
	(loop for icpd in (first inst))
  )
