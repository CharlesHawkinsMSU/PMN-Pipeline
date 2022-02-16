(defun check-enzr (&optional (orgids '(ara chlamy soy corn oryza)))
  (setq bad-enzr (for-orgids orgids
			(progn
			  (setq imported (with-open-file (f (concatenate 'string (string-downcase (symbol-name org))  "cyc.pwydata.lisp") :direction :input)
							   (loop for s = (read f nil nil) while s collect s)))
			  (setq enzr-list (loop for f in imported
									when (equal (first (first (last f))) "Enzymatic-Reactions")
									collect (list (first f) (second (assoc 'ENZYME (third f))))))
			  (list org
					(loop for (enzrxn enzyme) in enzr-list
						  when (when (coercible-to-frame-p enzrxn) (not (equal enzyme (setq n (symbol-name (get-frame-handle (get-slot-value enzrxn 'enzyme)))))))
						  collect (list enzrxn enzyme n)))))))
