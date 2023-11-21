(defun cpd-classes-in-rxns ()
  (loop for c in (get-class-all-subs '|Compounds|)
		when (reactions-of-compound c)
		collect (list (gfh c)
					  (gsv c 'common-name))))

(defun class-rxns ()
  (loop for r in (gcai '|Reactions|)
		when (loop for c in (compounds-of-reaction r)
				   thereis (class-p c))
		collect r))

(defun class-rxn-stats ()
  (loop for r in (gcai '|Reactions|)
		for cc = (loop for c in (compounds-of-reaction r)
				   when (class-p c)
				   collect c)
		for comb = (loop for c in cc
						 for p = (max 1 (length (gcai c)))
						 then (* p (max 1 (length (gcai c))))
						 finally (return p))
		for ll = (loop for c in cc
					   collect (length (gcai c)))
		when cc
		collect (list (gfh r)
					  (length cc)
					  ll
					  comb
					  )))
