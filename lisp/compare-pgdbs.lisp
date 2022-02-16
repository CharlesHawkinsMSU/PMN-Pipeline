; Functions that compare PGDB contents between two versions of ptools using (other-ptools); another copy of ptools should be running with -api

(defun opt-comp-frame-set (orgids class)
  "For each given orgid, compares the set of frames of teh given class in this ptools and the other ptools. Structure is (('orgid (this-only) (other-only)) ('orgid (this-only) (other-only))). If one ptools doesn't have one of the orgids, that orgid will be omitted from the list"
  (let ((our-orgids (all-orgids))
		(their-orgids (other-ptools '(all-orgids))))
	(loop for org in orgids
		  when (member org our-orgids)
		  when (member org their-orgids)
		  do
		  (so org)
		  (other-ptools `(so ,org))
		  (setq ours (set-from-list (get-frame-handles (get-class-all-instances class))))
		  (setq theirs (set-from-list (other-ptools `(get-frame-handles (get-class-all-instances ,class)))))
		  collect (list org (set-to-list (set-diff ours theirs)) (set-to-list (set-diff theirs ours))))))


