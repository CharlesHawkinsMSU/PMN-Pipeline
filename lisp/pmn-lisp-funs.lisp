; Created by Charles Hawkins, 2018
; Last modified by Charles Hawkins, 2019

; Loading this file will load all of our custom lisp functions from the various lisp files. It defines a function (load-my-funs) to do this and then calls it, so you can susequently call this function yourself to reaload everything into an existing session when you make changes
(format t "~%==Loading PMN Lisp functions==~%~%")
(setq *pmn-lisp-folder* (directory-namestring *load-truename*))
(defun load-pmn-funs ()
  "Load all of PMN's custom functions for use in Pathway Tools from the various .lisp files that contain them"
  (load (concat *pmn-lisp-folder* "utils.lisp"))
  (load (concat *pmn-lisp-folder* "misc-funs.lisp"))
  (load (concat *pmn-lisp-folder* "pathway-steps.lisp"))
  (load (concat *pmn-lisp-folder* "small-mol-rxn.lisp"))
  (load (concat *pmn-lisp-folder* "count-citations.lisp"))
  (load (concat *pmn-lisp-folder* "cpr-tree.lisp"))
  (load (concat *pmn-lisp-folder* "class-count.lisp"))
  (load (concat *pmn-lisp-folder* "nonredundant-tree.lisp"))
  (load (concat *pmn-lisp-folder* "met-domains.lisp"))
  (load (concat *pmn-lisp-folder* "modify.lisp"))
  (load (concat *pmn-lisp-folder* "pipeline.lisp"))
  (load (concat *pmn-lisp-folder* "refine-b.lisp"))
  (load (concat *pmn-lisp-folder* "compare-pgdbs.lisp"))
  (load (concat *pmn-lisp-folder* "check-enzr.lisp"))
  (load (concat *pmn-lisp-folder* "ravi.lisp"))
  (load (concat *pmn-lisp-folder* "savi.lisp"))
  )
(load-pmn-funs)
(so 'meta)
