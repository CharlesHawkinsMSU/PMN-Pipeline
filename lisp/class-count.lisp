; Set of functions and macros related to getting a nonredundant count of class members accross orgids
(defun multi-union (list)
	"Takes the union of multiple lists"
	(reduce #'(lambda (list1 list2) (union list1 list2 :test 'equal)) list))

(defmacro for-all-orgids (form)
	"Executes form for all orgids in the database and collects the results in a list"
	`(loop for org in (all-orgids) do (so org) collect ,form))

(defmacro for-orgids (orgids form)
	"Executes form for the given orgids and collects the results in a list"
	`(loop for org in ,orgids do (so org) collect ,form do (so org) (close-kb :save-updates-p NIL :flush-memory-p t)))

(defconstant *main-keywords* '(do collect collecting append appending nconc nconcing into count counting sum summing maximize return maximizing minimize minimizing doing thereis always never if when unless repeat while until))
(defmacro loop-orgids (orgids &rest forms)
  "Loops over the given orgids, passing remaining arguments to loop except special keywords. Special keywords are save (or saving) and close (or closing), indicating that each kb should be saved and/or closed, respectively, after we're done with it"
  (let ((save nil)
		(close nil))
	(append `(loop for org in ,orgids)

			(loop for form in forms
				  with no-so-yet = t
				  when (and no-so-yet (find form *main-keywords*))
				  do (setf no-so-yet nil)
				  and append '(do (so org))
				  unless (when (find form '(save saving)) (setf save t))
				  unless (when (find form '(close closing)) (setf close t))
				  collect form)
			(when save '(do (save-kb)))
			(when close '(do (close-kb :save-updates-p nil :flush-memory-p t))))))


(defun get-class-set-orgids (orgids class)
  "Returns a hash-based set of class members accross the given orgids"
  (let ((class-list (make-hash-table)))
    (loop for org in orgids
		  do (so org)
		     (add-list-to-set (mapcar #'get-frame-handle (handler-case
														   (get-class-all-instances class)
														   (not-coercible-to-frame () NIL)))
							  class-list)
			 (close-kb :save-updates-p NIL :flush-memory-p t)) 
    class-list))

(defun get-class-instances-all-orgids (class)
	"Returns a nonredundant list of class members accross all orgids"
    (set-to-list (get-class-set-orgids (all-orgids) class)))

(defun get-class-instances-orgids (orgids class)
  "Returns a nonredundant list of class members accross the given orgids"
  (set-to-list (get-class-set-orgids orgids class)))

(defun count-class-instances-orgids (orgids class)
  "Returns a nonredundant count of class members accross the given orgids"
  (set-length (get-class-set-orgids orgids class)))

(defun non-redundant (list)
	"Return a copy of list with duplicate members removed (duplicates don't have to be adjacent to be detected)"
	(reduce (l (x ls) (adjoin x ls :test 'equal)) list :initial-value NIL :from-end t))

(defun get-all-classes (class)
	"Gets a non-reduntant list of all direct and indirect subclasses of class"
	(non-redundant (append (list (get-frame-handle class)) (reduce #'append (mapcar #'get-all-classes (get-class-all-subs class))))))

(defun get-all-class-instances-orgids (orgids class)
	"Gets a non-redundnat list of all instances of the given class and all its direct and indirect subclasses across the given orgids. Return value is formatted as ((class (instance instance instance ...)) (class (instance instance instance ...)) (class (instance instance instance ...)) ...) with the class heirarchy flattened."
	(mapcar #'(lambda (cur-class) (list class (get-class-instances-orgids orgids cur-class))) (get-all-classes class)))

(defun count-class-instances-each-orgid (orgids class)
	"Count instances of the given class in each of the given orgids. Output is ((orgid1 count1) (orgid2 count2) ...)"
	(for-orgids orgids (list org (length (get-class-all-instances class)))))

(defun count-each-class-instances-each-orgid (orgids classes)
	"Count instances of each of the given classes in each of the given orgids. Output is ((orgid1 count1 count2 count3) (orgid2 count1 count2 count3) ...)"
	(for-orgids orgids (cons org (mapcar #'length (mapcar #'get-class-all-instances classes)))))

(defun count-all-class-instances-orgids (orgids class)
	"Gets a non-redundnat count of all instances of the given class and all its direct and indirect subclasses across the given orgids. Return value is formatted as ((class count) (class count) (class count) ...) with the class heriarchy flattened."
	(mapcar #'(lambda (cur-class) (list cur-class (length (get-class-instances-orgids orgids cur-class)))) (get-all-classes class)))

(defun write-classes-with-counts (filename orgids class)
	"Write a text file with all classes in the heirarchy and a non-redundant count of all instances across all orgids"
	(with-open-file (stream filename :direction :output :if-exists :supersede)
		(loop for class-count in (count-all-class-instances-orgids orgids class) do
			(format stream "~A: ~A~%" (first class-count) (second class-count)))))
(defun print-class-sizes-for-orgids (orgids classes)
	"Print a table of orgids and the size of each given class in that PGDB"
	(write-table (cons (cons "Orgid" classes) (count-each-class-instances-each-orgid orgids classes))))

(defun common-classes (frames)
  "Returns a set of the classes common to the given list or set of frames"
  (when (hash-table-p frames) (setq frames (set-to-list frames)))
  (if frames
     (let ((table (set-from-list (get-frame-all-parents (first frames)))))
       (loop for f in (rest frames) do (setq table (set-intersect table (set-from-list (get-frame-all-parents f))))) table)
     NIL))
