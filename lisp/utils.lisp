; General lisp utility functions, not specific to Pathway Tools

(defun yn (bool)
  (if bool "Y" "N"))

(defun match-re-all (re string &key (start 0) (return :string))
  "Returns a list of all matches to the regex re in string (nonoverlapping). If re ever matches an empty string the search terminates at that point. :start and :return act as in match-re"
  (loop with span
		with re-c = (if (eq (type-of re) 'regexp::regular-expression)
					  re
					  (excl:compile-re re))
		do (multiple-value-bind (found ispan)
			 (excl:match-re re-c string :return :index :start start)
			 (setq span ispan)
			 (setq start (rest ispan)))
		while start
		while (/= start (first span))
		when (eq return :string)
		collect (subseq string (first span) start)
		when (eq return :index)
		collect span))

(defun replace-re-using (re string fn &key (start 0))
  "Goes through string, looking for matches to the regex re. For each match, calls fn and replaces the matched text with the string returned by fn. If fn returns a non-string, then that match is not replaced. Fn is passed the match text as its only argument. Search begins after the previous match, so for overlapping matches only the first is found, even if that first match is not replaced"
  (let ((re-c (if (eq (type-of re) 'regexp::regular-expression)
				 re
				 (excl:compile-re re))))
	 (format nil "~{~A~}"
			 (loop with span
				   with match
				   do (multiple-value-bind (found ispan)
						(excl:match-re re-c string :return :index :start start)
						(setq span ispan)
						(setq match (subseq string (first span) (rest span))))
				   unless (and span (/= (first span) (rest span)))
				   collect (subseq string start)
				   and do (loop-finish)
				   collect (subseq string start (first span))
				   do (setq start (rest span))
				   do (setq rep (if (functionp fn) (funcall fn match) fn))
				   when (stringp rep)
				   collect rep
				   else collect match))))


(defun list-structure (list)
  "Returns the \"structure\" of the given list; the tree structure of list is preserved but all atoms are replaced with their types (as returned by type-of)"
  (cond
	((listp list) (mapcar #'list-structure list))
	((stringp list) 'string)
	((arrayp list) 'array)
	(t (type-of list))))
(defun as-list (thing)
  "If thing is a list, return it unchanged. If it is not, return a single-element list containing thing"
  (if (listp thing) thing (list thing)))

; A pair of functions that take any number of arguments and always return t and NIL, respectively; they're intended to be passed to first-class functions
(defun true (&rest _) t)
(defun false (&rest _) nil)

(defun starts-with-letter (letter string)
  "Returns t if string starts with the character #\letter"
  (and (< 0 (length string)) (eq letter (char string 0))))

(defun symbol (name)
  "Finds the existing symbol called \"name\", or creates it if it does not exist. Case-sensitive."
  (if (symbolp name) name (or (find-symbol name) (make-symbol name))))

(defun drop-last-n (n list)
  "Drops the last n entries from the given list"
  (loop for el in list
		for el2 in (nthcdr n list)
		collect el))

(defun first-n (n list)
  "Returns the first n elements of list (without needing to go through the whole list first)"
  (loop for e in list for i from 1 to n collect e))

(defmacro write-file (filename form)
  `(with-open-file (file ,filename :direction :output :if-exists :supersede) ,form))

(defun write-table (table &optional (out-stream *standard-output*))
  "Takes a list of lists ((item1 item2 item3) (item4 item5 item6) ...) and prints them as a table, one line per element in the outer list, with inner list elements delimited by tabs"
  (format out-stream "~{~{~A~^	~}~%~}" table))

(defun cons-apply (fun arg)
  "Applies the single-argument function fun to arg and combines the arg and the result into a list"
  (list arg (funcall fun arg)))

; Hey, #'(lambda) is annoying to have to type again and again, okay?
(defmacro l (args body) `#'(lambda ,args ,body))
(defmacro lx (body) `#'(lambda (x) ,body))

(defun mapfns (fns &rest args)
  "Takes a list of functions and N addtional arguments that will be passed to them. Applies each funciton to the arguments and returns a list of the results. All the listed functions must be able to accept N arguments."
  (loop for f in fns collect (apply f args)))

(defun maptree (fn tree)
  "Takes a function, fn, that can take a single atom as its argument, and a tree. Recursively goes down tree until atoms are reached, applies fn to each atom, and returns a tree with the same structure as the input tree, containing the results of applying fn to each atom in the input tree. So (maptree #'1+ '(1 2 (3 4 (5) 6))) would return (2 3 (4 5 (6) 7))"
  (cond
    ((null tree) nil)
    ((atom tree) (funcall fn tree))
	(t (loop for val in tree collect (maptree fn val)))))

(defun write-hash (hash &optional (stream t))
  "Prints a hash table as a tab-delimited table of keys and values"
  (loop for k being the hash-keys in hash do (format stream "~A ~A~%" k (gethash k hash))))

(defun ls (&optional (dir "."))
  (format t "~{~A~^~%~}~%" (mapcar #'file-namestring (directory (concatenate 'string dir "/*.*")))))

(defun cd (&optional (dir "~"))
  "Change directory"
  (if (setq d (probe-file dir))
      (setq *default-pathname-defaults* d)
      (format t "No such directory")))
(defun pwd ()
  "Print working directory"
  (format t "~A~%" *default-pathname-defaults*))

(defun dir-p (file)
  "Returns file if file exists and is a directory, otherwise NIL"
  (probe-file (concatenate 'string (namestring file) "/.")))

(defun first? (what)
  "Returns what if it is an atom, or its first element if it is a list"
  (if (listp what)
	(first what)
	what))

(defun rest? (what)
  "Returns (rest what) if what is a list, or NIL otherwise"
  (if (listp what)
	(rest what)
	what))

(defun print-tree (tree &key (tabs -1) (out-stream *standard-output*))
  "Print a tabbed tree of the given tree structured as '(parent (child (grandchild grandchild) child child))"
  (indent tabs out-stream)
  (if (listp tree)
      (loop for node in tree do (print-tree node :tabs (+ 1 tabs) :out-stream out-stream))
      (format out-stream "~A~%" (string tree))))

(defun indent (tabs &optional (out-stream *standard-output*) (indent-char " "))
  "Print some number of tabs"
  (format out-stream "~v@{~A~:*~}" tabs indent-char))

(defun write-list (stream list)
  "Writes list to stream, one per line. Elements of list should be of a type printable with \"~A\"."
  (if (streamp stream)
	(loop for elt in list do (format stream "~A~%" elt))
	(with-open-file (fd stream :direction :output :if-exists :supersede)
	  (loop for elt in list do (format fd "~A~%" elt)))))

(defmacro for-lines-in-file (file &rest cmds)
  (list 'from-file-or-stream file (append `(loop for line = (read-line stream nil 'eof) while (stringp line)) cmds)))

(defun read-list (from)
  "Reads a list of strings, one per line, from either a stream or a file"
  (for-lines-in-file from collect line))

(defmacro puthash (key val hash)
  "Puts key -> val into hash"
  `(setf (gethash ,key ,hash) ,val))

(defmacro help (fn)
  "Get help for a function or macro"
  `(documentation 'function #',fn))

(defun flatten (list)
  "Flattens the list, returning a simple list of its leaves"
  (cond ((null list) NIL)
        ((listp (car list)) (nconc (flatten (car list)) (flatten (cdr list))))
        (t (cons (car list) (flatten (cdr list))))))

(defun commas (n)
  "Returns a string representing the number n with appropriate number commas inserted (e.g. (commas 9829344) returns \"9,829,344\". Expect screwy results for floats because of precision"
  (multiple-value-bind (i f) (truncate n)
   (let ((s (write-to-string i)) (ws ""))
    (do ((place (length s) (- place 3)))
        ((< place -3) (concatenate 'string ws (string-left-trim "0" (write-to-string f))))
        (setq ws (concatenate 'string (subseq s (max 0 (- place 3)) (max 0 place)) ws))
        (when (> place 3) (setq ws (concatenate 'string "," ws)))))))

(defun nprint (x)
  "Prints the object x but doesn't return anything. Useful from the repl if you don't want the object to be printed once in full and then in part"
  (progn (print x) NIL))

(defun onemore (num)
  "Increment num, treating NIL as 0"
  (if num
      (1+ num)
      1))

(defun inc-hash-count (key hash)
  "Increment the number stored in hash under key, treating NIL as 0"
  (setf (gethash key hash) (1+ (gethash key hash 0))))

(defun add-to-hashvalue (key num hash)
  "Add num to the number stored in hash under key, treating NIL as 0, and store that value back in the hash under that key"
  (setf (gethash key hash) (+ num (gethash key hash 0))))

(defun append-hash-list (key elt hash)
  "Look up the list stored by key in hash, and append elt to it"
  (puthash key (append (gethash key hash) (list elt)) hash))

(defun count-functions (file)
    "Attempts to count the calls to each funciton in the given lisp file. Doesn't work very well currently, as there are a number of forms that look like function calls but aren't, and most of them will still fool the current version of this function"
  (defparameter not-a-fun '((defun 1) (destructuring-bind 1)))
  (defparameter ignore '(quote))
  (let ((fn-counts (make-hash-table)))
    (labels ((count-funs-in-list (list counts)
               (when (listp list)
                 (let ((fn (first list)))
                   (unless (member fn ignore)
                     (if (listp fn) (count-funs-in-list fn counts) (inc-hash-count fn counts))
                     (do ((exprs (rest list) (rest exprs))
                          (i 1 (1+ i)))
                         ((null exprs) NIL)
                         (unless (member i (find-in-sublists fn not-a-fun))
                           (finish-output)
                           (count-funs-in-list (first exprs) counts))))))))
      (format t "Lables defined~%")
      (with-open-file (fd file :direction :input)
        (defvar s)
        (loop while (setq s (read fd NIL)) do (format t "s: ~A~%" s) (count-funs-in-list s fn-counts))
        fn-counts))))

(defun read-alist (file &key nil-value (sep "\\t") (limit 0)) 
  (for-lines-in-file file collect (loop for field in (excl::split-re sep line :limit limit)
										collect (if (equal field nil-value) nil field))))
(defun assoc-every (item alist)
  "Works like assoc but returns all matching items from alist"
  (loop for element in alist when (eq (first element) item) collect element))

(defun invert-alist (alist &key (test 'equal))
  "Produces an inverse alist that reverses keys and values. Assumes values are lists; each unique list element in the input's values will produce a key in the output, and each key in the output will map to a list of all the keys in the input whose value was a list that contained it. So for example, (('rose 'red 'white) ('buttercup 'yellow) ('daffodil 'yellow 'white)) that maps from flowers to colors that flower can have would return (('red 'rose) ('white 'rose 'daffodil) (yellow 'buttercup 'daffodil)) that maps from colors to flowers that can have that color"
  (hash-to-alist (invert-alist-to-hash alist :test test)))
(defun invert-alist-to-hash (alist &key (test 'equal))
  "The same as invert-alist but leaves the result as a hash-table, not converting it back to an alist"
  (let ((h (make-hash-table :test test))) 
    (loop for entry in alist do
          (loop for val in (rest entry) do
                (if (gethash val h)
                    (unless (find (first entry) (gethash val h))
                      (setf (gethash val h) (cons (first entry) (gethash val h))))
                    (setf (gethash val h) (list (first entry)))))) h))

(defun hash-to-alist (hash)
  "Converts a hash-table to an alist (i.e. ((key1 val1.1 val1.2) (key2 val2.1 val2.2 val2.3) ...) for list values or ((key1 . val1) (key2 . val2) ...) for atomic values"
  (loop for key being the hash-keys in hash for val = (gethash key hash) collect (if (listp val) (cons key val) (list key val))))

(defun sort-alist (alist)
  "Sorts an alist by its keys"
  (sort alist #'< :key #'first))

;(defmacro to-file-or-stream (where &rest form)
;  "Executes form (an implicit progn) with the variable stream set to either where (if where is a stream, t, or nil), to a stream that writes to the file to which where refers (if where is a string containing valid file path), or to a newly-allocated string (if where is equal to :string)"
;  `(cond ((or (streamp ,where) (null ,where) (eq ,where t))
;		  (let ((stream ,where))
;			,(cons 'progn form)))
;		 ((eq ,where :string)
;		  ,(append
;			'(progn (setq fstr (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t)))
;
;		 (t (with-open-file (stream ,where :direction :output :if-exists :supersede)
;	   ,(cons 'progn form)))))

(defmacro to-file-or-stream (where &rest form)
  "Executes form (an implicit progn) with the variable stream set to either where (if where is a stream, t, or nil) or to a stream that writes to the file to which where refers (if where is a string containing valid file path)"
  `(if (or (streamp ,where) (null ,where) (eq ,where t))
	 (let ((stream ,where))
	  ,(cons 'progn form))
	 (with-open-file (stream ,where :direction :output :if-exists :supersede)
	   ,(cons 'progn form))))

(defmacro from-file-or-stream (where &rest form)
  "Executes form (an implicit progn) with the variable stream set to either where (if where is a stream, t, or nil) or to a stream that reads from the file to which where refers (if where is a string containing valid file path)"
  `(if (or (streamp ,where) (null ,where) (eq ,where t))
	 (let ((stream ,where))
	  ,(cons 'progn form))
	 (with-open-file (stream ,where :direction :input)
	   ,(cons 'progn form))))

(defun write-list-n (list seps stream)
  "Writes the given list of depth n (i.e. a list of lists is of depth 2, a list of lists of lists is depth 3, etc.) to the given stream, using the strings in seps to separate the different list levels. Seps should be a list of length n, containing strings. The argument stream is passed directly to (format) so t and nil have the usual behavior"
	(let ((fmt-str (format nil "~~{~~A~~^~A~~}" (first seps))))
	  (if (listp list)
		(format stream fmt-str
				(loop for elt in list
					  collect (write-list-n elt (rest seps) nil)))
		(format stream "~A" list))))

(defun write-list-3 (list where)
  "Writes a list of lists of lists to the given stream or filename (\"where\"). Outermost list is newline-separated. Second level is tab-separated. Innermost lists are semicolon-separated"
  (to-file-or-stream where
	  (write-list-n list '("~%" "	" ";") stream)
	  (terpri stream)))

(defun print-alist (outfile alist)
  "Writes the given alist to the given filename. Tab-delimited, key then value(s). Accepts proper lists or dotted pairs"
  (flet ((print-alist-to-stream (alist stream)
     (loop for pair in alist do
          (let ((key (first pair))
                (vals (if (listp (rest pair)) (rest pair) (list (rest pair)))))
            (format stream "~A	~{~A~^	~}~%" key vals)))      
           ))
    (cond ((stringp outfile)
            (with-open-file (fd outfile :direction :output :if-exists :supersede) 
              (print-alist-to-stream alist fd)))
          ((streamp outfile)
            (print-alist-to-stream alist outfile))
          (t
            (print-alist-to-stream alist *standard-output*)))))
(defun print-list-sublists (outfile alist)
  "Writes the given alist to the given filename. Tab-delimited, key then value(s). Accepts proper lists or dotted pairs"
  (flet ((print-alist-to-stream (list stream)
			(format stream "~{~A~%~}" 
					(loop for line in list collect
						  (format nil "~{~A~^	~}"
								  (loop for cell in line collect
										(if (listp cell)
										  (format nil "~{~A~^ // ~}" cell)
										  (format nil "~A" cell))))))))
    (cond ((stringp outfile)
            (with-open-file (fd outfile :direction :output :if-exists :supersede) 
              (print-alist-to-stream alist fd)))
          ((streamp outfile)
            (print-alist-to-stream alist outfile))
          (t
            (print-alist-to-stream alist *standard-output*)))))

; Functions related to hash-based sets. This implementation of sets stores set
; members as hash keys and uses t as the value for all of them. Some of the set
; functions are just aliases for standard hash functions, used to clarify the
; intent of the function call

(defun set-from-list (list &rest hash-args)
  "Makes and returns a hash with list as keys and t for each value, intended to be used as a set"
  (setq set (apply #'make-hash-table hash-args))
  (loop for member in list do (puthash member t set))
  set)

(defmacro set-member (member set)
  "Alias for gethash, tests if member is a member of set"
  `(gethash ,member ,set))

(defun set-length (set)
  "Get the number of set members"
  (hash-table-count set))

(defmacro add-to-set (member set)
  "Add member to set"
  `(puthash ,member ,t ,set))

(defun add-to-or-create-set (member set)
  (if set
	(progn (puthash member t set) set)
	(set-from-list `(,member))))

(defun add-list-to-set (list set)
  "Add all items in list to set"
  (loop for item in list do (add-to-set item set)))

(defun set-to-list (set)
  "Returns a list of all set members"
  (loop for memb being the hash-keys in set collect memb))

(defun set-diff (set1 set2)
  "Returns a new set that is the difference set1 - set2"
  (setq setd (make-hash-table :test 'equal))
  (loop for member being the hash-keys in set1 do (add-to-set member setd))
  (loop for member being the hash-keys in set2 do (remhash member setd))
  setd)

(defun nset-diff (set1 set2)
  "Returns a set that is the difference set1 - set2, modifying set1 in the process"
  (loop for member being the hash-keys in set2 do (remhash member set1))
  set1)

(defun set-intersect (set1 set2)
  "Returns a new set containing all members that are in both set1 and set2"
  (setq seti (make-hash-table :test 'equal))
  (loop for member being the hash-keys in set1 when (set-member member set2) do (add-to-set member seti))
  seti)

(defun set-union (set1 set2)
  "Returns a new set containing all members that are in set1, set2, or both"
  (setq setu (make-hash-table :test 'equal))
  (loop for member being the hash-keys in set1 do (add-to-set member setu))
  (loop for member being the hash-keys in set2 do (add-to-set member setu))
  setu)

(defun nset-union (set1 set2)
  "Returns a new set containing all members that are in set1, set2, or both, modifying set1 in the process"
  (loop for member being the hash-keys in set2 do (add-to-set member set1))
  set1)
(defun empty-set ()
  "Creates a new, empty set"
  (make-hash-table :test 'equal))

; Utility functions copied from elsewhere

(defun write-repeated-string (n string stream)
  (loop repeat n do (write-string string stream)))

; From http://reference-error.org/2015/08/30/common-lisp-finding-all-functions-in-a-package.html
(defun all-function-symbols (package-name)
  "Retrieves all function symbols from a package."
  (declare ((or package string symbol) package-name))
  (the list
       (let ((lst (list))
             (package (find-package package-name)))
         (cond (package
                 (do-all-symbols (symb package)
                   (when (and (fboundp symb)
                              (eql (symbol-package symb) package))
                     (push symb lst)))
                 lst)
               (t
                (error "~S does not designate a package" package-name))))))
