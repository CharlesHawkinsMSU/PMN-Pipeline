; Set of functions related to making a non-redundant class tree
(defun get-nonredundant-paths (class)
	"Wrapper for (nonredundant-paths) that takes a single class name (likely a top-level class, i.e. \"Compounds\", \"Pathways\", or \"Reactions\"), and returns a hash table containing CPRs as keys and inverted paths (cpr -> class -> superclass -> super-superclass -> ...)"
	(nonredundant-paths '(class) (make-hash-table :test 'equal)))

(defun nonredundant-paths (path table)
	"Tries to create a non-reduntant inverse-tree. For each class, lists the longest path along the class tree that leads to it"
	(update-path-table path table)
	(loop for sub-class in (get-class-all-subs (first path)) do
		(nonredundant-paths (cons sub-class path) table)))

(defun update-path-table (path table)
	"Updates a table of nonredundant paths if necessary. The goal is to find the longest path to the given node. The function takes path as the new path under consideration and finds the approporiate entry in the table. If the existing entry is a subset of the new path, then it is replaced by the new path. Function is used by (nonredundant-paths)"
	(multiple-value-bind (current found) (gethash (first path) table)
	(if (or (not found) (subsetp current path :test 'equal))
		(setf (gethash (first path) table) path))))

(defun print-nr-paths (table)
	"Prints a nonredundant class inverse-tree of the sort created by (nonredundant-tree)"
	(maphash #'(lambda (key val) (format t "~A: ~A~%" (get-frame-handle key) (mapcar #'get-frame-handle val))) table))

(defun invert-paths (table)
	"Extracts paths from a path dictionary of the sort produced by (get-nonredundant-paths) and returns a list of all the paths, reversed so that they start at the top-level class and end with the CPR"
	(setq paths NIL)
	(with-hash-table-iterator (get-item table)
		(loop (multiple-value-bind (more key value) (get-item)
			(unless more (return nil))
			(setq paths (cons (reverse (mapcar #'get-frame-handle value)) paths))))) paths)

(defun tree-from-paths (paths)
	"Takes a list of top-down paths of the type produced by (invert-paths) and constructs a tree out of them"
	(setq tree NIL)
	(loop for path in (paths) do
		(setq tree (add-path-to-tree path tree)))tree)

;(defun add-path-to-tree (path tree)
;	"Takes a path of the form (B F G) and a tree of the form (A B (C (D))) and returns a tree with the nodes from that path added to it (A B (C (D) F (G))"
;	(setq pos (position (first path) tree))
;	(if pos
;		(if (
