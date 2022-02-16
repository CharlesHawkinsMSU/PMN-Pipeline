The files in this directory contain various lisp functions useful in querying and maintaining PMN. You can load all of them by loading the pmn-lisp-funs.lisp file, which will load all the others. To do this you can either pass it to ptools at startup:
	./ptlisp -load "/Carnegie/DPB/Data/Shared/Labs/Rhee/Private/PMN/lisp/pmn-lisp-funs.lisp"
or you can load it from within an already-started ptools lisp session:
	(load  "/Carnegie/DPB/Data/Shared/Labs/Rhee/Private/PMN/lisp/pmn-lisp-funs.lisp")
