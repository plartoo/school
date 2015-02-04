#! /usr/staff/bin/alisp -#!
;; AUTHOR: Adam Purtee  <apurtee@cs.rochester.edu>
;; LAST MODIFIED:  August 2nd, 2012
;; Works for Allegro Lisp, other Lisps may need a different first #! sequence

(when (not (= (length (sys:command-line-arguments)) 3))
  (format t "USAGE: ./ttt-search.cl pattern treebank~%")
  (format t "pattern should be in quotation marks ~%")
  (format t "treebank should contain one tree per line. ~%")
  (format t "  (i.e., charniak-parser output)")
  (exit))

(load "all")
(load "lispify-parser-output.lisp")

(compile 'lispify-parser-output)
(defun search-tb (treebank-file patt-expr)
  (format t "pattern: ~a~%" patt-expr)
  (format t "treebank: ~a~%" treebank-file)
  (let ((*print-pretty* nil)
	(patt (build-pattern patt-expr))
	(n-searched 0)
	(n-matched 0))
    (with-open-file (treebank-fh treebank-file)
      (loop for tree = (read-line treebank-fh nil)  
	 while tree do
	   (incf n-searched)
	   (let ((binds (deep-match patt (build-tree (lispify-parser-output tree) :index-subtrees t))))
	     (when binds	
	       (incf n-matched)
	       (format t "~a~%~%" tree)
	       (maphash (lambda (k v) (format t "~a: ~a~%" k (mapcar #'to-expr (car v))))
			binds)
	       (format t "~%")))))
    (format t "~a trees searched.~%" n-searched)
    (format t "~a trees matched.~%" n-matched)))
(compile 'search-tb)
(search-tb (nth 2 (sys:command-line-arguments))
	   (lispify-parser-output (nth 1 (sys:command-line-arguments))))



