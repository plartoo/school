#! /usr/staff/bin/alisp -#!
;; AUTHOR: Adam Purtee  <apurtee@cs.rochester.edu>
;; LAST MODIFIED:  August 2nd, 2012
;; Works for Allegro Lisp, other Lisps may need a different first #! sequence

(defparameter *new-treebank-filename* 
  "treebank.ttt")  ;; set nil to suppress creation

(defparameter *inspection-filename*
  ;; treeID ruleApplied0 ruleApplied1 ... ruleAppliedN
  "inspect.ttt")  ;; set nil to suppress creation

(defparameter *inspection-show-source-trees*
  nil)

(defparameter *inspection-show-result-trees*
  nil)

(defparameter *display-summary*
  nil)

(when (not (= (length (sys:command-line-arguments)) 3))
  (format t "USAGE:  ./ttt-apply-ruleset.cl  ruleset treebank~%~%")
  (format t "see ruleset-format or example-ruleset for example of rulesets~%)
  (format t "treebank should contain one tree per line. ~%")
  (format t "  (i.e., charniak-parser output)")
  (exit))

(load "all")
(load "lispify-parser-output.lisp")
  
(defun process-treebank (ruleset treebank-file)
  "Apply a set of rules to a treebank. 
   treebank-file should be consist of one parse tree per line.
   ruleset should be a list of TTT rule objects
   note: Lisp will change (CD 0600) to (CD 600)."

  (let ((tree-n 0)
	(mod-trees 0)
	(mod-this-tree nil)
	(*print-pretty* nil)
	(treebank-fh (open treebank-file))
	(inspection-fh (if *inspection-filename* (open *inspection-filename*
						       :direction :output
						       :if-exists :supersede)))
	(new-treebank-fh (if *new-treebank-filename* 
			     (open *new-treebank-filename* 
				   :direction :output
				   :if-exists :supersede))))
    (loop for tree = (read-line treebank-fh nil)   
       while tree do
	 (let* ((tree-expr (lispify-parser-output tree))
		(source-tree tree-expr)
		(prev source-tree)
		rules-applied)
	   (incf tree-n)
	   (setf mod-this-tree nil)
	   (loop for rule-n from 0 to (1- (length ruleset)) do
		(setf tree-expr (apply-rule (nth rule-n ruleset) tree-expr))
		(when (not (equal tree-expr prev))
		  (when (not mod-this-tree)
		    (incf mod-trees)
		    (setf mod-this-tree t))
		  (push rule-n rules-applied)
		  (setf prev tree-expr)))
	   (setf rules-applied (nreverse rules-applied))
	   (when (and inspection-fh rules-applied)
	     (format inspection-fh "~a " tree-n)
	     (format inspection-fh "~{~a~^ ~}~%" rules-applied)
	     (if *inspection-show-source-trees*
		 (format inspection-fh "~a~%" source-tree))
	     (if *inspection-show-result-trees*
		 (format inspection-fh "~a~%" tree-expr)))
	   (format new-treebank-fh "~a~%" tree-expr)))
    (when *display-summary*
      (format t "~a trees modified~%" mod-trees)
      (format t "~a trees processed~%" tree-n))
    (dolist (fh (list treebank-fh inspection-fh new-treebank-fh))
      (if fh (close fh)))))

(let* ((treebank-filename (nth 2 (sys:command-line-arguments)))
       (ruleset-filename (nth 1 (sys:command-line-arguments))))
  (let ((ruleset (mapcar 
		  #'build-pattern
		  (with-open-file (fh ruleset-filename)
		    (eval (eval (read fh)))))))
    (process-treebank ruleset treebank-filename)))
		  
	   
	     


