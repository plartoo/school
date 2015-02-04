; Package taken from Peter Seibel's "Practical Common Lisp"
; URL: http://www.gigamonkeys.com/book/
; Thanks to classmate Tim for telling me about this resource.

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defvar *test-name* nil)

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use 'check' to run individual test
   cases."
  `(defun ,name ,parameters
    (let ((*test-name* (append *test-name* (list ',name))))
      ,@body)))

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order."
  (with-gensyms (result)
    `(let ((,result t))
      ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
      ,result)))

(defun report-result (result form)
  "Report the results of a single test case. Called by 'check'."
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

;;;;;;; Helper functions to do unit tasks ;;;;;;;;

;; Checks if a list contains just alphnumeric characters
(defun hasAlphaNumbers?(lst)
	(or (null lst)
		(and (alphanumericp (car lst))	(hasAlphaNumbers? (cdr lst)))))

(defun symToChar(sym)
	(coerce (string sym) `list))

(defun startsWith?(lst chr)
	(char= (car lst) chr))

;; Remove leading underscore, if any, from the list
(defun stripUnderscore (lst)
	(if (startsWith? lst #\_) (cdr lst) lst))

;; Checks if the string representation of the input
;; matches _?[!\?\*\+](\w\d)*
(defun varHelper (lst)
	(and (reduce #'(lambda (b c) (or (startsWith? lst c) b))
		`(#\? #\! #\* #\+) :initial-value nil) 
		(hasAlphaNumbers? (cdr lst))))

;; Checks if the symbol is a valid variable
(defun var?(sym)
	(and (symbolp sym) (varHelper (stripUnderscore (symToChar sym)))))


;;;;;;; For verbose print and other printing related stuff ;;;;;;;;;;
(defun vformat (x)
	(if *verbose* (format t x))
	(if *verbose* (format t "~%")))

(defun vprint (x)
	(if *verbose* (format t "~T~T~T~T~S~%" x)))

(defun newline ()
	(format t "~%"))

;;;;;;; Helper functions for testing ;;;;;;;;;;;;;;;;;
(defun my-eval(l) (apply (car l) (cdr l)))

;; Given a function (along with its arguments as a list), checks if
;; it produces the expected result and outputs whether it fails or passes
;; the check in STDOUT
(defun my-assert(function expected)
 (let ((result (my-eval function)))
  (format t (if (equal result expected) "ASSERTION TRUE:" "ASSERTION FAILED:"))
	    	(format t "~T~S ==> ~S~%" function expected)
	    (if (not (equal result expected)) 
		(format t "~T~T~T~T~T~S~%" result))))

