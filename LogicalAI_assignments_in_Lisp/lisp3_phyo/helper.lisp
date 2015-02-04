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

;; Helper function to help TA grading (by showing outputs in verbose mode)
;; If set to ":verbose", it'll print out deduced facts as well
;; Else, (e.g., set it to ":non_verbose"), it'll not print anything except
;; the facts and rules tables
(defmacro verbose? (&rest args) 
	`(if (equal *verbosity* :verbose) (format ,@args)))

;; Print content of the hash table
(defun print-hash-table (table) 
	(loop for k being the hash-keys in table using (hash-value v) do
		(format t "~a~%" k)
	(loop for item in v do (format t "   ~a~%" item))))

;; Prints a newline
(defun newline () (format t "~%"))

;; Helper functions from assignment 2 to help determine variable types
(defmacro var-type (chr)
	`(cond ((not (typep ,chr 'character)) nil)
		((char= #\! ,chr) '!)
		((char= #\? ,chr) '?)
		((char= #\* ,chr) '*)
		((char= #\+ ,chr) '+)
		(t nil)))

;; returns symbol if a given variable is unconstrained, else nil
(defun _varp (sexp)
	(if (listp sexp) nil
	(let ((lst (coerce (write-to-string sexp) 'list)))
	(cond ((char= #\_ (first lst)) (var-type (second lst)))
		((and (char= #\| (first lst))
		(char= #\_ (second lst)))
	(var-type (third lst)))
	(t nil)))))

;; returns symbol if a given variable is constrained, else nil
(defun varp (sexp)
	(if (or (not (listp sexp)) (listp (first sexp))) nil
	(let ((chr (first (coerce (write-to-string (first sexp)) 'list))))
	(var-type (if (char= chr #\|)
		(second (coerce (write-to-string (first sexp)) 'list)) chr)))))
