;; Lisp Assignment 3 - Phyo Thiha
;; Implementation of facts and rules tables

;; Load compiled ttt modules and helper functions
(load "src/ttt_loader")
(load "helper")

;; Hash table for "facts" and rules.
;; The hash is essentially key-value pairs where
;; value is a list of facts/rules mapped to a particular key.
;; Note that each fact is stored under two keys, "'P" and "'P tau"
;; as suggested in the assignment handout.
(defparameter *facts* (make-hash-table :test 'equal))
(defparameter *rules* (make-hash-table :test 'equal))

;; Please turn set this to ":verbose" for comprehensive output
(defparameter *verbosity* :verbose)

;; Add new item 'v' to the hash table
(defun add-to-hash-table (hash v table)
	(if (member v (gethash hash table) :test 'equal) nil
	(setf (gethash hash table) (append (gethash hash table) (list v)))))

;; Helper function for "store-fact" to retrieve the key
(defun get-key (fact)
	(if (equal (first fact) 'not) (list 'not (caadr fact))
					(list (car fact))))

;; Stores fact in hash table using two keys
(defun store-fact (fact)
	(let ((key1 (get-pred-key fact)) (key2 (get-key fact)))
	(cond ((or (and (not (equal (first key1) 'not))
		(member (list 'not fact)
			(gethash (cons 'not key1) *facts*) :test 'equal))
		(and (not (equal (first key2) 'not)) (member (list 'not fact)
			(gethash (cons 'not key2) *facts*) :test 'equal))) nil)
		((or (and (equal (first key1) 'not)
			(member (cadr fact) (gethash (cdr key1)
				 *facts*) :test 'equal))
		(and (equal (first key2) 'not)
			(member (cadr fact) (gethash (cdr key2) *facts*)
				 :test 'equal)))
		nil)
	(t	(add-to-hash-table key1 fact *facts*)
		(add-to-hash-table key2 fact *facts*)))))

;; Activates fact by searching for corresponding rules using two
;; keys we have associated with it in the hash
(defun activate-fact (fact)
	(loop for rule in (gethash (get-pred-key fact) *rules*) do
		(let ((never-seen (apply-rule rule fact :shallow t)))
		(verbose? t "=> ~a~%" never-seen)

	(if (equal '/ (first never-seen))
		(store-and-activate-rule never-seen)
		(store-and-activate-fact never-seen))))

	(loop for rule in (gethash (list (car fact)) *rules*) do
	(let ((never-seen (apply-rule rule fact :shallow t)))
		(verbose? t "=> ~a~%" never-seen)

	(if (equal '/ (first never-seen)) ; if this is new rule/fact, activate it
		(store-and-activate-rule never-seen)
		(store-and-activate-fact never-seen)))))

;; This function utilizes the above two helper functions
(defun store-and-activate-fact (fact)
	(if (not (null (store-fact fact))) (activate-fact fact)))

;; Get the associated key for a rule (helper function for store/activate-rule)
(defun get-rule-key (rule)
	(if (not (and (equal (list-length rule) 3)
		(equal (car rule) '/))) nil (get-pred-key (cadr rule))))

;; Get the associated key for a predicate (helper function for store/activate-rule)
(defun get-pred-key (pred)
	(cond ((equal (car pred) 'not)
		(append (list (car pred)) (get-pred-key (cadr pred))))
	((equal (list-length pred) 1) (list (car pred)))
	((equal (list-length pred) 2)
		(if (or (varp (cadr pred)) (_varp (cadr pred)))
			(list (car pred))
			(list (car pred) (cadr pred))))
	(t (if (or (varp (caddr pred)) (_varp (caddr pred)))
		(list (car pred)) (list (car pred) (caddr pred))))))

;; Stores rule in hash table
(defun store-rule (rule)
	(let ((key (get-rule-key rule)))
		(if (null key) nil (add-to-hash-table key rule *rules*))))

;; Activates rule by searching for corresponding fact using the key
;; and apply the rule using the fact found
(defun activate-rule (rule)
	(loop for fact in (gethash (get-rule-key rule) *facts*) do
		(let ((never-seen (apply-rule rule fact :shallow t)))
		(verbose? t "=> ~a~%" never-seen)

	(if (equal '/ (first never-seen)) ; if this is new rule/fact, activate it
		(store-and-activate-rule never-seen)
		(store-and-activate-fact never-seen)))))

;; This function utilizes the above two helper functions
(defun store-and-activate-rule (rule)
	(if (not (null (store-rule rule))) (activate-rule rule)))

