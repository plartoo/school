;; Lisp Assignment 4 - Phyo Thiha
;; Implementation of KB with the ability to detect condictradictions

;; Load compiled ttt modules and helper functions
(load "src/ttt_loader")
(load "helper")

(defparameter *verbose* t)
;; Note for Adam: Set this flag to nil to disable verbose mode
;(setq *verbose* nil)

;; Helper function for "get-fact-keys" which returns keys for a fact
(defun key-for-fact(f)
  (if (= (length f) 2) `((,(first f)) (,(first f) ,(second f)))
	`((,(first f)) (,(first f) ,(third f)))))

(defun get-fact-keys(f)
  (if (equal (car f) `not) (mapcar (lambda (x) (cons `not x))
	(key-for-fact (cdr f))) (key-for-fact f)))

;; Helper function for "get-rule-keys" which returns keys for a fact
(defun key-for-rule(r)
  (let ((focus (if (= (length r) 2) 1 2)))
	(if (or (var? (nth focus r)) (listp (nth focus r)))
		`(,(first r)) `(,(first r) ,(nth focus r)))))

(defun get-rule-keys(r)
  (if (equal (car (second r)) `not)
	(cons `not (key-for-rule (cdr (second r))))
	(key-for-rule (second r))))

;; Checks if a fact is already in either of the old or new hash
(defun known-fact?(f)
(let ((key (second (get-fact-keys f))))
  (and (or (member f (gethash key *old-facts*) :test 'equal)
           (member f (gethash key *new-facts*) :test 'equal))
  t)))

;; Checks if a rule is already in either of the old or new hash
(defun known-rule? (r)
  (let ((key (get-rule-keys r)))
    (and (or (member r (gethash key *old-rules*) :test 'equal)
	     (member r (gethash key *new-rules*) :test 'equal))
    t)))

;; Gets the negation of the fact/parameter
(defun negate (f)
 (if (equal (car f) 'not) (cdr f) (cons 'not f)))

(defun is-rule? (expr) (equal (car expr) '/))

;; Create four hashes for knowledge bases
;; Two of them will store old/known facts and rules
;; The other two will hold new ones temporarily
;; If a newly added knowledge is consistent, it'll be added
;; to the old corresponding hash directly.
;; Else, we'll negate the knowledge and add it to the old hash
;; if it doesn't already exist there
(defparameter *old-facts* (make-hash-table :test 'equal))
(defparameter *old-rules* (make-hash-table :test 'equal))
(defparameter *new-facts* (make-hash-table :test 'equal))
(defparameter *new-rules* (make-hash-table :test 'equal))

(defun reset-knowledge-base()
	(reset-hash *old-facts*)
	(reset-hash *old-rules*))

(defun print-hash-values(hash k)
  (let ((temp (make-hash-table :test 'equal)))
    (loop for lst being the hash-values of hash do
      (dolist (value lst) (if (not (equal k value))
			    (setf (gethash value temp) value))))
    (loop for val being the hash-values of temp do
      (format t "~T~T~T~T~S~%" val))))

;; Enter rule into new rule hash
(defun store-rule(r)
  (let ((key (get-rule-keys r)))
     (let ((oldlist (gethash key *new-rules*)))
	(if oldlist  (setf (gethash key *new-rules*) (cons r oldlist))
	  (setf (gethash key *new-rules*) `(,r))))))

;; Enter fact into new fact hash
(defun store-fact(f)
  (let ((keys (get-fact-keys f)))
    (setf (gethash (first keys) *new-facts*)
      (append `(,f) (gethash (first keys) *new-facts*)))
        (setf (gethash (second keys) *new-facts*)
	  (append `(,f) (gethash (second keys) *new-facts*)))))

;; Remove the hash table entry by entry
(defun reset-hash(h)
  (loop for k being the hash-keys of h do
    (remhash k h)))

;; Given a hash with items (h1), copy to a new hash (h2) and reset h1
(defun copy-hash(h1 h2)
  (loop for v1 being the hash-values of h1
    using (hash-key key) do
      (let ((v2 (gethash key h2))) (setf (gethash key h2) 
        (if v2  (append v1 v2) v1)))
	  (remhash key h1))) ; reset the hash entry

;; Copy the consistent facts/rules from new-facts/rules hash
;; into old-facts/rules hash
(defun transfer-entry(e)
 (progn (vformat "Inferences made:")
   (if *verbose* (print-hash-values *new-facts* e))
   (if *verbose* (print-hash-values *new-rules* e))
		 (copy-hash *new-facts* *old-facts*)
		 (copy-hash *new-rules* *old-rules*)))

;; Given a fact, retrieves matching entries from rule hashes
(defun get-matching-rules(f)
 (let ((k (get-fact-keys f)))
  (append (gethash (first k) *old-rules*)
	  (gethash (first k) *new-rules*)
	  (gethash (second k) *old-rules*)
	  (gethash (second k) *new-rules*))))

;; Given a rule, returns matching entries from fact hashes
(defun get-matching-facts(r)
 (let ((k (get-rule-keys r)))
  (append (gethash k *old-facts*)
	  (gethash k *new-facts*))))

;; Helper function for store-and-activate-fact
;; Returns "t" if a fact is stored and "nil" if inconsistent fact is detected
(defun store-fact-helper(f)
 (cond ((known-fact? f)	t)
	((known-fact? (negate f)) nil)
	(t (let ((ret t)) (store-fact f)
	   (dolist (rule (get-matching-rules f))
	(setf ret (store-fact-or-rule rule f))
        (if (not ret) (return))) ret))))

;; Helper function for store-and-activate-rule
;; Returns "t" if a fact is stored and "nil" if inconsistent fact is detected
(defun store-rule-helper(r)
  (cond ((known-rule? r) t)
  (t (let ((ret t)) (store-rule r)
	  (dolist (fact (get-matching-facts r))
	  (setf ret (store-fact-or-rule r fact))
	  (if (not ret) (return))) ret))))

;; Helper function which decides whether to call
;; "store-and-activate-fact" or "store-and-activate-rule"
(defun store-fact-or-rule(r f)
 (if (is-rule? (apply-rule r f :shallow t))
       (store-rule-helper (apply-rule r f :shallow t))
       (store-fact-helper (apply-rule r f :shallow t))))

;; Enters new fact to KB. This handles inconsistency checking and add
;; those facts, which are negated, if they don't already exist in the KB
(defun store-and-activate-fact(f)
 (cond ((known-fact? (negate f))
   (progn (vformat "Store inconsistent fact:") (vprint f)))
   ((known-fact? f) (progn (vformat "Store known fact:") (vprint f)))
   ((store-fact-helper f) (progn (vformat "Stored fact:") (vprint f)
	(transfer-entry f)))
   (t (progn (reset-hash *new-facts*)
	     (reset-hash *new-rules*)
	     (vformat "Processing inconsistent fact:")
	     (vprint f) (vformat "Adding negated fact:")
			(store-fact-helper (negate f)) (transfer-entry f)))))

;; Enters new fact to KB. This handles inconsistency checking and add
;; those facts, which are negated, if they don't already exist in the KB
(defun store-and-activate-rule(r)
  (newline)
  (cond ((known-fact? r)
	(progn (vformat "Store known rule:") (vprint r)	(return nil)))
  ((store-rule-helper r) (progn (vformat "Stored rule:") (vprint r) 
				(transfer-entry r)))
	(t (progn (reset-hash *new-facts*) (reset-hash *new-rules*)
	(vformat "Storing inconsistent rule:") (vprint r)))))


