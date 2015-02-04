;; Phyo Thiha CSC444 HW1


;; Helper functions for sanity checking
; checks if the list content are all alpha-numeric characters
(defun alphanumericList? (lst)
 (or (null lst)	(and (alphanumericp (car lst)) (alphanumericList? (cdr lst)))))

; checks if the list starts with a character
(defun beginsWith? (lst chr) (char= (car lst) chr))

; checks if the string representing the symbol sym matches _?[!\?\*\+](\w\d)*
(defun varHelper (lst)
 (and (reduce #'(lambda (b c) (or (beginsWith? lst c) b))
  `(#\? #\! #\* #\+) :initial-value nil)
    (alphanumericList? (cdr lst))))

; removes underscore
(defun removeUnderscore (lst)
 (if (beginsWith? lst #\_) (cdr lst) lst))

; returns the symbol as list of characters
(defun symToCharList (sym) (coerce (string sym) `list))

; checks if sym is a valid variable
(defun var? (sym)
 (and (symbolp sym) (varHelper (removeUnderscore (symToCharList sym)))))

(defun unconstrainedVar? (sym)
 (and (var? sym) (beginsWith? (symToCharList sym) #\_)))

(defun constrainedVar? (sym)
 (and (var? sym) (not (beginsWith? (symToCharList sym) #\_))))

; checks if sym is a valid, unconstrained variable of a given type
(defun uVarIsType? (sym chr)
 (and (unconstrainedVar? sym) (beginsWith? (cdr (symToCharList sym)) chr)))

; checks if sym is a valid, constrained variable of a given type
(defun cVarIsType? (sym chr)
 (and (constrainedVar? sym) (beginsWith? (symToCharList sym) chr)))

;; Helper functions for match-expr
; eliminate shadowed variables from a list
(defun removeShadowVars (lst)
 (if (or (null lst) (equal lst t))
  lst (let ((tail (removeShadowVars (cdr lst))))
   (if (not (member (car (car lst)) (mapcar 'car tail)))
    (cons (car lst) tail) tail))))

(defun joinLists (listA listB)
 (cond ((not (and listA listB)) nil)
	((equal listA t) listB)
	  ((equal listB t) listA)
	   (t (append listA listB))))

; returns everything in the list BEFORE the first tilda
(defun beforeTilda (lst)
 (if (or (not (listp lst))
  (null lst) (eq (car lst) `~))
   nil (cons (car lst) (beforeTilda (cdr lst)))))

; returns everything in the list AFTER the first tilda
(defun afterTilda (lst)
 (cond ((or (not (listp lst)) (null lst)) nil)
  ((eq (car lst) `~) (cdr lst))
   (t (afterTilda (cdr lst)))))

; match patt of unconstrained variables against expr
(defun matchUnconstrained (patt expr n cap)
 (cond ((and (> cap 0) (> n cap)) nil)		; check number of terms
	((> n (length expr)) nil)
	((equal (match-helper (cdr patt) (nthcdr n expr)) t)
		(cons (cons (car patt) (subseq expr 0 n)) nil))	
	((match-helper (cdr patt) (nthcdr n expr))
		(cons (cons (car patt) (subseq expr 0 n))
		(match-helper (cdr patt) (nthcdr n expr))))
	(t (matchUnconstrained patt expr (+ n 1) cap))))

; checks if exprs matches patt
(defun matchEachOnePattHelper (patt exprs)
 (cond ((null exprs) t)
        ((match-helper patt (car exprs))
	(matchEachOnePattHelper patt (cdr exprs)))
	(t nil)))

; mactches each term in exprs to some term in patt
(defun matchEachOnePatt (patts exprs)
 (cond ((null exprs) t)
	((null patts) nil)
	((and	(matchEachOnePattHelper (car patts) exprs)
		(or (uVarIsType? (car patts) #\*)
		(uVarIsType? (car patts) #\+)
		(and	(listp (car patts))
			(or (cVarIsType? (car (car patts)) #\*) (cVarIsType? (car (car patts)) #\+)))))
		(cons (cons
		(if (listp (car patts))
		  (car (car patts)) (car patts)) exprs) nil))
		(t (matchEachOnePatt (cdr patts) exprs))))

; mactches each term in exprs to some term in patt
(defun matchEach (patts pattsCopy exprs)
(cond ((equal exprs nil) t)
	((equal patts nil) nil)
	((and (match-expr (car patts) (car exprs))
		(matchEach pattsCopy pattsCopy (cdr exprs)))
	(joinLists (match-expr (car patts) (car exprs))
		(matchEach pattsCopy pattsCopy (cdr exprs))))
	(t (matchEach (cdr patts) pattsCopy exprs))))

; checks if one of the patterns matches one of the exprssions
(defun matchAnyone (patts pattsCopy exprs)
(cond ((null exprs) nil)
	((null patts) (matchAnyone pattsCopy pattsCopy (cdr exprs)))
	((match-expr (car patts) (car exprs)) t)
	(t (matchAnyone (cdr patts) pattsCopy exprs))))

(defun matchEachCheckTilda (patt exprs)
(and (not (matchAnyone (afterTilda patt) (afterTilda patt) exprs))
 (if (beforeTilda patt)
	(matchEach (beforeTilda patt) (beforeTilda patt) exprs)	t)))

; match patt of unconstrained variables against expr
(defun matchConstrained (patt expr base n cap)
 (cond ((> n (length expr)) nil)
	((and (= base 1) (> n cap) (listp patt))
	(if (and (matchEachOnePatt (cdr (car patt)) (subseq expr 0 n))
		(match-helper (cdr patt) (nthcdr n expr)))
		(cons (cons (car (car patt)) (subseq expr 0 n))
		(joinLists (matchEachOnePatt (cdr (car patt)) (subseq expr 0 n))
			(match-helper (cdr patt) (nthcdr n expr))))
		(matchConstrained patt expr base (+ n 1) cap)))
		((and (> cap 0) (> n cap)) nil)
		((and (equal (match-helper (cdr patt) (nthcdr n expr)) t)
			(matchEachCheckTilda (cdr (car patt)) (subseq expr 0 n)))
		(joinLists (cons (cons (car (car patt)) (subseq expr 0 n)) nil)
			(matchEachCheckTilda (cdr (car patt)) (subseq expr 0 n))))
		((and (match-helper (cdr patt) (nthcdr n expr))
			(matchEachCheckTilda (cdr (car patt)) (subseq expr 0 n)))
		(cons (cons (car (car patt)) (subseq expr 0 n))
			(joinLists (matchEachCheckTilda (cdr (car patt)) (subseq expr 0 n))
				(match-helper (cdr patt) (nthcdr n expr)))))
		(t (matchConstrained patt expr base (+ n 1) cap))))


(defun match-helper (patt expr)
 (cond ((and (null expr) (null patt)) t)
		((null patt) nil)
		((unconstrainedVar? patt) `((,patt ,expr)))
		((listp patt)
		 (cond ((uVarIsType? (car patt) #\!)
			(matchUnconstrained patt expr 1 1))
			((uVarIsType? (car patt) #\?)
				(matchUnconstrained patt expr 0 1))
			((uVarIsType? (car patt) #\*)
				(matchUnconstrained patt expr 0 -1))
			((uVarIsType? (car patt) #\+)
				(matchUnconstrained patt expr 1 -1))
			((and (constrainedVar? (car patt)) (listp expr)) nil)
			((cVarIsType? (car patt) #\!)	; check for uvars (! a) a
				(matchConstrained `(,patt) `(,expr) 1 1 1))
			((cVarIsType? (car patt) #\?)
				(matchConstrained `(,patt) `(,expr) 1 0 1))
			((cVarIsType? (car patt) #\*)
				(matchConstrained `(,patt) `(,expr) 0 0 -1))
			((cVarIsType? (car patt) #\+)
				(matchConstrained `(,patt) `(,expr) 0 1 -1))
			((listp (car patt))
				(cond ((cVarIsType? (car (car patt)) #\!)
					(matchConstrained patt expr 1 1 1))
				((cVarIsType? (car (car patt)) #\?)
					(matchConstrained patt expr 1 0 1))
				((cVarIsType? (car (car patt)) #\*)
					(matchConstrained patt expr 0 0 -1))
				((cVarIsType? (car (car patt)) #\+)
					(matchConstrained patt expr 0 1 -1))
				((listp (car expr))
					(joinLists (match-helper (car patt) (car expr))
						(match-helper (cdr patt) (cdr expr))))
				(t nil)))
				((not (listp expr)) nil)
				((equal (car patt) (car expr))
					(match-helper (cdr patt) (cdr expr)))
				(t nil)))
		((equal patt expr) t)
		(t nil)))

;; checks if all elements of lst satifsy the predicate pred
(defun all (pred lst)
 (or (null lst) (reduce (lambda (x y) (and x y)) (mapcar pred lst))))

;; checks if the parameter is a valid pattern
(defun validPatt (patt)
 (if (and (listp patt) (not (null patt)))
  (and patt (validPatt (car patt))
   (all (lambda (x) (and (not (constrainedVar? x)) (validPatt x))) (cdr patt))) t))

;; checks if the patt is a valid expression
(defun validExpr (expr)
 (cond ((listp expr) (all `validExpr expr))
  ((symbolp expr) (not (var? expr)))
   (t t)))

;; checks if an expression (expr) matches a given pattern (patt).
(defun match-expr (patt expr)
 (and (validPatt patt) (validExpr expr)
  (removeShadowVars (match-helper patt expr))))

;; checks if an expression (expr) matches a given pattern (patt).
(defun match-exprnc (patt expr)
(and (validPatt patt) (validExpr expr) (match-helper patt expr)))

;;; Part 2
;; checks if bindings is a valid list of bindings
(defun validBindings (bindings)
 (and
  (listp bindings)
   (all 
    (lambda (binding) (and (listp binding) (>= (length binding) 2)
     (var? (car binding))))
       bindings)))

;; helper function for subst-bindings
(defun subst-binding (old new expr)
 (cond
  ((null expr) nil)
  ((eq (car expr) old)
   (append new (subst-binding old new (cdr expr))))
    ((listp (car expr))
     (cons (subst-binding old new (car expr))
      (subst-binding old new (cdr expr))))
       (t (cons (car expr) (subst-binding old new (cdr expr))))))

;; Substitutes the specified bindings for all occurrences of the matched vars.
;; Accepts "bindings", which is a list of pairs bound variables and their values,
;; and "expr", an arbitrary expression, as parameters
(defun subst-bindings (bindings expr)
 (if (null bindings) expr
     (and (validBindings bindings) (listp expr)
	(subst-bindings	; subsitute one binding and recurse
	  (cdr bindings)
	  (subst-binding (car (car bindings)) (cdr (car bindings)) expr)))))

;;; Part 3
;; Performs template-to-template transduction
(defun apply-rule (rule expr)
 (and (listp rule) (= (length rule) 3)
 (eq (car rule) `/)
 (subst-bindings (match-expr (second rule) expr) (third rule))))


