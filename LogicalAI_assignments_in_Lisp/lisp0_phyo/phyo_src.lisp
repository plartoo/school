;; Phyo Thiha CSC444 HW0
;; Return a copy of lst
(defun my-copy (lst)
  (if (or (null lst) (not (typep lst 'list)))
      nil
    (append (list (first lst)) (my-copy (rest lst)))))

;; Return a reversed copy of lst
(defun my-reverse (lst)
  (if (or (null lst) (not (typep lst 'list)))
      nil
    (append (my-reverse (rest lst)) (list (first lst)))))

;; Return T if the symbol is of the form [?!][\d]*
(defun var? (x)
  (if (or (null x) (not (or (symbolp x) (typep x 'array))))
      nil
    (if (or (char= (char (string x) 0) #\!) 
	    (char= (char (string x) 0) #\?))
	;; Check if there are no more characters after the symbols
	(if (or (= (length (string x)) 1) 
		(null (char (string x) 1)))
	    T
	  ;; If there are, check that they're digits
	  (if (digit-char-p (char (string x) 1))
	      (var? (coerce (append (list (first (coerce (string x) 'list))) 
			    (rest (rest (coerce (string x) 'list)))) 'string))
	    nil))
      nil)))

;; Flatten a nested lst
(defun my-flatten (lst)
	(cond ((not (listp lst)) lst)
	        ((equal '() lst) '())
	        ((listp (first lst))
	         (append (my-flatten (first lst))
	                 (my-flatten (rest lst))))
	        (t (append (list (first lst))
	                   (my-flatten (rest lst))))))

;; Substitute new0 for old0 in lst
(defun my-subst (new old lst)
	(cond ((not (listp lst)) nil)
	        ((equal lst '()) '())
	        ((equal lst old) new)
	        ((and (equal (first lst) old) (equal new nil))
	         (my-subst new old (rest lst)))
	        ((equal (first lst) old)
	         (append (list new) (my-subst new old (rest lst))))
	        ((listp (first lst))
	         (cons (my-subst new old (first lst))
	               (my-subst new old (rest lst))))
	        (t (cons (first lst) (my-subst new old (rest lst))))))

;; Matches a list pattern, "patt", consisting of words
;; and possibly "match variables" against a list of words.
(defun match (patt sent)
	(cond ((or (not (listp patt)) (not (listp sent))) '())
	        ((equal patt sent) t)
	        ((not (var? (first patt)))
	         (if (equal (first patt) (first sent))
	           (match (rest patt) (rest sent))
	           '()))
	        ((char= (first (coerce (string (first patt)) 'list)) #\!)
	         (cond ((equal (match (my-subst (first sent) (first patt) (rest patt)) (rest sent)) t)
	                (list (list (first patt) (first sent))))
	               ((equal (match (my-subst (first sent) (first patt) (rest patt)) (rest sent)) '())
	                '())
	               (t (append (list (list (first patt) (first sent)))
	                          (match (my-subst (first sent) (first patt) (rest patt))
	                                 (rest sent))))))
	        ((char= (first (coerce (string (first patt)) 'list)) #\?)
	         (cond ((equal (match (my-subst nil (first patt) (rest patt)) sent) t)
	                (list (list (first patt))))
	               ((match (my-subst nil (first patt) (rest patt)) sent)
	                (append (list (list (first patt)))
	                        (match (my-subst (first sent) (first patt) (rest patt))
	                               sent)))
	               ((equal (match (my-subst (first sent) (first patt) (rest patt)) (rest sent)) t)
	                (list (list (first patt) (first sent))))
	               ((equal (match (my-subst (first sent) (first patt) (rest patt)) (rest sent)) '())
	                '())
	               (t (append (list (list (first patt) (first sent)))
	                          (match (my-subst (first sent) (first patt) (rest patt))
	                                 (rest sent))))))))

;; Return T if w is a well-formed formula
(defun wff? (w)
  (if (null w)
      nil
    (cond
      ;; If w is a symbol, recheck it as a list
     ((and (not (typep w 'list)) (symbolp w)) (wff? (list w)))
     ;; If w is a negated wff
     ((char= (char (string (first w)) 0) #\~) (wff? (rest w)))
     ;; If w is an "and" or "or" expression
     ((or (string= (string (first w)) "OR") (string= (string (first w)) "AND"))
      ;; If "and" or "or" have more than two arguments, recurse to get to two
      (if (> (length w) 3)
	  (and (wff? (nth 1 w)) (wff? (append (list (first w)) (rest (rest w)))))
	 ;; If there are only two arguments, check that they are WFF
	 (and (wff? (first (rest w)))
	   (wff? (first (rest (rest w)))))))
     ;; If w is an implication expression
     ((or (string= (string (first w)) "=>") (string= (string (first w)) "<=>"))
      (and (wff? (first (rest w)))
	   (wff? (first (rest (rest w))))))

     ((or (symbolp w) (and (typep w 'list) (null (rest w)) (symbolp (first w)))) T))))


