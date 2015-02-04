;; Phyo Thiha CSC444 HW1
;; A more powerful pattern matcher

;; returns nil if the character given is NOT one of "!, *, +, ?"
(defmacro var-type (chr)
	`(cond ((not (typep ,chr 'character)) nil)
		((char= #\! ,chr) '!)
		((char= #\* ,chr) '*)
		((char= #\+ ,chr) '+)
		((char= #\? ,chr) '?)
		(t nil)))

;; if S-expression is a match variable with underscore,
;; returns it as a symbol, else returns nil
(defun _varpatt (exp)
	(if (listp exp) nil
		(let ((lst (coerce (write-to-string exp) 'list)))
			(cond ((char= #\_ (first lst)) (var-type (second lst)))
				((and (char= #\| (first lst))
					(char= #\_ (second lst)))
				(var-type (third lst)))
				(t nil)))))

;; if S-expression is a match variable without underscore,
;; returns it as a symbol, else returns nil
(defun varpatt (exp)
	(if	(or (not (listp exp))
			(listp (first exp))) nil
		(let ((chr (first (coerce (write-to-string (first exp)) 'list))))
			(var-type (if (char= chr #\|)
			(second (coerce (write-to-string (first exp)) 'list))
			chr)))))

;; checks if a list of variable has matching pattern against the second list
(defun has-matchvarpatt (mvar mlst)
	(cond ((null mlst) nil)
		((equal mvar (caar mlst)) t)
		(t (has-matchvarpatt mvar (rest mlst)))))

;; matcher-helper for "!"
(defun exclamationsym (patt expr &optional constr)
	(cond ((and (null patt) (null expr)) t)
		((and (not (null constr)) (null (member (first expr) constr))) nil)
	        (t	(let ((result (match-expr (rest patt) (rest expr))))
			(cond ((null result) nil)
				((equal result t) (list (list (first patt) (first expr))))
				((has-matchvarpatt (first patt) result) result)
					(t (append (list (list (first patt) (first expr))) result)))))))

;; matcher for "!"
(defun exclamationsymc (patt expr constr)
	(block outer
		(loop for c in constr do
			(let ((result (match-expr (append (list c) (rest patt)) expr)))
				(cond ((null result) nil)
					((equal result t) (return-from outer (list (list (first patt) (first expr)))))
						((has-matchvarpatt (first patt) result)
						(return-from outer result))
						(result	(return-from outer
			(append (list (list (first patt) (first expr))) result))))))))


;; matcher-helper for "+"
(defun plussym (patt expr lst &optional constr)
	(cond ((null expr) nil)
		((and (not (null constr)) (null (member (first expr) constr))) nil)
        (t (let ((result (match-expr (rest patt) (rest expr))))
		(cond ((null result) (plussym patt (rest expr) (append lst (list (first expr))) constr))
		((equal result t)
		(list (append (list (first patt)) (append lst (list (first expr))))))
			((has-matchvarpatt (first patt) result) result)
		(t (append (list
			(append (list (first patt)) (list (first expr))))
result)))))))

;; matcher for "+"
(defun plussymc (patt expr lst constr)
	(if (null expr) nil
		(block outer
			(loop for c in constr do
				(cond ((_varpatt c) nil)
					((varpatt c) nil)
				(t (if (equal c (first expr))
					(let ((result (Match-expr (rest patt) (rest expr))))
				(cond ((null result) (return-from outer (plussymc patt
					(rest expr)
					(append lst (list(first expr)))
					constr)))
			((equal result t) (return-from outer (append (list (append (list (first patt)) lst (list (first expr)))))))
			((has-matchvarp (first patt) result)
				(return-from outer result))
			(t	(return-from outer (append (list (append (first patt) lst) result))))
)))))))))


;; matcher for "?"
(defun questionsym (patt expr &optional constr)
	(let ((result (match-expr (rest patt) expr)))
		(cond ((null result)
			(if (null constr)
				(exclamationsym patt expr)
				(exclamationsymc patt expr constr)))
				((equal result t) (list (list (first patt))))
				((has-matchvarpatt (first patt) result) result)
	(t (append (list (list (first patt))) result)))))

;; matcher for "*"
(defun asteriksym (patt expr &optional constr)
	(let ((result (match-expr (rest patt) expr)))
		(cond ((null result)
	(if (null constr)
		(plussym patt expr nil)
		(plussymc patt expr nil constr)))
		((equal result t) (list (list (first patt))))
		((has-matchvarpatt (first patt) result) result)
	(t (append (list (list (first patt))) result)))))


(defun matchn (start stop mvar match patt expr)
	(if (equal start (+ stop 1)) nil
		(let ((result (match-expr (if (zerop start)
			patt (rest patt)) expr)))
		(cond ((equal result t) (list (list mvar match)))
			((null result) (matchn (+ start 1) stop mvar
			(append match (first expr))
				patt (rest expr)))
			((has-matchvarpatt (first patt) result) result)
		(t (append (list (list (first patt) (first expr))) result))
))))

;; helper function checking if all args are null
(defmacro all-null (&rest args)
  `(and ,@(loop for a in args collect
                `(equal ,a nil))))

;; Main function of the homework that utilizes 
;; every helper function we defined above.
(defun match-expr (patt expr)
	(if (or (not (listp patt)) (not (listp expr))) nil
		(let ((phead (first patt)) (ehead (first expr))
		(ptail (rest patt))  (etail (rest expr)))
	(cond ((all-null phead ehead ptail etail) t)
		((or (_varpatt ehead) (varpatt ehead)) nil)
		((not (listp phead))
	(let ((vtype (_varpatt phead)))
		(cond ((equal phead ehead)
		(match-expr (rest patt) (rest expr)))
		((equal vtype '!) (exclamationsym patt expr))
		((equal vtype '?) (questionsym patt expr))
		((equal vtype '*) (asteriksym patt expr '()))
		((equal vtype '+) (plussym patt expr '()))
			((null vtype) nil)
			(t nil))))
		((varpatt phead)
		(let ((vtype (varpatt phead))
			(constr (rest phead)))
		(cond ((equal vtype '!) (exclamationsymc (append (list (caar patt)) (cdr patt)) expr constr))
		((equal vtype '?) (questionsym (append (list (caar patt)) (cdr patt)) expr constr))
                ((equal vtype '*) (asteriksym (append (list (caar patt)) (cdr patt)) expr constr))
                ((equal vtype '+) (plussymc (append (list (caar patt)) (cdr patt)) expr '() constr))
                ((null vtype) nil)
		(t nil))))
		(t (let ((head (match-expr phead ehead))
			(tail (match-expr ptail etail)))
			(cond	((and (equal head t) (equal tail t)) t)
				((equal head t) tail)
				((equal tail t) head)
			(t (append head tail)))))
))))
