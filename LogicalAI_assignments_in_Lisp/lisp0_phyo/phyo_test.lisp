(load "phyo_src.lisp")

(defun test (problem)
  (cond ((= problem 11)
	 (print "problem 1(a)")
	 ;; Simple list
	 (print "lst is (1 2 3), result is")
	 (print (my-copy (list '1 '2 '3)))
	 ;; Empty list
	 (print "lst is (), result is")
	 (print (my-copy (list nil)))
	 ;; Non-list
	 (print "lst is 3")
	 (print (my-copy 3))
	 ;; Nested list
	 (print "lst is (a (b c)), result is")
	 (print (my-copy '(a (b c)))))
	((= problem 12)
	 (print "problem 1(b)")
	 ;; Simple list
	 (print "lst is (1 2 3), result is")
	 (print (my-reverse (list '1 '2 '3)))
	 ;; Empty list
	 (print "lst is (), result is")
	 (print (my-reverse (list nil)))
	 ;; Non-list
	 (print "lst is 3")
	 (print (my-reverse 3))
	 ;; Nested list
	 (print "lst is (a (b c)), result is")
	 (print (my-reverse '(a (b c)))))
	((= problem 2)
	 (print "problem 2")
	 ;; A var with no numbers
	 (print "x is !, result is")
	 (print (var? '!))
	 ;; A var with numbers
	 (print "x is '?0312, result is")
	 (print (var? '?0312))
	 ;; Not a var - a number
	 (print "x is 1034, result is")
	 (print (var? 1034))
	 ;; Not a var - a symbol
	 (print "x is 'x, result is")
	 (print (var? 'x))
	 ;; NIL
	 (print "x is NIL, result is")
	 (print (var? NIL))
	 ;; Not  a var, a list
	 (print "x is '(1 2)")
	 (print (var? '(1 2))))
	((= problem 3)
	 (print "problem 3")
	 ;; A nested list
	 (print "lst is '(x (y z ((k)))), result is")
	 (print (my-flatten '(x (y z ((k))))))
	 ;; NIL
	 (print "lst is NIL")
	 (print (my-flatten NIL))
	 ;; Not a list
	 (print "lst is 42")
	 (print (my-flatten 42)))
	((= problem 4)
	 (print "problem 4")
	 ;; A nested list
	 (print "lst is '(OLD (a (OLD b) c) NEW), new0 is NEW, old0 is OLD, result is")
	 (print (my-subst 'NEW 'OLD '(OLD (a (OLD b) c) NEW)))
	 ;; Lst is NIL
	 (print "lst is NIL, new0 is n, old0 is o")
	 (print (my-subst 'n 'o NIL)))
	((= problem 5)
	 (print "problem 5")
	 ;; Patt and sent are identical
	 (print "patt is '(hello world), sent is '(hello world), result is")
	 (print (match '(hello world) '(hello world)))
	 ;; All !'s
	 (print "patt is '(hello !1 !2 world), sent is '(hello brave new world), result is")
	 (print (match '(hello !1 !2 world) '(hello brave new world)))
	 ;; Patt and sent are nil
	 (print "patt is nil, sent is nil, result is")
	 (print (match nil nil))
	 ;; Mixed !'s and ?'s
	 (print "patt is '(hello ?1 !1), sent is '(hello world)")
	 (print (match '(hello ?1 !1) '(hello world)))
	 ;; Coordinating matches
	 (print "patt is '(hello ?1 ?2 goodbye ?1), sent is '(hello world goodbye world)")
	 (print (match '(hello ?1 ?2 goodbye ?1) '(hello world goodbye world))))
	((= problem 6)
	 (print "problem 6")
	 ;; A WFF
	 (print "w is (or (=> x y) (~ z) w), result is")
	 (print (wff? '(or (=> x y) (~ z) w)))
	 ;; Multiple ANDs
	 (print "w is (and (~ x) y (or z w)), result is")
	 (print (wff? '(and (~ x) y (or z w))))
	 ;; Not a WFF
	 (print "w is (x <=> y), result is")
	 (print (wff? '(x <=> y))))))
