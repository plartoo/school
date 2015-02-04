(load "test-helper.lisp")
(load "phyo_src.lisp")

;; old tests from previous assignment
(test `(match-expr nil	nil) t)
(test `(match-expr (a b c) (a b c)) t)
(test `(match-expr (a b c) (a b)) nil)

(test `(match-expr (a _!0 c d) (a b b d)) nil)
(test `(match-expr (a _!0 _!1 d) (a b c d)) `((_!0 b) (_!1 c))) 
(test `(match-expr (a _!0 _!0 d) (a b c d)) `((_!0 c)))
(test `(match-expr (a _!0 _!1 d) (a b b d)) `((_!0 b) (_!1 b)))

(test `(match-expr (a _?0 _!1 d) (a b b d)) `((_?0 b) (_!1 b)))
(test `(match-expr (a _?0 _?0 d) (a b b d)) `((_?0 b)))
(test `(match-expr (a _?0 _?0 d) (a b c d)) `((_?0 c)))

(test `(match-expr (_?0 c) (b c)) `((_?0 b)))
(test `(match-expr (_?0)   (b)) `((_?0 b)))
(test `(match-expr (a _?0) (a b)) `((_?0 b)))
(test `(match-expr (a _?0 _?0 _?0) (a b)) `((_?0 b)))
(test `(match-expr (a _?0 _?0 _?0 b) (a b)) `((_?0)))

(test `(match-expr (a _? c) (a b c)) `((_? b)))
(test `(match-expr (a _* c) (a b c)) `((_* b)))
(test `(match-expr (a _* b) (a b)) `((_*)))
(test `(match-expr (a _* b) (a c c b)) `((_* c c)))

(test `(match-expr (a _+ b) (a b)) nil)
(test `(match-expr (a _+ b) (a c c b)) `((_+ c c)))
(test `(match-expr (_+ _*)  (a b c)) `((_+ a) (_* b c)))

(test `(match-expr ((_+)) ((a b c))) `((_+ a b c)))
(test `(match-expr ((a _+)) ((a b c))) `((_+ b c)))

(test `(match-expr ((a _? c) ((d)) (_?1 c))
	((a b c) ((d)) (c))) `((_? b) (_?1)))

(test `(match-expr (a ? c) (a b c)) nil)
(test `(match-expr ((* a c b)) (a b c)) `((* a b c)))
(test `(match-expr (* a c b) (a b c)) nil)
(test `(match-expr (* a c b) (a)) nil)
(test `(match-expr ((* a c b)) (a b c)) `((* a b c)))

(test `(match-expr b b)	t)
(test `(match-expr _! b) `((_! b)))
(test `(match-expr (a b _!) (a b _!)) nil)

(test `(match-expr (* a c b) b) `((* b)))
(test `(match-expr (! a) a) `((! a)))

(test `(match-expr ((* b c) a d) (c b c a d)) `((* c b c)))
(test `(match-expr ((! b c)) (a c)) nil)
(test `(match-expr ((! b c)) (c)) `((! c)))
(test `(match-expr ((+ a b c)) (a a c)) `((+ a a c)))

(test `(match-expr ((* b c) d) (c b c a d)) nil)
(test `(match-expr ((+ b c)) (c b c a)) nil)

(test `(match-expr (a (! _!) _!1) (a b c)) `((! b) (_! b) (_!1 c)))
(test `(match-expr (a (! _!) c)	(a b c)) `((! b) (_! b)))
(test `(match-expr (a (! _!)) (a b)) `((! b) (_! b)))

(test `(match-expr ((+ a _!) d _!0) (a c d e)) `((+ a c) (_! c) (_!0 e)))
(test `(match-expr ((+ a _!) d (!0 e)) (a c d e)) `((+ a c) (_! c) (!0 e)))
(test `(match-expr (d (*0 e _!0)) (d e f)) `((*0 e f) (_!0 f)))
(test `(match-expr ((+ a _!)) (a c)) `((+ a c) (_! c)))

(test `(match-expr ((+ (! b c))) (c)) `((+ c) (! c)))
(test `(match-expr (a (+ (! b c))) (a c)) `((+ c) (! c)))
(test `(match-expr (a (+ (!0 b c) (!1 d e))) (a d c)) `((+ d c) (!1 d) (!0 c)))

(test `(match-expr (a b c) (a b c d e)) nil)
(test `(match-expr (a (+ (* x))) (a x x x)) `((+ x x x) (* x)))
(test `(match-expr (a (+ _*)) (a x x x)) `((+ x x x) (_* x)))
(test `(match-expr (a (! _!)) (a x)) `((! x) (_! x)))
(test `(match-expr ((+ a _*) d) (a b c d)) `((+ a b c) (_* c)))
(test `(match-expr ((! _*)) (x x x)) `((! x x x) (_* x x x)))
(test `(match-expr ((? _*)) (x x x)) `((? x x x) (_* x x x)))
(test `(match-expr ((! (+ x))) (x x x)) `((! x x x) (+ x x x)))
(test `(match-expr ((! (!1 x))) (x x x)) nil)

(test `(match-expr (_*) nil) `((_*)))
(test `(match-expr (_?) nil) `((_?)))
(test `(match-expr ((* x y z)) nil) `((*)))
(test `(match-expr ((? x)) nil) `((?)))
(test `(match-expr (x (y ((* x y z)) z) k) (x (y nil z) k)) `((*)))
(test `(match-expr (x (y ((? x)) z) k) (x (y nil z) k)) `((?)))
(test `(match-expr ((! (+ x) (+ y))) (x x x)) `((! x x x) (+ x x x)))
(test `(match-expr (y (* (x _+) (y _?) (z _*)) k) (y c k)) nil)

;; Additional tests FOR '~'
(test `(match-expr ((* _! )) (a b)) `((* a b) (_! b)))
(test `(match-expr ((* _! ~ )) (a b)) `((* a b) (_! b)))
(test `(match-expr ((* _! ~ c)) (a b)) `((* a b) (_! b)))
(test `(match-expr ((* _! ~ b)) (a b)) nil)
(test `(match-expr ((+ (* _! ~ b))) (a b)) nil)
(test `(match-expr ((+ (* _!) ~ b)) (a b)) nil)
(test `(match-expr ((+ (* _! ~ c))) (a b)) `((+ a b) (* b) (_! b)))
(test `(match-expr ((+ (* _!) ~ c)) (a b)) `((+ a b) (* b) (_! b)))
(test `(match-expr ((* ~ b)) (a b)) nil)
(test `(match-expr ((* ~ c)) (a b)) `((* a b)))
(test `(match-expr (a b ~) (a b ~)) t)
(test `(match-expr (a b _!) (a b ~)) `((_! ~)))
(test `(match-expr ((* ~ ~)) (a b)) `((* a b)))
(test `(match-expr ((* _! ~ ~)) (a b)) `((* a b) (_! b)))

;; Tests for binding substitution
(test `(subst-bindings ((! (b)) (_* c))	((a) ! (c))) `((a) (b) (c)))
(test `(subst-bindings ((! a) (!2 c)) (! b !2 d)) `(a b c d))
(test `(subst-bindings ((! a)) (((a ! a)))) `((( a a a))))
(test `(subst-bindings nil (((a ! a)))) `((( a ! a))))
(test `(subst-bindings ((! a)) (a ! a !)) `(a a a a))
(test `(subst-bindings ((? nil)) (?)) `(nil))
(test `(subst-bindings ((_! hate) (! my) (* present job))
			(Why do you _! your * ?)) `(Why do you hate your present job ?))
;(test `(subst-bindings '((! b ? c) (? x y)) '(! a b)) '(b x y c a b)) ; TODO?

;; Tests for apply-rule (given in assignment)
(test `(apply-rule (/ (I _! (! my the) (* (!1 present stupid new) (!2 job boss room-mate)))
 (Why do you _! your * ?)) (I hate my new boss)) `(Why do you hate your new boss ?))

;(test `(apply-rule '(/ (dog _!x) (barks _!x)) '(dog rover)) '(barks rover)) ; TODO?

;;; Examples/Tests showing inference
;; A(x) v B(y) => A(x)
(test `(apply-rule (/ ( _!1(_!) or _!2(_!3) ) ( _!1(_!) ))
			(Professor(x) or Student(x))) `(Professor(x)))

;; A(x) => A(x) v A(x)
(test `(apply-rule (/ (_!1 ( _! )) (_!1 ( _! ) or _!1 (_!)))
			(Student (x))) `(Student(x) or Student(x)))

