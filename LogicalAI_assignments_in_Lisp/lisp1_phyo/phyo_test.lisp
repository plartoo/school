(load "test-helper.lisp")
(load "phyo_src.lisp")

;; testing underscore variable patterns (COMPLETE)
(deftest test-_varpatt ()
  (check
    (not (_varpatt '(2 a 2)))
    (not (_varpatt '?2))
    (equal (_varpatt '_?abc123) '?)
    (equal (_varpatt '_*abc123) '*)
    (equal (_varpatt '_+abc123) '+)
    (equal (_varpatt '_!abc123) '!)
))

;; testing non-underscore variable patterns (COMPLETE)
(deftest test-varpatt ()
  (check
    (not (varpatt '(2 a 2)))
    (not (varpatt '(a ?1)))
    (equal (varpatt '(!abc123 a b c)) '!)
    (equal (varpatt '(?abc123 a b c)) '?)
    (equal (varpatt '(*abc123 a b c)) '*)
    (equal (varpatt '(+abc123 a b c)) '+)
    (equal (varpatt '(! a b)) '!)
    (equal (varpatt '(? a b)) '?)
    (equal (varpatt '(* a b)) '*)
    (equal (varpatt '(+ a b)) '+)
    (equal (varpatt '(+1a a b)) '+)
    (equal (varpatt '(!a a b)) '!)
    (equal (varpatt '(?a a b)) '?)
    (equal (varpatt '(*a a b)) '*)
    (equal (varpatt '(+a a b)) '+)
    (equal (varpatt '(!1 a b)) '!)
    (equal (varpatt '(?1 a b)) '?)
    (equal (varpatt '(*1 a b)) '*)
    (equal (varpatt '(!1a a b)) '!)
    (equal (varpatt '(?1a a b)) '?)
    (equal (varpatt '(*1a a b)) '*)
))

;; testing for main function as a whole (COMPLETE)
;; added some of the tests that Adam emailed to the class
(deftest test-match-expr ()
  (check
    (match-expr () ())
    (match-expr '() '())
    (match-expr '(a b) '(a b))
    (match-expr '(a b (c d)) '(a b (c d)))
    (match-expr '(a b (c d) e) '(a b (c d) e))
    (match-expr '(a b (c d) e (f g)) '(a b (c d) e (f g)))

    (equal (match-expr '((? a b)) '(a)) '((? a)))
    (equal (match-expr '((? a b)) '(c)) '())
    (equal (match-expr '((? a b) c) '(c)) '((?)))

    (equal (match-expr '((+ a b)) '(a)) '((+ a)))
    (equal (match-expr '((+ a b)) '(b)) '((+ b)))
    (equal (match-expr '((+ a b)) '(c)) '())

    (equal (match-expr '((* a b)) '(a)) '((* a)))
    (equal (match-expr '((* b a)) '(a)) '((* a)))
    (equal (match-expr '((* a b)) '(c)) '())
    (equal (match-expr '((* a b)) '(b a b a)) '((* b a b a)))

    (not (match-expr 'a 'a))
    (not (match-expr '(a b (c e)) '(a b (c d))))
    (not (match-expr '((! a b)) '(c)))

    (equal (match-expr '(_!) '(a)) '((_! a)))
    (equal (match-expr '(_!) '((a b c))) '((_! (a b c))))
    (equal (match-expr '(a _! b) '(a (a b c) b)) '((_! (a b c))))
    (equal (match-expr '(a (c _! d) b) '(a (c (a b c) d) b))
           '((_! (a b c))))
    (equal (match-expr '(a (c _! d) b) '(a (c e d) b))
                  '((_! e)))
    (equal (match-expr '(_! _!2) '(a b)) '((_! a) (_!2 b)))
    (equal (match-expr '(_! _!) '(a b)) '((_! b)))

    (equal (match-expr '(_?) '(a)) '((_? a)))
    (equal (match-expr '(_?) ()) '((_?)))
    (equal (match-expr '(_?) '((a b c))) '((_? (a b c))))
    (equal (match-expr '(a _? b) '(a (a b c) b)) '((_? (a b c))))
    (equal (match-expr '(a (c _? d) b) '(a (c (a b c) d) b))
                  '((_? (a b c))))
    (equal (match-expr '(a (c _? d) b) '(a (c d) b))
                  '((_?)))
    (equal (match-expr '(a (c _? d) b) '(a (c e d) b))
                  '((_? e)))
    (equal (match-expr '(_? _?2) '(a b)) '((_? a) (_?2 b)))
    (equal (match-expr '(_? _?) '(a b)) '((_? b)))
    (equal (match-expr '(a _? b) '(a b)) '((_?)))

    (equal (match-expr '(_+) '(a)) '((_+ a)))
    (equal (match-expr '(_+) '(a b)) '((_+ a b)))
    (equal (match-expr '(q _+) '(q a b)) '((_+ a b)))
    (equal (match-expr '(_+ g) '(a b g)) '((_+ a b)))
    (equal (match-expr '(q _+ g) '(q a b g)) '((_+ a b)))
    (equal (match-expr '(q _+ _+ g) '(q a b g)) '((_+ b)))
    (equal (match-expr '(_+ _+ab12) '(q a b g)) '((_+ q) (_+ab12 a b g)))
    (equal (match-expr '(q _+ _+ab12 g) '(q a b g)) '((_+ a) (_+ab12 b)))

    (equal (match-expr '(_*) '(a)) '((_* a)))
    (equal (match-expr '(_*) '(a b)) '((_* a b)))
    (equal (match-expr '(_*) ()) '((_*)))
    (equal (match-expr '(_*) '((a b c))) '((_* (a b c))))
    (equal (match-expr '(a _* b) '(a b)) '((_*)))
    (equal (match-expr '(_* _*ab12) '(a b c)) '((_*) (_*ab12 a b c)))

    (equal (match-expr '(_! _* _+ _?) '(a b b b))
                  '((_! a) (_*) (_+ b) (_? b)))

    (equal (match-expr '((! a b)) '(a)) '((! a)))
    (equal (match-expr '(_! (! _!) (!1 _!)) '(a b c)) '((! b) (!1 c) (_! c)))

    (equal (match-expr '((+ a b)) '(b a c a)) '())		;; FIX THIS!
    (equal (match-expr '((* a b)) '(b a c a)) '())		;; FIX THIS!

))

(deftest test-all ()
  (combine-results
    (test-_varpatt)
    (test-varpatt)
    (test-match-expr)
))

