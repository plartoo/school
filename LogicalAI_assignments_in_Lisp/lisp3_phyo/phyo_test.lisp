; Side note: Youtube channel of Maru, the cat:
; http://www.youtube.com/user/mugumogu?feature=results_main
; and site of Boo, the dog:
; http://www.boothedog.net/

;;;; Test 1
;; Try out abstract rules
(load "phyo_src.lisp")
(store-and-activate-rule '(/ (R1 _!A _!B) (/ (R2 _!B _!C) (R3 _!C))))
(store-and-activate-fact '(R1 Boo Maru))
(store-and-activate-fact '(R1 Boo Catnip))
; EXPECTS/DEDUCES (/ (R2 Maru _!C) (R3 _!C)) and (/ (R2 CATNIP _!C) (R3 _!C))
; check hashes
(print-hash-table *facts*)
(print-hash-table *rules*)
; ============ comment out from here on if you want to test the above ===========


;;;; Test 2
;; Try out more specific rules/facts
(load "phyo_src.lisp") ; reload so that stuff from old tests are flushed out

; store and activate facts/rules while checking the contents
; of the respective hash tables along the way.
(store-and-activate-fact '(owns Mugumogu Maru))

; check hashes
(print-hash-table *facts*)
(print-hash-table *rules*)

; here comes another rule
(store-and-activate-rule '(/ (owns _!y Maru) (loves Maru _!y)))
; EXPECTS/DEDUCES (loves Maru Mugumogu) in STDOUT (terminal)
; ============ comment out from here on if you want to test the above ===========


; check hashes
(print-hash-table *facts*)
(print-hash-table *rules*)

; add new fact
(store-and-activate-fact '(owns Phyo Maru)) ; actually, this is my wish
; EXPECTS/DEDUCES (loves Maru Phyo) in STDOUT 
; ============ comment out from here on if you want to test the above ===========

; try out one more rule
(store-and-activate-rule '(/ (owns _!y _!x) (loves _!x _!y)))

; when hash is printed out, the newly stored rule should be
; associated with "owns"
(print-hash-table *facts*)
(print-hash-table *rules*)
; ============ comment out from here on if you want to test the above ===========


;;;; Test 3
;; Try adding more rules/facts and see what this system can deduce.
;; In this particular example, I will try to deduce as many fact as
;; possible about number's properties from given facts.
;; We will hopefully deduce some facts like the primality of some numbers.
(load "phyo_src.lisp")
(progn
	(store-and-activate-fact '(number one))
	(store-and-activate-fact '(number two))
	(store-and-activate-fact '(number three))
	(store-and-activate-fact '(number four))
	(store-and-activate-fact '(number five))
	(store-and-activate-fact '(number six))
	(store-and-activate-fact '(number seven))
	(store-and-activate-fact '(number eight))
	(store-and-activate-fact '(number nine))
	(store-and-activate-fact '(number ten))

	(store-and-activate-fact '(after one two))
	(store-and-activate-fact '(after two three))
	(store-and-activate-fact '(after three four))
	(store-and-activate-fact '(after four five))
	(store-and-activate-fact '(after five six))
	(store-and-activate-fact '(after six seven))
	(store-and-activate-fact '(after seven eight))
	(store-and-activate-fact '(after eight nine))
	(store-and-activate-fact '(after nine ten))

	(store-and-activate-fact '(divide two four))
	(store-and-activate-fact '(divide two six))
	(store-and-activate-fact '(divide two eight))
	(store-and-activate-fact '(divide two ten))
	(store-and-activate-fact '(divide three six))
	(store-and-activate-fact '(divide three nine))
	(store-and-activate-fact '(divide four eight))
	(store-and-activate-fact '(divide five ten))

	(store-and-activate-fact '(cant-divide two three))
	(store-and-activate-fact '(cant-divide two five))
	(store-and-activate-fact '(cant-divide two seven))
	(store-and-activate-fact '(cant-divide two nine))


	(store-and-activate-fact '(cant-divide three five))
	(store-and-activate-fact '(cant-divide three seven))
	(store-and-activate-fact '(cant-divide three eight))
	(store-and-activate-fact '(cant-divide three ten))
	(store-and-activate-fact '(cant-divide four five))
	(store-and-activate-fact '(cant-divide four six))
	(store-and-activate-fact '(cant-divide four seven))
	(store-and-activate-fact '(cant-divide four nine))
	(store-and-activate-fact '(cant-divide four ten))
	(store-and-activate-fact '(cant-divide five six))
	(store-and-activate-fact '(cant-divide five seven))
	(store-and-activate-fact '(cant-divide five eight))
	(store-and-activate-fact '(cant-divide five nine)))

	; a number is identical to itself
	(store-and-activate-rule `(/ (number _!x) (identical _!x _!x)))

	; if a number is "after", it implies '<' relationship
	(store-and-activate-rule `(/ (after _!x _!y) (less-than _!x _!y)))

	; "after" => cant divide with anything less than that number
	(store-and-activate-rule `(/ (after (!x ~ one) _!y) (cant-divide !x _!y)))

	; if a < b and b < c, then a < c
	(store-and-activate-rule `(/ (less-than _!x _!y) (/ (less-than _!y _!z) (less-than _!x _!z))))

	; rule for evens
	(store-and-activate-rule `(/ (divide two _!x) (even _!x)))
	(store-and-activate-rule `(/ (divide (!x ~ one) _!y) (/ (less-than !x _!y) (not-prime _!y))))

	; rules for prime (not including 2)
	(store-and-activate-rule `(/ (prime (!x ~ two)) (odd !x)))
	(store-and-activate-rule `(/ (min-factor _!x _!y) (/ (identical _!x _!y) (prime _!y))))

	; one can divide any number
	(store-and-activate-rule `(/ (number _!x) (divide one _!x)))

	; a number can divide itself
	(store-and-activate-rule `(/ (number _!x) (divide _!x _!x)))

	; if a divides b, and b divides c, then a divides c
	(store-and-activate-rule `(/ (divide _!x _!y) (/ (divide _!y _!z) (divide _!x _!z))))

	; The smallest factor of x other than one is at least two
	(store-and-activate-rule `(/ (number _!x) (min-factor two _!x)))

