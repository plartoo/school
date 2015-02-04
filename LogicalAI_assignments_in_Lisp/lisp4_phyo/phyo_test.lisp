(load "phyo_src.lisp")

;;; TEST 1 - small scale test showing the storage and 
;;; retrieval of keys and rules working correctly
(let 
((f1 `(prime two))
 (f2 `(prime three))
 (f3 `(not prime four))
 (f4 `(prime five))
 (f3 `(not prime six))

 (r1 `(/ (even _!x) (/ (prime _!x) (equal-to _!x two))))
 (r2 `(/ (prime (!x ~ two)) (odd !x)))
 (r3 `(/ (divides _!x _!y) (multiple-of _!y _!x)))
 (r4 `(/ (not prime _!x) (composite _!x)))
)

;; Test "get-fact-keys"
(newline)
(vformat "Testing 'get-fact-keys'")
(my-assert `(get-fact-keys ,f1) `((prime) (prime two)))
(my-assert `(get-fact-keys ,f2) `((prime) (prime three)))
(my-assert `(get-fact-keys ,f3) `((not prime) (not prime four)))

;; Test "get-rule-keys"
(newline)
(vformat "Testing 'get-rule-keys'")
(my-assert `(get-rule-keys ,r1)  `(even))
(my-assert `(get-rule-keys ,r2)  `(prime))
(my-assert `(get-rule-keys ,r3)  `(divides))
(my-assert `(get-rule-keys ,r4)  `(not prime))

(newline)
(vformat "Testing if store-and-activate adds fact/rule properly...")
(my-assert `(known-fact? ,f1) nil)	; for fact f1
(store-and-activate-fact f1)
(my-assert `(known-fact? ,f1) t)	; now it's in the KB
(my-assert `(known-fact? ,f2) nil)	; try again for fact f2
(store-and-activate-fact f2)
(my-assert `(known-fact? ,f2) t)	; it should be in the KB now

(my-assert `(known-rule? ,r1) nil)	; for rule r1
(store-and-activate-rule r1)
(my-assert `(known-rule? ,r1) t)	; now inserted to KB

(vformat "Done with Test 1. Resetting KB....")
(reset-knowledge-base)
(newline)
(newline)
;(exit)
)

;;; TEST 2 - we'll see if our KB can do inferences
;;; about numbers being even, odd or prime

(let
((f1 `(num one))
 (f2 `(num two))
 (f3 `(num three))
 (f4 `(num four))
 (f5 `(num five))
 (f6 `(num six))
 (f7 `(num seven))
 (f8 `(num eight))
 (f9 `(num nine))
 (f10 `(num ten))
 (f11 `(num eleven))
 (f12 `(num twelve))

 (f13 `(after one two))
 (f14 `(after two three))
 (f15 `(after three four))
 (f16 `(after four five))
 (f17 `(after five six))
 (f18 `(after six seven))
 (f19 `(after seven eight))
 (f20 `(after eight nine))
 (f21 `(after nine ten))
 (f22 `(after ten eleven))
 (f23 `(after eleven twelve))

 (f30 `(divides two four))
 (f31 `(divides two six))
 (f32 `(divides two eight))
 (f33 `(divides two ten))
; test to see if two divides twelve. Our KB should make inference about that
 (f34 `(divides three six))
 (f35 `(divides three nine))
 (f36 `(divides four eight))
 (f37 `(divides four twelve))
 (f38 `(divides six twelve))
 (f39 `(divides three twelve))

 (f40 `(not divides two five))
 (f41 `(not divides two three))
 (f42 `(not divides two seven))
 (f43 `(not divides three five))
 (f44 `(not divides three seven))
 (f45 `(not divides four five))
 (f46 `(not divides four seven))
 (f47 `(not divides five seven))
 (f48 `(not divides six seven))
 (f49 `(not divides three nine))
 (f50 `(not divides four nine))
 (f51 `(not divides five nine))
 (f52 `(not divides six nine))

; "after" implies "<" relationship
 (r1 `(/ (after _!x _!y) (less-than _!x _!y)))

; a number is equal to itself
 (r2 `(/ (num _!x) (identical _!x _!x)))

; a number is divisible by itself
 (r3 `(/ (num _!x) (divides _!x _!x)))

; after implies a number does not divides anything "<" that
 (r4 `(/ (after (!x ~ one) _!y) (not divides !x _!y)))

; even numbers are divisible by 2
 (r5 `(/ (divides two _!x) (even _!x)))

; even numbers are multiples of two and are not prime
 (r6 `(/ (divides (!x ~ one) _!y) (/ (less-than !x _!y) (not prime _!y))))

; if 'x' < 'y' and 'y' < 'z' => 'x' < 'z'
 (r7 `(/ (less-than _!x _!y) (/ (less-than _!y _!z) (less-than _!x _!z))))

; if 'x' divides 'y' and 'y' divides 'z' => 'x' divides 'z'
 (r8 `(/ (divides _!x _!y) (/ (divides _!y _!z) (divides _!x _!z))))

; prime numbers are odd, except two
 (r9 `(/ (prime (!x ~ two)) (odd !x)))

; _!y is prime if there are not non-one factors smaller than itself
 (r10 `(/ (smallest-factor _!x _!y) (/ (identical _!x _!y) (prime _!y))))

; One divides everything
 (r11 `(/ (num _!x) (divides one _!x)))

; the smallest minimum factor possible is 'two'
 (r12 `(/ (num _!x) (smallest-factor two _!x)))

; checks all possible factors of 'x' given limit 'z'
 (r13 `(/ (smallest-factor _!x _!z) (/ (not divides _!x _!z)
 (/ (after _!x _!y) (smallest-factor _!y _!z)))))
 )


 (store-and-activate-fact f1)
 (store-and-activate-fact f2)
 (store-and-activate-fact f3)
 (store-and-activate-fact f4)
 (store-and-activate-fact f5)
 (store-and-activate-fact f6)
 (store-and-activate-fact f7)
 (store-and-activate-fact f8)
 (store-and-activate-fact f9)
 (store-and-activate-fact f10)
 (store-and-activate-fact f11)
 (store-and-activate-fact f12)
 (store-and-activate-fact f13)
 (store-and-activate-fact f14)
 (store-and-activate-fact f15)
 (store-and-activate-fact f16)
 (store-and-activate-fact f17)
 (store-and-activate-fact f18)
 (store-and-activate-fact f19)
 (store-and-activate-fact f20)
 (store-and-activate-fact f21)
 (store-and-activate-fact f22)
 (store-and-activate-fact f23)

 (store-and-activate-fact f30)
 (store-and-activate-fact f31)
 (store-and-activate-fact f32)
 (store-and-activate-fact f33)
 (store-and-activate-fact f34)
 (store-and-activate-fact f35)
 (store-and-activate-fact f36)
 (store-and-activate-fact f37)
 (store-and-activate-fact f38)
 (store-and-activate-fact f39)

 (store-and-activate-fact f40)
 (store-and-activate-fact f41)
 (store-and-activate-fact f42)
 (store-and-activate-fact f43)
 (store-and-activate-fact f44)
 (store-and-activate-fact f45)
 (store-and-activate-fact f46)
 (store-and-activate-fact f47)
 (store-and-activate-fact f48)
 (store-and-activate-fact f49)
 (store-and-activate-fact f50)
 (store-and-activate-fact f51)
 (store-and-activate-fact f52)

 (store-and-activate-rule r1)
 (store-and-activate-rule r2)
 (store-and-activate-rule r3)
 (store-and-activate-rule r4)
 (store-and-activate-rule r5)
 (store-and-activate-rule r6)
 (store-and-activate-rule r7)
 (store-and-activate-rule r8)
 (store-and-activate-rule r9)
 (store-and-activate-rule r10)
 (store-and-activate-rule r11)
 (store-and-activate-rule r12)
 (store-and-activate-rule r13)

 ; FINALLY tests of primality, odd/even-ness
 (my-assert `(known-fact? (even four)) t)
 (my-assert `(known-fact? (odd five)) t)
 (my-assert `(known-fact? (even six)) t)
 (my-assert `(known-fact? (odd seven)) t)
 (my-assert `(known-fact? (even eight)) t)
 (my-assert `(known-fact? (even ten)) t) ; and so on as long as we add more facts

 (my-assert `(known-fact? (prime two)) t)
 (my-assert `(known-fact? (even two)) t)
 (my-assert `(known-fact? (odd two)) nil) ; KB knows two is not odd

 (my-assert `(known-fact? (prime three)) t)
 (my-assert `(known-fact? (odd three)) t)
 (my-assert `(known-fact? (even three)) nil)

 (my-assert `(known-fact? (prime four)) nil)
 (my-assert `(known-fact? (prime five)) t)
 (my-assert `(known-fact? (prime seven)) t)
 (my-assert `(known-fact? (prime nine)) nil)
 (my-assert `(known-fact? (prime ten)) nil)
 (my-assert `(known-fact? (prime eleven)) nil)
 (my-assert `(known-fact? (prime twelve)) nil)
)

(vformat "Done with Test 2. Resetting KB....")
(reset-knowledge-base)
(newline)
(newline)
(exit)

