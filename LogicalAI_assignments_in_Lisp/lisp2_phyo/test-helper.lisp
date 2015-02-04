; Package taken from Peter Seibel's "Practical Common Lisp"
; URL: http://www.gigamonkeys.com/book/
; Thanks to classmate Tim for telling me about this resource.

(defun newline ()
	(format t "~%"))

(defun evaluate (l)
	(apply (car l) (cdr l)))

; Tests whether a function produces the expected out and pretty prints.
; func:		A list containing the function and a list of its arguments
; result: 	The expected output
(defun testOld (func result)
	(format t 
		(if (equal (evaluate func) result)
			"Pass"
			"Fail"))
	(format t "~T~S == ~S~%" func result)
	(if (not (equal (evaluate func) result))
		(format t "~T~T~T~T~T~S~%" (evaluate func))))


(defun test (func expect)
	(let ((result (evaluate func)))
		(format t 
			(if (equal result expect)
				"Pass"
				"Fail"))
		(format t "~T~S == ~S~%" func expect)
		(if (not (equal result expect))
			(format t "~T~T~T~T~T~S~%" result))))


