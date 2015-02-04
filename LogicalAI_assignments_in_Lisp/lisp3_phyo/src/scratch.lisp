; sep-3
; made convergence checks in apply-rules and apply-rule more robust
; added trace options to apply-rules and apply-rule
;
; aug-23
; Bug in restricted-seq matching:  ! * + ? will all match arbitrary length
; tree sequences, when supplied with negative arguments and no positive arguments.
; this is in contrast to the desired semantics, where the length of the matchable
; tree sequences is contrained (e.g, exactly, between, at least, at most)

; Need to extend handling of ^ operator to pumped application.
; E.g, ^^ ^^^ ^^^^ ... 

; I may need to compute some "factored" form of bindings.
; Otherwise, if (<> P1 P2) (T1 T2)
; if P1 can match T1 in K ways
; and P2 can match T2 in M ways
; then there are K * M consistent sets of bindings for the overall pattern
; (assuming no sticky variables shared across P1 and P2)
;
; It appears to me that in order to properly match sticky variables, 
; TTT will need to be able to enumerate the set of all bindings consistent
; between a pattern and a tree sequence.   
; Computing this set will support inference.
; When interested only in match/fail of a pattern against a tree-sequence, 
; without sticky variables, all-bindings is not necessary. 
; When interested only in match/fail of a pattern against a tree-sequence
; which contains sticky variables, it may be more efficient to lazily
; compute all-bindings. 
; 
; ~~~~~~~~ Open Question ~~~~~~~~~~~
; Should TTT bind variables in a canonical order? 
; (E.g, bottom-up, left-to-right)
; If so, then it is unnecessary to specify arguments for all but 
; the deepest, left-most instance of a sticky variable. 
; 
; If not, then one could meaningfully specify arguments for each
; instance of a sticky variable, recompute matching each time, 
; and check that the sticky variable matched in a later step.
; This approach would be less efficient, as in circumstances where
; one instance of a sticky variable has only a few (or a single) 
; option for binding, but the subsequent occurence has several... 
; Any subsequent matching involving those occurences which do not match
; the other instance will be wasted effort. 
;
; Of course, if the one with options is matched first, then we will 
; waste much effort regardless. 
;
; The current semantics is to bind variables in a canonical order. 

; NEXT: continue getting restricted-seq.lisp to work with all-bindings



; aug-22
; The pattern (_* (^* (/ (!. SKOLEM?) !)) _* (!. = (! NAME?)) _*)
; against tree ((FRIEND0.SK FRIEND.N) (FRIEND0.SK PERTAIN-TO HE.PRO)
;               (FRIEND1.SK = FRIEND0.SK) (FRIEND1.SK = REGGI.NAME)
;               ((SET-OF BEN.NAME FRIEND1.SK) AT.P (Q THE.DET PARK.N)))

; may only attempt to bind ^* to (FRIEND1.SK = FRIEND0.SK) with !. bound to FRIEND0.SK
; but should also attempt !. bound to FRIEND1.SK 
; Moreover, it appears that the second instance of !. is not being recognized as a sticky op.
; 
; The simpler pattern (_* (_* (!. SKOLEM?) _*) _* (_![0] !. = (! NAME?)) _*)
; is correctly built with the second !. recognized as an instance of a previously bound
; sticky variable; however, the match still fails. 
; 
; The pattern (_* ((!. SKOLEM?) _*) _* (_![0] !. = (! NAME?)) _*) successfully 
; matches *lf-3*.  
; 
; The pattern (_* ((/ (!. SKOLEM?) !) _*) _* (_![0] !. = (! NAME?)) _*) also 
; successfully matches *lf-3*, and performs the correct transduction. 
	     



; The patterns (_* (^@ _* (/ (!. SKOLEM?) !)) _* (_![0] !. = (! NAME?)) _*)
; and          (_* (^* (/ (!. SKOLEM?) !)) _* (_![0] !. = (! NAME?)) _*)
; both fail to match because the subpattern (!. SKOLEM?) is only ever bound 
; to FRIEND0.SK in (FRIEND1.SK = FRIEND0.SK).
; This is because any pattern is only tested once against any tree, 
; all possible different bindings are not tried. Typically, this would not affect
; the overall match, but when sticky variables are present, the particular bindings 
; of operators matters.  Particular bindings may also matter when they appear
; in transduction operations.
; 
; To modify this might require substantial thought and code revision.
; An additional benefit might be increased utility with regard to inference.

; Alternatievly, I could implement the "all bindings" just for sticky variables.

; The only obvious benefit to pattern matching as unifiction would be that
; the unbound sticky variables could be matched first without arguments, i.e., 
; we could specify the arguments at any location instead of only the first in dfs order.
; HOWEVER that would introduce new ambiguity when the sticky operators appear
; at the head of lists.
; 
; Instead of requiring the idiom of prepending _![0] before a sticky operator
; to ensure it is matched as a stuck-pattern instead of a new restricted operator, 
; we could just declare that the first instance in dfs order of a sticky variable
; is the definining instance, and that all subsequent instances are matched as
; stuck patterns. -- this applies to oeprators requiring arguments, but what
; of the unrestricted operators? What about sticky variables where the defining
; instance could be chosen among a set? 
; Eg., what does the pattern (* (!. A) (!. B)) mean? 
; What about "if it's bound, then match as stuck with arguments making up the rest
; of a general pattern, otherwise, match as usual".  -- I like this. 
; *** clarify details and implement towards this tomorrow.
; 
; 
; * recognizing sticky variables and dispatching differently depending on boundedness
; * searching for any matching bindings, not just pattern/tree combinations
; * returning lists of all possible resulting bindings (or enumerating lazily)
; * returning lists of all possible (single) rule applications (or enumerating lazily)



(defun example (lf)
  (let* ((lf (apply-rule *separate-conjuncts* (list lf)))
	 (lf (apply-rule *skolemize-restricted-definite* lf))
	 (lf (apply-rule *separate-surface-conjuncts* lf :shallow t))
	 





; aug-21
;There appears to be a bug in TTT where the transduction operator 
;/ is not being bound when it appears embedded within either of 
;the vertical operators ^* or ^@.
;
;
;This matters when converting skolems to names, because skolems may
;appear nested arbitrarly deep within an expression, we have to use
;transductions such as: 
;
; (_* ((!. skolem?) = (! name?)) _* (^* (/ !. !)) _*)
;
;As opposed to a simpler pattern such as: 
; (/ (_* ((!. skolem?) = (! name?)) _*1 (_*2 !. _*3) _*4)
;    (_* (!. = !) _*1 (_*2 ! _*3) _*4)
;
;
;(apply-rule '(^* (/ X Y)) 'X)   - fixed
;The Lisp error is that "Non structure argument nil passed to do-transduction". 
;
;(apply-rule '(^@ (/ X Y)) 'X)
;The Lisp error is "Non structure argument (#<TREE: ...>) passed to do-transduction."
;
;The offending calls to do-transduction are at lines 63 and 85 of transductions.lisp, 
;appearing in apply-rules and apply-rule respectively.
;
;
;
;(match-encap '(^* (! X)) 'X)  - ! not bound
;(match-encap '(^@ (! X)) 'X)  - ! bound correctly
;
;There are two problems attempting to match a pattern such as
;(!. (!. X Y)) as a list of two identical objects.
;a)  the syntax without the sticky variable would be ambiguous
;    it would be read as (! (! X Y)) which matches a single thing
;b)  TTT patterns are matched depth first... 
;    so the workaround (_![0] !. (!. X Y)) doesn't work either, 
;    as TTT complains about matching an unbound sticky variable
;    without arguments. -- perhaps a unification matcher would be 
;    better? 

;I'm trying to use TTT to strip away the speech act U0 as well as the
;embedding by TELL.v, to Skolemize both indefinites and definites, 
;separate top-level conjunctions, and in the end make certain
;equality substitutions (replacing skolem constants by name that
;they are equated to, and ultimately deleting self-identities).

(defparameter *lf*
  '(EXISTS U0 (U0 SAME-TIME NOW0)
    ((SPEAKER TELL.V HEARER
	  (THAT
	   (THE.DET Y
		    (THE.DET Z 
			     ((Z FRIEND.N) AND (Z PERTAIN-TO HE.PRO))
			     ((Y = Z) AND (Y = REGGI.NAME)))
		    ((SET-OF BEN.NAME Y) AT.P (:Q THE.DET PARK.N)))))
     [**] U0)))



;(
; (the.det 
;  y
;  ;;(the.det z (...) (...)) becomes
;  (((FRIEND0.SK FRIEND.N) AND (FRIEND0.SK PERTAIN-TO HE.PRO))
;   AND
;   ((Y = FRIEND0.SK) AND (Y = REGGI.NAME)))
;   
;  ((SET-OF BEN.NAME Y) AT.P (:Q THE.DET PARK.N))
;
; )
;)
;
;
;(
; (((FRIEND0.SK FREIND.N) AND (FRIEND0.SK PERTAIN-TO HE.PRO))
;  AND
;  ((FRIEND1.SK = FRIEND0.SK) AND (FRIEND1.SK = REGGI.NAME)))
; AND
; ((SET-OF BEN.NAME FRIEND1.SK) AT.P (:Q THE.DET PARK.N)))
;
;
;((FRIEND2.SK FRIEND.N) AND (FRIEND2.SK PERTAIN-TO HE.PRO))
;((FRIEND1.SK = FRIEND2.SK) AND (FRIEND1.SK = REGGI.NAME))
;((SET OF BEN.NAME Y) AT.P (:Q THE.DET PARK.N))

 ; Separate conjuncts directly embedded by SAY.V, or other attitudes;
 ; we assume that we have "hidden" the episodic operators by wrapping
 ; [..] around them. Assume the formula is one in a list of formulas.
 ; [For the time being, strip off the speech-act wrapper -LKS]
(defparameter *separate-conjuncts*
  '(/ (_* ((! exists exist some some.det) _! _?
	   ((_! (! say.v tell.v) _? (that _!1)) (! [**] [*] [@]) _!2)) _*1)
      (_* _!1 _*1)))
 ; I don't see how this separates anything.  It does strip off the
 ; "exists u0 (u0 same-time now) ((speaker tell.v hearer ...))
 ; bit though.


 ; Skolemize an unrestricted existential (here the Skolem just extends
 ; the variable with an integer, dot, and SK)
(defparameter *skolemize-unrestricted-existential*
  '(/ (_* ((! exists exist some some.det) _! _!1) _*1)
      (_* (subst! (new-skolem! _!) _! _!1) _*1)))
 ; Works for simple case ((exists u0 (u0 same-time now))).


 ; Skolemize a restricted existential; use a conjunction in order to make
 ; the same substitution in both conjuncts, but another rule breaks it up.
 ; Here the Skolem is based on the type (if any) specified in the restrictor,
 ; which will be extracted from the second (optional) argument of 'new-skolem!'
(defparameter *skolemize-restricted-existential*
  '(/ (_* ((! exists exist some some.det) _! _!1 _!2) _*1)
      (_* (subst! (new-skolem! _!1) _! (_!1 and _!2)) _*1)))
 ; Seems OK

  
 ; Skolemize a restricted definite
 ; (We could add an assertion to the effect that the truth of the restrictor
 ; of "the" is presupposed for the entity at issue; but this is not yet vital))
(defparameter *skolemize-restricted-definite*
  '(/ (_* ((! the the.det) _! _!1 _!2) _*1)
      (_* (subst! (new-skolem! _!1) _! (_!1 and _!2)) _*1)))
 ; Seems OK.


 ; Separate surface conjuncts
(defparameter *separate-surface-conjuncts*
  '(/ (_* (_! (! and and.cc) _+) _*1)
      (_* _! _+ _*1)))

;; goal is:
;(
; (FRIEND0.SK FRIEND.N) 
; (FRIEND0.SK PERTAIN-TO HE.PRO)
; (FRIEND1.SK = FRIEND0.SK)
; (FRIEND1.SK = REGGI.NAME)
; ((SET-OF BEN.NAME FRIEND1.SK) AT (:Q THE.DET PARK.N))
;)
; 
; (((FRIEND0.SK FREIND.N) AND (FRIEND0.SK PERTAIN-TO HE.PRO))
;  AND
;  ((FRIEND1.SK = FRIEND0.SK) AND (FRIEND1.SK = REGGI.NAME)))
; AND
; ((SET-OF BEN.NAME FRIEND1.SK) AT.P (:Q THE.DET PARK.N)))
;
;
;
;
; For skolems equated to names, first make sure the skolem is on the LHS;
; then substitute the name for all occurrences of the skolem; then eliminate
; any self-identities (but not the skolem = name identity):
(defparameter *skolems-to-names*
  (list
   ;;move skolem to lhs
   '(/ (_* ((! name?) = (!1 skolem?)) _*1)
       (_* (!1 = !) _*1)) 
   
;   '(/ (_* ((!. skolem?) = (! name?)) _*1 (_*2 (!. skolem?) _*3) _*4); rightward instance
;       (_* (!. = !) _*1 (_*2 ! _*3) _*4))

   '(_* ((!. skolem?) = (! name?)) _* (^* (/ !. !)) _*)  ;rightward instance

;   '(/ (_* (_*2 (!. skolem?) _*3) _*1 (!.  = (! name?)) _*4); leftward instance
;       (_* (_*2 ! _*3) _*1 (!. = !) _*4))

;    BROKEN BUT SHOULD WORK
;   '(_* (^* (/ (!. skolem?) !)) _* (!. = (! name?)) _*) ;leftward instance
   
   ; insufficiently general, but works for this example.
   ; see comments at top of file
   '(_* ((/ (!. SKOLEM?) !) _*) _* (_![0] !. = (! NAME?)) _*) ; leftward instance


   '(/ (_* (_!. = _!.) _*1) (_* _*1))))


(defun skolem? (x)
;~~~~~~~~~~~~~~~~
; Check if x a skolem constant, i.e., with extension .SK
  (and (symbolp x)
       (equal (last (coerce (string x) 'list) 3) '(#\. #\S #\K))))

(defun name? (x)
; ~~~~~~~~~~~~~
; Check if x is a name, i.e., with extension .NAME
  (and (symbolp x)
       (equal (last (coerce (string x) 'list) 5) '(#\. #\N #\A #\M #\E))))

(defun subst! (x y z) (subst x y z))

(defun coord? (expr)
;~~~~~~~~~~~~~~~~~~~
; N.B.: Assume "neither ... nor" is expressed in terms of "not"
;       and "or" or "and" (not (... or ...), or else 
;       ((not ...) and (not ...))).
  (and expr (symbolp expr)
       (or (member expr '(and and-then as-well-as but or or-else either-or
                          => implies <=> equiv would-imply because
                          so-that))
           (eq (extension-type expr) 'cc))))

(defun extension-type (atm); July 16/12; tested
;~~~~~~~~~~~~~~~~~~~~~~~~~~
; Return the material after the last dot as an upper-case atom 
; (or nil if there is no dot); e.g., 'V, 'N, 'NAME, etc.
;
 (prog (chars ext)
       (if (not (symbolp atm)) (return nil))
       (setq chars (reverse (coerce (string atm) 'list)))
       (if (not (member #\. chars)) (return nil))
       (if (char= (car chars) #\.) (return nil)); ends in dot (unexpected)
       ; Peel off characters following final '.':
       (dotimes (i (- (length chars) (length (member #\. chars))))
          (push (pop chars) ext))
       (return (intern (string-upcase (coerce ext 'string))))
 )); end of extension-type

(defun new-skolem! (expr)
;~~~~~~~~~~~~~~~~~~~~~~~
; create a symbol ending in ".SK" based on the input expr (a restrictor
; expression from a quantified wff, or a variable if there is no
; restrictor -- e.g., an episode variable). For a restrictor expression
; we try to extract a type (nominal) to use as basis for the Skolem
; constant; e.g., restrictor (x ((attr big.a) dog.n)) might yield a
; Skolem constant DOG3.SK.
  (prog (pred)
    (if (symbolp expr)
        (return (intern (concatenate 'string
                          (string (gensym (string expr))) ".SK"))))
    ; 'expr' is an expression (if it is a non-symbol atom, an error
    ; will be -- and should be -- generated)
    (setq pred (find-likely-type-pred expr))
    (setq pred (string-minus-dot-extension pred))
    (return (intern (format nil "~S.SK" (gentemp (string pred)))))
 )); end of new-skolem!



(defun find-likely-type-pred (expr); tested
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; 'expr' is expected to be a quantifier restrictor such as 
; (x ((attr big.a) dog.n)). Extract a likely type predicate (usually
; a nominal with extension '.n') -- here, 'dog.n' -- and return it.
; We also allow for coordinated wffs. By default (and in particular
; if expr has 3 elements), return 'entity'.
;
; WARNING: This is intended for wffs *after* keyword reduction.
;   It is also not a model of good style.
;
  (prog ((n (length expr)) (poss-pred (second expr)) poss-preds)
        (if (and (= n 2) (symbolp poss-pred); a common case
                 (or (string-ends-in (string poss-pred) ".N")
                      ; avoid returning predicates like 'happy.a',
                      ; but we might lack an '.n', as in 'entity'
                      (lacks-dot-extension poss-pred)))
            (return poss-pred))
        ; non-symbolic "predicate" (probably an error)
        (if (and (= n 2) (atom poss-pred))
            (return 'entity))
        ; complex predicate?
        (if (= n 2) (go dig))

        ; Is expr a coordinated wff?
        (when (coord? poss-pred)
              (setq poss-preds
                    (mapcar #'find-likely-type-pred
                            (cons (car expr) (cddr expr))))
              ; is there a <noun>.n among the poss-preds?
              (setq poss-pred
                    (find-if #'(lambda (x)
                                 (and (symbolp x)
                                      (string-ends-in (string x) ".N")))
                              poss-preds))
              (if poss-pred (return poss-pred))
              ; failing the above, look for a likely nominal other than
              ; 'entity' (we don't have to check for unwanted dot-extensions,
              ; because the recursive call avoided these):
              (setq poss-pred
                    (find-if #'(lambda (x) (not (eq x 'entity)))
                             poss-preds))
              (if poss-pred (return poss-pred) (return 'entity)))

        ; n > 2 but expr is not a coordination (e.g., a relation):
        (return 'entity)

    dig ; n=2 but poss-pred is complex.
        (setq poss-pred (car (last poss-pred))); dig for embedded pred
        (if (and (symbolp poss-pred); may be embedded type
                 (or (string-ends-in (string poss-pred) ".N")
                     ; avoid returning predicates like 'happy.a',
                     ; but we might lack an '.n', as in 'entity'
                     (lacks-dot-extension poss-pred)))
            (return poss-pred))
        ; pred does not end in '.N' or is complex
        (if (atom poss-pred); probably not a type
            (return 'entity)
            ; more deeply nested, so back to 'dig'
            (go dig))
  )); end of find-likely-type-pred 


(defun string-minus-dot-extension (sym)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Create a string from a symbol but omitting the dot-extension, if any.
 (prog* ((str (string sym)) (chars (coerce str 'list)) n chars2)
        (setq chars2 (member #\. chars)); final sublist starting with dot
        (if (null chars2) (return str))
        (setq n (length chars2))
        (return (coerce (butlast chars n) 'string))
 )); end of string-minus-dot-extension 


(defun lacks-dot-extension (sym)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 (not (member #\. (coerce (string sym) 'list))))
(defun string-ends-in (string suffix)
  (and (>= (length string)
	   (length suffix))
       (equal (subseq string (- (length string)
				(length suffix)))
	      suffix)))


(setq *lf-1* (apply-rule *separate-conjuncts* (list *lf*)))
(setq *lf-2* (apply-rule *skolemize-restricted-definite* *lf-1*))
(setq *lf-3* (apply-rule *separate-surface-conjuncts* *lf-2* :shallow t))
(setq *lf-4* (apply-rules *skolems-to-names* *lf-3*))
