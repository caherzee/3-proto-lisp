;; Tests for verifying the workings of the ProtoLisp interpreter

(in-package 3-proto-lisp)

;;;;;;;;;;;; KERNEL PRIMITIVES ;;;;;;;;;


;;;;;;;;;;
;; TYPE ;;
;;;;;;;;;;

(let ((result
       (normalize-from-string "(type 3)")))
  (assert
      (and (atom-p result)
           (eql (unwrap result) 'number))
      ()
    "Type test nr 1 (type 3) failed"))

(let ((result
       (normalize-from-string "(type ?3)")))
  (assert
      (and (atom-p result)
           (eql (unwrap result) 'numeral))
      ()
    "Type test nr 2 (type ?3) failed"))

;(let ((result
 ;      (normalize-from-string "(type $T)")))
  ;(assert
   ;   (and (atom-p result)
    ;       (eql (uwrap result) 'truth-value))
     ; ()
    ;"Type test nr 3 (type $T) failed"))

;(let ((result
 ;      (normalize-from-string "(type ?$F)")))
  ;(format t "result ~s ~%" (unwrap result))
  ;(assert
   ;   (and (atom-p result)
    ;       (eql (unwrap result) (class-name (class-of *base-true*))))
     ; ()
    ;"Type test nr 4 (type ?$F) failed"))

(let ((result
       (normalize-from-string "(type [1 2 3])")))
  (assert
      (and (atom-p result)
           (eql (unwrap result) 'sequence))
      ()
    "Type test nr 5 (type [1 2 3]) failed"))

(let ((result
       (normalize-from-string "(type ?[1 2 3])")))
  (assert
      (and (atom-p result)
           (eql (unwrap result) 'rail))
      ()
    "Type test nr 6 (type ?[1 2 3]) failed"))

(let ((result
       (normalize-from-string "(type +)")))
  (assert
      (and (atom-p result)
           (eql (unwrap result) 'function))
      ()
    "Type test nr 7 (type +) failed"))

(let ((result
       (normalize-from-string "(type (up +))")))
  (assert
      (and (atom-p result)
           (eql (unwrap result) 'closure))
      ()
    "Type test nr 8 (type (up +)) failed"))

(let ((result
       (normalize-from-string "(type ?(+ 1 2))")))
  (assert
      (and (atom-p result)
           (eql (unwrap result) 'pair))
      ()
    "Type test nr 9 (type ?(+ 1 2)) failed"))

(let ((result
       (normalize-from-string "(type ?a)")))
  (assert
      (and (atom-p result)
           (eql (unwrap result) 'atom))
      ()
    "Type test nr 10 (type ?a) failed"))

(let ((result
       (normalize-from-string "(type ??3)")))
  (assert
      (and (atom-p result)
           (eql (unwrap result) 'handle)) 
      ()
    "Type test nr 11 (type ??3) failed"))

(let ((result
       (normalize-from-string "(type ??????3)")))
  (assert
      (and (atom-p result)
           (eql (unwrap result) 'handle)) 
      ()
    "Type test nr 12 (type ????????3) failed"))

;;;;;;;;;;;;;;;;;;;; 
;; PROCEDURE-TYPE ;;
;;;;;;;;;;;;;;;;;;;;

;(let ((result
 ;      (normalize-from-string "(procedure-type (ccons ?simple global ?[x] ?x))")))
  ;(assert
   ;   (and (atom-p result)
    ;       (eql (unwrap result) 'simple))
     ; ()
    ;"Procedure-type test nr 1 (procedure-type (ccons ?x ?[] ?y ?z)) failed"))

;(let ((result
 ;      (normalize-from-string "(procedure-type (up +))")))
  ;(assert
   ;   (and (atom-p result)
    ;       (eql (unwrap result) *primitive-tag*))
     ; ()
    ;"Procedure-type test nr 2 (procedure-type (up +)) failed"))

;;;;;;;;;;;
;; PCONS ;;
;;;;;;;;;;;

(let ((result
       (normalize-from-string "(pcons ?a ?b)")))
  (assert 
      (pair-p (unwrap result))
      ()
    "Pcons test nr 1 (pcons ?a ?b) failed"))

(let ((result
       (normalize-from-string "(pcons ?+ ?[2 3])")))
  (assert 
      (pair-p (unwrap result))
      ()
    "Pcons test nr 2 (pcons ?+ ?[2 3]) failed"))

(let ((result
       (normalize-from-string "(normalize (pcons ?+ ?[1 2]) global id)")))
    (assert 
        (and
         (numeral-p (unwrap result))
         (= 3 (unwrap (unwrap result))))
      ()
    "Pcons test nr 2 (pcons ?+ ?[2 3]) failed"))

;;;;;;;;;
;; CAR ;;
;;;;;;;;;

(let ((result
       (normalize-from-string "(pcar ?(a b))")))
  (assert 
      (and
       (atom-p (unwrap result))
       (eql (unwrap (unwrap result)) 'a))
      ()
    "Car test nr 1 (pcar ?(a b)) failed"))

(let ((result
       (normalize-from-string "(pcar ?(2 3))")))
  (assert 
      (and
       (numeral-p (unwrap result))
       (eql (unwrap (unwrap result)) 2))
      ()
    "Car test nr 2 (car ?(2 3)) failed"))

(let ((result
       (normalize-from-string "(pcar ?(+ 2 3))")))
  (assert 
      (and
       (atom-p (unwrap result))
       (eql (unwrap (unwrap result)) '+))
      ()
    "Car test nr 3 (car ?(+ 2 3)) failed"))

;;;;;;;;;
;; CDR ;;
;;;;;;;;;

(let ((result
       (normalize-from-string "(pcdr ?(a b))")))
  (assert 
       (rail-p (unwrap result))
      ()
    "Cdr test nr 1 (pcdr ?(a b)) failed"))

(let ((result
       (normalize-from-string "(pcdr ?(2 3))")))
  (assert 
       (rail-p (unwrap result))
      ()
    "Cdr test nr 2 (pcdr ?(2 3)) failed"))

(let ((result
       (normalize-from-string "(pcdr ?(+ 2 3))")))
  (assert 
       (rail-p (unwrap result))
      ()
    "Cdr test nr 3 (pcdr ?(+ 2 3)) failed"))

;;;;;;;;;;;
;; CCONS ;;
;;;;;;;;;;;

(let ((result
       (normalize-from-string "(ccons ?simple global ?[x] ?x)")))
  (assert
      (closure-p (unwrap result))
      ()
    "Ccons test nr 1 (ccons ?bla global ?[x] ?x) failed"))

(let ((result
       (normalize-from-string "((down (ccons ?simple global ?[x] ?(+ x 1))) 10)")))
  (assert
      (and (numeral-p result)
           (= (unwrap result) 11))
      ()
    "Ccons test nr 2 ((down (ccons ?simple global ?[x] ?(+ x 1))) 10) failed"))

;;;;;;;;;;;;;
;; PATTERN ;;
;;;;;;;;;;;;;

(let ((result
       (normalize-from-string "(pattern (ccons ?simple global ?[x] ?(+ x 1)))")))
  (assert
      (and (rail-p (unwrap result))
           (= (length (unwrap result)) 1))
      ()
    "Pattern test nr 1 (pattern (ccons ?simple global ?[x] ?(+ x 1))) failed"))

(let ((result
       (normalize-from-string "(pattern (up (lambda (x) (+ x 1))))")))
  (assert
      (and (rail-p (unwrap result))
           (= (length (unwrap result)) 1))
      ()
    "Pattern test nr 2 (pattern (up (lambda (x) (+ x 1))))  failed"))

;;;;;;;;;;
;; BODY ;;
;;;;;;;;;;

(let ((result
       (normalize-from-string "(body (ccons ?simple global ?[x] ?(+ x 1)))")))
  (assert
      (pair-p (unwrap result))     
      ()
    "Body test nr 1 (body (ccons ?simple global ?[x] ?(+ x 1))) failed"))

(let ((result
       (normalize-from-string "(body (up (lambda (b a) (pcons b a))))")))
  (assert
      (pair-p (unwrap result))
      ()
    "Body test nr 2 (body (up (lambda  (b a) (pcons b a)))) failed"))

;;;;;;;;;;;;;;;;;
;; ENVIRONMENT ;;
;;;;;;;;;;;;;;;;;

(let ((result
       (normalize-from-string "(environment (ccons ?simple global ?[x] ?(+ x 1)))")))
  (assert
      (environment-p (unwrap result))     
      ()
    "Body test nr 1 (environment (ccons ?simple global ?[x] ?(+ x 1))) failed"))

;;;;;;;;;;;
;; ACONS ;;
;;;;;;;;;;;

(format t "HHHHHHHHHHHHH ~%")

;;;;;;;;;;;
;; RCONS ;;
;;;;;;;;;;;

(let ((result
       (normalize-from-string "(rcons ?1 ?2 ?3)")))
  (assert
      (rail-p (unwrap result))     
      ()
    "Rcons test nr 1 (rcons ?1 ?2 ?3) failed"))

;;;;;;;;;;
;; PREP ;;
;;;;;;;;;;

(let ((result
       (normalize-from-string "(prep ?a ?[b c])")))
  (assert
      (and 
       (rail-p (unwrap result))
       (= (length (unwrap result)) 3)
       )
      ()
    "Prep test nr 1 (prep ?a ?[b c]) failed"))


;;;;;;;;;;;;
;; LENGTH ;;
;;;;;;;;;;;;

(let ((result
       (normalize-from-string "(length ?[a b c])")))
  (assert
      (and 
       (numeral-p result)
       (= (unwrap result) 3)
       )
      ()
    "Length test nr 1 (length ?[a b c]) failed"))

(let ((result
       (normalize-from-string "(length (rcons))")))
  (assert
      (and 
       (numeral-p result)
       (= (unwrap result) 0)
       )
      ()
    "Length test nr 2 (length (rcons)) failed"))

;;;;;;;;;
;; NTH ;;
;;;;;;;;;

(let ((result
       (normalize-from-string "(nth 1 ?[1 2 3])")))
  (assert
      (and 
       (numeral-p (unwrap result))
       (= (unwrap (unwrap result)) 2)
       )
      ()
    "Nth test nr 1 (nth 1 ?[1 2 3]) failed"))

;;;;;;;;;;
;; TAIL ;;
;;;;;;;;;;

(let ((result
       (normalize-from-string "(tail 1 (pcdr ?(rcons ?A ?B ?C)))")))
  (assert
      (and 
       (rail-p (unwrap result))
       (= (length (unwrap result)) 2)
       (atom-p (unwrap (pcar (unwrap result))))
       (atom-p (unwrap (pcar (pcdr (unwrap result)))))
       )
      ()
    "Tail test nr 1 (tail 1 (cdr ?(rcons ?A ?B ?C))) failed"))

;;;;;;;;;;;
;; EMPTY ;;
;;;;;;;;;;;

(let ((result
       (normalize-from-string "(empty ?[])")))
  (assert
      (and 
       (boolean-p result)
       (cl-bool (unwrap result))) 
      ()
    "Empty test nr 1 (empty ?[]) failed"))

(let ((result
       (normalize-from-string "(empty (rcons))")))
  (assert
      (and 
       (boolean-p result)
       (cl-bool (unwrap result)))
      ()
    "Empty test nr 1 (empty (rcons)) failed"))

;;;;;;;;
;; UP ;;
;;;;;;;;

(let ((result
       (normalize-from-string "(up 5)")))
  ;; is the result a handled numeral 5?
  (assert
      (and 
       (numeral-p (unwrap result))
       (= 5 (unwrap (unwrap result))))
      ()
    "Up test nr 1 (up 5) failed"))

(let ((result
       (normalize-from-string "(up (+ 2 3))")))
  ;; is the result a handled numeral 5?
  (assert
      (and 
       (numeral-p (unwrap result))
       (= 5 (unwrap (unwrap result))))
      ()
    "Up test nr 2 (up (+ 2 5)) failed"))

(let ((result
       (normalize-from-string "(up (lambda (x) x))")))
  ;; is the result a handled closure?
  (assert
      (closure-p (unwrap result))
      ()
    "Up test nr 3 (up (lambda (x) x)) failed"))

;;;;;;;;;;
;; DOWN ;;
;;;;;;;;;;

(let ((result
       (normalize-from-string "(down ?4)")))
  ;; is the result numeral 4?
  (assert
      (numeral-p result)
      ()
    "Down test nr 1 (down ?4) failed"))

(let ((result
       (normalize-from-string "(down (nth 1 ?[10 20 30]))")))
  ;; is the result numeral 20?
  (assert
      (and 
       (numeral-p result)
       (= 20 (unwrap result)))
      ()
    "Down test nr 2 (down (nth 1 ?[10 20 30])) failed"))

(let ((result
       (normalize-from-string "(down (up $T))")))
  ;; is the result boolean T?
  (assert
      (and 
       (boolean-p result)
       (cl-bool (unwrap result)))
      ()
    "Down test nr 3 (down (up $T)) failed"))

;;;;;;;
;; = ;;
;;;;;;;

(let ((result
       (normalize-from-string "(= 3 (+ 1 2))")))
  ;; is the result boolean T?
  (assert
      (and 
       (boolean-p result)
       (cl-bool (unwrap result)))
      ()
    "= test nr 1 (= 3 (+ 1 2)) failed"))

(let ((result
       (normalize-from-string "(= 5 ?5)")))
  ;; is the result boolean F?
  (assert
      (and 
       (boolean-p result)
       (not (cl-bool (unwrap result))))
      ()
    "= test nr 2 (= 5 ?5) failed"))

(let ((result
       (normalize-from-string "(= ?5 ?5)")))
  ;; is the result boolean T?
  (assert
      (and 
       (boolean-p result)
       (cl-bool (unwrap result)))
      ()
    "= test nr 3 (= ?5 ?5) failed"))

(let ((result
       (normalize-from-string "(= [10 20] [10 20])")))
  ;; is the result boolean T?
  (assert
      (and 
       (boolean-p result)
       (cl-bool (unwrap result)))
      ()
    "= test nr 4 (= [10 20] [10 20]) failed"))

(let ((result
       (normalize-from-string "(= ?[10 20] ?[10 20])")))
  ;; is the result boolean F?
  (assert
      (and 
       (boolean-p result)
       (not (cl-bool (unwrap result))))
      ()
    "= test nr 5 (= ?[10 20] ?[10 20]) failed"))

(let ((result
       (normalize-from-string "(= [?10 ?20] ?[10 20])")))
  ;; is the result boolean F?
  (assert
      (and 
       (boolean-p result)
       (not (cl-bool (unwrap result))))
      ()
    "= test nr 6 (= [?10 ?20] ?[10 20]) failed"))

;;;;;;;;;;;
;; SCONS ;;
;;;;;;;;;;;

(let ((result
       (normalize-from-string "(scons 1 2 3)")))
  (assert
      (and 
       (rail-p result)
       (= 3 (length result)))
      ()
    "Scons test nr 1 (scons 1 2 3) failed"))

(let ((result
       (normalize-from-string "(scons ?a (+ 2 2))")))
  (assert
      (and 
       (rail-p result)
       (= 2 (length result))
       (numeral-p (nth 1 result))
       (= 4 (unwrap (nth 1 result))))
      ()
    "Scons test nr 2 (scons ?a (+ 2 2)) failed"))

(format t "IIIIIIIII ~%")

;;;;;;;;;;;
;; APPLY ;;
;;;;;;;;;;;

(let ((result
       (normalize-from-string "(apply + ?[1 2])")))
  (assert
      (and 
       (numeral-p result)
       (= 3 (unwrap result)))
      ()
    "Apply test nr 1 (apply + ?[1 2]) failed"))

;(let ((result
 ;      (normalize-from-string "(apply lambda ?[ [x y] (+ x y)])")))
  ;(assert
   ;    (closure-p result)
    ;  ()
    ;"Apply test nr 2 (apply lambda ?[ [x y] (+ x y)]) failed"))

;; changed def of apply, below doesnt work
;; (apply lambda [simple (x y) (+ x y)])
;; (apply + [1 2]) 

;;;;;;;;;;;; KERNEL NON PRIMITIVES ;;;;;;;;;

;;;;;;;;;;
;; UNIT ;;
;;;;;;;;;;

;;;;;;;;;;;;
;; DOUBLE ;;
;;;;;;;;;;;;

;;;;;;;;;;
;; REST ;;
;;;;;;;;;;

;;;;;;;;;;;;
;; MEMBER ;;
;;;;;;;;;;;;

(let ((result
       (normalize-from-string "(member ?2 ?[1 2 3])")))
  ;; is the result boolean T?
  (assert
      (and 
       (boolean-p result)
       (cl-bool (unwrap result)))
      ()
    "member test nr 1 (member ?2 ?[1 2 3]) failed"))

(let ((result
       (normalize-from-string "(member ?[] ?[2 3 [] 0])")))
  ;; is the result boolean F?
  (assert
      (and 
       (boolean-p result)
       (not (cl-bool (unwrap result))))
      ()
    "member test nr 2 (member ?[] ?[2 3 [] 0]) failed"))

(let ((result
       (normalize-from-string "(member 1 [2 3 [] 1])")))
  ;; is the result boolean T?
  (assert
      (and 
       (boolean-p result)
       (cl-bool (unwrap result)))
      ()
    "member test nr 3 (member 1 [2 3 [] 1]) failed"))

(let ((result
       (normalize-from-string "(member 1 [2 3 []])")))
  ;; is the result boolean T?
  (assert
      (and 
       (boolean-p result)
       (not (cl-bool (unwrap result))))
      ()
    "member test nr 4 (member 1 [2 3 []]) failed"))

;;;;;;;;;;;;;;
;; INTERNAL ;;
;;;;;;;;;;;;;;

(let ((result
       (normalize-from-string "(external 123)")))
  ;; is the result boolean T?
  (assert
      (and 
       (boolean-p result)
       (cl-bool (unwrap result)))
      ()
    "internal test nr 1 (external 123) failed"))

(let ((result
       (normalize-from-string "(internal (+ 1 2))")))
  (assert
      (and 
       (boolean-p result)
       (not (cl-bool (unwrap result))))
      ()
    "internal test nr 2 (internal (+ 1 2)) failed"))

(let ((result
       (normalize-from-string "(internal ?+)")))
  ;; is the result boolean T?
  (assert
      (and 
       (boolean-p result)
       (cl-bool (unwrap result)))
      ()
    "internal test nr 3 (internal ?+) failed"))

(let ((result
       (normalize-from-string "(internal (up +))")))
  ;; is the result boolean T?
  (assert
      (and 
       (boolean-p result)
       (cl-bool (unwrap result)))
      ()
    "internal test nr 4 (internal (up +)) failed"))

;;;;;;;;;;;;;;;;;;;;;;;;
;; VECTOR-CONSTRUCTOR ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(let ((result
       (normalize-from-string "(vector-constructor ?[])")))
  (assert
      (closure-p result)
      ()
    "vector-constructor test nr 1 (vector-constructor ?[]) failed"))

(let ((result
       (normalize-from-string "((vector-constructor ?[]))")))
  (assert
      (rail-p (unwrap result))
      ()
    "vector-constructor test nr 2 ((vector-constructor ?[])) failed"))

(let ((result
       (normalize-from-string "(vector-constructor 100)")))
  (assert
      (closure-p result)
      ()
    "vector-constructor test nr 3 (vector-constructor 100) failed"))

(let ((result
       (normalize-from-string "((vector-constructor 100))")))
  (assert
      (rail-p result)
      ()
    "vector-constructor test nr 4 ((vector-constructor 100)) failed"))

;;;;;;;;;;;;
;; LAMBDA ;;
;;;;;;;;;;;;

(let ((result
       (normalize-from-string "((lambda (x) ((lambda (y) (+ x y)) 7)) 8)")))
  (assert
      (and 
       (numeral-p result)
       (= 15 (unwrap result)))
      ()
    "lambda test nr 1 ((lambda (x) ((lambda (y) (+ x y)) 7)) 8) failed"))

(format t "BBBBBBBBBBBB~%")

;;;;;;;;;
;; MAP ;;
;;;;;;;;; 

(let ((result
       (normalize-from-string "(map (lambda (nr) (+ 1 nr)) [1 2 3])")))
  (assert
      (and 
       (rail-p result)
       (= 3 (length result))
       (= 2 (unwrap (first result))))
      ()
    "map test nr 1 (map (lambda (nr) (+ 1 nr)) [1 2 3]) failed"))

(let ((result
       (normalize-from-string "(map up ?[1 A $T])")))
  (assert
      (and 
       (rail-p (unwrap result))
       (= 3 (length (unwrap result))))
      ()
    "map test nr 1 (map up ?[1 A $T]) failed"))

;;;;;;;;;;;;;;;;
;; REFLECTIVE ;;
;;;;;;;;;;;;;;;;

(let ((result
       (normalize-from-string "(reflective (up +))")))
  (assert
      (not (cl-bool (unwrap result)))
      ()
    "result test nr 1 (reflective (up +)) failed"))

;;;;;;;;;;;;;;;;
;; DE-REFLECT ;;
;;;;;;;;;;;;;;;;

(let ((result
       (normalize-from-string "(de-reflect (ccons ?simple global ?[y] ?y))")))
  (assert
      (closure-p (unwrap result))
           ;(primitive-p (unwrap result)))
      ()
    "de-reflect test nr 1 (de-reflect (ccons ?simple global ?[y] ?y)) failed"))

;;;;;;;;;;;;;;;;;;;;;;;;
;; ATOM, NUMERAL, ... ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(let ((result
       (normalize-from-string "(atom ?t)")))
  (assert
      (cl-bool (unwrap result))
      ()
    "atom test nr 1 (atom ?t)) failed"))

(let ((result
       (normalize-from-string "(atom ?4)")))
  (assert
      (not (cl-bool (unwrap result)))
      ()
    "atom test nr 2 (atom ?4)) failed"))

(let ((result
       (normalize-from-string "(numeral ?4)")))
  (assert
      (cl-bool (unwrap result))
      ()
    "atom test nr 3 (numeral ?4)) failed"))

(let ((result
       (normalize-from-string "(numeral 4)")))
  (assert
      (not (cl-bool (unwrap result)))
      ()
    "atom test nr 4 (numeral 4)) failed"))

(let ((result
       (normalize-from-string "(number 4)")))
  (assert
      (cl-bool (unwrap result))
      ()
    "atom test nr 5 (number 4)) failed"))

;;;;;;;;;;
;; TEST ;;
;;;;;;;;;;

(let ((result
       (normalize-from-string "(simple global ?[x] ?x)")))
  (assert
      (closure-p result)
      ()
    "simple test nr 1 (simple global ?[x] ?x) failed"))

(let ((result
       (normalize-from-string "((simple global ?[x] ?x) 7)")))
  (assert
      (= 7 (unwrap result))
      ()
    "simple test nr 2 ((simple global ?[x] ?x) 7) failed"))

;;;;;;;;;;;;;
;; BINDING ;;
;;;;;;;;;;;;;

(let ((result
       (normalize-from-string "(binding ?+ global)")))
  (assert
      (primitive-p (unwrap result))
      ()
    "binding test nr 1 (binding ?+ global) failed"))

;;;;;;;;;;
;; BIND ;;
;;;;;;;;;;

;;;;;;;;;;;;;;;
;; PRIMITIVE ;;
;;;;;;;;;;;;;;;

(let ((result
       (normalize-from-string "(primitive (up +))")))
  (assert
      (cl-bool (unwrap result))
      ()
    "binding test nr 1 (primitive (up +)) failed"))

(let ((result
       (normalize-from-string "(primitive (up member))")))
  (assert
      (not (cl-bool (unwrap result)))
      ()
    "primitive test nr 2 (primitive (up member)) failed"))

;;;;;;;;;;;;;;;;;;;;
;; LAMBDA-REFLECT ;;
;;;;;;;;;;;;;;;;;;;;

;(let ((result
 ;      (normalize-from-string "((lambda-reflect (dct cont z) (cont (+ 1 (down (1st z))))) 1)")))
  ;(assert
   ;   (and (numeral-p result)
    ;       (= 2 (unwrap result)))
     ; ()
    ;"lambda test nr 1 ((lambda-reflect (dct cont z) (cont (+ 1 (down (1st z)))) 1) failed"))

;;;;;;;;;;;;;;;
;; NORMALIZE ;;
;;;;;;;;;;;;;;;

;(normalize ?(lambda (x y) (+ x y)) global (lambda (x) x))

(let ((result
       (normalize-from-string "(normalize ?1 global (lambda (arg) arg))")))
  (assert
      (and (numeral-p (unwrap result))
           (= 1 (unwrap (unwrap result))))
      ()
    "normalize test nr 1 (normalize ?1 global (lambda (arg) arg)) failed"))

(let ((result
       (normalize-from-string "(normalize ?[1 2 3] global (lambda (arg) arg))")))
  (assert
      (rail-p (unwrap result))
      ()
    "normalize test nr 2 (normalize ?[1 2 3] global (lambda (arg) arg)) failed"))

(let ((result
       (normalize-from-string "(normalize ?(+ 1 2) global (lambda (arg) arg))")))
  (assert
      (and 
       (numeral-p (unwrap result))
       (= 3 (unwrap (unwrap result))))
      ()
    "normalize test nr 3 (normalize ?(+ 1 2) global (lambda (arg) arg)) failed"))

(let ((result1 (normalize-from-string "(set *f* (lambda (arg) (+ 1 arg)))"))
      (result
       (normalize-from-string "(normalize ?(*f* 2) global (lambda (arg) arg))")))
  (declare (ignore result1))
  (assert
      (and 
       (numeral-p (unwrap result))
       (= 3 (unwrap (unwrap result))))
      ()
    "normalize test nr 3 (normalize ?(f 2) global (lambda (arg) arg)) failed"))

(format t "ooooooooo~%")

(let ((result
       (normalize-from-string "(normalize (1st ?[(+ 1 2)]) global (lambda (arg) arg))")))
  (assert
      (and 
       (numeral-p (unwrap result))
       (= 3 (unwrap (unwrap result))))
      ()
    "normalize test nr 4  (normalize (1st ?[(+ 1 2)]) global (lambda (arg) arg)) failed"))

(let ((result
       (normalize-from-string "(normalize ?((lambda (x y z) (- (+ x y) z)) 1 2 3) global (lambda (arg) arg))")))
  (assert
      (and 
       (numeral-p (unwrap result))
       (= 0 (unwrap (unwrap result))))
      ()
    "normalize test nr 5 ((lambda (x y z) (- (+ x y) z)) 1 2 3) global (lambda (arg) arg)) failed"))

;;;;;;;;;;;
;; QUOTE ;;
;;;;;;;;;;;

(let ((result
       (normalize-from-string "(quote 2)")))
  (assert
      (and 
       (numeral-p (unwrap result))
       (= 2 (unwrap (unwrap result))))
      ()
    "quote test nr 1 (quote 2) failed"))

(let ((result
       (normalize-from-string "(quote (+ 2 2))")))
  (assert
      (pair-p (unwrap result))
      ()
    "quote test nr 2 (quote (+ 2 2)) failed"))

;(let ((result
 ;      (normalize-from-string "(normalize ?a global quote)")))
  ;(assert
   ;   (and
    ;   (pair-p (unwrap result))
     ;  (= 3 (length (unwrap (unwrap result)))))
      ;()
   ; "quote test nr 3 (normalise ?a global quote) failed"))

(format t "CCCCCCCCCC~%")

;;;;;;;;;;
;; COND ;;
;;;;;;;;;;

(let ((result
       (normalize-from-string "(cond [(= 4 4) (+ 1 2)])")))
  (assert 
      (and
       (numeral-p result)
       (= 3 (unwrap result)))
     ()
    "cond test nr 1 (cond [(= 4 4) (+ 1 2)]) failed")) 

(let ((result
       (normalize-from-string "(cond [(= 7 4) (+ 1 2)] [(= 7 7) (+ 9 8)])")))
  (assert 
      (and
       (numeral-p result)
       (= 17 (unwrap result)))
      ()
   "cond test nr 2 (cond [(= 4 4) (+ 1 2)]  [(= 7 7) (+ 9 8)]) failed")) 

;;;;;;;;;
;; LET ;;
;;;;;;;;;

(let ((result
       (normalize-from-string "(let [[x 42]] (let [[y 7]] (+ x y)))")))
  (assert 
      (and
       (numeral-p result)
       (= 49 (unwrap result)))
      ()
   "let test nr 1 (let [[x 42]] (let [[y 7]] (+ x y))) failed")) 

;;;;;;;;;;;;;;;;;
;; NASTY TESTS ;;
;;;;;;;;;;;;;;;;;

(normalize-from-string "(set nasty-foo (lambda (x) (+ 1 x)))")
(normalize-from-string "(set nasty-fee (lambda-reflect (x) (- (1st x) 1)))")
(let ((result1
       (normalize-from-string "(nasty-foo 100)"))
      (result2
       (normalize-from-string "(nasty-foo (block (set nasty-foo nasty-fee) 100))")))
  (assert
      (and
       (numeral-p result1)
       (numeral-p result2)
       (= (unwrap result1) 101)
       (= (unwrap result2) 101))
       ()
       "nasty test nr 1 failed"))
;; call (nasty-foo 100) again will result in an error

(let ((result1
       (normalize-from-string "(mc-normalize ?1 global id)"))
      (result2
       (normalize-from-string "(mc-normalize ?1 global quote)"))
      (result3
       (normalize-from-string "(mc-normalize ?+ global id)"))
      (result4
       (normalize-from-string "(mc-normalize ?+ global quote)"))
      (result5
       (normalize-from-string "(mc-normalize ?(+ 1 2) global id)"))
      (result6
       (normalize-from-string "(mc-normalize ?(+ 1 2) global quote)"))
      (result7
       (normalize-from-string "(mc-normalize-rail ?[] global id)"))
      (result8
       (normalize-from-string "(mc-normalize-rail ?[] global quote)"))
      (result9
       (normalize-from-string "(mc-normalize-rail ?[1] global id)"))
      (result10
       (normalize-from-string "(mc-normalize-rail ?[1] global quote)"))
      )
  (assert
      (and
       (numeral-p (unwrap result1))
       (atom-p (unwrap result2))
       (closure-p (unwrap result3))
       (pair-p (unwrap result4))
       (numeral-p (unwrap result5))
       (pair-p (unwrap result6))
       (rail-p (unwrap result7))
       (pair-p (unwrap result8))
       (rail-p (unwrap result9))
       (pair-p (unwrap result10)))
       ()
       "nasty test nr 2 failed"))

;;;;;;;;;;;;;;;;;
;; OTHER TESTS ;;
;;;;;;;;;;;;;;;;;

(let ((result
       (normalize-from-string "(reflective-factorial 4)")))
  (assert
      (and 
       (numeral-p result)
       (= 24 (unwrap result)))
      ()
    "reflective factorial test nr 1 (reflective-factorial 4) global (lambda (arg) arg)) failed"))

(let ((result
       (normalize-from-string "((lambda (arg) (my-if (= 5 5) 1 arg)) 100)")))
  (assert
      (and
       (numeral-p result)
       (= 1 (unwrap result)))
      ()
    "my-if test nr 1 ((lambda (arg) (my-if (= 5 5) 1 arg)) 100) failed"))

(let ((result
       (normalize-from-string "(for-each print [1 2 3])")))
  (declare (ignore result))
  (assert
      T
      ()
    "my-if test nr 1 ((lambda (arg) (my-if (= 5 5) 1 arg)) 100) failed"))

(let ((result
       (normalize-from-string "(search number [1 2 3])")))
  (declare (ignore result))
  (assert
      T
      ()
    "search test nr 1 (search number [1 2 3]) 100) failed"))
