;; Implementation kernel non primitives

(set unit 
     (lambda (vector) (if (not (empty vector)) (empty (rest vector)) $F)))  

(set rest
     (lambda (rail) (tail 1 rail)))

(set 1st
     (lambda (rail) (nth 0 rail))) 

(set 2nd
     (lambda (rail) (nth 1 rail))) 

(set 3rd
     (lambda (rail) (nth 2 rail))) 

(set 4th
     (lambda (rail) (nth 3 rail))) 

(set 5th
     (lambda (rail) (nth 4 rail))) 

(set 6th
     (lambda (rail) (nth 5 rail))) 

(set member
     (lambda (element rail)
       (if (empty rail)
           $F
         (if (= element (1st rail))
             $T
           (member element (rest rail))))))

(set internal
     (lambda (x)
       (member (type x) [(type ?[]) (type ?()) (type ?4) (type ??4) (type (up (= 1 1))) (type (up +))])))

(set external
     (lambda (x)
       (not (internal x))))

(set not 
     (lambda (exp)
       (if exp $F $T)))

(set vector-constructor
     (lambda (template) (if (external template) scons rcons))) 

(set map
     (lambda (function vector)
       (if (empty vector)
           ((vector-constructor vector))
         (prep (function (1st vector))
                   (map function (rest vector))))))

(set de-reflect
     (lambda (closure)
       (ccons ?simple (environment closure)
              (pattern closure)
              (body closure))))

(set atom
     (lambda (structure)
       (= (type structure) (type ?w))))

(set boolean
     (lambda (structure)
       (= (type structure) (type (up $T)))))

(set closure
     (lambda (structure)
       (= (type structure) (type (up +)))))

(set function
     (lambda (structure)
       (= (type structure) (type  +))))

(set handle
     (lambda (structure)
       (= (type structure) (type (up (up 4))))))

(set number
     (lambda (structure)
       (= (type structure) (type 4))))

(set numeral
     (lambda (structure)
       (= (type structure) (type (up 4)))))

(set pair
     (lambda (structure)
       (= (type structure) (type ?(+ 1 2)))))

(set rail
     (lambda (structure)
       (= (type structure) (type (rcons)))))

(set sequence
     (lambda (structure)
       (= (type structure) (type (scons)))))

(set truth-value
     (lambda (structure)
       (= (type structure) (type $T))))

(set simple
     (lambda (env pattern body)
       (down (ccons ?simple env pattern body))))

(set reflect
     (lambda (env1 pattern1 body1)
       (down (ccons ?reflect env1 pattern1 body1))))

(set quote (lambda-reflect (e c x) (c (up (1st x)))))

(set normal
     (lambda (exp)
       (if (rail exp)
           (normal-rail exp)
         (if (not (atom exp))
             (if (not (pair exp))
                 $T
               $F)
           $F))))

(set normal-rail
     (lambda (rail)
       (if (empty rail)
           $T
         (if (normal (1st rail))
             (normal-rail (rest rail))
           $F))))
       
(set mc-normalize 
     (lambda (expression1 env1 continuation1)
       (if (normal expression1)
           (continuation1 expression1)
         (if (atom expression1)
             (continuation1 (binding expression1 env1))
           (if (rail expression1)
              (mc-normalize-rail expression1 env1 continuation1)
             (if (pair expression1)
                 (mc-reduce (pcar expression1) (pcdr expression1) env1 continuation1)
               $F))))))

(set mc-reduce
     (lambda (procedure2 arguments env continuation)
       (mc-normalize procedure2 env 
                  (lambda (procedure!)
                    (if (reflective procedure!)
                        ((down (de-reflect procedure!)) env continuation arguments)
                      (if (abnormal procedure!)
                          (continuation (up (apply-abnormal (down procedure!) env arguments)))
                        (mc-normalize arguments env
                                   (lambda (arguments!)
                                     (if (primitive procedure!)
                                         (continuation (up (apply (down procedure!) arguments!)))
                                       (mc-normalize (body procedure!)
                                                  (bind (environment procedure!) (pattern procedure!) arguments!)
                                                  continuation))))))))))

(set mc-normalize-rail
     (lambda (rail env3 continuation3)
       (if (empty rail)
           (continuation3 (rcons))
         (mc-normalize (1st rail) env3
                    (lambda (first!)
                      (mc-normalize-rail (rest rail) env3 
                                      (lambda (rest!)
                                        (continuation3 (prep first! rest!)))))))))

(set id
     (lambda (arg) arg))

(set reflectify 
     (lambda (fun)
       (reflect (environment (up fun)) (pattern (up fun)) (body (up fun))))) 

(set block-helper
     (lambda (env cont clauses)
       (if (unit clauses)
           (normalize (1st clauses) env cont)
         (normalize (1st clauses) env
                    (lambda (res!) 
                      (block-helper env cont (rest clauses)) )))))

(set block 
     (reflectify block-helper))

(set begin block)

(set cond-helper 
     (lambda (env cont clauses)
         (normalize (1st (1st clauses)) env
                    (lambda (premise!)
                        (if (down premise!)
                            (normalize (2nd (1st clauses)) env cont)
                          (cond-helper env cont (rest clauses)))))))

(set cond (reflectify cond-helper))

(set for-each (lambda (f lis)
                (if (empty lis)
                    $T
                  (block (f (1st lis)) (for-each f (rest lis)) )) ))


; (let [[x 7] [y 9]] (+ x y))
; args = list body
(set let
     (lambda-reflect (env cont args)
                     (mc-reduce (ccons ?simple env (map 1st (1st args)) (2nd args))
                             (map 2nd (1st args))
                             env
                             cont)))

(set while
     (lambda-reflect (env cont exp) 
                     (normalize (pcons ?if (up [ (down (1st exp)) 
                                               (down (pcons ?block (up [(down (2nd exp)) 
                                                                       (down (pcons ?while (up [(down (1st exp)) 
                                                                                               (down (2nd exp)) ]) )) ] )))
                                               ?done
                                               ] ))
                                env
                                cont)))

(set when
     (lambda-reflect (env cont exp)
       (normalize (pcons ?if (up [ (down (1st exp))
                                 (down (2nd exp))
                                 $F ]))
                  env
                  cont)))

(set break
     (lambda-reflect (env cont exp)
       (normalize exp env id)))

(set search
     (lambda-reflect (env cont exp)
       (for-each (lambda (el)
                   (if ((down (binding (1st exp) env)) el)
                       (cont el)
                     $F))
                 (down (normalize (2nd exp) env id)))))

(set reflective-factorial
     (lambda-reflect (env cont exp)
       (normalize (1st exp) env 
                  (lambda (exp!)
                    (if (= exp! ?0)
                        (cont ?1)
                      (cont (up (* (down exp!) (reflective-factorial (- (down exp!) 1))))))))))
    
(set my-if
     (lambda-reflect (environ continu test+consequent+antesequent)
       (normalize (1st test+consequent+antesequent) environ
                  (lambda (p!)
                    (if (down p!)
                        (normalize (2nd test+consequent+antesequent) environ continu)
                      (normalize (3rd test+consequent+antesequent) environ continu))))))

(set print-infix (lambda (e)
                     [
                       (print 0 )
                       (print-infix (down (1st (pcdr e))))
                       (print (down (pcar e)))
                       (for-each print-infix (rest (pcdr e)))
                       (print 0 )
                       ]))



