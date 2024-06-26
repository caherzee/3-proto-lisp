(in-package 3-proto-lisp)

;; Primitives

(defun set-primitive (name lambda)
  (declare (special *global*))
  (set-binding *global* (wrap name) (wrap lambda)))

(defun set-primitive-abnormal (name lambda)
  (declare (special *global*))
  (declare (special *cl-false*))
  (set-binding *global* (wrap name) (make-instance 'abnormal-closure :body lambda :name name :cl-value lambda)))

;; arithmetic
(set-primitive '+ (function +))
(set-primitive '- (function -))
(set-primitive '* (function *))
(set-primitive '/ (function /))
(set-primitive '= (function proto-lisp=)) 
(set-primitive '< (lambda (cl-val1 cl-val2) (cl->cl-bool (< cl-val1 cl-val2))))
(set-primitive '> (lambda (cl-val1 cl-val2) (cl->cl-bool (> cl-val1 cl-val2))))
;; printing
(set-primitive 'print (function print))
;; pair
(set-primitive 'pcar (function pcar))
(set-primitive 'pcdr (function pcdr))
(set-primitive 'pcons (function pcons))  
;; rails and sequences
(set-primitive 'rcons (function rcons))
(set-primitive 'scons (function scons)) 
(set-primitive 'prep (function prep)) 
(set-primitive 'length (function length))  
(set-primitive 'nth (function nth))
(set-primitive 'tail (function tail))
(set-primitive 'empty (lambda (rail) (cl->cl-bool (empty-p rail))))
;; closure
(set-primitive 'body (function body))
(set-primitive 'pattern (function argument-pattern))
(set-primitive 'environment (function lexical-environment))
(set-primitive 'ccons (function ccons))
(set-primitive 'abnormal (lambda (closure) (cl->cl-bool (and (cl-closure-p closure) (abnormal-p (closure closure))))))
;; atoms
(set-primitive 'acons (function gensym))
;; typing
(set-primitive 'type (function external-type))
(set-primitive 'primitive (lambda (closure) (cl->cl-bool (primitive-p closure)))) 
(set-primitive 'reflective (lambda (closure) (cl->cl-bool (reflective-p closure))))
;; up & down
(set-primitive 'up (function wrap))
(set-primitive 'down (function unwrap))
;; abnormal primitives, i.e. primitives whose args are not normalized
(set-primitive-abnormal 'set (lambda (&rest args) (declare (ignore args)) (error "Trying to call set"))) 
(set-primitive-abnormal 'lambda (lambda (&rest args) (declare (ignore args)) (error "Trying to call lambda")))
(set-primitive-abnormal 'lambda-reflect (lambda (&rest args) (declare (ignore args)) (error "Trying to call lambda-reflect")))
(set-primitive-abnormal 'if (lambda (&rest args) (declare (ignore args)) (error "Trying to call if")))
(set-primitive-abnormal 'apply (lambda (&rest args) (declare (ignore args)) (error "Trying to call apply")))
(set-primitive-abnormal 'apply-abnormal (lambda (&rest args) (declare (ignore args)) (error "Trying to call apply-abnormal")))
;; global environment
(set-primitive 'global *global*)
(set-primitive 'binding (function binding))
(set-primitive 'bind (function bind))
;; normalize
(set-primitive 'normalize (function normalize))
(set-primitive 'reduce (function reduce))
;; load stl
(load-proto-lisp-file (concatenate 'string *3-proto-lisp-location* "standard-library.lisp"))
