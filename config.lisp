(defpackage 3-proto-lisp
  (:shadow boolean atom length prep first rest nth body reduce)
  (:use cl :clos)
  (:export read-normalize-print normalize-from-string)
  (:nicknames 3pl))

(in-package 3-proto-lisp)

(defparameter *3-proto-lisp-location* "/3-proto-lisp/")
