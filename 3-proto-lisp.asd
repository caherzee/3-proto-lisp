(asdf:defsystem #:3-proto-lisp
  :components ((:file "config")
               (:file "structural-field" :depends-on ("config"))
               (:file "internalization" :depends-on ("structural-field"))
               (:file "externalization" :depends-on ("structural-field"))
               (:file "normalization" :depends-on ("structural-field" "internalization"))
               (:file "primitives" :depends-on ("normalization" "repl" "config"))
               (:file "repl" :depends-on ("normalization"))))
  
