(asdf:defsystem #:yapack-test
  :depends-on (#:lisp-unit
               #:yapack)
  :serial t
  :components
  ((:module :t
            :components
            ((:file "pack"     :depends-on ("packages"))
             (:file "packages")))))
