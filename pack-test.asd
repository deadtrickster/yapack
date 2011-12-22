(asdf:defsystem #:pack-test
  :depends-on (#:lisp-unit
               #:pack)
  :serial t
  :components
  ((:module :t
            :components
            ((:file "pack"     :depends-on ("packages"))
             (:file "packages")))))
