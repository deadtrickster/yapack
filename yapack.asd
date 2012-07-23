(asdf:defsystem #:yapack
  :description "Interpret arrays as packed binary data."
  :license "MIT"
  :depends-on (#:alexandria
               #:babel
               #:babel-streams
               #:cffi
               #:ieee-floats)
  :serial t
  :components
  ((:file "packages")
   (:module :src
            :components
            ((:file "base-types" :depends-on ("pack"))
             (:file "pack")))))
