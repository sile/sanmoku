(in-package :asdf)

(defsystem dict-0.2.0
  :name "dict"
  :version "0.2.0"
  :author "Takeru Ohta"
  :description "A hash table"
  
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "functor")
               (:file "node-allocator")
               (:file "dict")
               (:file "predefine-test")))
