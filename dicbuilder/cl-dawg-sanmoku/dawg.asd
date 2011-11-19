(in-package :asdf)

(defsystem dawg
  :name "dawg"
  :author "Takeru Ohta"
  :version "0.3.0"

  :serial t
  :components ((:file "load-hashmap")
               (:file "global")
               (:file "byte-order")
               (:file "octet-stream")
               (:file "bintrie-builder")
               (:file "double-array-node-allocator")
               (:file "double-array-buffered-output")
               (:file "double-array-builder")
               (:file "dawg")))
