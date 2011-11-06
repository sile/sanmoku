(in-package :asdf)

(defsystem sanmoku
  :name "sanmku"
  :version "0.0.1"
  :author "Takeru Ohta"
  :description ""
  :depends-on (:dawg)
  :serial t
  
  :components ((:file "package")
               (:file "util")
               (:file "sanmoku")))
