(defpackage sanmoku.util
  (:use :common-lisp)
  (:export write-int
           with-time
           package-alias
           each-line
           it
           defmain))

(defpackage sanmoku
  (:use :common-lisp :sanmoku.util)
  (:export build-dic
           *text-dictionary-charset*
           done))
(in-package :sanmoku)

(deftype octet () '(unsigned-byte 8))
(defvar *text-dictionary-charset* :euc-jp)

(defvar *fastest* '(optimize (speed 3) (safety 0) (debug 0)))

