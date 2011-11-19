(defpackage dict-0.2.0
  (:use :common-lisp)
  (:shadow :common-lisp get set remove count map)
  (:export dict
           test
           
           make
           count
           get
           remove
           clear
           each
           map

           generate-test
           define-test
           undef-test
           find-test
           list-all-tests
           test-name))
(in-package :dict-0.2.0)

(deftype positive-fixnum () '(integer 0 #.most-positive-fixnum))
(deftype array-index () '(unsigned-byte 29))
(deftype hashcode () '(unsigned-byte 30))
(deftype hashcode-width () '(mod 31))
(deftype set-fn () '(function (t t dict) t))
(deftype get-fn () '(function (t dict t) (values t boolean)))
(deftype rem-fn () '(function (t dict) boolean))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *fastest* '(optimize (speed 3) (safety 0) (debug 0)))
  (defparameter *interface* '(optimize (speed 3) (safety 2) (debug 1)))
  (defparameter *normal* '(optimize (speed 1) (safety 3) (debug 2)))
  (defparameter *muffle-note* #-SBCL '(ignore)
                              #+SBCL '(sb-ext:muffle-conditions sb-ext:compiler-note))
  (defconstant +HASHCODE_WIDTH+ 30)
  (defconstant +MAX_HASHCODE+ #.(1- (ash 1 30))))

(defparameter *functor-repository* (make-hash-table :test #'eq))
