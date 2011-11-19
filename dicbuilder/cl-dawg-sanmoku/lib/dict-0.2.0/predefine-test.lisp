(in-package :dict-0.2.0)

(locally
 (declare #.*muffle-note*
          #.*fastest*)
 (define-test eq #+SBCL sb-impl::eq-hash #-SBCL sxhash eq)
 (define-test eql #+SBCL sb-impl::eql-hash #-SBCL sxhash eql)
 (define-test equal #+SBCL sb-impl::equal-hash #-SBCL sxhash equal)
 (define-test equalp #+SBCL sb-impl::equalp-hash #-SBCL sxhash equalp))
