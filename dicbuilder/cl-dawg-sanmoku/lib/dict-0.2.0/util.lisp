(in-package :dict-0.2.0)

(defmacro with-gensyms (symbols &body body)
  `(let ,(mapcar (lambda (s) `(,s (gensym))) symbols)
     ,@body))
