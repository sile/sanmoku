(when (> (length sb-ext:*posix-argv*) 2)
  (format *error-output* "Usage: sbcl --script make-build-dic-command.lisp [ouput-dir]~%")
  (sb-ext:quit :unix-status 1))

(require :asdf)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun load-local-system (package &optional (package-directory #P"./"))
    (let #.`((asdf:*central-registry* (directory package-directory))
             ;; or #+ASDF2
             ,@(when #.#1=(find-symbol "*DEFAULT-SOURCE-REGISTRIES*" :asdf)
                     `((,#1# nil))))
         (asdf:load-system package))))

(load-local-system :dict-0.2.0)
(load-local-system :dawg)
(load-local-system :sanmoku)

;(require :dawg)
;(asdf:load-system :sanmoku)

(sanmoku.util:defmain main (source-textdic-dir output-bindic-dir &optional (textdic-charset :euc-jp))
  "Usage: sanmoku-build-dic <source-textdic-dir> <output-bindic-dir> [textdic-charset]"
  (let ((sanmoku:*text-dictionary-charset* (intern (string-upcase textdic-charset) :keyword)))
    (sanmoku:build-dic source-textdic-dir output-bindic-dir)))

(let ((path (merge-pathnames
             #P"sanmoku-build-dic"
             (probe-file (or (second sb-ext:*posix-argv*)
                             *default-pathname-defaults*)))))
  (sb-ext:save-lisp-and-die path :toplevel #'main :executable t))
