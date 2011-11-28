(when (and (/= (length sb-ext:*posix-argv*) 3)
           (/= (length sb-ext:*posix-argv*) 4))
  (format *error-output* "~&Usage: sbcl --script gen-feature-ex-files.lisp TEXT_DIC_DIR OUTPUT_DIC_DIR [ENCODING]~%")
  (sb-ext:quit))

;;;; parse arguments
(destructuring-bind (in-dir out-dir &optional (encoding :euc-jp))
                    (cdr sb-ext:*posix-argv*)
  (defparameter *text-dic-dir* (probe-file in-dir))
  (defparameter *out-dic-dir* out-dir)
  (setf sb-impl::*default-external-format* encoding))

(ensure-directories-exist *out-dic-dir*)


;;;; define auxiliary functions

;; (split "abc,123,ABC" #\,) => '("abc" "123" "ABC")
(defun split (str delim &key (start 0) (end (length str)))
  (declare (simple-string str)
           (character delim)
           (fixnum start end))
  (when (< start end)
    (let ((p (position delim str :start start :end end)))
      (if p
          (cons (subseq str start p) (split str delim :start (1+ p) :end end))
        (list (subseq str start end))))))

;; write integer N to strean OUT in big-endian order
(defun write-uint (n width out)
  (loop FOR i FROM (1- width) DOWNTO 0
        DO
        (write-byte (ldb (byte 8 (* i 8)) n) out)))

;;;; struct
(defstruct morpheme
  (surface ""  :type string)
  (pos-id 0    :type integer)
  (cost 0      :type integer)
  (baseform "" :type string)
  (reading ""  :type string)
  (pronun ""   :type string)

  (omit-baseform? nil :type boolean)
  (omit-pronun?   nil :type boolean))


;;;; read morphemes

;; delete unused entries
(defun delete-unused-morp (morps)
  (flet ((morp< (a b)
           (if (< (morpheme-pos-id a) (morpheme-pos-id b))
               t
             (if (> (morpheme-pos-id a) (morpheme-pos-id b))
                 nil
               (< (morpheme-cost a) (morpheme-cost b))))))
    ;; delete all A: A is A.pos-id == B.pos-id and A.cost >= B.cost
    (delete-duplicates (sort morps #'morp<) :key #'morpheme-pos-id :from-end t)))

;; (read-morphemes "text-dic-dir/") => '((surface pos-id 
(defun read-morphemes (dir)
  (loop WITH map = (make-hash-table :test #'equal)
        FOR path IN (cons (format nil "~a/unk.def" dir)
                          (directory (format nil "~a/*.csv" dir)))
    DO
    (with-open-file (in path)
      (loop FOR line = (read-line in nil nil)
            WHILE line
        DO
        (destructuring-bind (surface pos-id _2 cost _4 _5 _6 _7 _8 _9 baseform &optional (reading "") (pronun ""))
                            (split line #\,)
          (declare (ignore _2 _4 _5 _6 _7 _8 _9))
          (when (string= reading "") 
            ;; for unk.def
            (setf surface (format nil "~c~a" (code-char 1) surface)
                  baseform ""))
          
          (when (= (length reading) 32)
            ;; for truncated text
            (setf reading ""
                  pronun ""))
          
          (push (make-morpheme :surface surface
                               :pos-id (parse-integer pos-id)
                               :cost (parse-integer cost)
                               :baseform baseform
                               :reading reading
                               :pronun pronun
                               :omit-baseform? (string= surface baseform)
                               :omit-pronun? (string= reading pronun))
                (gethash surface map)))))
    FINALLY
    (maphash (lambda (k ms)
               (setf (gethash k map) (delete-unused-morp ms)))
             map)
    (return map)))

(format *error-output* "~&; read morphemes: ~a~%" *text-dic-dir*)
(defparameter *ms* (read-morphemes *text-dic-dir*))
(format *error-output* "~&; => DONE: ~a valid entries~%" 
        (let ((total 0))
          (maphash (lambda (k v)
                     (declare (ignore k))
                     (incf total (length v)))
                   *ms*)
          total))


;;;; write baseform, reading and pronunciation texts
(format *error-output* "~2&; make baseform set: ~%")
(defparameter *baseforms*
  (let ((map (make-hash-table :test #'equal)))
    (maphash (lambda (k ms)
               (declare (ignore k))
               (dolist (m ms)
                 (unless (morpheme-omit-baseform? m)
                   (setf (gethash (morpheme-baseform m) map) t))))
             *ms*)
    map))
(format *error-output* "~&; => DONE: ~a count ~%" (hash-table-count *baseforms*))

(format *error-output* "~2&; make reading/pronunciation set: ~%")
(defparameter *reading-pronuns*
  (let ((map (make-hash-table :test #'equal)))
    (maphash (lambda (k ms)
               (declare (ignore k))
               (dolist (m ms)
                 (if (morpheme-omit-pronun? m)
                     (setf (gethash (morpheme-reading m) map) t)
                   (setf (gethash (format nil "~a,~a" (morpheme-reading m) (morpheme-pronun m)) map) t))))
             *ms*)
    map))
(format *error-output* "~&; => DONE: ~a count ~%" (hash-table-count *reading-pronuns*))

(format *error-output* "~2&; write feature texts: ~a ~%" (merge-pathnames "feature.text.bin" *out-dic-dir*))
(let ((text 
       (with-output-to-string (out)
         (loop FOR m IN (list *baseforms* *reading-pronuns*)
           DO
           (maphash (lambda (k v)
                      (declare (ignore v))
                      (setf (gethash k m) (file-position out))
                      (write-string k out))
                    m)))))

  (with-open-file (out (merge-pathnames "feature.text.bin" *out-dic-dir*)
                       :direction :output
                       :if-exists :supersede
                       :element-type '(unsigned-byte 8))
    (write-uint (length text) 4 out)
    (loop FOR c ACROSS text
          ;; XXX: (< (char-code c) #x10000)を仮定している
          DO (write-uint (char-code c) 2 out))))
(format *error-output* "~&; => DONE~%")


;;;; write baseform, reading and pronunciation index
(format *error-output* "~2&; write feature index: ~a ~%" (merge-pathnames "feature.idx.bin" *out-dic-dir*))
(load "fifo")

(defun morphash-to-list (morps-map &aux (acc '()))
  (maphash (lambda (k ms)
             (push ms acc))
           morps-map)
  (sort acc #'string< :key (lambda (x) (morpheme-surface (car x)))))

;; almost the same as 'reduce.lisp:fnfn'
(defun depthfirst-sort (morps &aux acc (que (fifo:make (copy-seq morps))))
  (loop WHILE (not (fifo:empty-p que))
        FOR x = (fifo:pop que)
        FOR i FROM 0
    DO
    (if (null (cdr x))
        (push (car x) acc)
      (progn
        (push (car x) acc)
        (fifo:push (cdr x) que))))
  (nreverse acc))

(defparameter *dep-ms* (depthfirst-sort (morphash-to-list *ms*)))

(with-open-file (out (merge-pathnames "feature.idx.bin" *out-dic-dir*)
                     :direction :output
                     :if-exists :supersede
                     :element-type '(unsigned-byte 8))
  (write-uint (length *dep-ms*) 4 out)
  (loop FOR m IN *dep-ms*
    DO
    (let ((n 0)
          (baseform (if (morpheme-omit-baseform? m) "" (morpheme-baseform m)))
          (read/pron (if (morpheme-omit-pronun? m)
                         (morpheme-reading m)
                       (format nil "~a,~a" (morpheme-reading m) (morpheme-pronun m)))))
      (declare ((unsigned-byte 48) n))
      (setf (ldb (byte 17  0) n) (or (gethash baseform *baseforms*) -1)   ;; XXX: 14bitで十分
            (ldb (byte 21 17) n) (gethash read/pron *reading-pronuns*)
            (ldb (byte  4 38) n) (length baseform)
            (ldb (byte  6 42) n) (length read/pron))
      (write-uint n 6 out))))
(format *error-output* "~&; => DONE~%")
