(defun split (str delim &key (start 0) (end (length str)))
  (declare (simple-string str)
           (character delim)
           (fixnum start end))
  (when (< start end)
    (let ((p (position delim str :start start :end end)))
      (if p
          (cons (subseq str start p) (split str delim :start (1+ p) :end end))
        (list (subseq str start end))))))

(defun delete-unused-morp (morps)
  (flet ((morp< (a b)
           (if (< (first a) (first b))
               t
             (if (> (first a) (first b))
                 nil
               (< (second a) (second b))))))
    (delete-duplicates (sort morps #'morp<) :key #'first :from-end t)))

(defun read-features (dir &optional (sb-impl::*default-external-format* :euc-jp))
  (loop WITH map = (make-hash-table :test #'equal)
        FOR path IN (directory (format nil "~a/*.csv" dir))
    DO
    (with-open-file (in path)
      (loop FOR line = (read-line in nil nil)
            WHILE line
        DO
        (destructuring-bind (surface pos-id _2 cost _4 _5 _6 _7 _8 _9 baseform yomi hatuon)
                            (split line #\,)
          (declare (ignore _2 _4 _5 _6 _7 _8 _9))
          (push (list (parse-integer pos-id) (parse-integer cost) baseform yomi hatuon)
                (gethash surface map)))))
    FINALLY
    (maphash (lambda (k ms)
               (setf (gethash k map) (delete-unused-morp ms)))
             map)
    (return map)))

(defparameter *f* (read-features "/home/ohta/downloads/mecab-ipadic-2.7.0-20070801/"))
(defparameter *f2*
  (let ((fs '()))
    (maphash (lambda (k ms)
               (loop FOR (_1 _2 baseform yomi hatuon) IN ms
                 DO
                 (let ((type (+ (if (string= k baseform) 1 0)
                                (ash (if (string= yomi hatuon) 1 0) 1))))
                   (push (case type
                           (#b00 (list type baseform yomi hatuon))
                           (#b01 (list type yomi hatuon))
                           (#b10 (list type baseform yomi))
                           (#b11 (list type yomi)))
                         fs))))
             *f*)
    (nreverse fs)))

;; type:
;; b00# surface /= baseform and yomi /= hatuon
;; b01# surface == baseform and yomi /= hatuon
;; b10# surface /= baseform and yomi == hatuon
;; b11# surface == baseform and yomi == hatuon

(loop FOR f IN *f2*
      SUM
      (loop FOR a IN (cdr f)
            SUM (length a)))

(defparameter *data1*
  (loop WITH m = (make-hash-table :test #'equal)
        FOR f IN *f2*
        WHEN (not (ldb-test (byte 1 0) (car f)))
        DO
        (setf (gethash (second f) m) t)
        FINALLY
        (let ((offset 0))
          (maphash (lambda (k v)
                     (declare (ignore v))
                     (setf (gethash k m) offset)
                     (incf offset (length k)))
                   m)
          (return m))))

(defparameter *data2*
  (loop WITH m = (make-hash-table :test #'equal)
        FOR f IN *f2*
        FOR ss = (if (not (ldb-test (byte 1 0) (car f)))
                     (cddr f)
                   (cdr f))
        DO
        (setf (gethash ss m) t)
        FINALLY
        (let ((offset 0))
          (maphash (lambda (k v)
                     (declare (ignore v))
                     (setf (gethash k m) offset)
                     (incf offset (+ (loop FOR x IN k SUM (length x))
                                     (1- (length k)))))
                   m)
          (return m))))

(defun write-uint (n width out)
  (loop FOR i FROM (1- width) DOWNTO 0
        DO
        (write-byte (ldb (byte 8 (* i 8)) n) out)))

(with-open-file (out "/tmp/feature.text.bin" 
                     :direction :output
                     :if-exists :supersede
                     :element-type '(unsigned-byte 8))
  (loop WITH offset = 0
        FOR m IN (list *data1* *data2*)
    DO
    (maphash (lambda (k v)
               (declare (ignore v))
               (let ((s (if (listp k)
                            (format nil "~{~a~^,~}" k)
                          k)))
                 (if (/= (length s) 65) ; NOTE: 読みとかが途中で切れてしまっているので飛ばす
                     (progn
                       (loop FOR c ACROSS s
                             ;; XXX: (char-code c) が 0xFFFF 以下であることを仮定
                             DO (write-uint (char-code c) 2 out))
                       (setf (gethash k m) offset) ; TODO: => (file-position out)
                       (incf offset (length s)))
                   (setf (gethash k m) nil))))
             m))
  'done)

(with-open-file (out "/tmp/feature.info.bin"
                     :direction :output
                     :if-exists :supersede
                     :element-type '(unsigned-byte 8))
  (loop FOR (type . fs) IN *f2*
    DO
    (multiple-value-bind (baseform yomi/hatu)
                         (case type 
                           (#b00 (values (car fs) (cdr fs)))
                           (#b01 (values nil fs))
                           (#b10 (values (car fs) (cdr fs)))
                           (#b11 (values nil fs)))
      (let ((n 0))
        (declare ((unsigned-byte 48) n))
        (setf (ldb (byte 17  0) n) (or (gethash baseform *data1*) -1)
              (ldb (byte 21 17) n) (or (gethash yomi/hatu *data2*) -1)
              (ldb (byte  4 38) n) (length baseform)
              (ldb (byte  6 42) n) (length yomi/hatu))
        (write-uint n 6 out))))
  'done)
    