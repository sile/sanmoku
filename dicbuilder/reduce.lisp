(unless (= (length sb-ext:*posix-argv*) 2)
  (format *error-output* "~&Usage: sbcl --script reduce.lisp BINARY_DICDIR~%")
  (sb-ext:quit))

(load "fifo")

(defparameter *dicdir* (probe-file (second sb-ext:*posix-argv*)))
(setf *default-pathname-defaults* *dicdir*)

(defun read-uint (in n)
   (loop FOR i FROM (1- n) DOWNTO 0
         SUM (ash (read-byte in) (* i 8))))

(defun read-int (in n)
  (let ((b1 (ash 1 (* n 8)))
        (b2 (ash 1 (* (1- n) 8)))
        (int (read-uint in n)))
    (if (< int b2)
        int
      (- int b1))))

(defun write-uint (n width out)
  (loop FOR i FROM (1- width) DOWNTO 0
        DO
        (write-byte (ldb (byte 8 (* i 8)) n) out)))

(defun flatten (list &aux acc)
  (labels ((recur (x)
             (when x
               (if (listp x)
                   (progn (recur (car x)) (recur (cdr x)))
                 (push x acc)))))
    (recur list)
    (nreverse acc)))

(defun maphash-to-list (fn hashtable &aux acc)
  (maphash (lambda (k v)
             (push (funcall fn k v) acc))
           hashtable)
  (nreverse acc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; for morpheme.bin

;; コストと品詞IDを読み込む
(with-open-file (in "morpheme.bin" :element-type '(unsigned-byte 8))
  (let ((count (read-uint in 4))
        (c '())
        (p '()))
    (loop REPEAT count
      DO
      (push (read-uint in 2) p)
      (push (read-uint in 2) c)
      FINALLY
      (defparameter *c* (nreverse c))
      (defparameter *p* (nreverse p)))))

(defparameter *pc* 
  (let ((m (make-hash-table)))
    (dolist (p *p* m)
      (incf (gethash p m 0)))))

(defparameter *cc* 
  (let ((m (make-hash-table)))
    (dolist (p *c* m)
      (incf (gethash p m 0)))))

(defparameter *pcc*
  (loop WITH m = (make-hash-table :test #'equal)
        FOR p IN *p*
        FOR c IN *c*
        DO
        (incf (gethash (cons p c) m 0))
        FINALLY
        (return m)))

(defparameter *pc-map*
  (loop WITH m = (make-hash-table :test #'equal)
        FOR p IN *p*
        FOR c IN *c*
        UNLESS (gethash (cons p c) m)
        DO
        (setf (gethash (cons p c) m) (hash-table-count m))
        FINALLY
        (return m)))

(with-open-file (out "morp.info.map"
                       :element-type '(unsigned-byte 8)
                       :if-exists :supersede
                       :direction :output)
    (write-uint (hash-table-count *pc-map*) 4 out)
    (loop FOR ((p . c) n) IN (sort (maphash-to-list #'list *pc-map*) #'< :key #'second)
          DO
          (write-uint p 2 out)
          (write-uint c 2 out)))
  
;; 単語ID(surface-id) => 形態素情報列、へのマッピング (同じ表層形を持つ複数の形態素)
(defparameter *id-morps-map*
  (with-open-file (in "id-morphemes-map.bin"
                      :element-type '(unsigned-byte 8))
    (let ((count (read-int in 4)))
      (loop FOR i FROM 0 BELOW count
            COLLECT (read-uint in 1)))))

(defparameter *morps*
  (loop WITH acc = '()
        WITH pca = (map 'vector (lambda (p c) (gethash (cons p c) *pc-map*)) *p* *c*)
        WITH offset = 0
        FOR im IN *id-morps-map*
    DO
    (push (coerce (subseq pca offset (+ offset im)) 'list) acc)
    (incf offset im)
    FINALLY
    (return (reverse acc))))

(defun fnfn (morps &aux acc (que (fifo:make (copy-seq morps))))
  (loop WHILE (not (fifo:empty-p que))
        FOR x = (fifo:pop que)
        FOR i FROM 0
    DO
    (when (zerop (mod i 1000))
      (format t "~&; ~a: ~a~%" i (length que)))
    (if (null (cdr x))
        (push (list 0 (car x)) acc)
      (progn
        (push (list 1 (car x)) acc)
        (fifo:push (cdr x) que))))
  (nreverse acc))

(defparameter *enc-morps* (fnfn *morps*))

(with-open-file (out "morp.info.bin"
                     :element-type '(unsigned-byte 8)
                     :if-exists :supersede
                     :direction :output)
  (write-uint (length *enc-morps*) 4 out)
  (loop FOR (_ v) IN *enc-morps*
        DO
        (write-uint v 2 out)))

(defparameter *enc-bits* (mapcar #'car *enc-morps*))
(loop WITH cnt = 0
      WITH acc = '()
      FOR b IN *enc-bits*
      FOR i FROM 0
  DO
  (when (zerop (mod  i 64))
    (push cnt acc))
  (when (= b 1)
    (incf cnt))
  FINALLY
  (defparameter *enc-cnts* (nreverse acc)))

(loop WITH enc = 0
      WITH acc = '()
      FOR b IN *enc-bits*
      FOR i FROM 0
  DO
  (when (= i 64)
    (push enc acc)
    (setf i 0
          enc 0))
  (setf (ldb (byte 1 i) enc) b)
  
  FINALLY
  (defparameter *enc-ns* (nreverse (cons enc acc))))

(with-open-file (out "morp.leaf.bin"
                     :element-type '(unsigned-byte 8)
                     :if-exists :supersede
                     :direction :output)
  (write-uint (length *enc-ns*) 4 out)
  (loop FOR v IN *enc-ns*
        DO
        (write-uint v 8 out)))

(with-open-file (out "morp.leaf.cnt.bin"
                     :element-type '(unsigned-byte 8)
                     :if-exists :supersede
                     :direction :output)
  (write-uint (length *enc-cnts*) 4 out)
  (loop FOR v IN *enc-cnts*
        DO
        (write-uint v 2 out)))

(with-open-file (out "morp.base.bin"
                     :element-type '(unsigned-byte 8)
                     :if-exists :supersede
                     :direction :output)
  (write-uint (length *morps*) 4 out))

;;
(delete-file (merge-pathnames "morpheme.bin" *default-pathname-defaults*))
(delete-file (merge-pathnames "id-morphemes-map.bin" *default-pathname-defaults*))



;;; 連続する文字コード情報は、まとめてしまう
(defparameter *cd*
  (with-open-file (in "code.bin"
                      :element-type '(unsigned-byte 8))
    (let ((count (read-int in 4)))
      (loop FOR i FROM 0 BELOW count
            COLLECT (list i (read-int in 3))))))

(defparameter *cd2*
  (loop FOR pred IN (cons nil *cd*)
        FOR cur IN *cd*
        UNLESS (eql (second pred) (second cur))
    COLLECT cur INTO list
    FINALLY (return (append list `((#x10000 0))))))

(with-open-file (out "code.bin"
                     :direction :output
                     :element-type '(unsigned-byte 8)
                     :if-exists :supersede)
  (write-uint (length *cd2*) 4 out)
  (loop FOR (code val) IN *cd2*
        DO
        (write-uint code 3 out)
        (write-uint val 3 out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; for matrix.bin

;; - 若干精度を落として、類似している品詞をまとめる
;; - 一つのコスト値を14bitで表現する

(defun load-matrix (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((left-num (read-uint in 4))
          (right-num (read-uint in 4)))
      (loop REPEAT left-num 
            COLLECT
            (loop REPEAT right-num
                  COLLECT (read-int in 2))))))

(defparameter *m* (load-matrix "matrix.bin"))

;; 類似度計算: 0に近いほど似ている
(defun calc (as bs &optional (n 1))
  (declare ((mod 10) n))
  (sqrt (/ (loop FOR a fixnum IN as
                 FOR b fixnum IN bs
                 SUM (expt (- (ash a (- n)) (ash b (- n))) 2))
           (length as))))

;; まとめた品詞のマッピング表
(defparameter *r* (make-hash-table))
(loop FOR i FROM 0 BELOW (length *m*)
      DO
      (setf (gethash i *r*) i))

(defun get-r (i)
  (let ((v (gethash i *r*)))
    (if (= v i)
        v
      (get-r v))))

;;
(defun merge1 (border &optional (limit 0.2))
  (loop FOR a IN *m*
        FOR i FROM 0
        WHEN (and (/= i 0)
                  (= i (gethash i *r*)))
    DO
    (when (zerop (mod i 50))
      (format t "~&; ~a~%" i))
    (loop FOR b IN *m*
          FOR j FROM 0
          WHEN (and (/= j 0)
                    (not (eq a b))
                    (= j (gethash j *r*)))
          WHEN (<= (calc a b border) limit)
      DO
      (setf (gethash i *r*) j)
      ))
  'done)

;; まとめる
(progn
  (merge1 0 0)
  (loop FOR i FROM 0 TO 5
        DO
        (loop FOR limit IN '(0.1 0.26)
              DO
              (format t "~2&;; ~a# ~a~%" i limit)
              (merge1 i limit))))

;; 
(defparameter *f* (make-hash-table))
(loop WITH new-id = -1
      FOR i FROM 0 BELOW (length *m*)
      WHEN (= i (gethash i *r*))
      DO
      (setf (gethash i *f*) (incf new-id)))

(loop FOR i FROM 0 BELOW (length *m*)
      WHEN (/= i (gethash i *r*))
      DO
      (setf (gethash i *f*) (gethash (get-r i) *f*)))

(with-open-file (out "posid-map.bin" 
                     :direction :output
                     :element-type '(unsigned-byte 8)
                     :if-exists :supersede)
  (write-uint (length *m*) 4 out)
  (loop FOR a IN (sort (maphash-to-list #'list *f*) #'< :key #'car)
        DO
        (write-uint (second a) 2 out))
  'done)

(with-open-file (out "matrix.bin"
                     :direction :output
                     :element-type '(unsigned-byte 8)
                     :if-exists :supersede)
  (let ((len (length (remove-duplicates (maphash-to-list #'list *f*) :key #'second))))
    (write-uint len 4 out)
    (write-uint len 4 out)
    (loop FOR as IN *m*
          FOR i FROM 0
          WHEN (= i (gethash i *r*))
      DO
      (loop FOR a IN as
            FOR j FROM 0
            WHEN (= j (gethash j *r*))
        DO
        (write-uint a 2 out)))))

;;;; 16bit => 14bit
(defparameter *m2*
  (flatten (load-matrix "matrix.bin")))

(defparameter *map*
  (loop WITH m = (make-hash-table)
        FOR b IN *m2*
        UNLESS (gethash b m)
        DO
        (setf (gethash b m) (hash-table-count m))
        FINALLY
        (return m)))

(defparameter *enc-m*
  (loop WITH n = 0
        WITH acc = '()
        FOR a IN *m2*
        FOR i FROM 0
    DO
    (when (= i 4)
      (push n acc)
      (setf i 0
            n 0))
    (setf (ldb (byte 14 (* 14 i)) n) (gethash a *map*))
    FINALLY
    (return (reverse (cons n acc)))))

(with-open-file (out "matrix.bin" 
                     :direction :output
                     :element-type '(unsigned-byte 8)
                     :if-exists :supersede)
  (let ((len (length (remove-duplicates (maphash-to-list #'list *f*) :key #'second))))
    (write-uint (length *enc-m*) 4 out)
    (write-uint len 4 out)
    (loop FOR a IN *enc-m*
          DO
          (write-uint a 7 out)))
  'done)

(with-open-file (out "matrix.map" 
                     :direction :output
                     :element-type '(unsigned-byte 8)
                     :if-exists :supersede)
  (write-uint (hash-table-count *map*) 4 out)
  (loop FOR (v _) IN (sort (maphash-to-list #'list *map*) #'< :key #'second)
        DO
        (write-uint v 2 out))
  'done)
