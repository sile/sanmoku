;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; for morpheme.bin
(defparameter *c* 
  (mapcar #'parse-integer (read-lines "/home/ohta/dev/java/sanmoku/cost.list")))

(defparameter *p* 
  (mapcar #'parse-integer (read-lines "/home/ohta/dev/java/sanmoku/posid.list")))

(loop FOR a IN *p*
      FOR b IN (cons -1 (butlast *p*))
      FOR c IN (cons -1 (cons -1 (butlast *p*)))
      FOR d IN (cons -1 (cons -1 (cons -1 (butlast *p*))))
      FOR e IN (cons -1 (cons -1 (cons -1 (cons -1 (butlast *p*)))))
      FOR f IN (cons -1 (cons -1 (cons -1 (cons -1 (cons -1 (butlast *p*))))))
      FOR g IN (cons -1 (cons -1 (cons -1 (cons -1 (cons -1 (cons -1 (butlast *p*)))))))
      WHEN (or (= a b)
               (= a c)
               (= a d)
               (= a e)
               (= a f)
               (= a g))
      SUM 1)

(defun word-rev (n)
  (rotatef (ldb (Byte 8 0) n)
           (ldb (byte 8 8) n))
  n)

(with-open-file (out "dev/java/sanmoku/posid.list" :direction :output
                     :if-exists :supersede)
  (dolist (p *p*)
    (princ p out)
    (terpri out)))

(let ((acc))
  (with-open-file (in "dev/java/sanmoku/dicbuilder/dic/morpheme.bin"
                      :element-type '(unsigned-byte 16))
    (ignore-errors
      (read-byte in)
      (read-byte in)
      (loop FOR posid = (read-byte in)
            FOR cost = (read-byte in)
            DO
            (push (word-rev posid) acc)))
    (defparameter *p* (reverse acc))))

(let ((acc))
  (with-open-file (in "dev/java/sanmoku/dicbuilder/dic/id-morphemes-map.bin"
                      :element-type '(unsigned-byte 8))
    (ignore-errors
      (read-byte in)
      (read-byte in)
      (read-byte in)
      (read-byte in)
      (loop FOR a = (read-byte in)
            DO
            (push a acc)))
    (defparameter *a* (reverse acc))))

(defparameter *d*
  (loop FOR f IN *f*
        WHEN (= (length f) 1)
        COLLECT (car f)))

(defparameter *f*
  (loop WITH offset = 0
        WITH p = (coerce *p* 'vector)
        WITH c = (coerce *c* 'vector)
        FOR a IN *a*
        COLLECT 
        (prog1 (map 'list 'list
                    (subseq p offset (+ offset a))
                    (subseq c offset (+ offset a)))
          (incf offset a))))

(defparameter *c-c* (loop WITH m = (make-hash-table) 
                          FOR a IN (mapcar #'second *d*) 
                          DO (incf (gethash a m 0))
                          FINALLY (return m)))
(defparameter *p-c* (loop WITH m = (make-hash-table) 
                          FOR a IN (mapcar #'first *d*)
                          DO (incf (gethash a m 0)) 
                          FINALLY (return m)))


(defparameter *posid-map* 
  (loop WITH m = (make-hash-table)
        REPEAT (1- (ash 1 9))
        FOR x IN (sort (maphash-to-list #'list *p-c*) #'> :key #'second)
        DO 
        (setf (gethash (car x) m) (hash-table-count m))
        FINALLY
        (return m)))

(defparameter *cost-map* 
  (loop WITH m = (make-hash-table)
        REPEAT (1- (ash 1 13))
        FOR x IN (sort (maphash-to-list #'list *c-c*) #'> :key #'second)
        DO 
        (setf (gethash (car x) m) (hash-table-count m))
        FINALLY
        (return m)))

(defun write-uint (n width out)
  (loop FOR i FROM (1- width) DOWNTO 0
        DO
        (write-byte (ldb (byte 8 (* i 8)) n) out)))

(with-open-file (out "dev/java/sanmoku/dicbuilder/dic/morp.posid.map"
                     :direction :output
                     :if-exists :supersede
                     :element-type 'octet)
  (write-uint (hash-table-count *posid-map*) 4 out)
  (loop FOR x IN (sort (maphash-to-list #'list *posid-map*) #'< :key #'second)
        DO
        (write-uint (car x) 2 out)))

(with-open-file (out "dev/java/sanmoku/dicbuilder/dic/morp.cost.map"
                     :direction :output
                     :if-exists :supersede
                     :element-type 'octet)
  (write-uint (hash-table-count *cost-map*) 4 out)
  (loop FOR x IN (sort (maphash-to-list #'list *cost-map*) #'< :key #'second)
        DO
        (write-uint (car x) 2 out)))

(defparameter *morp-ext* (make-hash-table))

(with-open-file (out "dev/java/sanmoku/dicbuilder/dic/morp.root.bin"
                     :direction :output
                     :if-exists :supersede
                     :element-type 'octet)
  (write-uint (length *f*) 4 out)
  (loop FOR f IN *f*
        WITH ext-pos = 0
    DO
    (let ((n 0))
      (if (= (length f) 1)
          (destructuring-bind ((pos cost)) f
            (setf (ldb (byte 14 0) n) (gethash cost *cost-map*)
                  (ldb (byte 9 14) n) (gethash pos *posid-map*)
                  (ldb (byte 1 23) n) 1))
        (progn
          (setf (ldb (byte 18 0) n) ext-pos
                (ldb (byte 5 18) n) (length f)
                (ldb (byte 1 24) n) 0)
          (setf (gethash ext-pos *morp-ext*) f)
          (incf ext-pos (length f))))
      (write-uint n 3 out))))

(with-open-file (out "dev/java/sanmoku/dicbuilder/dic/morp.ext.bin"
                     :direction :output
                     :if-exists :supersede
                     :element-type 'octet)
  (write-uint (reduce #'+ (maphash-to-list (lambda (k v) (length v)) *morp-ext*))
              4 out)
  (loop FOR (pos f) IN (sort (maphash-to-list #'list *morp-ext*) #'< :key #'first)
    DO
    (loop FOR (pos cost) IN f
      DO
      (write-uint pos 2 out)
      (write-uint cost 2 out))))

;; {1} 9 14
;; {0} 5 18



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; for matrix.bin
(defun read-int (in n)
  (let ((b1 (ash 1 (* n 8)))
        (b2 (ash 1 (* (1- n) 8)))
        (int (loop FOR i FROM (1- n) DOWNTO 0
                   SUM (ash (read-byte in) (* i 8)))))
    (if (< int b2)
        int
      (- int b1))))

(defparameter *m*
  (with-open-file (in "/home/ohta/dev/java/sanmoku/dicbuilder/dic/matrix.bin"
                      :element-type '(unsigned-byte 8))
    (let ((lefts (read-int in 4))
          (rights (read-int in 4)))
      (loop REPEAT (* lefts rights)
            COLLECT (read-int in 2)))))

(defparameter *pc* 
  (let ((m (make-hash-table)))
    (dolist (p *p* m)
      (incf (gethash p m 0)))))

(defparameter *cc* 
  (let ((m (make-hash-table)))
    (dolist (p *c* m)
      (incf (gethash p m 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; for code.bin
(defparameter *cd*
  (with-open-file (in "/home/ohta/dev/java/sanmoku/dicbuilder/dic/code.bin"
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

(with-open-file (out "/home/ohta/dev/java/sanmoku/dicbuilder/dic/code.bin"
                     :direction :output
                     :element-type '(unsigned-byte 8)
                     :if-exists :supersede)
  (write-uint (length *cd2*) 4 out)
  (loop FOR (code val) IN *cd2*
        DO
        (write-uint code 3 out)
        (write-uint val 3 out)))