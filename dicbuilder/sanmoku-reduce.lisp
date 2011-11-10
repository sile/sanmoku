(defun read-uint (in byte-width)
  (loop FOR i FROM (1- byte-width) DOWNTO 0
        SUM (ash (read-byte in) (* i 8))))

(defun to-int (n byte-width)
  (let ((b (ash 1 (1- (* byte-width 8))))
        (b2 (ash 1 (* byte-width 8))))
    (if (< n b)
        n
      (- n b2))))

(defun read-int (in byte-width)
  (to-int (read-uint in byte-width) byte-width))

(defun write-int (int stream &key (width 1))
  (declare ((member 1 2 4 8) width))
  (flet ((write-impl (pos)
           (write-byte (ldb (byte 8 (* pos 8)) int) stream)))
    (declare (inline write-impl))
    (loop FOR i FROM (1- width) DOWNTO 0 DO (write-impl i))))

(defun load-morphemes (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((count (read-uint in 4)))
      (loop REPEAT count
            COLLECT (list (read-uint in 2) (read-uint in 2))))))

(defun make-posid-morphemes-map (path &aux (map (make-hash-table)))
  (loop FOR m IN (load-morphemes path)
        DO
        (push m (gethash (car m) map)))
  map)

(defun load-matrix (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((left-num (read-uint in 4))
          (right-num (read-uint in 4)))
      (loop REPEAT left-num 
            COLLECT
            (loop REPEAT right-num
                  COLLECT (read-int in 2))))))

(defparameter *m* (load-matrix "/home/ohta/dev/java/sanmoku/dicbuilder/dic/matrix.bin"))


(defun calc (as bs)
  (sqrt (/ (loop FOR a IN as
                 FOR b IN bs
                 SUM (expt (- a b) 2))
           (length as))))

(defparameter *r* (make-hash-table))
(loop FOR i FROM 0 BELOW (length *m*)
      DO
      (setf (gethash i *r*) i))

(defun merge1 (border)
  (loop FOR a IN *m*
        FOR i FROM 0
        WHEN (and (/= i 0)
                  (<= border (/ 10 (gethash i *pc*)))
                  (= i (gethash i *r*)))
    DO
    (when (zerop (mod i 10))
      (format t "~&; ~a~%" i))
    (loop FOR b IN *m*
          FOR j FROM 0
          WHEN (and (/= j 0)
                    (not (eq a b))
                    (= j (gethash j *r*)))
          WHEN (<= (calc a b) border)
      DO
      (setf (gethash i *r*) j)))
  'done)

(loop FOR i FROM 0 BELOW 10
      DO
      (format t "~2&;; ~a~%" i)
      (merge1 i))

(defun get-r (i)
  (let ((v (gethash i *r*)))
    (if (= v i)
        v
      (get-r v))))

(with-open-file (out "/tmp/posid-map.bin" 
                     :direction :output
                     :element-type '(unsigned-byte 8)
                     :if-exists :supersede)
  (write-int (length *m*) out :width 4)
  (loop FOR a IN (sort (maphash-to-list #'list *f*) #'< :key #'car)
        DO
        (write-int (second a) out :width 2))
  'done)


(with-open-file (out "/tmp/matrix.bin"
                     :direction :output
                     :element-type '(unsigned-byte 8)
                     :if-exists :supersede)
  (let ((len (length (remove-duplicates (maphash-to-list #'list *f*) :key #'second))))
    (write-int len out :width 4)
    (write-int len out :width 4)
    (loop FOR as IN *m*
          FOR i FROM 0
          WHEN (= i (gethash i *r*))
      DO
      (loop FOR a IN as
            FOR j FROM 0
            WHEN (= j (gethash j *r*))
        DO
        (write-int a out :width 2)))))

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

      