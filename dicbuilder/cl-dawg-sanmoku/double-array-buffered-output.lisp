(defpackage dawg.double-array.buffered-output
  (:use :common-lisp :dawg.global)
  (:export buffered-output
           with-output
           write-uint))
(in-package :dawg.double-array.buffered-output)

;;;;;;;;;;;;;;;
;;; declamation
(declaim #.*fastest*)

;;;;;;;;;;;;
;;; constant
(defconstant +BUFFER_SIZE+ 819200)

;;;;;;;;;;;;;;;;;;;
;;; buffered-output
(defstruct buffered-output
  (binary-output nil :type file-stream)
  (width           0 :type positive-fixnum)
  (buffer        #() :type simple-array)
  (offset          0 :type array-index))

;;;;;;;;;;;;;;;;;;;;;
;;; external function
(defmacro with-output ((out path &key (byte-width 1)) &body body)
  (declare ((member 1 2 4 5 8) byte-width))
  `(with-open-file (,out ,path :element-type '(unsigned-byte 8)
                               :direction :output
                               :if-exists :supersede)
     (let ((,out (make-buffered-output 
                  :binary-output ,out
                  :width ,byte-width
                  :buffer (make-array ,+BUFFER_SIZE+
                                      :element-type `(unsigned-byte ,,(* 8 byte-width))
                                      :initial-element 0))))
       (unwind-protect
           (locally ,@body)
         (flush ,out :final t)))))

(defun write-bytes (n width output)
  (loop FOR i FROM (1- width) DOWNTO 0
        DO
        (write-byte (ldb (byte 8 (* 8 i)) n) output)))

(defun write-uint (uint out &key (position 0))
  (declare (buffered-output out)
           (positive-fixnum position))
  (with-slots (binary-output buffer offset width) out
    (cond ((< position offset)
           (file-position binary-output (* width position))
           (write-bytes uint width binary-output))
          ((< position (+ offset +BUFFER_SIZE+))
           (muffle
            (setf (aref buffer (- position offset)) uint)))
          (t
           (flush out)
           (incf offset +BUFFER_SIZE+)
           (fill buffer 0)
           (write-uint uint out :position position)))))

(defun flush (out &key final)
  (declare (buffered-output out))
  (with-slots (binary-output buffer offset width) out
    (file-position binary-output (* offset width))
    (if (null final)
        (loop FOR n ACROSS buffer DO (write-bytes n width binary-output))
      (let ((end (muffle
                  (or (position-if-not #'zerop buffer :from-end t)
                      (1- +BUFFER_SIZE+)))))
        (loop REPEAT (1+ end) FOR n ACROSS buffer DO (write-bytes n width binary-output))
        (loop REPEAT #x100 DO (write-bytes 0 width binary-output))))))
