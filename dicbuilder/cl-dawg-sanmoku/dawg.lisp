(defpackage dawg
  (:use :common-lisp :dawg.global)
  (:shadow :common-lisp load)
  (:export dawg
           build
           load
           member?
           get-id
           each-common-prefix
           each-predictive))
(in-package :dawg)

(package-alias :dawg.octet-stream :stream)

;;;;;;;;;;;;;;;;;;;;
;;; special variable
(eval-when (:compile-toplevel)
  (defvar *args-type* '(simple-characters dawg &key (:start positive-fixnum)
                                                    (:end positive-fixnum))))
(defconstant +ARC_LIMIT+ #x100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dawg (double-array format)
(defstruct dawg
  (node #() :type (simple-array uint8))
  (ext  #() :type (simple-array uint4)))

(defmethod print-object ((o dawg) stream)
  (print-unreadable-object (o stream :type t :identity t)
    (format stream "~A:~A" :node-count (length (dawg-node o)))))

;;;;;;;;;;;;;;;
;;; declamation
(declaim (inline check-encoded-children get-node 
                 base chck terminal? sibling-total inc-id 
                 get-id-impl member?-impl
                 each-common-prefix-impl each-predictive-impl)
         (ftype (function #.*args-type* boolean) member?)
         (ftype (function #.*args-type* (or null positive-fixnum)) get-id))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auxiliary function(1)
(defun read-array (index-path &key size element-type offset byte-order)
  (declare ((member uint4 uint8) element-type))
  (with-open-file (in index-path :element-type element-type)
    (file-position in offset)
    (let ((ary (make-array size :element-type element-type)))
      (read-sequence ary in)
      (unless (or (eq byte-order :native)
                  (eq byte-order +NATIVE_ORDER+))
        (let ((byte-size (ecase element-type
                           (uint4 4)
                           (uint8 8))))
          (dotimes (i size)
            (setf (aref ary i) (byte-reverse (aref ary i) byte-size)))))
      ary)))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; external function(1)
(defun build (&key input output (byte-order :native) show-progress) 
  (declare ((or string pathname list) input)
           ((or string pathname) output)
           ((member :native :little :big) byte-order))
  (let ((trie (if (listp input)
                  (dawg.bintrie-builder:build-from-list input :show-progress show-progress)
                (dawg.bintrie-builder:build-from-file input :show-progress show-progress))))
    (dawg.double-array-builder:build-from-bintrie 
     trie :output-file output :byte-order byte-order :show-progress show-progress))
  t)

(defun load (index-path &key (byte-order :native))
  (declare ((or string pathname file-stream) index-path)
           ((member :native :little :big) byte-order))
  (let ((sizes (read-array index-path :size 2 :element-type 'uint4 :offset 0 
                           :byte-order byte-order)))
    (make-dawg
     :node (read-array index-path :element-type 'uint8
                                  :size (/ (aref sizes 0) 8)
                                  :offset 1
                                  :byte-order byte-order)
     :ext  (read-array index-path :element-type 'uint4
                                  :size (/ (aref sizes 1) 4)
                                  :offset (+ 2 (/ (aref sizes 0) 4))
                                  :byte-order byte-order))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auxiliary function(2)
(declaim (inline %type))
(defun terminal? (node) (ldb-test (byte 1 31) (the uint8 node)))
(defun base (node) (ldb (byte 29  0) (the uint8 node)))
(defun chck (node) (ldb (byte  8 32) (the uint8 node)))
(defun %type (node) (ldb (byte 2 29) (the uint8 node)))

(defun sibling-total (da node)
  (ecase (%type node)
    (0 (ldb (byte  8 56) node))
    (1 (ldb (byte 16 48) node))
    (2 (ldb (byte 24 40) node))
    (3 (aref (dawg-ext da)
             (ldb (byte 24 40) node)))))

(defun inc-id (id da node)
  (let ((terminal (if (terminal? node) 1 0))
        (sibling-total (sibling-total da node)))
    (the uint4 (+ id terminal sibling-total))))

(defun check-encoded-children (in node)
  (declare (uint8 node))
  (labels ((enc-chck (n)
             (ldb (byte 8 (+ 40 (* 8 n))) node))
           (check (n &aux (chck (enc-chck n)))
             (or (zerop chck)
                  (and (= chck (stream:read in))
                       (not (stream:eos? in))))))
    (declare (inline enc-chck check))
    (case (%type node)
      (0 (and (check 0) (check 1)))
      (1 (check 0))
      (t t))))

(defun get-node (dawg index)
  (declare (dawg dawg)
           (positive-fixnum index))
  (aref (dawg-node dawg) index))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; external function(2)
(defun member?-impl (in dawg)
  (nlet recur ((node (get-node dawg 0)))
    (if (stream:eos? in)
        (terminal? node)
      (when (check-encoded-children in node)
        (let* ((arc (stream:read in))
               (next (get-node dawg (+ (base node) arc))))
          (when (= (chck next) arc)
            (recur next)))))))

(defun get-id-impl (in dawg)
  (nlet recur ((node (get-node dawg 0)) (id -1))
    (if (stream:eos? in)
        (and (terminal? node) (inc-id id dawg node))
      (when (check-encoded-children in node)
        (let* ((arc (stream:read in))
               (next (get-node dawg (+ (base node) arc))))
          (when (= (chck next) arc)
            (recur next (inc-id id dawg node))))))))

(defun each-common-prefix-impl (fn in dawg)
  (nlet recur ((node (get-node dawg 0)) (id -1))
    (when (terminal? node)
      (funcall fn (inc-id id dawg node) (stream:position in)))
    (unless (stream:eos? in)
      (when (check-encoded-children in node)
        (let* ((arc (stream:read in))
               (next (get-node dawg (+ (base node) arc))))
          (when (= (chck next) arc)
            (recur next (inc-id id dawg node))))))))

(defun traverse-descendant (fn dawg node id)
  (declare #.*fastest*
           (function fn)
           (dawg dawg)
           (uint8 node)
           (fixnum id))
  (when (terminal? node)
    (funcall fn (inc-id id dawg node)))
  (loop FOR arc FROM 1 BELOW +ARC_LIMIT+ 
        FOR next OF-TYPE uint8 = (get-node dawg (+ (base node) arc))
        WHEN (= (chck next) arc)
      DO
      (traverse-descendant fn dawg next (inc-id id dawg node))))

(defun each-predictive-impl (fn in dawg)
  (nlet recur ((node (get-node dawg 0)) (id -1))
    (declare (fixnum id))
    (if (stream:eos? in)
        (traverse-descendant fn dawg node id)
      (when (check-encoded-children in node)
        (let* ((arc (stream:read in))
               (next (get-node dawg (+ (base node) arc))))
          (when (= (chck next) arc)
            (recur next (inc-id id dawg node))))))))


(defmacro with-key-stream ((in key &key start end) &body body)
  (let ((k (gensym))
        (s (gensym))
        (e (gensym)))
    `(let ((,k ,key)
           (,s ,start)
           (,e ,end))
       (declare #.*interface*
                (simple-characters ,k)
                (positive-fixnum ,s ,e))
       (locally
        (declare #.*fastest*)
        (let ((,in (stream:make ,k :start ,s :end ,e)))
          (declare (dynamic-extent ,in))
          ,@body)))))

(defun member? (key dawg &key (start 0) (end (length key)))
  (with-key-stream (in key :start start :end end)
    (member?-impl in dawg)))
  
(defun get-id (key dawg &key (start 0) (end (length key)))
  (with-key-stream (in key :start start :end end)
    (get-id-impl in dawg)))

(defmacro each-common-prefix ((match-id match-end)
                              (key dawg &key (start 0) (end `(length ,key)))
                              &body body)
  (let ((in (gensym)))
    `(block nil
       (with-key-stream (,in ,key :start ,start :end ,end)
         (each-common-prefix-impl 
          (lambda (,match-id ,match-end)
            (declare (positive-fixnum ,match-id)
                     (array-index ,match-end))
            ,@body)
          ,in ,dawg))
       t)))

(defmacro each-predictive ((match-id)
                           (key dawg &key (start 0) (end `(length ,key)))
                           &body body)
  (let ((in (gensym)))
    `(block nil
       (with-key-stream (,in ,key :start ,start :end ,end)
         (each-predictive-impl 
          (lambda (,match-id)
            (declare (positive-fixnum ,match-id))
            ,@body)
          ,in ,dawg))
       t)))


(package-alias :dawg.octet-stream)
