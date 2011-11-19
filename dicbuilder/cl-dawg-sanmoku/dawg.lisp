(defpackage dawg
  (:use :common-lisp :dawg.global)
  (:shadow :common-lisp load)
  (:export dawg
           build
           load
           member?
           get-id))
(in-package :dawg)

(package-alias :dawg.octet-stream :stream)

(deftype uint5 () '(unsigned-byte 40))

;;;;;;;;;;;;;;;;;;;;
;;; special variable
(eval-when (:compile-toplevel)
  (defvar *args-type* '(simple-characters dawg &key (:start positive-fixnum)
                                                    (:end positive-fixnum))))
(defconstant +ARC_LIMIT+ #x100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dawg (double-array format)
(defstruct dawg
  (node #() :type (simple-array uint1))
  (ext  #() :type (simple-array uint4))
  (char-map #() :type (simple-array uint1)))

(defmethod print-object ((o dawg) stream)
  (print-unreadable-object (o stream :type t :identity t)
    (format stream "~A:~A" :node-count (length (dawg-node o)))))

;;;;;;;;;;;;;;;
;;; declamation
(declaim (inline check-encoded-children get-node 
                 base chck terminal? sibling-total inc-id 
                 get-id-impl member?-impl)
         (ftype (function #.*args-type* boolean) member?)
         (ftype (function #.*args-type* (or null positive-fixnum)) get-id))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auxiliary function(1)
(defun read-count (filepath byte-order)
  (with-open-file (in filepath :element-type 'uint4)
    (if (or (eq byte-order :native)
            (eq byte-order +NATIVE_ORDER+))
        (read-byte in)
      (byte-reverse (read-byte in) 4))))

(defun read-array (index-path &key size element-type offset byte-order)
  (declare ((member uint1 uint4 uint8) element-type))
  (with-open-file (in index-path :element-type element-type)
    (file-position in offset)
    (let ((ary (make-array size :element-type element-type)))
      (read-sequence ary in)
      (unless (or (eq byte-order :native)
                  (eq byte-order +NATIVE_ORDER+))
        (let ((byte-size (ecase element-type
                           (uint1 1)
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
  (let ((char-path (format nil "~a.char" index-path))
        (ext-path (format nil "~a.ext" index-path))
        (node-path (format nil "~a.node" index-path)))
    (make-dawg
     :node (read-array node-path :size (read-count node-path byte-order) 
                                 :element-type 'uint1 :offset 4 :byte-order byte-order)
     :ext (read-array ext-path :size (floor (read-count ext-path byte-order) 4)
                               :element-type 'uint4 :offset 1 :byte-order byte-order)
     :char-map (read-array char-path :size #x100 :element-type 'uint1 :byte-order byte-order))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auxiliary function(2)
(declaim (inline %type))
(defun terminal? (node) (ldb-test (byte 1 19) (the uint5 node)))
(defun base (node) (ldb (byte 19  0) (the uint5 node)))
(defun chck (node) (ldb (byte  7 20) (the uint5 node)))
(defun %type (node)
  (declare (uint5 node))
  (if (ldb-test (byte 1 39) node)
      (+ 2 (ldb (byte 1 38) node))
    0))

(defun sibling-total (da node)
  (case (%type node)
    (0 (ldb (byte  5 34) node))
    (2 (ldb (byte 11 27) node))
    (t (aref (dawg-ext da)
             (ldb (byte 11 27) node)))))

(defun inc-id (id da node)
  (let ((terminal (if (terminal? node) 1 0))
        (sibling-total (sibling-total da node)))
    (the uint4 (+ id terminal sibling-total))))

(defun arc (char dawg)
  (aref (dawg-char-map dawg) char))

(defun check-encoded-children (in node dawg)
  (declare (uint5 node))
  (labels ((enc-chck ()
             (ldb (byte 7 27) node))
           (check (&aux (chck (enc-chck)))
             (or (zerop chck)
                  (and (= chck (arc (stream:read in) dawg))
                       (not (stream:eos? in))))))
    (declare (inline enc-chck check))
    (case (%type node)
      (0 (check))
      (t t))))

(defun get-node (dawg index)
  (declare (dawg dawg)
           (positive-fixnum index))
  (with-slots (node) (the dawg dawg)
    (let ((n 0)
          (i (* index 5)))
      (declare (uint5 n))
      (setf (ldb (byte 8 32) n) (aref node (+ i 0))
            (ldb (byte 8 24) n) (aref node (+ i 1))
            (ldb (byte 8 16) n) (aref node (+ i 2))
            (ldb (byte 8 08) n) (aref node (+ i 3))
            (ldb (byte 8 00) n) (aref node (+ i 4)))
      n)))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; external function(2)
(defun member?-impl (in dawg)
  (nlet recur ((node (get-node dawg 0)))
    (if (stream:eos? in)
        (terminal? node)
      (when (check-encoded-children in node dawg)
        (let* ((arc (arc (stream:read in) dawg))
               (next (get-node dawg (+ (base node) arc))))
          (when (= (chck next) arc)
            (recur next)))))))

(defun get-id-impl (in dawg)
  (nlet recur ((node (get-node dawg 0)) (id -1))
    (if (stream:eos? in)
        (and (terminal? node) (inc-id id dawg node))
      (when (check-encoded-children in node dawg)
        (let* ((arc (arc (stream:read in) dawg))
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

(package-alias :dawg.octet-stream)
