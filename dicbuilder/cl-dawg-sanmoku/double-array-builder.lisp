(defpackage dawg.double-array-builder
  (:use :common-lisp :dawg.global)
  (:export build-from-bintrie))
(in-package :dawg.double-array-builder)

(package-alias :dawg.double-array.node-allocator :node-allocator)
(package-alias :dawg.double-array.buffered-output :output)
(package-alias :dawg.bintrie-builder :bintrie)

;;;;;;;;;;;;;;;
;;; declamation
(declaim #.*fastest*
         (inline set-base set-chck set-opts))

;;;;;;;;;;;;
;;; constant
(defconstant +BUFFER_SIZE+ 819200)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; da (abbreviation of "double array")
(defstruct da
  (node t :type output:buffered-output)
  (exts t :type stream)
  (done-count 0 :type positive-fixnum)
  (char-chck (make-hash-table) :type hash-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; node
(defstruct node
  (index         0 :type positive-fixnum)
  (base          0 :type positive-fixnum)
  (type          0 :type (mod 4))
  (terminal?     t :type boolean)
  (sibling-total 0 :type positive-fixnum)
  (chck          0 :type uint1)
  (children    '() :type list))

(defun new-node (parent-base-idx trie da)
  (declare (positive-fixnum parent-base-idx))
  (make-node :index (+ parent-base-idx (node-label trie da))
             :sibling-total #1=(bintrie:node-sibling-total trie)
             :terminal? (bintrie:node-terminal? trie)
             :chck (node-label trie da)
             :type (cond ((< #1# #.(ash 1 5))  #b00)
                         ((< #1# #.(ash 1 11)) #b10)
                         (t                    #b11))))

(defun child-acceptable-p (node)
  (with-slots (type children) (the node node)
    (let ((capacity (case type
                      (#b00 1)
                      (otherwise 0))))
      (plusp (- capacity (length children))))))

(defun add-child (node child da)
  (with-slots (children) (the node node)
    (setf children (nconc children (list (node-label child da))))))

;;;;;;;;;;;;;;;;;;;;;;
;;; auxiliary function
(defmacro show (fmt &rest args)
  `(when show-progress
     (format t ,fmt ,@args)))

(defun node-label (trie da)
  (let ((c (bintrie:node-label trie)))
    (with-slots (char-chck) (the da da)
      (unless (gethash c char-chck)
        (if (= c 0)
            (setf (gethash c char-chck) 0)
          (setf (gethash c char-chck) (1+ (hash-table-count char-chck)))))
      (gethash c char-chck))))

(defun byte-reverse (n width)
  (loop FOR i FROM 0 BELOW width
        FOR j FROM (1- width) DOWNTO 0
        WHILE (< i j)
        DO (rotatef (ldb (byte 8 (* i 8)) n)
                    (ldb (byte 8 (* j 8)) n)))
  n)

(defun write-bytes (n width output)
  (loop FOR i FROM (1- width) DOWNTO 0
        DO
        (write-byte (ldb (byte 8 (* 8 i)) n) output)))

(defun add-file-size (filepath)
  (let ((tmp (format nil "~p.tmp" filepath)))
    (with-open-output-file (out tmp 'uint1)
      (with-open-file (in filepath :element-type 'uint1)
        (write-bytes (file-length in) 4 out)
        (dotimes (i (file-length in))
          (write-byte (read-byte in) out))))
    (rename-file tmp filepath)))

;;;;;;;;;;;;;;;;;;
;;; build function
(defun write-node-impl (node da)
  (with-slots (index type base terminal? sibling-total chck children) (the node node)
    (let ((n 0))
      (declare ((unsigned-byte 40) n))
      (muffle
       (setf (ldb (byte 19  0) n) base
             (ldb (byte  1 19) n) (if terminal? 1 0)
             (ldb (byte  7 20) n) chck
             (ldb (byte  2 38) n) type))
       (case type
         (#b00
          (setf (ldb (byte  7 27) n) (or (first children) 0)
                (ldb (byte  5 34) n) sibling-total))
         (#b10 
          (setf (ldb (byte 11 27) n) sibling-total))
         (#b11
          (setf (ldb (byte 11 27) n) (file-position (da-exts da)))
          ;; XXX: little-endianを想定
          (write-byte (byte-reverse sibling-total 4) (da-exts da))))

       
       (output:write-uint n (da-node da) :position index))))

(defun write-node (node da &key base)
  (when base
    (setf (node-base node) base))
  (write-node-impl node da))

(defmacro show-and-write-node (node da &key base)
  `(progn 
     (incf #1=(da-done-count ,da))
     (when (and show-progress (zerop (mod #1# 100000)))
       (show ";  ~a nodes~%" #1#))
     (write-node ,node ,da :base ,base)))

(defun build-impl (trie alloca da node memo &optional show-progress)
  (let ((children (bintrie:collect-children trie)))
    (loop WHILE (and (not #1=(gethash (bintrie:node-child trie) memo))
                     (null (cdr children))
                     (not (bintrie::node-terminal? (car children)))
                     (child-acceptable-p node))
      DO
      (add-child node (car children) da)
      (setf trie (car children))
      (setf children (bintrie:collect-children trie)))
  
    (a.if #1#
          (show-and-write-node node da :base it)
      (if (null children)
          (show-and-write-node node da)
        (let ((base-idx (node-allocator:allocate
                         alloca 
                         (mapcar (lambda (c) (node-label c da)) children))))
          (setf #1# base-idx)
          (show-and-write-node node da :base base-idx)
            
          (dolist (child children)
            (build-impl child alloca da (new-node base-idx child da) memo show-progress)))))))
                        

;;;;;;;;;;;;;;;;;;;;;
;;; external function
(defun build-from-bintrie (trie &key output-file byte-order show-progress)
  (show "~2&; build double array from trie:~%")
  (let ((node-file (format nil "~a.node" output-file))
        (exts-file (format nil "~a.ext" output-file))
        (char-file (format nil "~a.char" output-file)))
    (show ";  create tmpfiles: ~a, ~a~%" node-file exts-file)

    (show "; build:~%")
    (output:with-output (node node-file :byte-width 5)
      (with-open-output-file (exts exts-file 'uint4)
        (let ((da (make-da :node node :exts exts)))
          (build-impl trie (node-allocator:make) da 
                      (new-node 0 trie da)
                      (make-hash-table :test #'eq)
                      show-progress)
          (let ((char-chck (da-char-chck da)))
            (with-open-output-file (char char-file 'uint1)
              (dotimes (i #x100)
                (write-byte (gethash i char-chck (hash-table-count char-chck)) char)))))))
    (add-file-size node-file)
    (add-file-size exts-file)
    (show "; concatenate tempfiles to ~A~%"  output-file))
  'done)

(package-alias :dawg.double-array.node-allocator)
(package-alias :dawg.double-array.buffered-output)
(package-alias :dawg.bintrie-builder)
