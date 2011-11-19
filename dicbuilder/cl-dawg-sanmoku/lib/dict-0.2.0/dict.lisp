(in-package :dict-0.2.0)

(declaim (inline make-dict dict-buckets dict-bitlen dict-count 
                 dict-rehash-threshold dict-rehash-border dict-functor

                 recalc-rehash-border bucket-index find-candicate
                 normalize-hashcode rehash-node count-and-check-border

                 make get (setf get) remove count map clear))
(declaim #.*fastest*)

(deftype node () 'array-index)
(defconstant +TAIL+ 0)

(defstruct dict
  (alloca           t :type node-allocator)
  (buckets        #() :type (simple-array node))
  (bitlen           2 :type hashcode-width)
  (count            0 :type positive-fixnum)
  (rehash-threshold 0 :type number  :read-only t)
  (rehash-border    0 :type positive-fixnum)
  (functor          t :type functor :read-only t))

(defmethod print-object ((o dict) stream)
  (declare (stream stream))
  (print-unreadable-object (o stream :identity t :type t)
    (with-slots (count functor) o
      (format stream "~s ~s ~s ~s" 
              :test (functor-name functor)
              :count count))))

;;;;;;;;;;;;;;;;;;;;;;
;;; internal functions
(defun recalc-rehash-border (dict)
  (declare #.*muffle-note*)
  (with-slots (buckets rehash-threshold rehash-border) (the dict dict)
    (setf rehash-border (ceiling (* (length buckets) rehash-threshold))))
  dict)

(defun bucket-index (hashcode dict)
  (declare (hashcode hashcode))
  (ldb (byte (dict-bitlen dict) 0) hashcode))

(defun normalize-hashcode (hashcode)
  (declare (hashcode hashcode))
  (ldb (byte #.(1- +HASHCODE_WIDTH+) 0) hashcode))

(defun find-candidate (hashcode dict &aux (index (bucket-index hashcode dict)))
  (declare (hashcode hashcode))
  (labels ((recur (pred node)
             (if (> hashcode (node-hash node (dict-alloca dict)))
                 (recur node (node-next node (dict-alloca dict)))
               (values index pred node))))
    (recur nil (aref (dict-buckets dict) index))))

(defmacro with-node-place (place (pred dict bucket-index) &body body)
  `(if ,pred
       (symbol-macrolet ((,place (node-next ,pred (dict-alloca ,dict))))
         ,@body)
     (symbol-macrolet ((,place (aref (dict-buckets ,dict) ,bucket-index)))
       ,@body)))

(defmacro with-candidate ((node place) (hashcode dict) &body body)
  (with-gensyms (bucket-index pred)
    `(multiple-value-bind (,bucket-index ,pred ,node)
                          (find-candidate ,hashcode ,dict)
       (with-node-place ,place (,pred ,dict ,bucket-index)
         ,@body))))

(defmacro find-node-case ((node &optional (place (gensym)) (hashcode (gensym)))
                          (key dict hash-fn test-fn) 
                          &key if-existing if-absent)
  (with-gensyms (bucket-index pred recur)
    `(let ((,hashcode (normalize-hashcode (,hash-fn ,key))))
       (multiple-value-bind (,bucket-index ,pred ,node)
                            (find-candidate ,hashcode ,dict)
         (declare (ignorable ,bucket-index))
         (labels ((,recur (,pred ,node)
                    (cond ((/= ,hashcode (node-hash ,node (dict-alloca ,dict)))
                           (with-node-place ,place (,pred ,dict ,bucket-index)
                             ,if-absent))
                          ((,test-fn ,key (node-key ,node (dict-alloca ,dict)))
                           (with-node-place ,place (,pred ,dict ,bucket-index)
                             ,if-existing))
                          (t
                           (,recur ,node (node-next ,node (dict-alloca ,dict)))))))
           (,recur ,pred ,node))))))

(defmacro each-node ((node buckets alloca &optional return-form) &body body)
  (with-gensyms (head next)
    `(loop FOR ,head ACROSS (the (simple-array node) ,buckets) DO
       (loop FOR ,node = ,head THEN ,next
             FOR ,next = (node-next ,node ,alloca)
             UNTIL (eq ,node +TAIL+) DO
         (locally ,@body))
       FINALLY
       (return ,return-form))))

(defun rehash-node (node dict &aux (hashcode (node-hash node (dict-alloca dict))))
  (with-candidate (next place) (hashcode dict)
    (declare (node next))
    (setf place node
          (node-next node (dict-alloca dict)) next)))
  
(defun enlarge (dict)
  (with-slots (bitlen buckets alloca) (the dict dict)
    (let ((old-buckets buckets))
      (incf bitlen)
      (setf buckets (make-array (ash 1 bitlen) :element-type 'node :initial-element +TAIL+))
      
      (each-node (node old-buckets alloca (recalc-rehash-border dict))
        (rehash-node node dict)))))


(defun count-and-check-border (dict)
  (with-slots (rehash-border count) (the dict dict)
    (incf count)
    (< count rehash-border)))

(defmacro generate-get-fn (hash-fn test-fn)
  (with-gensyms (key dict node default)
    `(lambda (,key ,dict ,default)
       (find-node-case (,node) (,key ,dict ,hash-fn ,test-fn)
         :if-existing (values (node-value ,node (dict-alloca ,dict)) t)
         :if-absent   (values ,default nil)))))

(defmacro generate-set-fn (hash-fn test-fn)
  (with-gensyms (new-value key dict place node new-node hashcode)
    `(lambda (,new-value ,key ,dict)
       (find-node-case (,node ,place ,hashcode) (,key ,dict ,hash-fn ,test-fn)
         :if-existing (setf (node-value ,node (dict-alloca ,dict)) ,new-value)
         :if-absent
         (let ((,new-node (make-node (dict-alloca ,dict)
                                     :key ,key :value ,new-value 
                                     :hash ,hashcode :next ,node)))
           (setf ,place ,new-node)
           (unless (count-and-check-border ,dict)
             (enlarge ,dict)))))))

(defmacro generate-rem-fn (hash-fn test-fn)
  (with-gensyms (key dict place node)
    `(lambda (,key ,dict)
       (find-node-case (,node ,place) (,key ,dict ,hash-fn ,test-fn)
         :if-absent (values nil)
         :if-existing 
         (progn (setf ,place (node-next ,node (dict-alloca ,dict)))
                (delete-node ,node (dict-alloca ,dict))
                (decf (dict-count ,dict))
                (values t))))))

;;;;;;;;;;;;;;;;;;;;;;
;;; external functions
(defun make (&key (test 'eql) (rehash-threshold 1.0) (size 8))
  (declare #.*interface*
           (number rehash-threshold)
           (positive-fixnum size)
           ((or symbol functor) test))
  (let* ((bitlen (integer-length (1- size)))
         (buckets-size (the positive-fixnum (ash 1 bitlen))))
    (recalc-rehash-border
     (make-dict :alloca (make-allocator buckets-size)
                :buckets (make-array buckets-size :element-type 'node :initial-element +TAIL+)
                :bitlen bitlen
                :rehash-threshold rehash-threshold
                :functor (get-test test)))))

(defun get (key dict &optional default)
  (declare #.*interface*)
  (funcall (functor-get (dict-functor dict)) key dict default))

(defun (setf get) (new-value key dict)
  (declare #.*interface*)
  (funcall (functor-set (dict-functor dict)) new-value key dict)
  new-value)

(defun remove (key dict)
  (declare #.*interface*)
  (funcall (functor-rem (dict-functor dict)) key dict))

(defun count (dict)
  (declare #.*interface*)
  (dict-count dict))

(defmacro each ((key value dict &optional return-form) &body body)
  (with-gensyms (node)
    `(each-node (,node (dict-buckets ,dict) (dict-alloca ,dict) ,return-form)
       (let ((,key (node-key ,node (dict-alloca ,dict)))
             (,value (node-value ,node (dict-alloca ,dict))))
         ,@body))))

(defun map (fn dict &aux acc)
  (declare #.*interface*
           (function fn))
  (each (key value dict (nreverse acc))
    (push (funcall fn key value) acc)))

(defun clear (dict)
  (declare #.*interface*)
  (with-slots (count buckets) (the dict dict)
    (setf count 0)
    (clear-nodes (dict-alloca dict))
    (fill buckets +TAIL+))
  (values t))

(defun test-name (dict)
  (declare #.*interface*)
  (functor-name (dict-functor dict)))
