(defpackage fifo
  (:use :common-lisp)
  (:shadow push pop)
  (:export make
	   push
	   pop
	   empty-p
	   to-list))
(in-package :fifo)

(declaim (inline make push pop empty-p to-list)
	 (optimize (speed 3) (safety 0) (debug 1) (compilation-speed 0)))

(defun make (&optional initial-contents)
  (let ((que (cons nil initial-contents)))
    (setf (car que) (last que))
    que))

(defun push (x que)
  (setf (car que)
        (setf (cdar que) (list x)))
  que)

(defun pop (que)
  (prog1 (common-lisp:pop (cdr que))
    (when (null (cdr que))
      (setf (car que) que))))

(defun empty-p (que)
  (eq (car que) que))

(defun to-list (que)
  (cdr que))
