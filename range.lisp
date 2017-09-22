(when (not (find-package 'deftest)) (load "deftest"))
(defpackage :cl-range
  (:use common-lisp
	deftest)
  (:nicknames :range)
  (:export :range
	   :range!
	   :range-elt
	   :range-index
	   :range-reverse
	   :range-to-list
	   :range-slice
	   :range-subseq))
(in-package :cl-range)

(defstruct range
  (start nil :type integer)
  (stop nil :type integer)
  (step nil :type integer)
  (length nil :type integer))

(defun range (start &optional stop (step 1))
  (when (null stop)
    (setf stop start)
    (setf start 0))
  (let ((length (multiple-value-bind (div rem)
		    (floor (abs (- stop start)) (abs step))
		  (if (> rem 0)
		      (1+ div)
		      div))))
    (make-range :start start :stop stop :step step :length length)))

(defgeneric range-elt (r idx))
(defgeneric range-index (r value))
(defgeneric range-reverse (r))
(defgeneric range-to-list (r))
(defgeneric range-slice (r &optional from to by to-list?))
(defgeneric range-subseq (r start &optional end step))

(defun range! (start &optional stop (step 1))
  (range-to-list (range start stop step)))

(defmethod range-elt ((r range) idx)
  "Generate the element of a given RANGE designated by the given index"
  (when (< idx 0)
    (setf idx (+ (range-length r) idx)))
  (when (and (>= idx 0)
	     (< idx (range-length r)))
    (+ (range-start r) (* idx (range-step r)))))

(defmethod range-index ((r range) value)
  "Find, in a given RANGE, the index of a specified value"
  (let* ((step (range-step r))
	 (difference (- value (range-start r)))
	 (quotient (/ difference step))
	 (remainder (mod difference step)))
    (when (and (eql remainder 0)
	       (<=  0 quotient)
	       (< quotient (range-length r)))
      (abs quotient))))

(defmethod range-to-list ((r range))
  "Build a list from the given RANGE"
  (let ((start (range-start r))
	(step (range-step r))
	(stop (range-stop r)))
    (when (< step 0)
      (let ((tmp stop))
	(setf stop (1+ start))
	(setf start (+ start (* step (floor (- (+ tmp 1) start) step))))
	(setf step (abs step))))
    (do ((i start (+ i step))
	 (acc nil (push i acc)))
	((>= i stop) (if (< (range-step r) 0) acc (reverse acc))))))

(defmethod range-reverse ((r range))
  (let* ((step (range-step r))
	 (sign (/ step (abs step)))
	 (last (+ (range-start r) (* (- (range-length r) 1) step))))
    (range last (- (range-start r) sign) (* -1 step))))

(defmethod range-slice ((r range) &optional from to (by 1) to-list?)
  "Get the specified sub-RANGE of the given RANGE"
  (if (< by 0)
      (range-reverse (range-slice r from to (* -1 by) to-list?))
      (let* ((rlen (slot-value r 'length))
	     (start (range-start r))
	     (start-idx (if (null from) (range-index r start) from))
	     (stop-idx (if (null to) rlen to))
	     (step (range-step r))
	     (step-by (if (null by) step (* by step)))
	     (tempr r)
	     (result-range (range (range-elt tempr start-idx)
				  (if (< step 0)
				      (- (Range-elt tempr (- stop-idx 1)) 1)
				      (1+ (range-elt tempr (- stop-idx 1))))
				  step-by)))
	(if to-list?
	    (range-to-list result-range)
	    result-range))))

(defmethod range-subseq ((r range) start &optional end step)
  "Get a sequence representing the specified portion of the given RANGE"
  (range-slice r start end step t))

(defmacro make-tests ()
  `(deftests
     (test-inst range (10) (r)
	(assert (equal '(0 1 2 3 4 5 6 7 8 9)
		       (range-to-list r))))
       
     (test-inst range (3 8) (r)
	(assert (equal '(3 4 5 6 7)
		       (range-to-list r))))
     
     (test-inst range (5 33 2) (r)
	(assert (equal '(5 7 9 11 13 15 17 19 21 23 25 27 29 31)
		       (range-to-list r))))

     (test-inst range (10 1 -1) (r)
	(assert (equal '(10 9 8 7 6 5 4 3 2)
		       (range-to-list r))))

     (test-inst range (30 5 -3) (r)
	(assert (equal '(30 27 24 21 18 15 12 9 6)
		       (range-to-list r))))
     
     (test-inst range (21 -4 -4) (r)
	(assert (equal '(21 17 13 9 5 1 -3)
		       (range-to-list r))))
     (test-inst range (-11 3 3) (r)
	(assert (equal '(-11 -8 -5 -2 1)
		       (range-to-list r)))
	(let ((a (range-slice r 2 4))
	      (b (range-slice r nil nil -1)))
	  (assert (equal '(-5 -2) (range-to-list a)))
	  (assert (equal '(1 -2 -5 -8 -11) (range-to-list b)))))))
