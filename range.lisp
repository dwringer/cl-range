(when (not (find-package 'deftest)) (load "deftest"))
(defpackage :cl-range
  (:use common-lisp
	deftest)
  (:export :range
	   :range-elt
	   :range-index
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
  (let ((length (+ (floor (/ (- stop start) step))
		   (if (> (mod (- stop start) step) 0) 1 0))))
    (make-range :start start :stop stop :step step :length length)))

(defgeneric range-elt (r idx))
(defgeneric range-index (r value))
(defgeneric range-to-list (r))
(defgeneric range-slice (r &optional from to by to-list?))
(defgeneric range-subseq (r start &optional end step))

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
    (range last (- (range-start r sign)) (* -1 step))))

(defmethod range-slice ((r range) &optional (from 0) to (by 1) to-list?)
  "Get the specified sub-RANGE of the given RANGE"
  (let ((start (if (null from) (range-start r) from))
	(stop (if (null to)  (range-stop r) to)))
    (when (< start 0) (setf start (max 0 (+ start (range-length r)))))
    (when (< stop 0) (setf stop (max (start (+ stop (range-length r))))))
    (if (or (null by) (> by 0))
	(range start stop (if step step 1))
	(let ((rng (range-reverse r)))
	  (setf (slot-value rng 'step) step)
	  rng))))
	
  ;; (with-slots ((r-start start) (r-step step) (r-stop stop) (r-len length)) r
  ;;   (let* ((r-first (range-elt r 0))
  ;; 	   (r-last (range-elt r (- r-len 1)))
  ;; 	   (sel-first (range-elt r from))
  ;; 	   (sel-last (range-elt r to))
  ;; 	   (new-step (* by r-step))
  ;; 	   (new-range (if (or (and (< new-step 0)
  ;; 				   (< sel-first sel-last))
  ;; 			      (and (> new-step 0)
  ;; 				   (< sel-last sel-first)))
  ;; 			  (range-slice r sel-last from by to-list?);; sel-last (- sel-first 1) new-step)
  ;; 			  (range sel-first sel-last new-step))))
  ;;     (if to-list? (range-to-list new-range) new-range))))

 ;;     (list r-first r-last sel-first sel-last))
;   
    
;; (let* ((r-start (range-start r))
  ;; 	 (r-step (range-step r))
  ;; 	 (r-stop (range-stop r))
  ;; 	 (r-len (range-length r))
  ;; 	 (orig-last
  ;; 	  (progn
  ;; 	    (when (null from)) (setf from 0)
  ;; 	    (when (null to)) (setf to r-len)
  ;; 	    (+ 1 r-start (* r-step
  ;; 			    (- r-len 1)))))
  ;; 	 (new-step (* by r-step))
  ;; 	 (new-start
  ;; 	  (if (< from 0)
  ;; 	      (+ orig-last (* r-step from))
  ;; 	      (+ r-start (* from r-step))))
  ;; 	 (new-stop
  ;; 	  (if (< to 0)
  ;; 	      (+ orig-last (* r-step to))
  ;; 	      (+ r-start (* to r-step)))))
  ;;   (pprint orig-last)
  ;;   (if (< by 0)
  ;; 	(let ((fin-last (+ new-start (* r-step
  ;; 					(floor (- (- new-stop new-start)
  ;; 						  1) r-step)))))
  ;; 	  (range fin-last (- new-start 1) new-step))
  ;; 	(range new-start new-stop new-step))))

(defmethod range-subseq ((r range) start &optional end step)
  "Get a sequence representing the specified portion of the given RANGE"
  (range-slice r start end step t))


(defun run-test ()
  (let* ((r (range-slice (range 10 101 3) 2 :to -2))
	 (midpoint (floor (/ (range-length r) 2))))
    (labels ((println (x) (format t "~A...~&" x)))
      (map nil #'println (range-subseq r 0 midpoint))
      (format t "Halfway done...~&")
      (map nil #'println (range-subseq r midpoint)))))

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
	      (b (range-slice r -1 0 -1)))
	  (assert (equal '(-5 -2) (range-to-list a)))
	  (assert (equal '(1 -2 -5 -8 -11) (range-to-list b)))))
    
     ))



;; (defun range-fns (start &optional stop (step 1))
;;   (when (null stop)
;;     (setf stop start)
;;     (setf start 0))
;;   (let ((len (+ (/ (- stop start)
;; 		   step)
;; 		(if (> (mod (- stop start) step) 0) 1 0))))
;;     (values
;;      #'(lambda (slice)
;; 	 (if (consp slice)
;; 	     (let ((new-start (+ start (* step (car slice))))
;; 		   (new-stop (+ start (* step (cadr slice)))))
;; 	       (if (= (length slice) 2)
;; 		   (range-fns new-start new-stop step)
;; 		   (range-fns new-start new-stop (* (caddr slice) step))))
;; 	     (let ((val (+ start (* step slice))))
;; 	       (when (< val stop) val))))
;;      #'(lambda (i)
;; 	 (let* ((difference (- i start))
;; 		(quotient (/ difference step))
;; 		(remainder (mod difference step)))
;; 	   (when (and (eql remainder 0)
;; 		      (<=  0 quotient)
;; 		      (< quotient len))
;; 	     (abs quotient)))))))

