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
(defgeneric range-slice (r from &key to by to-list?))
(defgeneric range-subseq (r start &optional end step))

(defmethod range-elt ((r range) idx)
  "Generate the element of a given RANGE designated by the given index"
  (let* ((val (+ (range-start r) (* (range-step r) idx))))
    (when (< val (range-stop r)) val)))

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
  (do ((i (range-start r) (+ i (range-step r)))
       (acc nil (push i acc)))
      ((>= i (range-stop r)) (reverse acc))))

(defmethod range-slice ((r range) from &key to by to-list?)
  "Get the specified sub-RANGE of the given RANGE"
  (when (< from 0) (setf from (- (range-length r) from)))
  (if (null to)
      (setf to (range-length r))
      (if (< to 0)
	  (setf to (+ (range-length r) to))))
  (let* ((start (range-start r))
	 (step (range-step r))
	 (new-start (+ start (* step from)))
	 (new-stop  (+ start (* step to)))
	 (slice
	  (if (null by)
	      (range new-start new-stop step)
	      (range new-start new-stop (* by step)))))
    (if to-list? (range-to-list slice) slice)))

(defmethod range-subseq ((r range) start &optional end step)
  "Get a sequence representing the specified portion of the given RANGE"
  (range-slice r start :to end :by step :to-list? t))


(defun run-test ()
  (let* ((r (range-slice (range 10 101 3) 2 :to -2))
	 (midpoint (floor (/ (range-length r) 2))))
    (labels ((println (x) (format t "~A...~&" x)))
      (map nil #'println (range-subseq r 0 midpoint))
      (format t "Halfway done...~&")
      (map nil #'println (range-subseq r midpoint)))))




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
