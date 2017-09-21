;;;; DEFTEST.LISP - Lisp macro-based testing framework
;;;; Copyright 2017 Darren W. Ringer <dwringer@gmail.com>

;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation
;;;; files (the "Software"), to deal in the Software without
;;;; restriction, including without limitation the rights to use,
;;;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;;;; copies of the Software, and to permit persons to whom the
;;;; Software is furnished to do so, subject to the following
;;;; conditions:

;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.

;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.
(defpackage :deftest
  (:use :common-lisp)
  (:export :deftests
	   :test-inst
	   :test-post-method
	   :test-pre-method
	   :run-tests))
(in-package :deftest)

(defmacro test-inst (name args return-values-as &rest body)
  "Construct NAME w/ARGS binding to RETURN-VALUES-AS, then eval body forms"
  `(multiple-value-bind ,return-values-as ,(cons name args)
     ,@body))

(defmacro test-post-method (name inst args inst-as return-values-as &rest body)
  "Call NAME(INST,*ARGS) binding to RETURN-VALUES-AS w/INST-AS then eval body"
  `(multiple-value-bind ,inst-as ,inst
     (multiple-value-bind ,return-values-as
	 ,(cons name (append inst-as args))
       ,@body)))

(defmacro test-pre-method (name args inst inst-as return-values-as &rest body)
  "Call NAME(*args,INST,**kws), bind to RETURN-VALUES-AS w/INST-AS, eval body"
  (let* (preargs
	 (kwargs (do* ((remain args (cdr remain))
		       (next (car remain) (car remain)))
		      ((or (typep next 'keyword) (null remain))
		       (progn (setf preargs (reverse preargs))
			      remain))
		   (push next preargs))))
    `(multiple-value-bind ,inst-as ,inst
       (multiple-value-bind ,return-values-as
	   ,(append (list name) preargs inst-as kwargs)
	 ,@body))))

(defparameter *tests* nil)

(defmacro deftests (&rest tests)
  "Expand a block of TESTS and push forms onto the *tests* list"
  (map nil #'(lambda (tst)
	       (push tst *tests*))
       tests))

(defmacro run-tests ()
  "Evaluate all forms in *tests* list, resetting list when[/if] complete"
  (let ((forms
	 (mapcar #'(lambda (tst i)
		     `(progn
			(format t "Running test ~A..." ,i)
			(when (null ,tst)
			  (format t "OK~&"))))
		 (reverse *tests*)
		 (do* ((i 1 (+ i 1))
		       (acc (list i) (cons i acc)))
		      ((> i (length *tests*)) (reverse acc))))))
    (list 'time
	  (cons 'progn
		(append forms
			(list '(setf deftest::*tests* nil)))))))
