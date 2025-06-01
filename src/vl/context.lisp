;; The compiler context
;;
;; Copyright (C) 2024--2025 Simon Dobson
;;
;; This file is part of verilisp, a very Lisp approach to hardware synthesis
;;
;; verilisp is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; verilisp is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with verilisp. If not, see <http://www.gnu.org/licenses/gpl.html>.

(in-package :vl)


;; ---------- Environment ----------

(defvar *global-environment* (empty-environment)
  "The global environment for the compiler.")


(defun clear-global-environment ()
  "Clear the global environment.

This should, in principle, never be needed because the environment
should be handled correctly using WITH-NEW-FRAME. However...."
  (setf *global-environment* (empty-environment)))


(defmacro with-frame (f &body body)
  "Attach F to the current global environment for BODY."
  (with-gensyms (oldenv)
    `(let ((,oldenv *global-environment*))
       (unwind-protect
	    (progn
	      ;; attach the new frame to the global environment
	      (setq *global-environment* (attach-frame ,f ,oldenv))

	      ;; run the body in the extended environment
	      ,@body)

	 ;; detach the attached frame and restore the environment
	 (progn
	   (detach-frame *global-environment*)
	   (setq *global-environment* ,oldenv))))))


(defmacro with-new-frame (&body body)
  "Run BODY in an environment extended with an empty frame onto the current environment."
  `(with-frame (make-frame)
     ,@body))


(defun declare-variable (n props)
  "Declare a new variable N with properties PROPS in the global environment."
  (declare-environment-variable n props *global-environment*))


(defun variable-declared-p (n)
  "Test whether variable N is declared in the global environment."
  (variable-declared-in-environment-p n *global-environment*))


(defun variables-declared ()
  "Return the variables declared in the current global environment."
  (get-environment-names *global-environment*))


(defun variables-declared-in-current-frame ()
  "Return the variables declared in only the shallowest frame of the global environment."
  (get-frame-names *global-environment*))


(defun variable-properties (n)
  "Return the property list of variable N in the global environment."
  (get-environment-properties n *global-environment*))


(defun variable-property (n p &key default)
  "Return the value of property P of variable N in the global environment."
  (get-environment-property n p *global-environment* :default default))


(defun set-variable-property (n p v)
  "Set the value of property P of variable N in the global environment to V."
  (set-environment-property n p v *global-environment*))


;;---------- Common properties ----------

(defun get-type (n)
  "Return the type of N.

The type is the most definite of an inferred type (:INFERRED-TYPE),
any explicitly-provided type (:TYPE), and the default type (a
standard-width unsigned integer).

(The logic of this ordering is to allow the same function
to be used during and after type inferenece.)"
  (or (variable-property n :inferred-type :default nil)
      (variable-property n :type :default nil)
      `(unsigned-byte ,*default-register-width*)))


(defun get-representation (n)
  "Return the representation of N."
  (variable-property n :as))


(defun get-initial-value (n)
  "Return the initial value of N."
  (variable-property n :initial-value :default 0))


(defun get-constant (n)
  "Return whether N is constant."
  (if-let ((rep (get-representation n)))
    (eql rep :constant)))


(defun get-direction (n)
  "Return the direction of N."
  (variable-property n :direction))


;; ---------- Form context ----------

(defparameter *current-form-queue* nil
  "The current form being evaluated.

This is a stack of forms being evaluated, used to contextualise
conditions and change synthesis based on a form's position in
the larger program.")


(defmacro with-current-form (form &body body)
  "Execute BODY within the current FORM.

Any conditions reported in BODY will be pointed as FORM as the current
form."
  `(unwind-protect
	(progn
	  (push ,form *current-form-queue*)
	  ,@body)

     (pop *current-form-queue*)))


(defun current-form ()
  "Return the current form."
  (car *current-form-queue*))


(defun containing-form (&optional form)
  "Return the form containing the current form.

If FORM is provided then return the shallowest containing form
that is that form."
  (labels ((find-form (l)
	     (cond ((null l)
		    nil)

		   ((eql (caar l) form)
		    (car l))

		   (t
		    (find-form (cdr l))))))

    (if form
	;; search for the shallowest containing form with the given tag
	(find-form (cdr *current-form-queue*))

	;; extract the immediately containing form
	(cadr *current-form-queue*))))


(defun containing-containing-form ()
  "Return the form containing the containing form."
  (if (> (length *current-form-queue*) 2)
      (caddr *current-form-queue*)))


(defun in-top-level-context-p ()
  "Test whether the current context is top-level."
  (and (= (length *current-form-queue*) 1)
       (eql (car (current-form)) 'module)))


(defun in-block-context-p ()
  "Test whether the current context is within an @-block."
  (some (lambda (form)
	  (eql (car form) '@))
	*current-form-queue*))


(defun in-module-context-p ()
  "Test whether the current context is directly within a module."
  (not (in-block-context-p)))


(defun in-setf-context-p ()
  "Test if we're ina  SETF or SETQ context."
  (some (lambda (form)
	  (member (car form) '(setq setf)))
	*current-form-queue*))


(defun in-non-expression-context-p ()
  "Test whether we're in a non-expression context."
  (or (in-block-context-p)
      (in-module-context-p)
      (and (>= (length *current-form-queue*) 1)
	   (eql (car (containing-form)) 'progn))))


(defun in-let-decl-context-p ()
  "Test whether we're in a LET decl context."
  (some (lambda (form)
	  (variable-declared-p (car form)))
	*current-form-queue*))


(defun in-expression-context-p ()
  "Test whether we're in an expression context."
  (or (in-setf-context-p)
      (in-let-decl-context-p)
      (not (in-non-expression-context-p))))
