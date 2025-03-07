;; Environments
;;
;; Copyright (C) 2024--2025 Simon Dobson
;;
;; This file is part of cl-vhdsl, a Common Lisp DSL for hardware design
;;
;; cl-vhdsl is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; cl-vhdsl is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with cl-vhdsl. If not, see <http://www.gnu.org/licenses/gpl.html>.

(in-package :cl-vhdsl/rtl)


;; ---------- Defaults ----------

(defparameter *default-register-width* 8
  "Default width for registers.

This is used absent any specfic width specification. It will
generally reflect the word size of the desired circuit, for
example 8, 16, 32, or 64 bits.")


;; ---------- Type checking ----------

(defun ensure-subtype (ty1 ty2)
  "Ensure TY1 is a sub-type of TY2.

Signals TYPE-MISMATCH is the types are not compatible. This
can be ignored for systems not concerned with loss of precision."
  (if (not (subtypep ty1 ty2))
      (signal 'type-mismatch :expected ty2 :got ty1)))


;; ---------- Type environments and frames ----------

(defun empty-frame ()
  "Return a new empty frame."
  (copy-list '(:frame)))


(defun empty-environment ()
  "Return a new empty environment consisting of a single frame."
  (list (empty-frame)))


(defun add-frame (env)
  "Return a new environment consisting of ENV with a new empty frame.

ENV is unchanged by this operation."
  (cons (empty-frame) env))


(defun declare-variable (n props env)
  "Declare a variable N with properties PROPS in the shallowest frame of ENV.

Signals a DUPLICATE-VARIABLE error if the variable already exists in this frame."
  (let ((frame (car env)))
    (when (member n (cdr frame) :key #'car)
      (error 'duplicate-variable :variable n))

    (setf (cdr frame) (cons (cons n props) (cdr frame)))
    n))


;; ---------- Frame operations ----------

(defun get-frame-bindings (env)
  "Return the list of bindings in the topmost frame of ENV."
  (cdar env))


(defun get-frame-properties (n env)
  "Return the properties of N in the top frame of ENV.
N can be a symbol (usually) or a string. In the latter case the
variable is checked by string equality aginst the symbol name.

An UNKNOWN-VARIABLE error is signalled if N is undefined."
  (if-let ((kv (if (symbolp n)
		   (assoc n (get-frame-bindings env))
		   (assoc n (get-frame-bindings env)
			  :key #'symbol-name :test #'string-equal))))
    (cdr kv)

    (error 'unknown-variable :variable n
			     :hint "Make sure the variable is in scope in the current frame")))


(defun get-frame-names (env)
  "Return the names of the variables in the topmost frame of ENV."
  (mapcar #'car (cdr (car env))))


(defun variable-declared-in-frame-p (n env)
  "Test whether N is defined in the topmost frame of ENV."
  (not (null (member n (get-frame-names env)))))


(defun get-frame-property (n prop env &key default)
  "Return the property PROP of variable N in the topmost frame of ENV.

Undeclared properties value value NIL, which can be changed using the
:DEFAULT argument. An UNKNOWN-VARIABLE error is signalled if N is
undefined."
  (let ((props (get-frame-properties n env)))
    (if-let ((m (assoc prop props)))
      ;; we found a binding, return the property
      (cadr m)

      ;; no property defined, return the default
      default)))


(defun set-frame-property (n prop v env)
  "Set the value of PROP to V for variable N in the topmost frame of ENV.

The property is updated if it is defined, and created if not.
An UNKNOWN-VARIABLE error is signalled if N is undefined."
  (declare (optimize debug))
  (let ((props (get-frame-properties n env)))
    (if-let ((m (assoc prop props)))
      ;; property exists, update it
      (setf (cdr m) (list v))

      ;; property does not exist, add it
      (let ((e (last props)))
	(setf (cdr e) (list (list prop v)))))))


;; ---------- Global environment operations ----------

(defun get-environment-properties (n env)
  "Return the key/value list for N in ENV.

N can be a symbol (usually) or a string. In the latter case the
variable is checked by string equality aginst the symbol name.

An UNKNOWN-VARIABLE error is signalled if N is undefined."
  (cond ((variable-declared-in-frame-p n env)
	 (get-frame-properties n env))

	((null env)
	 (error 'unknown-variable :variable n
				  :hint "Make sure the variable is in scope"))

	(t
	 (get-environment-properties n (cdr env)))))


(defun get-environment-names (env)
  "Return the names in ENV."
  (foldr #'union (mapcar (lambda (frame)
			   (get-frame-names (list frame)))
			 env)
	 '()))


(defun get-environment-property (n prop env &key default)
  "Return PROP for N in ENV.

If there is no property associated with N then NIL will be
returned: this can be changed by defining the :DEFAULT argument."
  (if-let ((p (assoc prop (get-environment-properties n env))))
    (cadr p)

    ;; property doesn't exist, return the default
    default))


(defun variable-declared-p (n env)
  "Test whether N is declared in ENV."
  (not (null (member n (get-environment-names env)))))


(defun filter-environment (pred env)
  "Return an environment containing all the entries of ENV matching PRED.

PRED should be a predicate taking a name and the environment with the
frame containing that name on top (so that a call to GET-ENVIRONMENT-PROPERTY
will return the correct value for that variable at that depth)."
  (foldl (lambda (frame fenv)
	   (cons (cons (car frame)
		  (remove-if-not (lambda (decl)
				   (funcall pred (car decl) (list frame)))
				 (cdr frame)))
		 fenv))
	  env
	  '()))


(defun map-environment (f env)
  "Map F across each declaration in ENV.

F should be a function taking a name and an environment. The
result is a list of the values returned from F. The list will be
flat, regardless of the frame structure of ENV,"
  (apply #'append (foldl (lambda (frame fenv)
			    (cons (mapcar (lambda (decl)
					    (funcall f (car decl) (list frame)))
					  (cdr frame))
				  fenv))
			  env
			  '())))


;;---------- Common property access ----------

(defun get-type (n env)
  "Return the type of N in ENV."
  (get-environment-property n :type env))


(defun get-representation (n env)
  "Return the representation of N in ENV."
  (get-environment-property n :as env))


(defun get-width (n env)
  "Return the width of N in ENV."
  (or (get-environment-property n :width env)

      ;; default value
      *default-register-width*))


(defun get-initial-value (n env)
  "Return the initial value of N in ENV."
  (or (get-environment-property n :initial-value env)

      ;; default value
      0))


(defun get-constant (n env)
  "Return whether N is constant in ENV."
  (if-let ((rep (get-representation n env)))
    (eql rep :constant)))


(defun get-direction (n env)
  "Return the direction of N in ENV."
  (get-environment-property n :direction env))
