;; Arrays of variables
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
(declaim (optimize debug))


;; ---------- Array construction ----------

(defun valid-array-shape-p (shape env)
  "Test that SHAPE is a valid array shape in ENV.

At the moment this means a one-dimensional list of integers
whose values are statically determinable."
  (handler-bind ((error nil))
    (and (listp shape)
	 (= (length shape) 1)
	 (every (rcurry #'eval-in-static-environment env) shape))))


(defun ensure-valid-array-shape (shape env)
  "Ensure SHAPE is a valid array shape in ENV."
  (unless (valid-array-shape-p shape env)
    (error 'type-mismatch :expected "constant" :got "something else"
			  :hint "Make sure shape is valid for an array")))


(defmacro unquote (place)
  "Remove any leading quote from the data in PLACE.

This is mainly used to preserve compatability with Lisp, where
these forms need to be quoted. We /allow/ them to be quoted in
RTLisp, but don't /require/ it."
  `(if (and (not (null ,place))
	    (listp ,place)
	    (eql (car ,place) 'quote))
       (setq ,place (cadr ,place))))


(defun data-has-shape-p (data shape)
  "Test whether DATA has the given SHAPE."
  (and (= (length shape) 1)
       (= (length data) (car shape))))


(defun ensure-data-has-shape (data shape)
  "Ensure DATA has the given SHAPE."
  (unless (data-has-shape-p data shape)
    (signal 'shape-mismatch :expected shape
			    :hint "Ensure initial contents have the right shape")))


;; shape needs to be made up of constants (or parameters)

(defmethod typecheck-sexp ((fun (eql 'make-array)) args env)
  (destructuring-bind (shape &key
			       initial-element initial-contents
			       element-width element-type)
      args
    ;; skip an initial quotes, allowed for Lisp compatability
    (unquote shape)
    (unquote element-type)
    (unquote initial-contents)

    (ensure-valid-array-shape shape env)

    ;; check initial element value
    (if initial-element
	(let* ((ty (typecheck initial-element env))
	       (w (bitwidth ty env)))
	  (if element-width
	      (unless (<= w element-width)
		(signal 'type-mismatch :expected element-width :got w
				       :hint "Ensure variable is wide enough for initial element value"))
	      (setq element-width w)))

	;; set to zero if omitted
	(setq initial-element 0))

    ;; check or derive element width
    (if element-width
	(ensure-width-can-store element-width (typecheck initial-element env) env)

	;; set width from initial value
	(setq element-width (bitwidth initial-element env)))

    ;; check or derive element type
    (if element-type
	(ensure-subtype (typecheck initial-element env) element-type)

	;; default is a fixed-width unsigned
	(setq element-type`(fixed-width-unsigned ,element-width)))

    ;; initial contents must either match the size of the array
    ;; or identify a file
    (if initial-contents
	(if (listp initial-contents)
	    (cond ((eql (car initial-contents) :file)
		   (error 'not-synthesisable :hint "File-based array initialisation not yest implemnted"))

		  (t
		   (ensure-data-has-shape initial-contents shape)
		   (dolist (c initial-contents)
		     (ensure-subtype (typecheck c env) element-type))))))

    `(array ,element-type ,@shape)))


;; Only works for one-dimensional arrays at the moment
;; Should expand constants

(defun synthesise-array-init (data shape)
  "Return the initialisation of DATA with the given SHAPE."
  (as-literal " = " :newline t)

  (with-indentation
    (as-list data :inexpression
	     :before "{ " :after " }"
	     :per-row 5)))


(defmethod synthesise-sexp ((fun (eql 'make-array)) args (context (eql :indeclaration)))
  (destructuring-bind (shape &key initial-element initial-contents)
      args
    ;; skip an initial quote, allowed for Lisp compatability
    (unquote shape)
    (unquote initial-contents)

    (as-literal "[ ")
    (synthesise (car shape) :inexpression)
    (as-literal " - 1 : 0 ]")

    (if initial-contents
	(synthesise-array-init initial-contents shape))))

(defmethod synthesise-sexp ((fun (eql 'make-array)) args (context (eql :inexpression)))
  (synthesise-sexp fun args :indeclaration))


;; ---------- Array access ----------

(defun valid-array-index-p (ty indices env)
  "Ensure INDICES are a potentially valid index into TY in ENV.

TY must be an array type, and INDICES must have the correct
dimensions, and must be unsigned integers.

We don't check the validity of the values -- although we could, and
probably should, for those that are statically determined."
  (and (subtypep ty 'array)
       (or (not (listp ty))
	   (= (length (cddr ty))
	      (length indices)))
       (every (lambda (i)
		(subtypep (typecheck i env)
			  'fixed-width-unsigned))
	      indices)))


(defun ensure-valid-array-index (ty indices env)
  "Ensure INDICES are valid for accessing TY in ENV."
  (unless (valid-array-index-p ty indices env)
    (error 'type-mismatch :expected ty :got indices
			  :hint "Indices must match array dimension")))


(defun element-type-of-array (ty)
  "Extract the element type of array TY."
  (cadr ty))


(defmethod typecheck-sexp ((fun (eql 'aref)) args env)
  (destructuring-bind (var &rest indices)
      args
    (let ((ty (typecheck var env)))
      (ensure-subtype ty 'array)
      (ensure-valid-array-index ty indices env)

      ;; the type is the type of the elements
      (element-type-of-array ty))))


(defmethod generalised-place-sexp-p ((fun (eql 'aref)) args env)
  t)


(defmethod synthesise-sexp ((fun (eql 'aref)) args (context (eql :inexpression)))
  (destructuring-bind (var &rest indices)
      args
    (synthesise var :inexpression)
    (as-literal "[ ")
    (as-list indices :inexpression)
    (as-literal " ]")))
(defmethod synthesise-sexp ((fun (eql 'aref)) args (context (eql :inassignment)))
  (synthesise-sexp fun args :inexpression))


(defmethod lispify-sexp ((fun (eql 'aref)) args env)
  (destructuring-bind (var &rest indices)
      args
    `(aref ,var ,@indices)))
