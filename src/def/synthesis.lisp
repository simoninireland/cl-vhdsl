;; Synthesising synthesisable components to RTLisp
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

(in-package :cl-vhdsl/def)
(declaim (optimize debug))


;; ---------- Module arguments and parameters ----------

(defun generate-module-args (c)
  "Return the RTLisp code to declare the module arguments.

The module arguments are constructed from the pin interface of C.

The code is a list of declarations suitable for a LET form."
  (flet ((pin-interface-to-decl (slot)
	   (let* ((slot-def (find-slot-def c slot))
		  (name (slot-definition-name slot-def))
		  (v (if (slot-boundp c slot)
			 (slot-value c slot)
			 0))
		  (width (slot-width c slot))
		  (rep (slot-representation c slot))
		  (direction (slot-direction c slot)))
	     `(,name ,@(if width
			   `(:width ,width))
		     ,@(if direction
			   `(:direction ,direction))
		     ,@(if (/= v 0)
			   `(:initial-value ,v))
		     ,@(if rep
			   `(:as ,rep))))))

    (mapcar #'pin-interface-to-decl (pin-interface c))))


;; ---------- Sub-components ----------

(defun generate-module-params (c)
  "Return the RTLisp code to declare the module parameters.

The code is a list of declarations suitable for a LET form."
  (flet ((param-to-decl (slot)
	   (let* ((slot-def (find-slot-def c slot))
		  (name (slot-definition-name slot-def))
		  (v (if (slot-boundp c slot)
			 (slot-value c slot)
			 0)))
	     `(,name :initial-value ,v))))

    (mapcar #'param-to-decl (parameters c))))


(defun generate-subcomponents (c)
  "Generate the sub-components of C by importing them."
  )


;; ---------- Wiring ----------

(defun generate-wire-to-subcomponent (c to from)
  "Generate wiring of TO on component C to FROM.

Functionally this involves changing the arguments to C when it is instanciated."
  (set-subcomponent-arg c to from))


(defun generate-wire (wires cs)
  "Return the assignments needed to wire-up the elements in CS onto WIRES."
  (flet ((wire-up (ws w)
	   (destructuring-bind (from to)
	       w
	     (cond ((and (listp from)
			 (not (listp to)))
		    ;; from sub-component to wires
		    (destructuring-bind (c cw)
			from
		      (generate-wire-to-subcomponent c cw to)
		      ws))

		   ((and (listp to)
			 (not (listp from)))
		    ;; from wires to sub-component
		    (destructuring-bind (c t)
			to
		      (generate-wire-to-subcomponent c cw from)
		      ws))

		   ((and (not (listp from))
			 (not (listp to)))
		    ;; between wires
		    (append ws (list`(setq ,from ,to))))

		   (t
		    (error 'unsupported :feature "Wiring components directly together"
					:hint "Add an explicit wire to connect to"))))))

    (let ((ws (successive-pairs cs)))
      (append wires (foldr #'wire-up ws '())))))


(defun generate-wiring (c)
  "Return the RTLisp code for wiring component C.

This code is inserted as the \"asynchronous assignments\" at the
end of the module's code block."
  (let ((diag (wiring-diagram c)))
    (foldr #'generate-wire diag nil)))


;;---------- Components ----------

(defmethod to-rtl ((c component))
  (let* ((modname (class-name (class-of c)))
	 (modparams (generate-module-params c))
	 (modargs (generate-module-args c))
	 (modheader (if modparams
			`(,modparams ,modargs)
			modargs))
	 (wiring (generate-wiring c)))

    `(module ,modname ,modheader
	     ,@wiring
	     )))


;; ---------- Mixins ----------
