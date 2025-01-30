;; Synthesising synthesisable components
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

;; ---------- Synthesising components ----------

(defun synthesise-module-args (c)
  "Return the RTLisp code to declare the module arguments.

The module arguments are constructed from the pin interface of C.

The code is a list of declarations suitable for a LET form."
  (let ((cl (class-of c)))

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

      (mapcar #'pin-interface-to-decl (pin-interface c)))))


(defun synthesise-module-params (c)
  "Return the RTLisp code to declare the module parameters.

The code is a list of declarations suitable for a LET form."
  (let ((cl (class-of c)))

    (flet ((param-to-decl (slot)
	     (let* ((slot-def (find-slot-def-in-class cl slot))
		    (name (slot-definition-name slot-def))
		    (v (if (slot-boundp c slot)
			   (slot-value c slot)
			   0)))
	       `(,name :initial-value ,v))))

      (mapcar #'param-to-decl (parameters c)))))


(defun synthesise-module ()
  "doc"
  )

(defmethod synthesise ((c component) context)
  (let ((modname (symbol-name (class-name (class-of c))))
	(modparams (synthesise-module-params c))
	(modargs (synthesise-module-args c))))
  `(module ,modname ,modparams ,modargs

	   (let ((a :width 8))
	     (setq a (+ a 1))))
  )


;; ---------- Synthesising mixins ----------

(defmethod synthesise ((c clocked) context)

  )
