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

(defun synthesise-module-args (cl)
  "Return the RTLisp code to declare the module arguments.

The module arguments are constructed from the pin interface of CL.

The code is a list of declarations suitable for a LET form."
  (flet ((pin-interface-to-arg (slot)
	   (let ((name (slot-name cl slot))
		 (width (slot-width cl slot))
		 (rep (slot-representation cl slot))
		 (direction (slot-direction cl slot)))
	     `(,name ,@(if width
			  `(:width ,width))
		     ,@(if direction
			  `(:direction ,direction))
		     ,@(if rep
			  `(:as ,rep))))))

    (mapcar #'pin-interface-to-arg (pin-interface cl))))


(defun synthesise-module-params (c)
  "Return the RTLisp code to declare the module parameters.

The module parameters are constructed from the slots of CL that are
exported from the component.

The code is a list of declarations suitable for a LET form."
  (flet ((pin-interface-to-param (slot)
	   (let ((name (slot-name cl slot))
		 (width (slot-width cl slot))
		 (rep (slot-representation cl slot))
		 (direction (slot-direction cl slot)))
	     `(,name ,@(if width
			  `(:width ,width))
		     ,@(if direction
			  `(:direction ,direction))
		     ,@(if rep
			  `(:as ,rep))))))

    (mapcar #'pin-interface-to-param (parameters (class-of c)))))


(defun synthesise-module ()
  "doc"
  )

(defmethod synthesise ((c component))
  )


;; ---------- Synthesising mixins ----------

(defmethod synthesise ((c clocked))

  )
