;; pin.lisp: Pins attached to components
;;
;; Copyright (C) 2023 Simon Dobson
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

(in-package :cl-vhdsl)

;; ---------- Pins ----------

(defclass pin ()
  ((component :initarg :part-of
	      :initform nil
	      :reader pin-component)
   (connection :initarg :connected-to
	       :initform nil
	       :accessor pin-connection)
   (value :initform 0
	  :accessor pin-value))
  (:documentation "A pin carrying a single bit value."))


;; Interface
(defgeneric pin-set-value (p v)
  (:documentation "Set the value of pin P to V.

This may give rise to triggered behavour in some pins."))


;; Implementation
(defmethod pin-set-value ((p pin) v)
  (setf (pin-value p) v))
