;; Register definitions
;;
;; Copyright (C) 2024 Simon Dobson
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

;; ---------- Implementation ----------

(defclass register ()
  ((print-name
    :documentation "The print name of the register."
    :type string
    :initarg :print-name
    :reader register-name)
   (bit-width
    :documentation "The width of the register in bits."
    :type integer
    :initarg :bit-width
    :initform 8
    :reader register-width)
   (description
    :documentation "Description of the register."
    :type string
    :initarg :documentation))
  (:documentation "A register with a fixed bit-width."))


(defgeneric print-register (reg stream)
  (:documentation "Print the name of REG to STREAM"))


(defmethod print-register ((reg register) stream)
  (format stream "~a" (register-name reg)))


;; ---------- Macro interface ----------

(defmacro def-register (name &key (width 8) documentation)
  "Define NAME as a register of WIDTH bits.

WIDTH defaults to 8."
  (let ((print-name (symbol-name name)))
    `(defvar ,name (make-instance 'register
				  :print-name ,print-name
				  :bit-width ,width
				  :documentation ,documentation))))
