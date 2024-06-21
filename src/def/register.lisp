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

;; ---------- Registers ----------

(defclass register ()
  ((print-name
    :documentation "The print name of the register."
    :type string
    :initarg :name
    :reader register-name)
   (bit-width
    :documentation "The width of the register in bits."
    :type integer
    :initarg :width
    :reader register-width)
   (description
    :documentation "Description of the register."
    :type string
    :initarg :documentation))
  (:documentation "A register with a fixed bit-width."))


(defclass data-register (register)
  ()
  (:documentation "A register holding general-purpose data."))


(defclass index-register (register)
  ()
  (:documentation "A register holding offsets for addressing."))


(defclass address-register (register)
  ()
  (:documentation "A register holding addresses."))


(defclass special-register (register)
  ()
  (:documentation "A register used internally by the processor and offering bitwise access."))


;; ---------- Flags within registers ----------

(defclass flag ()
  ((print-name
    :documentation "The print name of the flag."
    :type string
    :initarg :name
    :reader flag-name)
   (register
    :documentation "The register holding the flag."
    :type special-register
    :initarg :register
    :reader flag-register)
   (bit
    :documentation "The bit within the register."
    :type integer
    :initarg :bit
    :reader flag-bit)
   (description
    :documentation "Description of the flag."
    :type string
    :initarg :documentation))
  (:documentation "A one-bit flag within a register."))


;; ---------- Description of an architecture ----------

(defclass architecture ()
  ((memory
    :documentation "The available memory in bytes."
    :initform (floor (expt 2 16)) ;; 64Kb
    :initarg :memory-size
    :accessor architrecture-memory-size))
  (:documentation "Full architectural description of a processor."))




;; ---------- Macro interface ----------

(defmacro defregister (name &key (width 8) documentation)
  "Define NAME as a register of WIDTH bits.

WIDTH defaults to 8."
  (let ((print-name (symbol-name name)))
    `(defvar ,name (make-instance 'register
				  :name ,print-name
				  :width ,width
				  :documentation ,documentation))))
