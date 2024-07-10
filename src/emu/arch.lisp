;; Fully-software-emulated architectural components
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

(in-package :cl-vhdsl/emu)

;;---------- Memory ----------

(defclass memory ()
  ((size
    :documentation "Size of memory."
    :initarg :size
    :reader memory-size)
   (locations
    :documentation "The locations in the memory."
    :accessor memory-locations))
  (:documentation "A memory represented by an in-memory array.

Memory locations can hold values of any type -- typically bytes, of
course, and this is the default. But older processors often have
word-addressable memory, and there are applications requiring more
sophisticated structures."))


(defgeneric memory-initialise(mem)
  (:documentation "Initialise the initial value of all locations in MEM.

This will typically involve calling `make-array' to create the storage
and then initialising each value. The default creates a byte array
initialised with zeros."))


(defmethod memory-initialise ((mem memory))
  (setf (memory-locations mem)
	(make-array (list (memory-size mem))
		    :element-type 'unsigned-byte
		    :initial-element 0)))


(defgeneric memory-location (mem addr)
  (:documentation "Return address ADDR in MEM."))


(defmethod memory-location ((mem memory) addr)
  (aref (slot-value mem 'locations) addr))


(defmethod (setf memory-location) (v (mem memory) addr)
  (setf (aref (slot-value mem 'locations) addr) v))


;; ---------- Registers ----------

(defclass register ()
  ((name
    :documentation "The register's print name."
    :initarg :name)
   (value
    :documentation "The register's value."
    :initform 0
    :initarg :initial-value
    :accessor register-value)
   (width
    :documentation "The register's width in bits."
    :initform 8
    :initarg :width
    :reader register-width))
  (:documentation "An emulated register.

Registers provide sanity checks on the sizes of values written."))


(defmethod (setf register-value) (v (reg register))
  (if (>= (abs v) (floor (expt 2 (register-width reg))))
      (error 'illegal-register-access :register reg :value v)
      (setf (slot-value reg 'value) v)))


;; ---------- Flags ----------

(defclass flag ()
  ((register
    :documentation "The register containing the flag."
    :initarg :register
    :reader flag-register)
   (bit
    :documentation "The bot within the register."
    :initarg :bit
    :reader flag-bit))
  (:documentation "A one-bit flag within an emulated register."))


(defgeneric flag-value (f)
  (:documentation "The value of the flag F."))


(defmethod flag-value ((f flag))
  (logand 1 (ash (register-value (flag-register f)) (- (flag-bit f)))))


(defmethod (setf flag-value) (b (f flag))
  (let ((v (register-value (flag-register f)))
	(mask (ash 1 (flag-bit f))))
    (setf (register-value (flag-register f))
	  (if b
	      (logior v mask)
	      (logand v (lognot mask))))))
