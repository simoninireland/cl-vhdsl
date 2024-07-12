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
  ((print-name
    :documentation "The register's print name."
    :initarg :name
    :reader register-name)
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
  ((print-name
    :documentation "The register's print name."
    :initarg :name
    :reader flag-name)
   (register
    :documentation "The register containing the flag."
    :initarg :register
    :reader flag-register)
   (bit
    :documentation "The bit within the register."
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


;; ---------- Cores ----------

(defclass core ()
  ((register-table
     :documentation "Hash table mapping register names to emulated registers."
     :initform (make-hash-table)
     :reader core-registers)
   (flag-table
    :documentation "Hash table mapping flag names to emulated registers."
    :initform (make-hash-table)
    :reader core-flags)
   (memory
    :documentation "Main memory."
    :initarg :memory
    :reader core-memory))
  (:documentation "An emulated core."))


(defgeneric core-add-register (c r)
  (:documentation "Add register R to core ."))


(defmethod core-add-register ((c core) r)
  (setf (gethash (register-name r) (core-registers c)) r))


(defgeneric core-add-flag (c f)
  (:documentation "Add lag F to core ."))


(defmethod core-add-flag ((c core) f)
  (setf (gethash (flag-name f) (core-flags c)) f))


(defgeneric core-register (rname c)
  (:documentation "Return the  register RNAME on core C"))


(defmethod core-register (rname (c core))
  (gethash rname (core-registers c)))


(defgeneric core-register-value (rname c)
  (:documentation "Return the value of register RNAME on core C"))


(defmethod core-register-value (rname (c core))
  (register-value (core-register rname c)))


(defmethod (setf core-register-value) (v rname (c core))
  (setf (register-value (core-register rname c)) v))


(defgeneric core-flag (fname c)
  (:documentation "Return the flag FNAME on core C"))


(defmethod core-flag (fname (c core))
  (gethash fname (core-flags c)))


(defgeneric core-flag-value (fname c)
  (:documentation "Return the value of flag FNAME on core C"))


(defmethod core-flag-value (fname (c core))
  (flag-value (core-flag fname c)))


(defmethod (setf core-flag-value) (v fname (c core))
  (setf (flag-value (core-flag fname c)) v))


(defgeneric core-memory-location (addr c)
  (:documentation "Return the value stord at memory address ADDR on core C"))


(defmethod core-memory-location (addr (c core))
  (memory-location (core-memory c) addr))


(defmethod (setf core-memory-location) (v addr (c core))
  (setf (memory-location (core-memory c) addr) v))
