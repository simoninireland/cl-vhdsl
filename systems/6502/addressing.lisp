;; 6502 addressing modes
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

(in-package :cl-vhdsl/systems/6502)

;; ---------- Implicit ----------

(defclass implicit (addressing-mode)
  ()
  (:documentation "Implicit addressing, with no literal value or address."))


(defmethod addressing-mode-data ((mode implicit) arch)
  nil)


(defmethod addressing-mode-regexp ((cls (eql 'implicit)))
  "")


(defmethod addressing-mode-bytes ((mode immediate))
  nil)


;; ---------- Immediate ----------

(defclass immediate (addressing-mode)
  ((value
    :documentation "The value, as an unsigned 8-bit byte."
    :type word-8
    :initarg :value
    :reader immediate-value))
  (:documentation "Immediate addressing, with an inline 8-bit value."))


(defmethod addressing-mode-data ((mode immediate) arch)
  (immediate-value mode))


(defmethod addressing-mode-regexp ((cls (eql 'immediate)))
  "#((?:[0-9]+)|(?:[0-9a-fA-F]+H))")


(defmethod addressing-mode-bytes ((mode immediate))
  (list (immediate-value mode)))


;; ---------- Absolute ----------

(defclass absolute (addressing-mode)
  ((address
    :documentation "The address, a 16-bit word."
    :type word-16
    :initarg :address
    :reader absolute-address))
  (:documentation "Absolute addressing, with an inline 16-bit address.

The address is an absolute address within the entire address
space of the processor."))


(defmethod addressing-mode-data ((mode absolute) arch)
  (memory-read-byte (architecture-memory arch) (absolute-address mode)))


(defmethod addressing-mode-regexp ((cls (eql 'absolute)))
  "((?:[0-9]+)|(?:[0-9a-fA-F]+H))")


(defun little-endian-word-16 (w)
  "Return a list of bytes in the 16-bit word, little-endian."
  (let ((l (logand w #16rFF))
	(h (logand (ash w -8) #16rFF )))
    (list l h)))


(defmethod addressing-mode-bytes ((mode absolute))
  (little-endian-word-16 (absolute-address mode)))


;; ----------  Zero-page absolute ----------

(defclass zero-page (addressing-mode)
  ((offset
    :documentation "The offset into page zero, an 8-bit byte."
    :type word-8
    :initarg :address
    :reader zero-page-address))
  (:documentation "Absolute zero-page addressing, with an inline 8-bit offset.

The offset is used to construct an address within page zero."))


(defmethod addressing-mode-bytes ((mode zero-page))
  (little-endian-word-8 (zero-page-address mode)))


;; ---------- Absolute indexed ----------

(defclass absolute-indexed (absolute)
  ((index
    :documentation "The index register to add to the address."
    :type index-register
    :initarg :indexed-by
    :reader absolute-indexed-index))
  (:documentation "An absolute address plus the contents of an index register.

The address is an absolute address wihtin the entire address
space of the processor."))


(defmethod addressing-mode-data ((mode absolute) arch)
  (let ((index (absolute-indexed-index mode))))
  (memory-read-byte mem (+ (absolute-address mode)
			   (X arch))))


(defmethod addressing-mode-regexp ((cls (eql 'absolute-indexed)))
  "((?:[0-9]+)|(?:[0-9a-fA-F]+H)), \\s*([XY])")


(defmethod addressing-mode-bytes ((mode absolute-indexed))
  (little-endian-word-16 (absolute-address mode)))


;; ---------- Relative ----------

(defclass relative (zero-page)
  ()
  (:documentation "Reletive addressing, with an inline 8-bit offset.

Rather than spcifying an offset into page zero, the offset is
relative to the current program counter."))


(defmethod addressing-mode-bytes ((mode relative))
  (little-endian-word-8 (relative-offset mode)))


;; ---------- Indexed indirect ----------

(defclass indexed-indirect (zero-page)
  ((index
    :documentation "The index register to add to the address."
    :type index-register
    :initarg :indexed-by
    :reader indexed-indirect-index))
  (:documentation "An offset into page zero plus the contents of an index register.

The offset is used to generate an address in page zero, to which
the contents of the index register are added."))


(defmethod addressing-mode-bytes ((mode indexed-indirect))
  (little-endian-word-16 (indexed-indirect-address mode)))


;; ---------- Indirect indexed ----------

(defclass indirect-indexed (zero-page)
  ((index
    :documentation "The index register to add to the address."
    :type index-register
    :initarg :indexed-by
    :reader indirect-indexed-index))
  (:documentation "An offset into page zero plus the contents of an index register.

The offset is used to generate an offset into page zero which
is retrieved as an absolute address, and then the contents of the
index register are added to it."))


(defmethod addressing-mode-bytes ((mode indirect-indexed))
  (little-endian-word-16 (indirect-indexed-address mode)))


;; ---------- Addressing mode encoding/decoding ----------

;; 6502 instructions encode their addressing mode using a small
;; bit pattern within the opcode.

(defgeneric addressing-mode-encode (mode)
  (:documentation "Return the code used for MODE in opcodes."))


(defmethod addressing-mode-encode ((mode absolute)) #2r011)
(defmethod addressing-mode-encode ((mode immediate)) #2r010)
(defmethod addressing-mode-encode ((mode absolute-indexed))
  (if (equal (absolute-indexed-index mode) X)
      #2r111
      #2r110))


;; ---------- Assembler ----------

(defun parse-instruction-arguments (args)
  "Convert ARGS into an addressing mode object."
  (cond ((atom args)
	 (make-instance 'immediate :value args))
	((equal (length args) 1)
	 (make-instance 'absolute) :address (car args))
	((equal (length args) 2)
	 (make-instance 'absolute-indexed :address (car args) :index (cadr args)))
	(t
	 (signal 'unrecognised-addressing-mode :args args))))
