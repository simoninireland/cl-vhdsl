;; Fully-software-emulated registers
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

(defclass register ()
  ((value
    :documentation "Register current value."
    :initform 0
    :accessor register-value)
   (name
    :documentation "Register print name."
    :initarg :name
    :reader register-name)
   (width
    :documentation "Width of the register in bytes."
    :initform 8
    :initarg :width
    :reader register-width))
  (:documentation "A register held as a variable."))


(defmethod (setf register-value) (v (r register))
  ;; check that the proposed value fits into the register
  (if (>= v (floor (expt 2 (register-width r))))
      (error (make-instance 'register-overflow :register r :value v))
      (setf (slot-value r 'value) v)))


(defclass flag ()
  ((register
    :documentation "The register contaiing the flag."
    :initarg :register
    :reader flag-register)
   (bit
    :documentation "The bit within the register."
    :initarg :bit
    :accessor flag-bit))
  (:documentation "A single-bit flag within a register."))


(defmethod flag-bit ((f flag))
  (logand 1 (ash (register-value (flag-register f)) (- (flag-bit f)))))


(defmethod (setf flag-bit) (b (f flag))
  (setf (ldb (flag-bit f) (register-value (flag-register f))) b))
