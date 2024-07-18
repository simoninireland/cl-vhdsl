;; Conditions for fully-software-emulated architectures
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

(define-condition illegal-memory-access (error)
  ((address
    :documentation "Memory address accessed."
    :initarg :address))
  (:report (lambda (c str)
	     (format str "Illegal memory access: ~xH" (slot-value c 'address))))
  (:documentation "Signalled when an illegal memory address is accessed.

This can happen if the address is outside the valif range of
the memory, or if the location is unreadable or unwriteable
for some other reason."))


(define-condition illegal-register-access (error)
  ((register
    :documentation "The register causing the condition.")
   (value
    :documentation "The value beign written."))
  (:report (lambda (c str)
	     (format str "Illegal access to register ~s (writing ~a)"
		     (slot-value c 'register)
		     (slot-value c 'value))))
  (:documentation "Condition signalled when a register is accessed illegally.

This typically means that the register is receiving a value
that's too wide to it."))
