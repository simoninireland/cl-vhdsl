;; Conditions
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


;; ---------- Instruction and addressing mode conditions ----------

(define-condition bad-addressing-mode (error)
  ((ins
    :documentation "The instruction."
    :type abstract-instruction
    :initarg :instruction)
   (mode
    :documentation "The addressing mode."
    :type addressing-mode
    :initarg :addressing-mode))
  (:report (lambda (c str)
	     (format str "Instruction ~s does not accept mode ~s"
		     (slot-value c 'ins)
		     (slot-value c 'mode))))
  (:documentation "Error signalled when an instruction is passed an addressing mode it can't use."))


(define-condition unknown-mnemonic (error)
  ((mnemonic
    :documentation "The mnemonic."
    :initarg :mnemonic))
  (:report (lambda (c str)
	     (format str "Unkown instruction mnemonic ~s"
		     (slot-value c 'moemonic))))
  (:documentation "Error signalled when an unrecognised instruction mnemonic is encountered."))
