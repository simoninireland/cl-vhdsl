;; Instruction definitions
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

;; ---------- Addressing modes ----------

(defclass addressing-mode ()
  ()
  (:documentation "A description of an addressing mode."))


(defgeneric print-address (mode stream)
  (:documentation "Print the value of the address MODE on STREAM."))


;; ---------- Instructions ----------

(defclass instruction ()
  ((mnemonic
    :documentation "The mnemonic (also the print name) of the instruction."
    :type string
    :initarg :mnemonic
    :reader mnemonic))
  (:documentation "An assembly language instruction."))


;; ---------- Macro interface ----------
