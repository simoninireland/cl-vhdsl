;; Software-emulated control units
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

(in-package :cl-vhdsl/hw)

;; ---------- Control units ----------

;; The controller is a source of micro-instructions, in the sense that
;; it generates a stream of them to control its components. Where
;; those instructions come from is somewhat irrelevent: we shuld be
;; able to abstract over explicit decoding, microcoded instructions,
;; and other forms within the same framework.

(defclass control (component enabled clocked)
  ()
  (:documentation "A control unit.

Control units emit micro-instructions to control the rest of the system.
The micro-instuctions are themselves composed of nano-instructions that
define the states of control lines for components, one nano-instruction
per component.

The control unit itself is a finite-state machine that emits micro-instructions
when triggered by the clock.
"))
