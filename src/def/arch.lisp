;; Architectural description and components
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

;; ---------- Architecture ----------

(defclass architecture ()
  ((components-plist
    :documentation "The components of the architecture."
    :initform '()
    :accessor architecture-components))
  (:documentation "The description of a machine."))


;; ---------- Components ----------

(defclass component ()
  ()
  (:documentation "A component within an architecture.

A component represents a piece of hardware, that is to be enumalted
or synthesised in the final system."))


;; ---------- Cores ----------

(defclass core (component)
  ((registers
    :documentation "The registers defined in the core."
    :initform '()
    :accessor core-registers)
   (flags
    :documentation "The status flags defined in the core."
    :initform '()
    :accessor core-flags)
   (instruction-set
    :documentation "The instruction set of the core."
    :initform '()
    :accessor core-instruction-set))
  (:documentation "A processor core."))


;; ---------- Memory ----------

(defclass memory (component)
  ((size
    :documentation "Size of memory in bytes."
    :initarg :size
    :initform (* 64 KB)                                 ;; 64KB by default
    :reader memory-size))
  (:documentation "A linear byte-addressable RAM."))


;; ---------- Buses ----------

(defclass bus (component)
  ((connections-plist
    :documentation "Components on this bus."
    :initform '()
    :initarg :connections
    :reader bus-connections))
  (:documentation "A bus connecting several other components."))
