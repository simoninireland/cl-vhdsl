;; The SAP-1 processor core
;;
;; Copyright (C) 2024--2025 Simon Dobson
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

(in-package :cl-vhdsl/examples/sap-1-component)


(defclass core (component clocked resetable)
  ((bus
    :documentation "The processor bus."
    :as :wire
    :width 8)
   (a
    :documentation "The accumulator A."
    :as :subcomponent
    :initform (make-instance 'register :name "A" :width 8))
   (b
    :documentation "The ALU B register."
    :as :subcomponent
    :initform (make-instance 'register :name "B" :width 8))
   (ir
    :documentation "The instruction register IR."
    :as :subcomponent
    :initform (make-instance 'register :name "IR" :width 8))
   (pc
    :documentation "The program counter PC."
    :as :subcomponent
    :initform (make-instance 'register :name "PC" :width 8))
   (memory
    :documentation "The memory."
    :as :subcomponent
    :initform (make-instance 'memory :name "Memory" :width 8 :depth 16))
   (adder
    :documentation "The adder."
    :as :subcomponent
    :initform (make-instance 'adder :name "Adder" :width 8))
   (controller
    :documentation "The controller."
    :as :subcomponent
    :initform (make-instance 'controller :name "Controller")))

  (:wiring
   ;; global clock and reset
   (clk (a clk) (b clk) (pc clk) (ir clk) (controller clk))
   (rst (a rst) (b rst) (pc rst) (ir rst) (controller rst))

   ;; adder
   ((a alu-bus) (adder a-bus))
   ((b alu-bus) (adder b-bus))
   (bus (alu c-bus))

   ;; maths registers
   (bus (a data-bus))
   (bus (b data-bus))

   ;; special registers
   (bus (pc data-bus))
   (bus (ir data-bus))
   )
  (:documentation "The SAP-1 processor core."))
