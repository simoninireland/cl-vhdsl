;; Package definition for the hardware simulation package
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

(in-package :common-lisp-user)

(defpackage cl-vhdsl/hw
  (:use :cl :alexandria :serapeum :cl-vhdsl)
  (:local-nicknames (:def :cl-vhdsl/def))
  (:import-from :closer-mop
		#:standard-class
		#:class-slots
		#:compute-slots
		#:slot-definition-type
		#:slot-definition-name
		#:compute-effective-slot-definition
		#:effective-slot-definition-class
		#:class-precedence-list
		#:validate-superclass
		#:slot-value-using-class
		#:slot-definition-name)
  (:import-from :slot-extra-options
		#:def-extra-options-metaclass
		#:slot-exists-and-bound-p)

  (:export
   ;; components
   #:metacomponent
   #:make-pin-for-role
   #:pin-role-for-slot
   #:pin-slots-for-roles
   #:pins-for-slot
   #:pin-interface
   #:pin-interface-p
   #:component
   #:component-name
   #:component-pin
   #:component-enabled-p
   #:component-pin-changed
   #:component-pin-triggered
   #:clocked
   #:readwrite
   #:component-write-enabled-p
   #:component-read-enabled-p

   ;; wires and pins
   #:wire
   #:wire-state
   #:wire-pins
   #:wire-add-pin
   #:pin
   #:pin-state
   #:pin-wire
   #:pin-component

   ;; accessing multiple pins simultaneously
   #:pins-states
   #:pins-floating
   #:pins-to-value
   #:pins-from-value

   ;; buses
   #:bus
   #:bus-width
   #:bus-wires

   ;; registers
   #:register
   #:register-width
   #:register-value

   ;; ring counters
   #:ring-counter
   #:ring-counter-reset

   ;; arithmetic logic
   #:alu

   ;; RAM
   #:ram
   #:ram-size
   #:ram-elements

   ;; micro-instructions
   #:microinstruction
   #:run-microinstruction
   #:defmicroinstruction

   ;; debugging support
   #:components-seen-by

   ;; conditions
   #:conflicting-asserted-values
   #:reading-floating-value
   #:unrecognised-alu-operation
   #:mismatched-wires
   #:non-component-type
   ))
