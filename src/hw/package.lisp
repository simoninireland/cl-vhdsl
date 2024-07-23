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
  (:use :cl :alexandria :serapeum)
  (:local-nicknames (:def :cl-vhdsl/def))
  (:import-from :closer-mop
		#:class-slots
		#:class-precedence-list
		#:slot-definition-name)
  (:import-from :slot-extra-options
		#:def-extra-options-metaclass
		#:slot-exists-and-bound-p)

  (:export
   ;; components
   #:metacomponent
   #:make-pin-for-role
   #:component
   #:component-pin
   #:component-enabled-p
   #:component-pin-changed
   #:component-pin-triggered
   #:clocked

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
   #:pin-for-wire
   #:pins-for-wires
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

   ;; arithmetic logic
   #:alu

   ;; conditions
   #:conflicting-asserted-values
   #:reading-floating-value
   #:unrecognised-alu-operation
   #:mismatched-wires
   ))
