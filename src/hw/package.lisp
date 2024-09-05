;; Package for fully-software-emulated hardware
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
		#:class-direct-slots
		#:ensure-class-using-class
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
  (:import-from #:cl-ppcre
		#:scan-to-strings)

  (:export
   ;; ---------- Top-level interface ----------

   ;; elements
   #:wire
   #:bus
   #:pin
   #:connector
   #:component

   ;; MOP
   #:metacomponent
   #:guarded

   ;; common generic operations over elements
   #:name
   #:pins
   #:wires
   #:components
   #:width
   #:state
   #:fully-wired-p
   #:floating-p

   ;; standard components and mixins
   #:enabled
   #:clocked
   #:readwrite
   #:register
   #:register-value
   #:alu
   #:ram
   #:ram-size
   #:ram-elements
   #:ring-counter

   ;; well-known pins
   #:clock
   #:enable
   #:readwrite

   ;; operations
   #:ensure-pin-state
   #:ensure-fully-wired
   #:pin-interface
   #:pin-interface-p
   #:pin-states
   #:pins-value
   #:configure-pin-for-role
   #:slot-connector
   #:enabled-p
   #:write-enabled-p
   #:read-enabled-p
   #:subcomponent-p
   #:subcomponent-interface
   #:wiring-diagram
   #:connect-slots
   #:connect-pins

   ;; behavioural callbacks
   #:on-pin-changed
   #:on-pin-triggered
   #:on-enabled
   #:on-disabled

   ;; macro interface
   #:defcomponent
   #:connect-component

   ;; debugging
   #:components-seen-by

   ;; conditions
   #:conflicting-asserted-values
   #:reading-floating-value
   #:reading-non-reading-pin
   #:unrecognised-alu-operation
   #:mismatched-wires
   #:non-component-type
   #:non-pin-interface-slot
   #:invalid-wiring-diagram-slot
   #:incompatible-pin-widths
   #:incompatible-pin-slot-widths
   #:invalid-endpoint
   #:not-fully-wired

   ;; ---------- Inner interface ----------

   ;; components
   #:configure-pin-for-role
   #:pin-role-for-slot
   #:pin-slots-for-roles

   ;; wires and pins
   #:wire-state
   #:wire-pin-assertions
   #:wire-add-pin
   #:pin-tristated-p
   #:pin-reading-p
   #:pin-asserted-p
   #:pin-floating-p
   #:pin-wired-p
   #:pin-state

   ;; micro-instructions
   #:microinstruction
   #:run-microinstruction
   #:defmicroinstruction
   ))
