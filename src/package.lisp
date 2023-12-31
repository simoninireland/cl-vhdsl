;; cl-vhdsl.lisp: Packasge definition for cl-vhdsl
;;
;; Copyright (C) 2023 Simon Dobson
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

(defpackage cl-vhdsl
  (:use :cl
	:alexandria
	:cl-bitfields)
  (:export ;; utilities
	   #:ensure-binary
	   #:indexed-list

	   ;; pins
	   #:pin
	   #:pin-compoent
	   #:pin-connection
	   #:pin-value
	   #:pin-set-value

	   ;; triggers
	   #:trigger
	   #:trigger-set-behaviour
	   #:trigger-run-behaviour
	   #:make-active-low-trigger
	   #:make-active-high-trigger

	   ;; buses
	   #:bus
	   #:bus-width
	   #:bus-wire-names
	   #:bus-wire-value
	   #:bus-set-wire-value
	   #:bus-set-wire-values
	   #:bus-connect
	   #:bus-pins-connected-to-wire
	   #:bus-propagate

	   ;; components
	   ))

(in-package :cl-vhdsl)
