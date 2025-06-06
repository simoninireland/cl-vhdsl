;; Package definition for component definitions
;;
;; Copyright (C) 2024-2025 Simon Dobson
;;
;; This file is part of verilisp, a very Lisp approach to hardware synthesis
;;
;; verilisp is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; verilisp is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with verilisp. If not, see <http://www.gnu.org/licenses/gpl.html>.

(in-package :common-lisp-user)

(defpackage verilisp/def
  (:use :cl :alexandria :verilisp/utils :verilisp/core)
  (:import-from :slot-extra-options
		#:def-extra-options-metaclass
		#:slot-exists-and-bound-p)
  (:import-from :closer-mop
		#:finalize-inheritance
		#:class-slots
		#:slot-boundp
		#:slot-definition-name
		#:slot-definition-type)
  (:export
   ;; Constants
   #:KB
   #:MB

   ;; components
   #:synthesisable-component
   #:component
   #:on-startup
   #:pin-interface
   #:subcomponents
   #:parameters
   #:wiring-diagram
   #:instruction

   ;; mixins and their behaviours
   #:enabled
   #:en
   #:on-enable
   #:on-disable
   #:clocked
   #:clk
   #:on-clock
   #:resetable
   #:rst
   #:on-reset
   #:readwrite
   #:rw
   #:on-read
   #:on-write

   ;; generating RTLisp from components
   #:to-rtl

   ;; conditions
   #:slot-type-mismatch
   #:subcomponent-mismatch
   ))
