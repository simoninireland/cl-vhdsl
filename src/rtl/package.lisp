;; Package for RTLisp, a synthesisable fragment of Lisp
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

(in-package :common-lisp-user)

(defpackage cl-vhdsl/rtl
  (:documentation "RTLisp, the synthesisiable fragment of Common Lisp, as an embedded DSL")
  (:use :cl :alexandria
	:cl-vhdsl)
  (:import-from :cl-ppcre
		#:create-scanner
		#:scan
		#:regex-replace-all)
  (:import-from :str
		#:concat
		#:s-first
		#:containsp
		#:shorten
		#:words)

  (:export
   ;; fixed-width types
   #:bitwidth

   ;; environments
   #:empty-environment
   #:add-frame
   #:declare-variable
   #:variable-declared-p
   #:get-frame-names
   #:get-environment-names
   #:filter-environment
   #:get-type
   #:get-representation
   #:get-width
   #:get-initial-value

   ;; evaluating expressions in environments
   #:close-form-in-environment
   #:close-form-in-static-environment
   #:eval-in-static-environment
   #:ensure-static
   #:eval-if-static

   ;; extra RTLisp functions and macros not in Common Lisp
   #:module
   #:module-interface
   #:<<
   #:>>
   #:@
   #:posedge
   #:negedge
   #:bits
   #:with-bitfields
   #:make-bitfields
   #:extend-bits
   #:0=
   #:0/=
   #:2*
   #:let-wires
   #:let-registers
   #:let-constants
   #:state-machine
   #:next
   #:exit

   ;; DSL functions
   #:widthcheck
   #:typecheck
   #:detect-shadowing
   #:float-let-blocks
   #:simplify-progn
   #:expand-macros
   #:synthesise
   #:lispify

   ;; loader
   #:clear-module-registry
   #:get-module
   #:get-module-interface
   #:get-modules-for-synthesis
   #:defmodule
   #:synthesise-module

   ;; conditions
   #:not-synthesisable
   #:unknown-variable
   #:bad-variable
   #:unknown-module
   #:duplicate-variable
   #:duplicate-module
   #:not-importable
   #:not-static
   #:value-mismatch
   #:direction-mismatch
   #:type-mismatch
   #:coercion-mismatch
   #:bitfield-mismatch
   #:shape-mismatch
   #:state-machine-mismatch
   #:type-inferred
   #:representation-mismatch
   ))
