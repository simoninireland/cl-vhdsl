;; Package for RTLisp, a synthesisable fragment of Lisp
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

(defpackage cl-vhdsl/rtl
  (:use :cl :alexandria
   :cl-vhdsl            ;; for utility functions
   :cl-ppcre)           ;; for specifying identifiers
  (:import-from :str
		#:shorten)

  (:export
   ;; fixed-width types
   #:fixed-width-unsigned
   #:fixed-width-signed
   #:bitwidth

   ;; environments
   #:empty-environment
   #:extend-environment
   #:get-environment-names
   #:get-environment-properties

   ;; validation
   #:typecheck
   #:typecheck-sexp

   ;; transformations
   #:float-let-blocks
   #:float-let-blocks-sexp
   #:expand-macros
   #:expand-macros-sexp

   ;; synthesis
   #:synthesise
   #:synthesise-sexp

   ;; Lispification
   #:lispify
   #:lispify-sexp

   ;; pretty-printing
   #:indentation
   #:in-logical-block

   ;; loader
   #:get-modules-for-synthesis
   #:defmodule

   ;; conditions
   #:not-synthesisable
   #:unknown-variable
   #:value-mismatch
   #:direction-mismatch
   #:type-mismatch
   ))
