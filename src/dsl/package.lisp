;; Package for DSL definition facility
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

(defpackage cl-vhdsl/dsl
  (:documentation "A facility for defining embedded Lisp domain-specific languages and sub-sets.")
  (:use :cl :alexandria)
  (:import-from :str
		#:concat)   ;; for building function names

  (:export
   ;; top-level macros
   #:defdsl
   #:in-dsl
   #:defun/dsl
   #:defdslform
   #:defdslfun
   #:defdslmacro

   ;; conditions
   #:no-current-dsl
   #:duplicate-dsl-function
   #:unknown-dsl-function
   #:unknown-dsl-form
   #:duplicate-dsl-macro
   #:unknown-dsl-macro
   ))
