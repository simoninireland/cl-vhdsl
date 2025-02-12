;; Package definition for utilities
;;
;; Copyright (C) 2023--2025 Simon Dobson
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
  (:use :cl :alexandria :serapeum)
  (:export
   ;; list utilities
   #:index-non-nil
   #:non-nil-subseq
   #:uniquify
   #:listify
   #:delistify
   #:zip-without-null
   #:remove-nulls
   #:foldl
   #:foldr
   #:mapn
   #:n-copies
   #:filter-by-predicates
   #:sublist
   #:successive-pairs

   ;; data structure manipulations
   #:safe-cadr
   #:mapappend

   ;; string functions
   #:string-times

   ;; conditions
   #:vhdsl-condition
   #:format-condition-context
   ))
