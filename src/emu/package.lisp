;; Package definition for the emulation package
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

(defpackage cl-vhdsl/emu
  (:use :cl :alexandria)
  (:local-nicknames (:def :cl-vhdsl/def))
  (:export
   ;; memory
   #:memory
   #:memory-initialise
   #:memory-size
   #:memory-locations
   #:memory-location
   #:cached-memory
   #:memory-instruction

   ;; registers
   #:register
   #:register-value
   #:flag
   #:flag-value

   ;; cores
   #:core
   #:core-add-register
   #:core-register
   #:core-pc
   #:core-pc-value
   #:core-register-value
   #:core-add-flag
   #:core-flag
   #:core-flag-value
   #:core-memory
   #:core-memory-location

   ;; construction
   #:make-core
   #:load-instruction
   #:load-program

   ;; execution
   #:run-instruction
   #:run-program
   #:core-end-program

   ;; conditions
   #:illegal-memory-access
   #:illegal-register-access
   ))
