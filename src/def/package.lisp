;; Package definition for the component definition package
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

(defpackage cl-vhdsl/def
  (:use :cl :cl-bitfields :cl-vhdsl :cl-ppcre)
  (:export
   ;; data types and type construtors
   #:unsigned-bitfield
   #:signed-bitfield
   #:unsigned-8
   #:signed-8
   #:unsigned-16
   #:signed-16
   #:list-of-addressing-modes

   ;; registers
   #:register
   #:print-registern
   #:def-register

   ;; addressing modes
   #:addressing-mode
   #:addressing-mode-parse
   #:addressing-mode-regexp
   #:addressing-mode-bytes

   ;; instructions
   #:instruction
   #:instruction-mnemonic
   #:instruction-addressing-mode
   #:instruction-addressing-modes
   #:instruction-bytes
   #:instruction-assemble
   #:instruction-add-class-for-mnemonic
   #:instruction-class-for-mnemonic

   ;; assembler
   #:assembler-get-mnemonic
   #:assembler-make-mnemonic-regexp
   #:assembler-get-addressing-mode
   #:assembler-make-addressing-modes-regexp

   ;; memory
   #:memory
   #:memory-size
   #:memory-write-byte
   #:memory-read-byte

   ))
