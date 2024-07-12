;; Package definition for the 6502 emulator
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

(defpackage cl-vhdsl/6502
  (:use :cl :cl-ppcre :cl-interpol :cl-bitfields :alexandria :cl-vhdsl/def)
  (:local-nicknames (:emu :cl-vhdsl/emu))

  (:export
   ;; extended architectural elements
   #:memory
   #:memory-instruction

   ;; addressing modes
   ;; (all but the classes can be hidden eventually)
   #:immediate
   #:immediate-value
   #:absolute
   #:absolute-address
   #:absolute-indexed
   #:absolute-indexed-index

   ;; instructions (both classes and functions)
   #:LDA
   #:LDX
   #:LDY
   #:STA
   #:DEX
   #:BNZ
   #:BRK

   ;; assembler
   #:*assembler-instructions*
   #:*assembler-addressing-modes*
   #:assembler-parse-number
   #:assembler-parse-instruction
   #:assembler-parse-line
   #:assembler-parse-sexp

   ;; macro interface
   ))
