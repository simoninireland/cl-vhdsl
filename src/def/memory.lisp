;; Memory
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

(in-package :cl-vhdsl/def)

(defclass memory ()
  ((size
    :documentation "Size of memory in bytes."
    :initarg :size
    :initform (floor (expt 2 16))                    ;; 64Kb by default
    :reader memory-size))
  (:documentation "A linear byte-addressable RAM."))


(defgeneric memory-write-byte (mem addr b)
  (:documentation "Write B to ADDR in MEM."))


(defgeneric memory-read-byte (mem addr)
  (:documentation "Read byte from ADDR in MEM."))
