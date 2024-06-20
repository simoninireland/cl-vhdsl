;; Fully-software-emulated memory
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

(in-package :cl-vhdsl/emu)

(defclass emulated-memory (memory)
  ((bytes
    :documentation "The bytes in the memory."
    :type (array byte)
    :accessor emulated-memory-bytes))
  (:documentation "A memory represented by an in-memory byte array."))


(defun make-emulated-memory (&key size)
  "Make a memory of the given byte SIZE."
  (let ((mem (make-instance 'emulated-memory :size size)))
    (setf (emulated-memory-bytes mem) (make-array (memory-size mem)
						  :element-type 'unsigned-8
						  :initial-element 0))
    mem))


(defmethod memory-write-byte ((mem emulated-memory) addr b)
  (setf (aref (emulated-memory-bytes mem) addr) b))


(defmethod memory-read-byte ((mem emulated-memory) addr)
  (aref (emulated-memory-bytes mem) addr))
