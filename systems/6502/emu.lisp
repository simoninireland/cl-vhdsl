;; 6502 emulation
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

(in-package :cl-vhdsl/systems/6502)

;; ---------- Architecture emulation ----------

(defclass 6502-emulation (6502-architecture)
  ((mem
    :documentation "Memory"
    :initform (make-array (floor (expt 2 16))            ;; 64Kb
			  :element-type 'unsigned-8
			  :initial-element 0)
    :initarg :memory
    :reader architecture-memory))
  (:documentation "An emulation of the 6502 architecture."))




;; ---------- Instruction emulation ----------

(defgeneric instruction-emulate (ins mode reg mem)
  (:documentation "doc"))


(defmethod instruction-emulate ((ins LDA) (mode immediate) arch)
  (let ((v (immediate-value mode)))
    (setf (A arch) v)
    (setf (Z arch) (zerop v))))


(defmethod instruction-emulate ((ins LDA) (mode absolute) arch)
  (let* ((addr (absolute-address mode))
	 (v (memory-read-byte mem addr)))
    (setf (A arch) v)
    (setf (Z arch) (zerop v))))



;; ---------- Top-level excution ----------
