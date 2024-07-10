;; 6502 emulation in software
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

(in-package :cl-vhdsl/6502)

;; ---------- Memory ----------

(defclass memory (emu:memory)
  ()
  (:documentation "A memory with pre-decoded instruction cache.

Each location contains a byte of \"real\" content and an optional
function that holds the behaviour of the location based at that location.
This saves decoding at run-time."))


;; We re-define the access functions to access the car of the pair
;; at each location, and add `memory-instruction' as an accessor
;; for the cdr.
;;
;; As an alternative to making the instructions setf-able like this
;; we could specialise `memory-location' to set depending on the
;; type of ots value argument (byte or function), and add a
;; different method for accessing the cached instruction.

(defmethod memory-initialise ((mem memory))
  (setf (emu:memory-locations mem)
	(make-array (list (emu:memory-size mem))))
  (let ((locs (emu:memory-locations mem)))
    (dolist (addr (iota (emu:memory-size mem)))
      (let ((cell (cons 0 nil)))
	(setf (aref locs addr) cell)))))


(defmethod memory-location ((mem memory) addr)
  (car (aref (emu:memory-locations mem) addr)))


(defmethod (setf memory-location) (v (mem memory) addr)
  (setf (car (aref (emu:memory-locations mem) addr)) v))


(defgeneric memory-instruction (mem addr)
  (:documentation "Return the instruction cached at location ADDR in MEM."))


(defmethod memory-instruction ((mem memory) addr)
  (cdr (aref (emu:memory-locations mem) addr)))


(defmethod (setf memory-instruction) (v (mem memory) addr)
  (setf (cdr (aref (emu:memory-locations mem) addr)) v))


;; ---------- Assembly ----------

(defun assembler-make-core-registers (core)
  "Return the list of variables needed to represent CORE.

This is a list suitable for `let' that incldues a variable for each
register, assigned to an instance of the appropriate emulation class."
  (flet ((make-register (r)
	   `(list
	    ,(register-name r)
	    (make-instance 'emu:register
			   :name ,(register-name r)
			   :width ,(register-width r)))))
    (mapcar #'make-register (core-registers core))))


(defun assembler-make-memory (mem)
  "Return an emulated memory matching MEM.

This is a list appropriate for `let'."
  `(make-instance 'emu:memory :size ,(emu:memory-size mem)))


(defun assembler-make-instruction-behaviour (ins)
  "Return the behaviour of INS.

This is returned as a lambda-term encapsulating the behaviour,
expecting to be closed by the architecture variables.."
  `(lambda () ,@(instruction-code ins)))


(defun assembler-make-instruction (ins)
  "Return the code to assemble INS.

This is returned as a list of code required to place the bytes of the
instruction into memory based at PC, decode and cache the
instruction, and increment PC to the next instruction base address."
  (let ((bytes (instruction-bytes ins)))
    `((setf (memory-instruction mem (register-value PC))
	    ,(assembler-make-instruction-behaviour ins))
      ,@(mapcar (lambda (i)
		  `(setf (emu:memory-location mem ,(if (> i 0)
						       `(+ PC ,i)
						       'PC))
			 ,(elt bytes i)))
		(iota (length bytes)))
      (incf (emu:register-value PC) ,(length bytes)))))


;; ---------- Macro interface ----------
