;; Constructing emulations
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

;; ---------- Core emulation ----------

(defun make-core (core)
  "Build an emulation of CORE.

CORE is a description of a core, for which we construct a
software emulation."
  (let ((c (make-instance 'core)))
    ;; add registers
    (maphash #'(lambda (rname reg)
		 (let ((r (make-instance 'register
					 :name rname
					 :width (def:register-width reg))))
		   ;; add the register
		   (core-add-register c r)

		   ;; if it is a program counter, capture it
		   (if (typep reg 'def:program-counter)
		       (if (null (slot-value c 'pc))
			   (setf (slot-value c 'pc) r)
			   (error "Duplicate progam counter defined for core")))))
	     (def:core-registers core))

    ;; check we did get a program counter
    (if (null (slot-value c 'pc))
	(error "No program counter defined for core"))

    ;; add flags
    (maphash #'(lambda (fname f)
		 (core-add-flag c (make-instance 'flag
						 :name fname
						 :register (core-register (def:flag-register f) c)
						 :bit (def:flag-bit f))))
	     (def:core-flags core))

    ;; return the created core
    c))


(defun load-instruction (ins mem addr)
  "Load INS at address ADDR of MEM, returning the new PC."
  (let* ((bs (def:instruction-bytes ins))
	 (n (length bs)))
    (dolist (i (iota n))
      (setf (memory-location mem (+ addr i)) (elt bs i)))
    (setf (memory-instruction mem addr)
	  (lambda (c)
	    (incf (core-pc-value c) n)
	    (def:instruction-behaviour ins c)))
    (+ addr n)))


(defun load-program (p mem &key (initial #16r300))
  "Load P into memory MEM."
  (let ((pc initial))
    (dolist (ins p)
      (setq pc (load-instruction ins mem pc)))))


(defun run-instruction (c mem addr)
  "Run the instruction stored at ADDR of MEM on core C."
  (let ((ins (memory-instruction mem addr)))
    (funcall ins c)))


(defun run-program  (c mem &key (initial #16r300))
  "Run the program loaded into MEM on core C, starting from the INITIAL address."
  (setf (core-memory c) mem)
  (setf (core-pc-value c) initial)
  (catch *END-PROGRAM*
    (loop (run-instruction c mem (core-pc-value c)))))
