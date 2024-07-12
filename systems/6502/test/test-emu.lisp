;; Tests of software emulation
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

(defun make-emu-core (core mem)
  "Build an emulation of CORE connected to the given MEM."
  (let ((c (make-instance 'emu:core :memory mem)))
    (maphash #'(lambda (rname r)
		 (emu:core-add-register c (make-instance 'emu:register
							 :name rname
							 :width (register-width r))))
	     (core-registers core))
    (maphash #'(lambda (fname f)
	       (emu:core-add-flag c (make-instance 'emu:flag
				     :name fname
				     :register (emu:core-register (flag-register f) c)
				     :bit (flag-bit f))))
	     (core-flags core))

    c))


(defun load-instruction (ins mem addr)
  "Load INS at address ADDR of MEM, returning the new PC."
  (let* ((bs (instruction-bytes ins))
	 (n (length bs)))
    (dolist (i (iota n))
      (setf (memory-location mem (+ addr i)) (elt bs i))
      (setf (memory-instruction mem (+ addr i))
	    (lambda (c)
	      (incf (emu:core-register-value 'PC c) n)
	      (instruction-behaviour ins c))))
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


(defun run-program  (c &key (initial #16r300))
  "Run the program loaded onto core C, starting from the INITIAL address."
  (let ((mem (emu:core-memory c)))
    (setf (emu:core-register-value 'PC c) initial)
    (catch 'EOP
      (loop (run-instruction c mem (emu:core-register-value 'PC c))))))


(let* ((mem (make-instance 'cached-memory  :size (* 8 KB)))
       (core (make-emu-core *MOS6502* mem)))
  (memory-initialise mem)
  (let ((p (list (make-instance 'LDA :addressing-mode (immediate :value 25))
		 (make-instance 'STA :addressing-mode (absolute :address #16r200))
		 (make-instance 'BRK :addressing-mode (implicit)))))
    (load-program p mem)
    (run-program core)
    (memory-location mem #16r200)
    )
  )
