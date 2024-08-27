;; A minimal, explicitly-coded reference SAP-1
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

(in-package :cl-vhdsl/SAP-1)


(defun run-reference (mem)
  "Run a SAP-1 program from MEM.

This is a hand-cded reference implementation ofthe SAP-1 processor, It
takes a memory space of 16 bytes in MEM, starting execution from
address 0, and returns the contents of the OUT register."

  (block RUN
    (let ((PC 0)
	  (A 0)
	  (OUT 0))

      (labels ((lda (addr)
		 (setf A (aref mem addr)))

	       (add (addr)
		 (setf A (+ A (aref mem addr))))

	       (sub (addr)
		 (setf A (- A (aref mem addr))))

	       (out ()
		 (setf OUT A))

	       (hlt ()
		 (return-from RUN OUT))

	       (execute ()
		 "Run the machine's execution loop."
		 (let* ((IR (aref mem PC))
			(INS (ash IR -4))
			(W (logand IR #2r1111)))

		   ;; increment the program counter
		   (incf PC)

		   ;;decode the instruction
		   (eswitch (INS)
		     (#2r0000 (lda W))
		     (#2r0001 (add W))
		     (#2r0010 (sub W))
		     (#2r1110 (out))
		     (#2r1111 (hlt)))

		   ;; continue execution
		   (execute))))

	;; run the machine
	(execute)))))
