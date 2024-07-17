;; Tests of SAP-1 software emulation
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

(in-package :cl-vhdsl/SAP-1/test)

(test test-10.2
  "Test the basics (example 10-2)."
  (let* ((mem (make-instance 'emu:cached-memory :size 16))
	 (core (emu:make-core *SAP-1-core*)))
    (emu:memory-initialise mem)

    (let ((p (list (make-instance 'LDA :addressing-mode (absolute :address #16rF))
		   (make-instance 'ADD :addressing-mode (absolute :address #16rE))
		   (make-instance 'OUT :addressing-mode (def:implicit))
		   (make-instance 'HLT :addressing-mode (def:implicit)))))
      (emu:load-program p mem :initial #16r0)
      (setf (emu:memory-location mem #16rF) 1)
      (setf (emu:memory-location mem #16rE) 2)

      (emu:run-program core mem :initial #16r0)

      (is (equal (emu:core-register-value 'OUT core) 3)))))


(test test-10-4
  "Test the basics (example 10-4)."
  (let* ((mem (make-instance 'emu:cached-memory :size 16))
	 (core (emu:make-core *SAP-1-core*)))
    (emu:memory-initialise mem)

    (let ((p (list (make-instance 'LDA :addressing-mode (absolute :address #16r9))
		   (make-instance 'ADD :addressing-mode (absolute :address #16rA))
		   (make-instance 'ADD :addressing-mode (absolute :address #16rB))
		   (make-instance 'SUB :addressing-mode (absolute :address #16rC))
		   (make-instance 'OUT :addressing-mode (def:implicit))
		   (make-instance 'HLT :addressing-mode (def:implicit)))))
      (emu:load-program p mem :initial #16r0)
      (setf (emu:memory-location mem #16r9) #16r10)
      (setf (emu:memory-location mem #16rA) #16r14)
      (setf (emu:memory-location mem #16rB) #16r18)
      (setf (emu:memory-location mem #16rC) #16r20)

      (emu:run-program core mem :initial #16r0)

      ;; check result
      (is (equal (emu:core-register-value 'OUT core) (- (+ #16r10 #16r14 #16r18) #16r20)))

      ;; check opcode compilation
      (is (equal (emu:core-memory-location #16r0 core) #16r09))
      (is (equal (emu:core-memory-location #16r1 core) #16r1A))
      (is (equal (emu:core-memory-location #16r2 core) #16r1B))
      (is (equal (emu:core-memory-location #16r3 core) #16r2C))
      (is (equal (emu:core-memory-location #16r4 core) #16rE0))
      (is (equal (emu:core-memory-location #16r5 core) #16rF0)))))
