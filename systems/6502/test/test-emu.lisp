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

(let* ((A (make-instance 'emu:register :name "A" :width 8))
       (PC (make-instance 'emu:register :name "PC" :width 16))
       (P (make-instance 'emu:register :name "P" :width 8))
       (Z (make-instance 'emu:flag :register P :bit 0))
       (mem (make-instance 'memory :size (* 8 KB))))
  (memory-initialise mem)
  (setf (emu:memory-location mem #16r200) 25)

  (setf (emu:register-value PC) #16r400)
  (print(emu:register-value PC))
  (setf (memory-instruction mem (emu:register-value PC))
	(lambda ()
	  (progn
	    (princ "LDA")
	    (setf (emu:register-value A) (emu:memory-location mem #16r200))
	    (setf (emu:flag-value Z) (= (emu:register-value A) 0)))))
  (incf (emu:register-value PC) 3)
  (print(emu:register-value PC))

  (setf (memory-instruction mem (emu:register-value PC))
	(lambda ()
	  (throw 'EOP t)))
  (incf (emu:register-value PC) 1)
  (print(emu:register-value PC))

  (setf (emu:register-value PC) #16r400)
  (print (emu:register-value PC))
  (print (emu:memory-location mem (emu:register-value PC)))
  (catch 'EOP
    (loop
      (let ((ins (memory-instruction mem (emu:register-value PC))))
	(print (emu:register-value PC))
	(incf (emu:register-value PC) 3)
	(funcall ins))))
  (emu:register-value A)
  )


(defmethod addressing-mode-code ((mode immediate))
  (immediate-value mode))

(defmethod instruction-code ((ins LDA))
  `((setf (emu:register-value A) ,(instruction-addressing-mode-code ins))
    (setf (emu:flag-value Z) (= (emu:register-value A) 0))))

(assembler-make-core-registers *MOS6502*)
(assembler-make-memory (make-instance 'memory :size (* 8 KB)))
(assembler-make-instruction-behaviour (make-instance 'LDA :addressing-mode (immediate :value 24)))
(instruction-opcode (make-instance 'LDA :addressing-mode (immediate :value 24)))
(print (assembler-make-instruction (make-instance 'LDA :addressing-mode (immediate :value 24))))
