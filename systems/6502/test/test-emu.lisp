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

(let* ((mem (make-instance 'emu:cached-memory  :size (* 8 KB)))
       (core (emu:make-core *MOS6502*)))
  (emu:memory-initialise mem)
  (let ((p (list (make-instance 'LDA :addressing-mode (immediate :value 25))
		 (make-instance 'STA :addressing-mode (absolute :address #16r200))
		 (make-instance 'BRK :addressing-mode (implicit)))))
    (emu:load-program p mem)
    (emu:run-program core mem)
    (emu:memory-location mem #16r200)
    )
  )


(defun create-instruction (mnemonic mode mem pc)
  "doc"
  (let ((ins (funcall #'make-instance mnemonic :addressing-mode mode)))
    (load-instruction ins mem pc)))


(defmacro defprogram (var (mem &key (initial #16r300)) &body body)
  "Assemble a program to a list of instructions."
  `(macrolet ((LDA (mode)
		`(setq pc (create-instruction 'LDA ,mode mem pc)))
	      (STA (mode)
		`(setq pc (create-instruction 'STA ,mode mem pc)))
	      (DEX (mode)
		`(setq pc (create-instruction 'DEX ,mode mem pc)))
	      (INX (mode)
		`(setq pc (create-instruction 'INX ,mode mem pc)))
	      (TAX (mode)
		`(setq pc (create-instruction 'TAX ,mode mem pc)))
	      (BEQ (mode)
		`(setq pc (create-instruction 'BEQ ,mode mem pc)))
	      (BNZ (mode)
		`(setq pc (create-instruction 'BNZ ,mode mem pc)))
	      (BRK (mode)
		`(setq pc (create-instruction 'BRK ,mode mem pc)))

	      (immediate (&rest args)
		`(apply #'make-instance (list 'immediate ,@args)))
	      (absolute (&rest args)
		`(apply #'make-instance (list 'absolute ,@args)))
	      (absolute-indexed (&rest args)
		`(apply #'make-instance (list 'absolute-indexed ,@args)))
	      (relative (&rest args)
		`(apply #'make-instance (list 'relative ,@args)))

	      (.LABEL (label)
		`(setf (gethash ',label symbols) pc))
	      (.TO-LABEL (label)
		`(if-let ((addr (gethash ',label symbols)))
		  (- addr pc)
		  (print "Forward reference"))))
     (let ((mem ,mem)
	   (pc ,initial)
	   (symbols (make-hash-table)))
       ,@body
       mem)))



(let ((mem (make-instance 'cached-memory :size (* 8 KB))))
  (memory-initialise mem)
  (defprogram test (mem)
    (LDA (immediate :value 100))))


(defprogram copy-block ((make-instance 'cached-memory :size (* 8 KB)) :initial #16r400)
  (let ((SOURCE #16r200)
	(DEST #16r300)
	(LEN #16r2ff))
    (.LABEL START)
    (LDA (absolute :address LEN))
    (BEQ (relative :offset (.TO-LABEL END)))
    ;;(TAX)
    ;;(INX)
    (.LABEL COPY)
    (LDA (absolute-indexed :address SOURCE :index 'X))
    (STA (absolute-indexed :address DEST :index 'X))
    ;;(DEX)
    (BNZ (relative :offset (.TO-LABEL COPY)))
    (.LABEL END)
    ;;(BRK)
    )


  )
