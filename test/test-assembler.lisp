;; Tests of assembler
;;
;; Copyright (C) 2023 Simon Dobson
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

(in-package :cl-vhdsl/test)
(in-suite cl-vhdsl)


;; ---------- Regexps for parsing ----------

(test test-instruction-mnemonics
  "Test we can extract mnemoics from instruction objects."
  (is (string= (instruction-mnemonic (make-instance 'LDA)) "LDA"))
  (is (string= (instruction-mnemonic (make-instance 'LDX)) "LDX")))


(test test-instuction-class-mnemonics
  "Test we can extract mnemonics from classes."
  (is (string= (instruction-mnemonic 'LDA) "LDA"))
  (signals error
    (instruction-mnemonic 'ttt)))


(test test-mnemonic-regexp
  "Test we can extract the right class from its menmonic."
  (let ((clns (list 'LDA 'LDX 'STA)))
    (is (string= (assembler-get-mnemonic "LDA" clns) "LDA"))
    (is (string= (assembler-get-mnemonic "LDX" clns) "LDX"))
    (is (string= (assembler-get-mnemonic "STA" clns) "STA"))

    (is (null (assembler-get-mnemonic "TXA" clns)))))


(test test-mnemonic-regexp-pre
  "Check we can construct and re-use the regexp."
  (let* ((clns (list 'LDA 'LDX 'STA))
	 (re (assembler-make-mnemonic-regexp clns)))
     (is (string= (assembler-get-mnemonic "LDA" clns re) "LDA"))))
