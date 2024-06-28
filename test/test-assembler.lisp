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


;; ---------- Regexps for parsing opcodes ----------

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


;; ---------- Number parsing ----------

(test test-bases
  "Test we can parse different number bases."
  (is (= (assembler-parse-number "0") 0))
  (is (= (assembler-parse-number "1") 1))
  (is (= (assembler-parse-number "9") 9))
  (is (= (assembler-parse-number "009") 9))
  (is (= (assembler-parse-number "10") 10))
  (is (= (assembler-parse-number "10B") #2r10))
  (is (= (assembler-parse-number "10H") #16r10))
  (is (= (assembler-parse-number "10FFH") #16r10FF))

  (is (= (assembler-parse-number "-20") (- 20)))
  (is (= (assembler-parse-number "-10H") (- #16r10)))

  (signals error
    (assembler-parse-number "F"))
  (signals error
    (assembler-parse-number "G"))
  (signals error
    (assembler-parse-number "2B"))
  (signals error
    (assembler-parse-number "--1"))
  (signals error
    (assembler-parse-number "10H0")))


;; ---------- Regexps for parsing addressing modes ----------

(test test-addressing-mode-regexp
  "Test we can extract the right addressing mode."
  (let ((aclns (list 'immediate 'absolute 'absolute-indexed)))
    (is (equalp (assembler-get-addressing-mode "#123" aclns) '(immediate ("123"))))
    (is (equalp (assembler-get-addressing-mode "123FH" aclns) '(absolute ("123FH"))))
    (is (equalp (assembler-get-addressing-mode "123, X" aclns) '(absolute-indexed ("123" "X"))))))


;; ---------- Instruction parsing ----------

(test test-simple-instruction
  "Test we can parse an instruction."
  (let ((*assembler-instructions* (list 'LDA 'LDX 'STA 'DEX))
	(*assembler-addressing-modes* (list 'implicit 'immediate 'absolute 'absolute-indexed)))
    (assembler-parse-instruction '("LDA" "#123"))
    (assembler-parse-instruction '("LDA" "1234H"))
    (assembler-parse-instruction '("LDA" "123, X"))
    (assembler-parse-instruction '("DEX" ""))

    ;; missing addressing mode (and doesn't allow implicit addressing)
    (signals error
      (assembler-parse-instruction '("LDA" "")))

    ;; incorrect addressing mode
    (signals error
      (assembler-parse-instruction '("DEX" "#100")))
    ))
