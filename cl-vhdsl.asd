;; System definitions
;;
;; Copyright (C) 2023--2024 Simon Dobson
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

;; ---------- cl-vhdsl ----------

(asdf:defsystem "cl-vhdsl"
  :description "An experiment in building a hardware and processor description DSL."
  :author "Simon Dobson <simoninireland@gmail.com"
  :version (:read-file-form "version.sexp")
  :license "GPL3"
  :depends-on ("alexandria" "cl-bitfields"
	       "cl-ppcre" "cl-interpol" "str"
	       "closer-mop" "slot-extra-options")
  :pathname "src/"
  :serial t
  :components ((:file "package")
	       (:file "utils")
	       (:module "def"
		:components ((:file "package")
			     (:file "types")
			     (:file "constants")
			     (:file "conditions")
			     (:file "arch")
			     (:file "register")
			     (:file "addressing")
			     (:file "instruction")))
	       (:module "emu"
		:components ((:file "package")
			     (:file "arch")
			     (:file "cached-memory")
			     (:file "emu")
			     (:file "conditions")))
	       (:module "hw"
		:components ((:file "package")
			     (:file "arch")
			     (:file "mop")
			     (:file "component")
			     (:file "wiring")
			     (:file "register")
			     (:file "alu")
			     (:file "ram")
			     (:file "ring-counter")
			     (:file "microinstruction")
			     (:file "control")
			     (:file "debugging")
			     (:file "conditions"))))
  :in-order-to ((test-op (test-op "cl-vhdsl/test"))))


(asdf:defsystem "cl-vhdsl/test"
  :description "Global tests of VHDSL."
  :depends-on ("alexandria" "cl-vhdsl" "cl-vhdsl/6502" "fiveam")
  :pathname "test/"
  :serial t
  :components ((:file "package")
	       (:file "test-utils")
	       (:file "test-debugging")
	       (:file "test-mop")
	       ;;(:file "test-assembler")
	       (:file "test-wires")
	       (:file "test-wiring")
	       (:file "test-registers")
	       (:file "test-datapath")
	       (:file "test-ring-counters")
	       ;;(:file "test-microinstructions")
	       )
  :perform (test-op (o c) (uiop:symbol-call :fiveam '#:run-all-tests)))


;; ---------- 6502 emulator ----------

(asdf:defsystem "cl-vhdsl/6502"
  :description "A 6502 emulator in software"
  :author "Simon Dobson <simoninireland@gmail.com"
  :license "GPL3"
  :depends-on ("alexandria" "cl-ppcre" "cl-vhdsl")
  :pathname "systems/6502/"
  :serial t
  :components ((:file "package")
	       (:file "arch")
	       (:file "addressing")
	       (:file "instructions")
	       (:file "assembler")
	       (:file "emu"))
  :in-order-to ((test-op (test-op "cl-vhdsl/6502/test"))))


(asdf:defsystem "cl-vhdsl/6502/test"
  :description "Tests of VHDSL 6502 emulator."
  :depends-on ("cl-vhdsl/6502" "cl-ppcre" "fiveam")
  :pathname "systems/6502/test/"
  :serial t
  :components ((:file "package")
	       (:file "test-assembler"))
  :perform (test-op (o c) (uiop:symbol-call :fiveam '#:run-all-tests)))


;; ---------- SAP-1 emulator ----------

(asdf:defsystem "cl-vhdsl/SAP-1"
  :description "A SAP-1 emulator in software"
  :author "Simon Dobson <simoninireland@gmail.com"
  :license "GPL3"
  :depends-on ("alexandria" "cl-ppcre" "cl-vhdsl")
  :pathname "systems/sap-1/"
  :serial t
  :components ((:file "package")
	       (:file "sap-1")
	       (:file "explicit"))
  :in-order-to ((test-op (test-op "cl-vhdsl/SAP-1/test"))))


(asdf:defsystem "cl-vhdsl/SAP-1/test"
  :description "Tests of VHDSL SAP-1 emulator."
  :depends-on ("cl-vhdsl/SAP-1" "cl-ppcre" "fiveam")
  :pathname "systems/sap-1/test/"
  :serial t
  :components ((:file "package")
	       (:file "test-emu"))
  :perform (test-op (o c) (uiop:symbol-call :fiveam '#:run-all-tests)))
