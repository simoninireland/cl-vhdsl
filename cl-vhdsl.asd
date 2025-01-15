;; System definitions
;;
;; Copyright (C) 2024--2025 Simon Dobson
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
  :depends-on ("alexandria"
	       "str"
	       "cl-bitfields"
	       "cl-ppcre"
	       "closer-mop"
	       "slot-extra-options")
  :pathname "src/"
  :serial t
  :components ((:file "package")
	       (:file "utils")
	       (:module "rtl"
			:components ((:file "package")
				     (:file "env")
				     (:file "pretty-printer")
				     (:file "passes")
				     (:file "fixed-width")
				     (:file "literals")
				     (:file "references")
				     (:file "operators")
				     (:file "comparisons")
				     (:file "bits")
				     (:file "arrays")
				     (:file "control-flow")
				     (:file "conditionals")
				     (:file "binders")
				     (:file "assignment")
				     (:file "casting")
				     (:file "modules")
				     (:file "eval")
				     (:file "loader")
				     (:file "embedding")
				     (:file "conditions")))
	       (:module "def"
		:components ((:file "package")
			     (:file "constants")
			     (:file "mop")
			     (:file "component")
			     (:file "mixins")
			     (:file "synthesis")
			     (:file "conditions"))))
  :in-order-to ((test-op (test-op "cl-vhdsl/test"))))


(asdf:defsystem "cl-vhdsl/test"
  :description "Global tests of VHDSL."
  :depends-on ("alexandria" "cl-vhdsl" "fiveam")
  :pathname "test/"
  :serial t
  :components ((:file "package")
	       (:file "test-utils")
	       ;;(:file "test-debugging")
	       ;;(:file "test-mop")
	       ;;(:file "test-assembler")
	       ;;(:file "test-wires")
	       ;;(:file "test-wiring")
	       ;;(:file "test-registers")
	       ;;(:file "test-alu")
	       ;;(:file "test-datapath")
	       ;;(:file "test-ring-counters")
	       ;;(:file "test-microinstructions")
	       (:file "test-rtl-base")
	       (:file "test-rtl-types")
	       (:file "test-rtl-assignment")
	       (:file "test-rtl-literals")
	       (:file "test-rtl-references")
	       (:file "test-rtl-bits")
	       (:file "test-rtl-arrays")
	       (:file "test-rtl-binders")
	       (:file "test-rtl-control-flow")
	       (:file "test-rtl-conditionals")
	       (:file "test-rtl-modules")
	       (:file "test-rtl-macros")
	       (:file "test-rtl-float-let")
	       (:file "test-rtl-shadowing")
	       (:file "test-rtl-rewriting")
	       (:file "test-rtl-cond")
	       (:file "test-rtl-with-bitfields")
	       (:file "test-def-components")
	       )
  :perform (test-op (o c) (uiop:symbol-call :fiveam '#:run-all-tests)))


;; ---------- Transpiler CLI ----------

(asdf:defsystem "cl-vhdsl/cli"
  :description "Command line tool to transpile VHDSL to Verilog."
  :author "Simon Dobson <simoninireland@gmail.com"
  :license "GPL3"
  :depends-on ("alexandria" "serapeum" "unix-opts" "cl-vhdsl")
  :pathname "src/cli/"
  :serial t
  :components ((:file "package")
	       (:file "vhdslc"))
  :build-operation "program-op"
  :build-pathname "../../vhdslc"
  :entry-point "cl-vhdsl/cli:main")
