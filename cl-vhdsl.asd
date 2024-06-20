;; System definition
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

(asdf:defsystem "cl-vhdsl"
  :description "An experiment in building a hardware and processor description DSL."
  :author "Simon Dobson <simoninireland@gmail.com"
  :version (:read-file-form "version.sexp")
  :license "GPL3"
  :depends-on ("alexandria" "cl-bitfields")
  :pathname "src/"
  :serial t
  :components ((:file "package")
	       (:module "def"
		:components ((:file "package")
			     (:file "types")
			     (:file "register")
			     (:file "instruction")))
	       (:module "emu"
		:components ((:file "package")
			     (:file "memory"))))
  :in-order-to ((test-op (test-op "cl-vhdsl/test"))))


;; (asdf:defsystem "cl-vhdsl/test"
;;   :description "Global tests of VHDSL."
;;   :depends-on ("cl-vhdsl" "fiveam")
;;   :pathname "test/"
;;   :serial t
;;   :components ((:file "package")
;;	       (:file "test-bitfields"))
;;   :perform (test-op (o c) (uiop:symbol-call :fiveam '#:run-all-tests)))
