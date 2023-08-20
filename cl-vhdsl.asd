;; cl-vhdsl.asd: ASDF syystem definition for cl-vhdsl
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

(defsystem "cl-vhdsl"
  :description "An experiment in building a hardware description DSL"
  :author "Simon Dobson <simoninireland@gmail.com"
  :version (:read-file-form "version.sexp")
  :license "GPL3"
  :depends-on ("alexandria" "trivia")
  :pathname "src/"
  :components ((:file "package")
	       (:file "utils")
	       (:file "bitfields"))
  :in-order-to ((test-op (test-op "cl-vhdsl/test"))))

(defsystem "cl-vhdsl/test"
  :depends-on ("cl-vhdsl" "fiveam")
  :pathname "test/"
  :components ((:file "package")
	       (:file "test-bitfields"))
  :perform (test-op (o c) (uiop:symbol-call :fiveam '#:run-all-tests)))
