;; 6502 emulator system definition
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

(asdf:defsystem "6502"
  :description "A 6502 emulator in software"
  :author "Simon Dobson <simoninireland@gmail.com"
  :version (:read-file-form "version.sexp")
  :license "GPL3"
  :depends-on ("alexandria" "cl-vhdsl")
  :serial t
  :components ((:file "package")
	       (:file "arch")
	       (:file "addressing")
	       (:file "instructions"))
  :in-order-to ((test-op (test-op "6502/test"))))
