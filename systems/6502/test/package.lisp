;; Test package for the 6502 emulator
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

(defpackage cl-vhdsl/6502/test
  (:use :cl :cl-ppcre :fiveam
	:cl-vhdsl :cl-vhdsl/systems/6502)
  (:import-from :fiveam #:is #:test))

(in-package :cl-vhdsl/6502/test)

(def-suite cl-vhdsl/6502)
