;; Top-level test package
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

(defpackage cl-vhdsl/test
  (:use :cl :alexandria :serapeum :fiveam
	:cl-vhdsl)
  (:local-nicknames (:def :cl-vhdsl/def)
		    (:emu :cl-vhdsl/emu)
		    (:hw :cl-vhdsl/hw))
  (:import-from :fiveam #:is #:test))

(in-package :cl-vhdsl/test)
(def-suite cl-vhdsl)
