;; Tests of MAKE-BITFIELDS
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

(in-package :cl-vhdsl/test)
(in-suite cl-vhdsl/rtl)
(declaim (optimize debug))


(test test-make-bitfields-literal
  "Test we can make a bitfield from literals."
  (is (subtypep (rtl:typecheck '(rtl:make-bitfields #2r111 #2r100)
			    emptyenv)
		'(rtl::fixed-width-unsigned 6)))

  (is (subtypep (rtl:typecheck '(rtl:make-bitfields #2r111
				 (the (rtl::fixed-width-unsigned 12) 0))
			       emptyenv)
		'(rtl::fixed-width-unsigned 15))))
