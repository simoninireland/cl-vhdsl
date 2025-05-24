;; Tests of references to variables
;;
;; Copyright (C) 2024--2025 Simon Dobson
;;
;; This file is part of verilisp, a very Lisp approach to hardware synthesis
;;
;; verilisp is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; verilisp is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with verilisp. If not, see <http://www.gnu.org/licenses/gpl.html>.

(in-package :verilisp/test)
(in-suite verilisp/rtl)


(test test-variable-scope
  "Test we can see variables in scope."
  (is (subtypep (vl:typecheck '(let ((a 12))
				 a))
		'(unsigned-byte 8))))


(test test-variable-not-scope
  "Test we can see variables not in scope."
  (signals (vl:unknown-variable)
    (vl:typecheck '(let ((a 12))
		     b))))
