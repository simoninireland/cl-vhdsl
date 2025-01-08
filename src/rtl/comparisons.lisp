;; Synthesisable comparison operators
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

(in-package :cl-vhdsl/rtl)

(defun ensure-boolean (form env)
  "Ensure FORM is a boolean (bit) in ENV.

Signals TYPE-MISMATCH if FORM isn't boolean, which may be ignored
in many applications."
  (ensure-subtype form 'bit))


(defmethod synthesise-sexp ((fun (eql 'logand)) args (context (eql :inexpression)))
  (as-infix 'logand args))
