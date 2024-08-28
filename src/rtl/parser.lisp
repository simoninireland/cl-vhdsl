;; Tree traversal and parsing for RTLisp
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


(defun validate (form &optional vars)
  "Validate that FORM is a valid fragment in an environment containing VARS.

FORM must be a valid synthesisable Lisp fragment. Any variables
appearing free in FORM must be mentioned in VARS. If VARS ar
omitted then FORM must be a closed form.

Failure to validate signals either `not-synthesisable' or
`unknown-variable' as appropriate."
  (let ((f (ast:parse form)))

    ;; syntax
    (unless (fragment-p f)
      (error 'not-synthesisable :fragment f))

    ;; closure
    (unless (closed-fragment-p f vars)
      (let ((vars (set-difference (ast:free-variables f) vars)))
	(error 'unknown-variable :variables vars)))))
