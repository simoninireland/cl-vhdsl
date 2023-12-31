;; utils.lisp: utilities
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

(in-package :cl-vhdsl)

;; ---------- Type checks ----------

(defun ensure-binary (v)
  "Ensure that V takes a value 0 or 1: a single bit."
  (if (not (or (equal v 0)
	       (equal v 1)))
      (error "Value ~S not a valid bit" v)))


;; ---------- List processing ----------

(defun indexed-list (is vs &optional (missing-value 0))
  "Return a list with elements at positions indexed by IS come from VS.

Indices start at 0 for the first element, as for `nth'. Elements that
are not assigned a value are given the value in MISSING-VALUE, which
defaults to 0."
  (let* ((n (1+ (apply #'max is)))
	 (l (make-list n :initial-element missing-value)))
    (mapc (lambda (i v)
	    (setf (nth i l) v))
	  is vs)
    l))
