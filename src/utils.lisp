			;; Helper functions and macros
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

(in-package :cl-vhdsl)

;; ---------- Searching lists for non-nil ----------

(defun index-non-nil (l)
  "Return the index (0-based) of the first non-nil element of sequnce L."
  (dolist (i (alexandria:iota (length l)))
    (if (not (null (elt l i)))
	(return i))))


(defun non-nil-subseq (l)
  "Return the non-nil sub-sequence of L.

This will extract the first such sub-sequence, beginning from the index
extracted by `index-not-nil' and containing all elements up to the
next nil element of the end of the sequence."
  (alexandria:when-let ((i (index-non-nil l)))
    (dolist (j (alexandria:iota (- (length l) (1+ i)) :start 1))
      (if (null (elt l (+ i j)))
	  (return-from non-nil-subseq (subseq l i (+ i j)))))

    ;; if we get here, the rest of the list is non-nil
    (subseq l i)))
