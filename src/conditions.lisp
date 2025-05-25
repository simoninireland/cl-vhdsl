;; Conditions with hints
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

(in-package :verilisp)


(define-condition verilisp-condition ()
  ((hint
    :documentation "A hint as to how to fix the condition."
    :initarg :hint
    :initform nil
    :reader hint)
   (underlying-condition
    :documentation "Any underlying condition that was converted to this.

This allows VL-BASE-CONDITION to be used to mask other, typically
implementation-specific, conditions encountered during processing."
    :initform nil
    :initarg :underlying-condition
    :reader underlying-condition))
  (:documentation "Base class for Verilisp conditions.

A hint can be given to suggest how to fix the issue."))


(defgeneric format-condition-context (detail c str)
  (:documentation "Format the DETAIL and other information of a condition C.

The text is written to stream STR.")
  (:method (detail (c verilisp-condition) str)
    (format str "~a" detail)

    ;; add hint if present
    (if-let ((hint (hint c)))
      (format str " (~a)" hint))))
