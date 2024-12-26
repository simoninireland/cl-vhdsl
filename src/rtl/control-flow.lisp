;; Synthesisable control flow
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


;; ---------- PROGN ----------

(defmethod synthesise-sexp ((fun (eql 'progn)) args as)
  (dolist (form args)
    (synthesise form :statement)))


;; ---------- Triggered blocks ----------

(defmethod synthesise-sexp ((fun (eql '@)) args (as (eql :statement)))
  (let ((test (car args))
	(body (cdr args)))
    (format *synthesis-stream* "~aalways @(" (indentation))
    (synthesise test :rvalue)
    (format *synthesis-stream* ")~&")
    (in-logical-block (:before "begin" :after "end")
      (dolist (form body)
	(synthesise form :statement)))))
