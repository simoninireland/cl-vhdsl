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


(defmethod synthesise-sexp ((fun (eql 'progn)) args as str)
  (format str "~{~a ~^~&~}" (mapcar (lambda (form) (synthesise form :statement))
				    args)))


(defmethod synthesise-sexp ((fun (eql 'when)) args (as (eql :statement)) str)
  (let ((test (car args))
	(body (cdr args)))
    (format str "always @(~a) begin~&~a~&end"
	    (synthesise test :rvalue)
	    (synthesise (cons 'progn body) :statement))))


;; statement form
(defmethod synthesise-sexp ((fun (eql 'if)) args (as (eql :statement)) str)
  (destructuring-bind (condition then &optional else)
      args
    (let ((condbranch (synthesise condition :rvalue))
	  (thenbranch (synthesise then :statement))
	  (elsebranch (if else
			  (format nil "else~&~a" (synthesise else :statement))
			  "")))
      (format str "if (~a)~&~a~&~a~&end~&"
	      condbranch
	      thenbranch
	      elsebranch))))


;; conditional expression form
(defmethod synthesise-sexp ((fun (eql 'if)) args (as (eql :rvalue)) str)
  (destructuring-bind (condition then &optional else)
      args
    (let ((condbranch (synthesise condition :rvalue))
	  (thenbranch (synthesise then :rvalue))
	  (elsebranch (if else
			  (format nil ": ~a" (synthesise else :rvalue))
			  "")))
      (format str "~a ? ~a ~a"
	      condbranch
	      thenbranch
	      elsebranch))))
