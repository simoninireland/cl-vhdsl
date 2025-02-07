;; References to variables
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


;; ---------- Identifiers ----------

(defparameter *identifier-legal-leading-character-regexp* (create-scanner "[A-Za-z_]")
  "Regexp identifying the characters allowed as the first character of an identifier.")


(defparameter *identifier-illegal-character-regexp* (create-scanner "[^A-Za-z0-9_]")
  "Regexp identifying all chanaters that are illegal in an identifier.")


(defparameter *identifier-illegal-character-replacement* "_"
  "The character used to replace any illegal characters in identifiers.

This is also added to the front of any identifiers that clash
with *RESERVED-WORDS*.")

;; There are probably more reserved words
(defvar *reserved-words* '("module" "input" "output" "inout"
			   "always" "@"  "posedge" "negedge" "assign"
			   "parameter" "localparam" "reg" "wire"
			   "signed")
  "Reserved words in Verilog.")


(defun ensure-legal-identifier (n)
  "Ensure that N is a legal identifier.

Any non-permitte characters (identifiers by *IDENTIFIER-ILLEGAL-CHARACTER-REGEXP*)
are replaced with instances of *IDENTIFIER-ILLEGAL-CHARACTER-REPLACEMENT*. If
the resulting identifier is a reserved word it is prefixed with an instance
of *IDENTIFIER-ILLEGAL-CHARACTER-REPLACEMENT*."

  ;; replace any unacceptable characters
  (let ((no-illegal (regex-replace-all *identifier-illegal-character-regexp*
				       (if (symbolp n)
					   (symbol-name n)
					   n)
				       *identifier-illegal-character-replacement*)))

    ;; prepend any keywords
    (if (member no-illegal *reserved-words* :test #'string-equal)
	(setq no-illegal (concat *identifier-illegal-character-replacement* no-illegal)))

    ;; prepend any identifiers not starting with a legal starting character
    (if (not (scan *identifier-legal-leading-character-regexp* (s-first no-illegal)))
	(setq no-illegal (concat *identifier-illegal-character-replacement* no-illegal)))

    ;; bomb-out if the identifier is still illegal
    (if (string-equal no-illegal "_")
	(error 'bad-variable :variable n
			     :hint "Use meaningful variable names :-)"))

    ;; return the legalised identifier
    no-illegal))


;; ---------- General references ----------

(defmethod typecheck ((form symbol) env)
  (get-type form env))


(defmethod float-let-blocks ((form symbol))
  `(,form ()))


(defmethod simplify-progn ((form symbol))
  form)


(defmethod synthesise ((form symbol) (context (eql :inexpression)))
  (as-literal (format nil "~(~a~)" (ensure-legal-identifier (symbol-name form)))))

(defmethod synthesise ((form symbol) (context (eql :inassignment)))
  (synthesise form :inexpression))

(defmethod synthesise ((form symbol) (context (eql :indeclaration)))
  (synthesise form :inexpression))


(defmethod lispify ((form symbol) env)
  form)
