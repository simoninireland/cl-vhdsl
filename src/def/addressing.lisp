;; Addressing mode definitions
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

(in-package :cl-vhdsl/def)
(named-readtables:in-readtable :interpol-syntax)


;; ---------- Addressing modes ----------

(defclass addressing-mode ()
  ()
  (:documentation "A description of an addressing mode."))


(defmethod addressing-mode-regexp ((ins addressing-mode))
  (addressing-mode-regexp (class-name (class-of ins))))


(defgeneric addressing-mode-bytes (mode)
  (:documentation "Return the bytes that encode the address in MODE."))


(defgeneric addressing-mode-behaviour (mode c)
  (:documentation "Return the data specified by MODE on core C."))


;; These three functions are used to parse an addressing mode from its
;; assembler source form. `addressing-mode-regexp' should return the
;; regexp identifying the addressing mode in assembly code source.
;; This should be designed to return the important fields only, which
;; are then passed as a list to `addressing-mode-parse' to be included
;; into the `addressing-mode' object.
;;
;; The `initialize-instance' method is called during construction of
;; the `addressing-mode' object and is passed the extracted strings
;; under the `:parse' key, which is passes to `addressing-mode-parse'
;; as the last part of the initialisation chain (this is what the
;; `:after' advice is for).

(defgeneric addressing-mode-regexp (cls)
  (:documentation "Return the regexp used to represent data in addressing mode CLS."))


(defgeneric addressing-mode-parse (mode strs)
  (:documentation "Parse STRS to setup the value of MODE.

STRS will be the patterns parsed from the assembler text by
`addressing-mode-regexp'."))


(defmethod initialize-instance :after ((mode addressing-mode) &key parse &allow-other-keys)
  (when parse
    (addressing-mode-parse mode parse)))


;; ---------- Impicit addressing ----------

(defclass implicit (addressing-mode)
  ()
  (:documentation "The implicit addressing mode where all the information is in the instruction."))


(defun implicit (&rest args)
  (apply #'make-instance (cons 'implicit args)))

(defmethod addressing-mode-regexp ((cls (eql 'implicit)))
  "")

(defmethod addressing-mode-parse ((mode implicit) ss))

(defmethod addressing-mode-behaviour ((mode implicit) c)
  nil)

(defmethod addressing-mode-bytes ((mode implicit))
  nil)


;; ---------- Addressing modes type specifier ----------

(defun list-of-addressing-modes-p (modes)
  "Helper predicate to check a list of addressing mode types (not instances)."
  (and (consp modes)
       (every #'(lambda (mode) (subtypep mode 'addressing-mode)) modes)))


(deftype list-of-addressing-modes ()
  "The type of lists of addressing modes"
  `(satisfies list-of-addressing-modes-p))


;; ---------- Assembler patterns look-up ----------

(defun assembler-make-addressing-modes-regexp (clns)
  "Convert a list of class names CLNS to a regexp that recognises their address modes."
  (flet ((addressing-mode-re (cln)
	   (let ((mn (addressing-mode-regexp cln)))
	     #?"(?:${mn})")))
    (let ((pats (mapcar #'addressing-mode-re clns)))
      (format nil "^(?:~{~a~^|~})$" pats))))


(defun assembler-get-addressing-mode (s clns &optional re)
  "Return the class from the list CLNS implementing the addressing mode in S.

The return value is a list of the class and the list of strings parsed.

If RE is provided it should be a regexp constructed by
`assembler-make-addressing-modes-regexp' which will be used for the matching.
This will not be checked against the list of classes. This is an optimisation
to allow the regexp to be re-used across multiple instructions, rather
than being re-created."
  (when (null re)
    (setq re (assembler-make-addressing-modes-regexp clns)))
  (multiple-value-bind (suc matches) (scan-to-strings re s)
    (when suc
      (let ((i (index-non-nil matches))
	    ;; the strings come back as a vector, but that's hard
	    ;; to work with: it's easier to keep everything as lists
	    (ss (coerce (non-nil-subseq matches) 'list)))
	(list (if (null i)
		  nil
		  (elt clns i))
	      ss)))))
