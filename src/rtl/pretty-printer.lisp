;; A simple pretty -printer for block-structured languages
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
(declaim (optimize debug))


;; ---------- Streams and indentation ----------

(defvar *synthesis-stream* *standard-output*
  "Stream receiving the logical blocks.")


(defvar *indentation-level* 0
  "The current print indentation level.")


(defvar *indentation* 4
  "Number of spaces used at each indentation level.")


(defvar *at-start-of-line* t
  "Whether the printer is at the start of a line.

The first literal output to a line is indented according
to *INDENTATION-LEVEL*.")


(defun indentation ()
  "Return an indentation for the current level."
  (string-times " " (* *indentation-level*
		       *indentation*)))


;; ---------- Helper macros ----------

(defmacro with-synthesis-to-stream (str &body body)
  "Send all code synthesised in the BODY forms to STR."
  `(let ((*synthesis-stream* str))
     ,@body))


(defmacro with-indentation (&body body)
  "Output BODY with an extra level of indentation."
  `(let ((*indentation-level* (1+ *indentation-level*)))
     ,@body))


;; ---------- Pretty-printing forms ----------

;; The pretty-printer understands several different structures:
;;
;; - Inline literals, printed one after the other (as-literal)
;; - Inline lists, printed one a line with a separator (as-list)
;; - Body forms, printed on separate lines with pre and post (as-block)
;; - Argument lists, printed on seperate lines with pre and post (as-argument-list)
;; - Operators, with the operator between elements of an line list (as-infix)
;;
;; Each makes use of four lower-level functions:
;;
;; - Printing an individual inline form (as-form)
;; - A set of forms separated by newlines (as-block-forms)
;; - A set of forms on the same line (as-inline-forms)
;; - A newline (as-newline)
;;
;; Each of these functions can take a processor to generate the
;; actual form (synthesise by default).


(defun as-form (arg context &key (process #'synthesise))
  "Format a single ARG in the given CONTEXT.."
  (funcall process arg context))


(defun as-block-forms (args context &key sep (process #'synthesise))
  "Format all ARGS with indentation."
  (let ((n (length args)))
    (dolist (i (iota n))
      (as-form (elt args i) context :process process)

      (when (and sep
		 (< i (- n 1)))
	(as-literal sep))

      (as-newline))))


(defun as-inline-forms (args context &key sep (process #'synthesise))
  "Format all ARGS as an inline list along with any separator strings."
  (let ((n (length args)))
    (dolist (i (iota n))
      ;; form
      (as-form (elt args i) context :process process)

      ;; seperator
      (when (and sep
		 (< i (1- n)))
	(as-literal sep)))))


(defun as-literal (s &key newline)
  "Output the given literal value S."
  ;; indentation (if at the start start of a line)
  (when *at-start-of-line*
    (format *synthesis-stream* "~a" (indentation))
    (setq *at-start-of-line* nil))

  ;; form
  (format *synthesis-stream* "~a" s)

  ;; newline (if requested)
  (if newline
      (as-newline)))


(defun as-newline ()
  "Start a new line."
  (unless *at-start-of-line*
    (format *synthesis-stream* "~%")
    (setq *at-start-of-line* t)))


(defun as-blank-line (&optional (n 1))
  "Output one of more blank lines."
  (as-newline)
  (dolist (i (iota n))
    (format *synthesis-stream* "~%")))


(defun as-list (args context
		&key before after
		  (sep ", ")
		  (process #'synthesise))
  "Synthesise ARGS as an inline list along with BEFORE and AFTER brackets."
  ;; leading bracket
  (when before
    (as-literal before))

  ;; arguments
  (as-inline-forms args :inexpression :sep sep :process process )

  ;; trailing bracket
  (when after
    (as-literal after)))


(defun as-argument-list (args context
			 &key before after
			   (sep ", ")
			   (process #'synthesise))
  "Synthesise ARGS as a list with newlines along with BEFORE abd AFTER brackets."
  ;; leading bracket
  (when before
    (as-literal before :newline t))

  ;; arguments
  (with-indentation
    (as-block-forms args :inexpression :sep sep :process process))

  ;; trailing bracket
  (when after
    (as-literal after :newline t)))


(defun as-block (args context &key before after
				always
				(sep "")
				(process #'synthesise))
  "Output ARGS in CONTEXT within a block."
  (let ((n (length args)))
    ;; leading bracket
    (when (and before
	       (or always
		   (> n 1)))
      (as-literal before :newline t))

    ;; arguments
    (with-indentation
      (as-block-forms args context :sep sep :process process))

    ;; trailing bracket
    (when (and after
	       (or always
		   (> n 1)))
      (as-literal after :newline t))))


(defun as-infix (op args)
  "Synthesise ARGS with OP between them.

Every argument is sythresised in the :inexpression context."
  ;; arguments
  (as-list args :inexpression :sep (format nil " ~a " op)))
