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


(defvar *synthesis-stream* *standard-output*
  "Stream receiving the logical blocks.")


(defvar *indentation-level* 0
  "The current print indentation level.")


(defvar *indentation* 4
  "Number of spaces used at each indentation level.")


(defun indentation ()
  "Return an indentation for the current level."
  (string-times " " (* *indentation-level*
		       *indentation*)))


(defmacro with-indentation (&body body)
  "doc"
  `(let ((*indentation-level* (1+ *indentation-level*)))
     ,@body))


(defun as-block (args context &key before after
				(always t)
				(indented t)
				(sep "")
				(newlines t)
				(process #'synthesise))
  "doc"
  (let ((n (length args)))
    (labels ((format-arg (arg)
	       "Format a single ARG along with any terminator string."
	       (funcall process arg context))

	     (format-args (args)
	       "Format all ARGS along with any separator strings and indentation."
	       (dolist (i (iota n))
		 (if (and indented
			  (or newlines
			      (= i 0)))
		     (format *synthesis-stream* "~a" (indentation)))

		 (format-arg (elt args i))

		 (when (< i (1- n))
		   (format *synthesis-stream* "~a" sep))
		 (if newlines
		     (format *synthesis-stream* "~&")))))

      ;; leading bracket
      (when (and before
		 (or always
		     (> n 1)))
	(if indented
	    (format *synthesis-stream* "~a" (indentation)))
	(format *synthesis-stream* "~a" before)
	(if newlines
	    (format *synthesis-stream* "~&")))

      ;; arguments
      (if (and before after indented)
	  ;; indent the contents
	  (with-indentation
	    (format-args args))

	  ;; format the arguments at the current level
	  (format-args args))

      ;; trailing bracket
      (when (and after
		 (or always
		     (> n 1)))
	(if indented
	    (format *synthesis-stream* "~a" (indentation)))
	(format *synthesis-stream* "~a" after)
	(if newlines
	    (format *synthesis-stream* "~&"))))))


(defun as-body (args context &key before after (process #'synthesise))
  "doc"
  (as-block args context
	    :before before :after after
	    :indented t :newlines t
	    :process process))


(defun as-infix (op args)
  "Synthesise ARGS with OP between them.

Every argument is sythresised in the :inexpression context."
  (as-block args :inexpression :before "("
			       :after ")"
			       :sep (format nil " ~a " op)
			       :always nil
			       :indented nil
			       :newlines nil))


(defun as-list (args context
		&key before after
		  indented newlines
		  (process #'synthesise))
  "Synthesise ARGS as a list.

Each element of ARGS is synthesised in the CONTEXT role.

The list defaults to space-separated, which can be changed using the
SEP key. If BEFORE and AFTER are given, they bracket the list. If
INDENT is T (the default) ARGS are output indented. If NEWLINES is T
(not the default) each element appears on a new line, as do the
brackets. PROCESS (defaults to SYNTHESISE) is applied to each argument
before synthesis, passing the argument and role."
  (as-block args context :before before
			 :after after
			 :sep ", "
			 :indented indented
			 :newlines newlines
			 :process process))
