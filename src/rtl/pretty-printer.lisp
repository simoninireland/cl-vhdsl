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


(defmacro in-logical-block ((&key before after (always t)
			       (indented t))
			    &body body)
  "Synthesise BODY forms in an indented logical block."
  (if (not (or before after))
      ;; no bracketing
      (if indented
	  ;; indented
	  `(let ((*indentation-level* (1+ *indentation-level*)))
	     ,@body)

	  ;; no indentation
	  body)

      ;; insert before and/or after brackets
      `(progn
	 ,(if (and before
		   (or always
		       (> (length body) 1)))
	      `(format *synthesis-stream* "~a~a~&" (indentation) ,before))
	 ,(if indented
	      ;; indented
	      `(let ((*indentation-level* (1+ *indentation-level*)))
		 ,@(mapcar (lambda (form)
			     `(progn
				(format *synthesis-stream* "~a" (indentation))
				form))))

	      ;; no indentation
	      body)
	 ,(if (and after
		   (or always
		       (> (length body) 1)))
	      `(format *synthesis-stream* "~a~a~&" (indentation) ,after)))))


(defun as-infix (op args as)
  "Synthesise ARGS with OP between them.

Each element of ARGS is synthesised in the AS role."
  (format *synthesis-stream* "(")
  (dolist (i (iota (length args)))
    (synthesise (elt args i) as)
    (if (< i (1- (length args)))
	(format *synthesis-stream* " ~a " (symbol-name op))))
  (format *synthesis-stream* ")"))


(defun as-list (args as
		&key before after
		  (sep " ")
		  in-logical-block
		  (process #'synthesise))
  "Synthesise ARGS as a list.

Each element of ARGS is synthesised in the AS role.

The list defaults to space-separated, which can be changed using the
SEP key. If BEFORE and AFTER are given, they bracket the list. If
INDENT is T (the default) ARGS are output indented. If NEWLINES is T
(not the default) each element appears on a new line, as do the
brackets. PROCESS (defaults to SYNTHESISE) is applied to each argument
before synthesis, passing the argument and role."
  (flet ((construct-list ()
	   (dolist (i (iota (length args)))
	     (if in-logical-block
		 (format *synthesis-stream* "~a" (indentation)))
	     (funcall process (elt args i) as)
	     (if (< i (1- (length args)))
		 (format *synthesis-stream* sep))
	     (if in-logical-block
		 (format *synthesis-stream* "~&")))))

    ;; start bracket
    (when before
      (if in-logical-block
	  (in-logical-block ()
	    (format *synthesis-stream* "~a" before))))

    ;; elements
    (in-logical-block (:indented t)
	(construct-list))

    ;; end bracket
    (when after
      (if in-logical-block
	  (in-logical-block ()
	    (format *synthesis-stream* "~a" after))))))
