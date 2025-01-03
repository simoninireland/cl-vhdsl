;; Command line VHDSL-to-Verilog transpiler
;;
;; Copyright (C) 2024--2025 Simon Dobson
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

(in-package :cl-vhdsl/cli)


;; ---------- Command-line options ----------

(defvar *errors* 0
  "Number of errors caught.")


(defvar *verbosity* 0
  "Verbosity (reporting) level (higher is more verbose).")


(defvar *fail-on-load* nil
  "If T, don't synthesise any files if there were errors in loading any.")


(opts:define-opts
  (:name :verbose
   :description "Verbose output"
   :short #\v
   :long "verbose")
  (:name :fail-on-load
   :description "Don't synthesise if there were errors loading"
   :short #\f
   :long "fail"))


(defun unknown-option (condition)
  (format t "warning: ~s option is unknown~%" (opts:option condition))
  (invoke-restart 'skip-option))


(defmacro when-option ((options opt) &body body)
  "Run forms in BODY when OPT if defined in OPTIONS."
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))


(defun parse-command-line ()
  "Parse the command line options.

This will set the various status flags and return
a list of files to be processed."
  ;; parse the command-line arguments
  (multiple-value-bind (options free-args)
      (handler-case
	  (handler-bind
	      ((opts:unknown-option #'unknown-option))
	    (opts:get-opts))

	(opts:missing-arg (condition)
	  (format t "fatal: option ~s needs an argument~%"
		  (opts:option condition)))
	(opts:arg-parser-failed (condition)
	  (format t "fatal: cannot parse ~s as argument of ~s~%"
		  (opts:raw-arg condition)
		  (opts:option condition)))
	(opts:missing-required-option (con)
	  (format t "fatal: ~a~%" con)
	  (opts:exit 1)))

    (when-option (options :verbose)
      (setq *verbosity* 1))
    (when-option (options :fail-on-load)
      (setq *fail-on-load* t))

    ;; use standard input if - appears as a filename
    ;; TBD

    ;; return the files, which are all the non-option arguments
    free-args))


;; ---------- Filename handling ----------

(defun filename-for-module (modname)
  "Return the filename used to store module MODNAME.

The filename is the name of the module in lowwr case with
a .v (Verilog) extension."
  (format nil "~(~a~).v" modname))


(defun load-module (fn)
  "Load the code in FN."
  (load fn :if-does-not-exist t))


(defun synthesise-module (m fn)
  "Synthesise module M into FN."
  (with-open-file (str fn :direction :output
			  :if-exists :supersede)
    (let ((*synthesis-stream* str))
      (synthesise m :toplevel))))


;; ---------- Main function ----------

(defun main ()
  (let ((files (parse-command-line)))

    ;; exit if no files given
    (when (= (length files) 0)
      (format *error-output* "No input files given~%" c)
      (uiop:quit 1))

    (handler-bind
	((error (lambda (c)
		  (format *error-output* "~a~%" c)
		  (incf *errors*)
		  (invoke-restart 'ignore-file-with-errors))))

      ;; load all the source files in order
      (dolist (fn files)
	(restart-case
	    (progn
	      (when (> *verbosity* 0)
		(format *standard-output* "Loading ~a~%" fn))
	      (load-module fn))

	  (ignore-file-with-errors () nil)))

      ;; check whether to bail out
      (when (and *fail-on-load*
		 (> *errors* 0))
	(format *error-output* "~a errors~%" *errors*)
	(format *error-output* "Not synthsising")
	(uiop:quit 1))

      ;; generate the Verilog for each module
      (dolist (m (get-modules-for-synthesis))
	(restart-case
	    (destructuring-bind (modname module)
		m
	      (let ((fn (filename-for-module modname)))
		(when (> *verbosity* 0)
		  (format *standard-output* "Synthesiing ~a to ~a~%"
			  modname fn))
		(synthesise-module module fn)))

	  (ignore-file-with-errors () nil))))

    ;; report errors if there were any
    (when (> *verbosity* 0)
      (format *standard-output* "~a errors~%" *errors*))

    ;; exit
    (uiop:quit (if (> *errors* 0)
		   1
		   0))))
