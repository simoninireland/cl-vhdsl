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

(opts:define-opts
    (:name :output
     :description "Verilog output file"
     :short #\o
     :long "output"
     :arg-parser #'identity
     :default "a.v"
     :meta-var "FILENAME"))


(defun unknown-option (condition)
  (format t "warning: ~s option is unknown~%" (optsmagi:option condition))
  (invoke-restart 'skip-option))


(defmacro when-option ((options opt) &body body)
  "Run forms in BODY when OPT if defined in OPTIONS."
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))


;; ---------- Main function ----------

(defun main ()
  (let ((files '())
	(outputfile nil))

    ;; parse the command-line arguments
    (multiple-value-bind (options free-args)
	(handler-case
	    (handler-bind ((opts:unknown-option #'unknown-option))
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

      (when-option (options :output)
	(setq outputfile it))

      ;; non-option arguments are the input files
      (setq files free-args))

    (format *standard-output* "~a -> ~a~%" files outputfile)

    )
  )
