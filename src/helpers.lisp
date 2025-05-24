;; Helper macros for writing DSL functions
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

(in-package :vl)


(defmacro with-vl-errors-not-synthesisable (&body body)
  "Run BODY within a handler that makes Verilisp errors non-synthesisable.

Non-error conditions are ignored; non-Verilisp-specific errors are reported
as NON-SYNTHESISABLE errors."
  `(handler-bind ((error #'(lambda (condition)
			     (cond ((subtypep (type-of condition) 'vl-condition)
				    (error condition))
				   (t
				    (error 'not-synthesisable :underlying-condition condition))))))
     ,@body))


(defun failed-form (condition)
  "Return the form that can't be handled based on CONDITION.

If CONDITION does not indicate such a failure, return nil.

There is no standard way to retrieve this information, so this
function is implementation-dependent."

  #+sbcl
  (if (subtypep (type-of condition) 'sb-pcl::no-applicable-method-error)
      (car (slot-value condition 'sb-pcl::args))

      ;; condition is not caused by an unknown form
      nil)

  ;; can't handle other Lisp implementations for now
  #-sbcl
  nil)


(defmacro with-unknown-forms (&body body)
  "Run BODY in an environment that traps errors due to unknown forms.

Any unknown forms are reported as UNKNOWN-FORM exceptions. The
actual way these forms are captured is unfortunately implementation-specific."
  `(handler-bind ((error #'(lambda (condition)
			     (if-let ((form (failed-form condition)))
			       ;; we encountered an unknown form signal it as such
			       (error 'unknown-form :form form)

			       ;; propagate the condition
			       (error condition)))))
     ,@body))


(defmacro with-continue-on-error (body &body recovery)
  "Run the BODY form, offrering a restart that runs RECOVERY on error.

The restart is called CONTINUE and takes no arguments. The recovery
code should do whatever is necessary to best continue compilation, the
assumption being that not code will be synthesised after such an
error."
  `(restart-case
       ,body
     (continue ()
       ,@recovery)))
