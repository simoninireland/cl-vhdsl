;; Bitwise access to variables
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

(in-package :cl-vhdsl/rtl)
(declaim (optimize debug))


;; ---------- Single-bit access ----------

(defmethod typecheck-sexp ((fun (eql 'bit)) args env)
  (destructuring-bind (var bit)
      args
    '(fixed-width-unsigned 1)))


(defmethod generalised-place-sexp-p ((fun (eql 'bit)) args env)
  t)


(defmethod synthesise-sexp ((fun (eql 'bit)) args context)
  (destructuring-bind (var bit)
      args
    (synthesise var :indeclaration)
    (as-literal "[ ")
    (synthesise bit :inexpression)
    (as-literal " ]")))


(defmethod lispify-sexp ((fun (eql 'bit)) args env)
  (destructuring-bind (var bit)
      args
    `(logand (ash ,(lispify var) (- ,bit))
	     1)))


;; ---------- Multi-bit access ----------

(defun compute-end-bit (start end width)
  "Compute the end bit given START, END, and WIDTH."
  (if (null end)
      (if width
	  ;; no end set, extract from width if present
	  (progn
	    (setq end (1+ (- start width)))
	    (if (< end 0)
		(error 'not-synthesisable :hint "Width greater than the number of remaining bits")
		end))

	  ;; default is to the end of the pattern
	  0)

      (if width
	  ;; if both are set, width and end must agree
	  (if (/= width (1+ (- start end)))
	      (error 'not-synthesisable :hint "Explicit width does not agree with start and end positions")
	      end)

	  ;; otherwise just use the given end
	  end)))


(defmethod generalised-place-sexp-p ((fun (eql 'bits)) args env)
  t)


(defmethod typecheck-sexp ((fun (eql 'bits)) args env)
  (destructuring-bind (var start &key end width)
      args
    (let ((tyvar (typecheck var env)))
      (setq end (compute-end-bit start end width))

      (let ((l (1+ (- start end))))
	(when (> l (bitwidth tyvar env))
	  (error 'not-synthesisable :hint "Width greater than base variable"))
	`(fixed-width-unsigned ,l)))))


(defmethod synthesise-sexp ((fun (eql 'bits)) args (context (eql :inexpression)))
  (destructuring-bind (var start &key end width)
      args
    (setq end (compute-end-bit start end width))

    (synthesise var :inassignment)
    (as-literal "[ ")
    (synthesise start :inexpression)
    (as-literal " : ")
    (synthesise end :inexpression)
    (as-literal " ]")))


(defmethod lispify-sexp ((fun (eql 'bits)) args env)
  (destructuring-bind (var start &key end width)
      args
    (let ((l (eval-in-static-environment `(+ 1 (- ,start ,end)) env)))
      `(logand (ash ,(lispify var) (- ,end)) (1- (ash 1 ,l))))))


;; ---------- Bitfields ----------

;; We do these with a macro that re-writes variables in the body.
;; TODO: Decide what we do if patterns don't match because of
;; incorrect 0s and 1s. My current idea is don't execute the body,
;; but that'd be a silent failure on the hardware (although it could
;; be checked in simulation)

(defun extract-runs (pattern)
  "Extract consecutive runs of a symbol from PATTERN.

Detection happens regardless of repetition.

Return an alist mapping symbol to start and end positions,
with the rightmost position being 0."
  (labels ((extract (pat i currentrun)
	     (let ((s (car pat)))
	       (cond ((< i 0)
		      (list currentrun))

		     ((null currentrun)
		      (extract (cdr pat)
			       (1- i)
			       (list s i i)))

		     ((equal s (car currentrun))
		      (extract (cdr pat)
			       (1- i)
			       (list s (cadr currentrun) i)))

		     (t
		      (cons currentrun
			    (extract (cdr pat)
				     (1- i)
				     (list s i i))))))))

    (if (null pattern)
	nil
	(extract pattern (1- (length pattern)) nil))))


(defun duplicate-keys-p (l)
  "Test whether alist L has duplicate keys."
  (if (null l)
      nil

      (let ((k (caar l)))
	(if (assoc k (cdr l))
	    t
	    (duplicate-keys-p (cdr l))))))


(defun extract-bitfields (pattern)
  "Extract bitfields from PATTERN.

Return an alist from variable name to bits (as a list).
A MISMATCHED-BITFIELD error is signalled if the same
variable appears in non-consecutive places.

The bitfields also include any 0, 1, and - symbols."
  (let ((runs (extract-runs pattern)))
    ;; check for duplicate symols /except/ 0,1, and -
    (let ((bindings (remove-if (lambda (s)
				 (member (car s) '(0 1 -)))
			       runs)))
      (when (duplicate-keys-p bindings)
	(error 'bitfield-mismatch :pattern pattern
				  :hint "Pattern has non-consecutive runs"))

      runs)))


(defun bitfield-fixed-bit-p (b)
  "Test whether B is a fixed bit.

Fixed bits are constant 0s or 1s."
  (member b '(0 1)))


(defun bitfield-contains-fixed-bits-p (runs)
  "Test whether RUNS includes fixed bits."
  (not (null (find-if (lambda (run)
			(bitfield-fixed-bit-p (car run)))
		      runs))))


(defun run-to-decl (arg run)
  "Return the RTLisp declaration correponding to RUN over ARG."
  (destructuring-bind (s start end)
      run
    (if (= start end)
	;; single bit
	`(,s (bit ,arg ,start) :width 1)

	;; several bits
	`(,s (bits ,arg ,start :end ,end) :width ,(1+ (- start end))))))


(defmacro with-bitfields (pattern arg &body body)
  "Create variables matching the bitfield PATTERN applied to ARG in BODY.

The pattern consists of a list of variable names, with each
entry corresponding to a bit position. The rightmost bit is
bit zero, then bit one to its left, and so forth. If several
consecutive positions have the same variable name, the variable
takes multiple bits.

The pattern may also contain 0, 1, and - for literal 0, 1, and
ignored bits.

For example:

(with-bitfields (o o o a a a a 0)
   opcode
...)

would declare variables o and a in the body, with the values
extracted from bits 7 to 5 and 4 to 1 respectively, and
the lowest-order bit being 0.

If the fixed bits do not match, BODY is not evaluated."

  ;; catch the common error of forgetting the value
  ;; to match against with a one-form body
  (when (< (length body) 1)
    (error 'not-synthesisable :hint "No value to match against?"))

  (let ((runs (extract-bitfields pattern)))
    (if (bitfield-contains-fixed-bits-p runs)
	(error 'not-synthesisable :hint "Fixed bits not yet implemented")

	;; no fixed bits, don't synthesise tests
	(let* ((decls (mapcar (curry #'run-to-decl arg) runs))
	       (newbody (rewrite-variables `(progn ,@body) decls)))
	  newbody))))
