;; Helper functions and macros
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

(in-package :cl-vhdsl)

;; ---------- Searching lists for non-nil ----------

(defun index-non-nil (l)
  "Return the index (0-based) of the first non-nil element of sequnce L."
  (dolist (i (iota (length l)))
    (if (not (null (elt l i)))
	(return i))))


(defun non-nil-subseq (l)
  "Return the non-nil sub-sequence of L.

This will extract the first such sub-sequence, beginning from the index
extracted by INDEX-NOT-NIL and containing all elements up to the
next nil element of the end of the sequence."
  (when-let ((i (index-non-nil l)))
    (dolist (j (iota (- (length l) (1+ i)) :start 1))
      (if (null (elt l (+ i j)))
	  (return-from non-nil-subseq (subseq l i (+ i j)))))

    ;; if we get here, the rest of the list is non-nil
    (subseq l i)))


;; ---------- Remove duplicates and nil values from a sequence ----------

(defun uniquify (s)
  "Remove duplicates and nils from sequence S."
  (remove-if #'null (remove-duplicates s)))


;; ---------- Increasing and decreasing list nesting ----------

(defun listify (l)
  "Wrap each element of L in a singleton list."
  (mapcar #'list l))


(defun delistify (l)
  "Remove a level of nesting from each element of L."
  (mapcar #'car l))


;; ---------- Zipping in the presence of null ----------

(defun zip-without-null (xs ys)
  "Zip lists XS and YS when elements are not null.

If either element is null, the pair is omitted."
  (when (not (or (null xs)
		 (null ys)))
    (if (or (null (car xs))
	    (null (car ys)))
	(zip-without-null (cdr xs) (cdr ys))

	(cons (list (car xs) (car ys))
	      (zip-without-null (cdr xs) (cdr ys))))))


;; ---------- Predicate combinators ----------

(defun generate-predicate-clause (p c)
  "Geerate the predicate clause P testing variable C."
  (cond ((symbolp p)
	 `(,p ,c))
	((consp p)
	 (equal (symbol-name (car p)) "lambda")
	 `(funcall ,p ,c))
	(t
	 (error "Can't parser disjunct ~a" p))))


(defun generate-predicate-clauses (preds c)
  "Generate clauses for PREDS testing variable C."
  (mapcar #'(lambda (p)
	      (generate-predicate-clause p c))
	  preds))


(defmacro any-of-p (&rest preds)
  "Generate an inline lambda predicate matching any of PREDS.

Each element of PREDS can be a symbol representing a function (predicate)
or an inline function (lambda-expression)."
  (with-gensyms (c)
    (let ((pred-clauses (generate-predicate-clauses preds c)))
      `#'(lambda (,c)
	   (or ,@pred-clauses)))))


(defmacro all-of-p (&rest preds)
  "Generate an inline lambda predicate matching all of PREDS.

Each element of PREDS can be a symbol representing a function (predicate)
or an inline function (lambda-expression)."
  (with-gensyms (c)
    (let ((pred-clauses (generate-predicate-clauses preds c)))
      `#'(lambda (,c)
	   (and ,@pred-clauses)))))


;; ---------- Second element of pair or list ----------

;; Neither of `elt' or `cadr' ar safe when applied to pairs.

(defun safe-cadr (l)
  "Return  the second element of L.

L can be a list or a pair."
  (if (consp (cdr l))
      (cadr l)
      (cdr l)))


;; ---------- Flat maps ----------

(defun mapappend (f &rest ls)
  "Apply F to all elements of lists LS at whatever depth, returning a flat list of results."
  (flatten (mapcar f (flatten ls))))


;; ---------- A map returning only the last result ----------

;; this could be a lot more optimised
(defun mapn (fun &rest lists)
  "Map FUN across the cars of LISTS, returning the value of the last application.

If the lists are empty, the result is NIL.

The name MAPN is supposed to invoke the behaviour of PROGN."
  (let ((res (apply #'mapcar (cons fun lists))))
    (if (listp res)
	(car (last res)))))


;; ---------- Folds ----------

;; These are just wrappers around REDUCE, but I find them easier to remember.

(defun foldl (fun l init)
  "Fold the values of L left through FUN, starting with initial value INIT."
  (reduce fun l :from-end t :initial-value init))


(defun foldr (fun l init)
  "Fold the values of L rightwards through FUN, starting with INIT."
  (reduce fun l :initial-value init))


;; ---------- String utilities ----------

(defun string-times (str n)
  "Return a string consisting of N copies of STR."
  (apply #'concatenate (cons'string (mapcar (lambda (i) str) (iota n)))))
