;; Tests of utility functions
;;
;; Copyright (C) 2023--2025 Simon Dobson
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

(in-package :cl-vhdsl/test)
(in-suite cl-vhdsl)


;; ---------- Looking for non-nil elements of lists  ----------

(test test-index-non-nil
  "Test w can find non-nil elements of lists."
  (is (null (index-non-nil '())))
  (is (null (index-non-nil '(nil))))
  (is (null (index-non-nil '(nil nil nil nil))))

  (is (equal (index-non-nil '(1)) 0))
  (is (equal (index-non-nil '(1 nil)) 0))
  (is (equal (index-non-nil '("str" nil)) 0))
  (is (equal (index-non-nil '(nil 1)) 1))
  (is (equal (index-non-nil '(1 nil)) 0))
  (is (equal (index-non-nil '(nil 1 nil)) 1))
  (is (equal (index-non-nil '(nil nil 1 nil)) 2)))


(test test-non-nil-subseq
  "Test we can extract non-nil sub-sequences of lists."
  (is (null (non-nil-subseq '())))
  (is (null (non-nil-subseq '(nil))))
  (is (null (non-nil-subseq '(nil nil nil))))

  (is (equal (non-nil-subseq '(nil 1 nil)) '(1)))
  (is (equal (non-nil-subseq '(nil 1)) '(1)))
  (is (equal (non-nil-subseq '(nil 1 2)) '(1 2)))
  (is (equal (non-nil-subseq '(nil 1 nil 1 2)) '(1)))
  (is (equal (non-nil-subseq '(nil 1 2 3 nil 1 2)) '(1 2 3))))


;; ---------- Changing list depths ----------

(test test-listify
  "Test we can increase depth."
  (is (null (listify '())))
  (is (equal (listify '(1 2 3))
	     '((1) (2) (3))))
  (is (equal (listify '(1 (2 3) 4))
	     '((1) ((2 3)) (4)))))


(test test-delistify
  "Test we can decrease depth."
  (is (null (delistify '())))
  (is (equal (delistify '((1) (2) (3)))
	     '(1 2 3)))
  (is (equal (delistify '((1) ((2 3)) (4)))
	     '(1 (2 3) 4))))


;; ---------- Zipping while ignoring nulls ----------

(test test-zip-straight
  "Test we can zip lists without nulls."
  (is (equal (zip-without-null '(1 2 3) '(4 5 6))
	     '((1 4) (2 5) (3 6))))
  (is (equal (zip-without-null '(1 (2 3) 4) '(5 6 7))
	     '((1 5) ((2 3) 6) (4 7)))))


(test test-zip-null
  "Test we can zip lists with nulls."
  (is (equal (zip-without-null '(1 nil 3) '(4 5 6))
	     '((1 4) (3 6))))
  (is (equal (zip-without-null '(1 2 3) '(4 5 nil))
	     '((1 4) (2 5)))))


(test test-zip-unequal
  "Test we can zip unequal-length lists with nulls."
  (is (equal (zip-without-null '(1 nil 3) '(4 5 6 7 8))
	     '((1 4) (3 6)))))


(test test-zip-all-null
  "Test we can zip lists that are all nulls"
  (is (null (zip-without-null '(nil nil nil) '(1 2 3))))
  (is (null (zip-without-null '(1 2 3) '(nil nil nil)))))


(test test-zip-empty
  "Test we can zip one empty list."
  (is (null (zip-without-null '(1 2 3) nil)))
  (is (null (zip-without-null nil '(1 2 3)))))


;; ---------- Predicate combinators ----------

(test test-any
  "Test we can construct any-of-p."

  ;; with predicate symbols
  (is (funcall (any-of-p evenp oddp) 2))
  (is (not (funcall (any-of-p symbolp numberp) (list 1 2))))


  ;; with inline function
  (is (funcall (any-of-p evenp (lambda (n) (= n 15))) 15))
  (is (funcall (any-of-p evenp (lambda (n) (= n 15))) 10))

  ;; in a map
  (is (equal (mapcar (any-of-p evenp oddp) (list 1 2 3))
	     '(t t t))))


(test test-all
  "Test we can construct all-of-p."

  ;; with predicate symbols
  (is (funcall (all-of-p evenp) 2))


  ;; with inline function
  (is (funcall (all-of-p oddp (lambda (n) (= n 15))) 15))
  (is (not (funcall (all-of-p evenp (lambda (n) (= n 15))) 10)))

  ;; in a map
  (is (equal (mapcar (all-of-p oddp (lambda (n) (= n 15))) (list 15 15))
	     '(t t)))
  (is (equal (mapcar (all-of-p oddp (lambda (n) (= n 15))) (list 15 14))
	     '(t nil)))
  (is (equal (mapcar (all-of-p evenp (lambda (n) (= n 15))) (list 15 15))
	     '(nil nil))))


;; ---------- safe-cadr ----------

(test test-safe-cadr
  "Test we can safely extract the second element of a pair or list."
  (is (equal (safe-cadr (cons 1 2)) 2))
  (is (equal (safe-cadr (cons 1 (cons 2 3))) 2))
  (is (equal (safe-cadr (list 1 2)) 2))
  (is (equal (safe-cadr (list 1 2 3)) 2))
  (is (null (safe-cadr (list 1))))
  (is (null (safe-cadr (cons 1 nil)))))


;; ---------- Flat maps ----------

(test test-mapappend-as-mapcar
  "Test mapappend defaults to working like mapcar"
  (let ((l (list 1 2 3 4 5)))
    (is (equal (mapcar #'1+ l)
	       (mapappend #'1+ l)))))


(test test-mapappend-flattens
  "Test that mapappend flattens its results."
  (let ((l (list 1 2 (list 3 4) 5)))
    (is (equal (mapcar #'1+ (flatten l))
	       (mapappend #'1+ l)))))


(test test-mapappend-flattens-leading-list
  "Test that mapappend flattens its results with a leading list."
  (let ((l (list (list 1 2) (list 3 4) 5)))
    (is (equal (mapcar #'1+ (flatten l))
	       (mapappend #'1+ l)))))


;; ---------- Last maps ----------

(test test-mapn-one
  "Test we can extract the last value of mapping over a single list."
  (is (= (mapn (lambda (a) (1+ a)) '(1 2 3))
	 4))
  (is (null (mapn (lambda (a) (1+ a)) '()))))


(test test-mapn-two
  "Test we can extract the last value of mapping over two lists."
  (is (= (mapn #'+ '(1 2 3) '(4 5 6))
	 9)))


;; ---------- Repetition ----------

(test test-n-copies-atom
  "Test we can replicate atoms."
  (is (equal (n-copies 5 3)
	     '(5 5 5)))
  (is (equal (n-copies 'var 3)
	     '(var var var)))
  (is (equal (n-copies "bish" 5)
	     '("bish" "bish" "bish" "bish" "bish"))))


(test test-n-copies-list
  "Test we can replicate lists, and they're independent."
  (let ((l '(1 2 3)))
    (is (equal (n-copies l 3)
	       '((1 2 3) (1 2 3) (1 2 3))))

    (is (equal (n-copies nil 4)
	       '(() () () ())))

    (let ((rep (n-copies l 3)))
      ;; changing the original doesn't change the replicas
      (setf (car l) 4)
      (is (equal rep
		 '((1 2 3) (1 2 3) (1 2 3))))

      ;; changing one of the replicas doesn't change the others
      (setf (car (car rep)) 4)
      (is (equal rep
		 '((4 2 3) (1 2 3) (1 2 3))))

      ;; adding an element to a list doesn't change the others
      (appendf (car rep) (list 5 6))
      (is (equal rep
		 '((4 2 3 5 6) (1 2 3) (1 2 3))))

      ;; we can add an element to one repeated nil
      (let ((repnil (n-copies nil 5)))
	(appendf (cadr repnil) (list 1 2))
	(is (equal repnil
		   '(() (1 2) () () ())))))))


;; ---------- Filtering by predicates ----------

(test test-filter
  "Test we can filter by predicates."
  (is (equal (filter-by-predicates '(1 2 3 4 5 6 7)
				   #'oddp #'evenp)
	     '((1 3 5 7) (2 4 6))))

  ;; repetition
  (is (equal (filter-by-predicates '(1 2 3 4 1 5 6 7)
				   #'oddp #'evenp)
	     '((1 3 1 5 7) (2 4 6))))

  ;; one predicate never satisfied
  (is (equal (filter-by-predicates '(1 3 5 7)
				   #'oddp #'evenp)
	     '((1 3 5 7) ())))

  ;; one value satisfying multiple predicates (0 is even and zero)
  (is (equal (filter-by-predicates '(1 3 0 5 7)
				   #'oddp #'evenp #'zerop)
	     '((1 3 5 7) (0) (0)))))
