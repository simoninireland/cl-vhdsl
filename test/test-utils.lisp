;; Tests of utility functions
;;
;; Copyright (C) 2023 Simon Dobson
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


;; ---------- Unique-ifying ----------

(test test-unique-empty
  "Test an empty list bis unique."
  (is (equal (uniquify '()) '())))


(test test-unique-unchanged
  "Test we leave all elements if there are none to remove."
  (let* ((l1 '(1 2 3 4 5))
	 (l2 (uniquify l1)))
    (dolist (e l1)
      (is (member e l2)))
    (dolist (e l2)
      (is (member e l1)))))


(test test-unique-remove-one
  "Test we remove a duplicate element."
  (let* ((l1 '(1 5 2 3 4 5))
	 (l2 (uniquify l1)))
    (dolist (e l1)
      (is (member e l2)))
    (is (equal (length l2) (1- (length l1))))))


(test test-unique-remove-one-several
  "Test we remove a duplicate element when duplicateed several times."
  (let* ((l1 '(1 5 2 3 5 4 5))
	 (l2 (uniquify l1)))
    (dolist (e l1)
      (is (member e l2)))
    (is (equal (length l2) (- (length l1) 2)))))


(test test-unique-remove-two
  "Test we two remove duplicate elements."
  (let* ((l1 '(1 5 2 3 4 5 4))
	 (l2 (uniquify l1)))
    (dolist (e l1)
      (is (member e l2)))
    (is (equal (length l2) (- (length l1) 2)))))


(test test-unique-in-order
  "Test we leave the order the same if we don't remove any duplicates."
  (let* ((l1 '(1 2 3 4 5))
	 (l2 (uniquify l1 :in-order t)))
    (is (equal l1 l2))))
