;; Tests of SAP-1 reference implementation
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

(in-package :cl-vhdsl/SAP-1/test)


(test test-immediate-exit
  "Test we can immediately exit."
  (let ((mem  (make-array (list 16)
			  :element-type '(integer 0 255)
			  :initial-element 0)))
    (setf (aref mem 0) #2r11110000)    ;; hlt

    (is (equal (run-reference mem) 0))))


(test test-load
  "Test a load."
  (let ((mem  (make-array (list 16)
			  :element-type '(integer 0 255)
			  :initial-element 0)))
    ;; (lda 8) (out) (hlt)
    (setf (aref mem 0) #2r00001000)    ;; lda 8
    (setf (aref mem 1) #2r11100000)    ;; out
    (setf (aref mem 2) #2r11110000)    ;; hlt

    ;; data
    (setf (aref mem 8) #2r11111110)    ;; 254

    (is (equal (run-reference mem) 254))))


(test test-load-no-output
  "Test a load that doesn't change the output register."
  (let ((mem  (make-array (list 16)
			  :element-type '(integer 0 255)
			  :initial-element 0)))
    (setf (aref mem 0) #2r00001000)    ;; lda 8
    (setf (aref mem 1) #2r11110000)    ;; hlt

    ;; data
    (setf (aref mem 8) #2r11111110)    ;; 254

    (is (equal (run-reference mem) 0))))


(test test-load-add-sub
  "Test we can add and subtract."
  (let ((mem  (make-array (list 16)
			  :element-type '(integer 0 255)
			  :initial-element 0)))
    (setf (aref mem 0) #2r00001000)    ;; lda 8
    (setf (aref mem 1) #2r00011001)    ;; add 9
    (setf (aref mem 2) #2r00101010)    ;; sub 10
    (setf (aref mem 3) #2r11100000)    ;; out
    (setf (aref mem 4) #2r11110000)    ;; hlt

    ;; data
    (setf (aref mem 8)  #2r00001100)   ;; 12
    (setf (aref mem 9)  #2r00000010)   ;; 2
    (setf (aref mem 10) #2r00000011)   ;; 3

    (is (equal (run-reference mem) 11))))
