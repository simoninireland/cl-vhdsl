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
