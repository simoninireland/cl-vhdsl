;; Tests of shadowing pass
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

(in-package :verilisp/test)
(in-suite verilisp/vl)


(test test-detect-shadowed
  "Test we can detect a shadowed variable."
  (signals (vl:duplicate-variable)
    (let ((p (copy-tree '(let ((a 12))
			  (setq a (+ a 1))
			  (let ((b 1)
				(a 34))
			    (setq a (+ b a)))))))
      (vl:typecheck p)
      (vl::detect-shadowing p)))

  ;; no shadowing
  (let ((p (copy-tree '(let ((a 12))
			(setq a (+ a 1))
			(let ((b 1)
			      (c 34))
			  (setq c (+ b a)))))))
    (vl:typecheck p)
    (is (vl::detect-shadowing p))))


(test test-detect-shadowed-module
  "Test we can detect shadowing in the body of a module."
  (signals (vl:duplicate-variable)
    (let ((p  (copy-tree '(vl::module test ((clk :direction :in))
			   (let ((a 12))
			     (setq a (+ a 1))
			     (let ((b 1)
				   (a 34))
			       (setq a (+ b a))))))))
      (vl:typecheck p)
      (vl::detect-shadowing p))))
