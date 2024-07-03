;;; A test piece of 6502 assembly code.
;;;
;;; Copyright (C) 2024 Simon Dobson
;;;
;;; This file is part of cl-vhdsl, a Common Lisp DSL for hardware design
;;;
;;; cl-vhdsl is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; cl-vhdsl is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with cl-vhdsl. If not, see <http://www.gnu.org/licenses/gpl.html>.

	;; source and destination block addresses
SOURCE	.EQU	200H
DEST	.EQU	300H

	;; start of program
	.ORG	100H

START	LDX	25		; move 25 bytes
COPY	LDA	SOURCE, X
	STA	DEST, X
	DEX
	BNZ	COPY

	.END
