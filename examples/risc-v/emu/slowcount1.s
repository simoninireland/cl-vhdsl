/*
	Counter using a subroutine

	Copyright (C) 2024--2025 Simon Dobson

	This file is part of cl-vhdsl, a Common Lisp DSL for hardware design

	cl-vhdsl is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	cl-vhdsl is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with cl-vhdsl. If not, see <http://www.gnu.org/licenses/gpl.html>.
*/

.section .text

.equ	SLOW_BIT, 15

.global	start

	add x10,x0,x0
.L0:
	addi x10,x10,1
	jal x1, .WAIT
	jal x0, .L0

.WAIT:
	addi x11, x0, 1
	slli x11, x11, SLOW_BIT
.L1:
	addi x11, x11, -1
	bne x11, x0, .L1
	jalr x0, x1, 0
