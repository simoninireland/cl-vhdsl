/*
	Sum numbers using a loop, exercising memory access instructions

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

.global _start

_start:
	/* sum in t1 */
	li t1, 0

	/* count loaded into t2 */
	lui a1, %hi(.COUNT)
	addi a1, a1, %lo(.COUNT)
	lb t2, 0(a1)

	/* address of data loaded into a1 */
	lui a1, %hi(.DATA)
	addi a1, a1, %lo(.DATA)
.L0:
	/* load data and add to sum */
	lw t3, 0(a1)
	add t1, t1, t3

	/* increment address and decrement count */
	addi a1, a1, 4
	addi t2, t2, -1

	/* continue if we still have data */
	bne t2, zero, .L0

	/* store result into memory (also in t1) */
	lui a1, %hi(.RESULT)
	addi a1, a1, %lo(.RESULT)
	sw t1, 0(a1)

	/* end */
	ebreak

.align 4
.COUNT:
	/* count of data */
	.byte 4

.align 4
.DATA:
	/* the data itself */
	.word 1
	.word 2
	.word 3
	.word 4
	.word 5   /* this is one more than we count, to check we don't overrun */

.align 4
.RESULT:
	/* the result */
	.word 0
