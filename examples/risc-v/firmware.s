.section .text

.global	start

	add  x1, x0, x0
.L0:
	addi x1, x1, 1
	jal  x0, .L0
