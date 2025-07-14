	.text
	.globl main
main:
	addi sp, sp, -52
	sw ra, 48(sp)
	sw s0, 44(sp)
	sw s1, 40(sp)
	sw s2, 36(sp)
	sw s3, 32(sp)
	sw s4, 28(sp)
	sw s5, 24(sp)
	sw s6, 20(sp)
	sw s7, 16(sp)
	sw s8, 12(sp)
	sw s9, 8(sp)
	sw s10, 4(sp)
	sw s11, 0(sp)
	li a0, 0
	j main_return
main_return:
	lw s0, 44(sp)
	lw s1, 40(sp)
	lw s2, 36(sp)
	lw s3, 32(sp)
	lw s4, 28(sp)
	lw s5, 24(sp)
	lw s6, 20(sp)
	lw s7, 16(sp)
	lw s8, 12(sp)
	lw s9, 8(sp)
	lw s10, 4(sp)
	lw s11, 0(sp)
	lw ra, 48(sp)
	addi sp, sp, 52
	jr ra
