	.text
	.globl main
main:
	addi sp, sp, -56
	sw ra, 52(sp)
	sw s0, 48(sp)
	sw s1, 44(sp)
	sw s2, 40(sp)
	sw s3, 36(sp)
	sw s4, 32(sp)
	sw s5, 28(sp)
	sw s6, 24(sp)
	sw s7, 20(sp)
	sw s8, 16(sp)
	sw s9, 12(sp)
	sw s10, 8(sp)
	sw s11, 4(sp)
	li t0, 1
	sw t0, -4(sp)
	lw t0, -4(sp)
	addi sp, sp, -4
	sw t0, 0(sp)
	li t0, 2
	lw t1, 0(sp)
	addi sp, sp, 4
	add t0, t1, t0
	sw t0, -4(sp)
	lw a0, -4(sp)
	j main_return
main_return:
	lw s0, 48(sp)
	lw s1, 44(sp)
	lw s2, 40(sp)
	lw s3, 36(sp)
	lw s4, 32(sp)
	lw s5, 28(sp)
	lw s6, 24(sp)
	lw s7, 20(sp)
	lw s8, 16(sp)
	lw s9, 12(sp)
	lw s10, 8(sp)
	lw s11, 4(sp)
	lw ra, 52(sp)
	addi sp, sp, 56
	jr ra
