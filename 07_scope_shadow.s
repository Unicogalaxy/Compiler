	.text
	.globl main
main:
	addi sp, sp, -64
	sw ra, 60(sp)
	sw s0, 56(sp)
	addi s0, sp, 64
	sw s1, -8(s0)
	sw s2, -12(s0)
	sw s3, -16(s0)
	sw s4, -20(s0)
	sw s5, -24(s0)
	sw s6, -28(s0)
	sw s7, -32(s0)
	sw s8, -36(s0)
	sw s9, -40(s0)
	sw s10, -44(s0)
	sw s11, -48(s0)
	li t0, 1
	sw t0, -56(s0)
	li t0, 2
	sw t0, -56(s0)
	lw t0, -56(s0)
	sw t0, -64(s0)
	li t1, 1
	lw t0, -64(s0)
	add t0, t0, t1
	sw t0, -56(s0)
	lw a0, -56(s0)
	j main_return
main_return:
	lw s1, -8(s0)
	lw s2, -12(s0)
	lw s3, -16(s0)
	lw s4, -20(s0)
	lw s5, -24(s0)
	lw s6, -28(s0)
	lw s7, -32(s0)
	lw s8, -36(s0)
	lw s9, -40(s0)
	lw s10, -44(s0)
	lw s11, -48(s0)
	lw ra, 60(sp)
	lw s0, 56(sp)
	addi sp, sp, 64
	jr ra
