	.text
	.globl main
add:
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
	sw a0, -52(s0)
	sw a1, -56(s0)
	lw t0, -52(s0)
	sw t0, -64(s0)
	lw t1, -56(s0)
	lw t0, -64(s0)
	add a0, t0, t1
	j add_return
add_return:
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
	sw t0, -60(s0)
	sw t1, -64(s0)
	sw t2, -68(s0)
	sw t3, -72(s0)
	sw t4, -76(s0)
	sw t5, -80(s0)
	sw t6, -84(s0)
	li t0, 3
	mv a0, t0
	li t0, 4
	mv a1, t0
	jal add
	lw t0, -84(s0)
	lw t1, -80(s0)
	lw t2, -76(s0)
	lw t3, -72(s0)
	lw t4, -68(s0)
	lw t5, -64(s0)
	lw t6, -60(s0)
	mv t0, a0
	sw t0, -52(s0)
	lw a0, -52(s0)
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
