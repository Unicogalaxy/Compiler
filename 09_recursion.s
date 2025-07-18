	.text
	.globl main
fact:
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
	lw t0, -52(s0)
	sw t0, -60(s0)
	li t1, 1
	lw t0, -60(s0)
	slt t0, t1, t0
	xori t0, t0, 1
	beq t0, zero, else_0
	li a0, 1
	j fact_return
	j endif_1
else_0:
	lw t0, -52(s0)
	sw t0, -60(s0)
	sw t0, -64(s0)
	sw t1, -68(s0)
	sw t2, -72(s0)
	sw t3, -76(s0)
	sw t4, -80(s0)
	sw t5, -84(s0)
	sw t6, -88(s0)
	lw t0, -52(s0)
	sw t0, -92(s0)
	li t1, 1
	lw t0, -92(s0)
	sub t0, t0, t1
	mv a0, t0
	jal fact
	lw t0, -88(s0)
	lw t1, -84(s0)
	lw t2, -80(s0)
	lw t3, -76(s0)
	lw t4, -72(s0)
	lw t5, -68(s0)
	lw t6, -64(s0)
	mv t1, a0
	lw t0, -60(s0)
	mul a0, t0, t1
	j fact_return
endif_1:
fact_return:
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
	sw t0, -56(s0)
	sw t1, -60(s0)
	sw t2, -64(s0)
	sw t3, -68(s0)
	sw t4, -72(s0)
	sw t5, -76(s0)
	sw t6, -80(s0)
	li t0, 5
	mv a0, t0
	jal fact
	lw t0, -80(s0)
	lw t1, -76(s0)
	lw t2, -72(s0)
	lw t3, -68(s0)
	lw t4, -64(s0)
	lw t5, -60(s0)
	lw t6, -56(s0)
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
