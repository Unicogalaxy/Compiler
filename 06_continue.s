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
	li t0, 0
	sw t0, -52(s0)
	li t0, 0
	sw t0, -56(s0)
while_loop_0:
while_continue_1:
	lw t0, -52(s0)
	sw t0, -64(s0)
	li t1, 5
	lw t0, -64(s0)
	slt t0, t0, t1
	beq t0, zero, while_end_2
	lw t0, -52(s0)
	sw t0, -64(s0)
	li t1, 1
	lw t0, -64(s0)
	add t0, t0, t1
	sw t0, -52(s0)
	lw t0, -52(s0)
	sw t0, -64(s0)
	li t1, 3
	lw t0, -64(s0)
	sub t0, t0, t1
	xori t0, t0, 0
	beq t0, zero, eq_true_5
	li t0, 0
	j eq_end_6
eq_true_5:
	li t0, 1
eq_end_6:
	beq t0, zero, endif_4
	j while_continue_1
	j endif_4
endif_4:
	lw t0, -56(s0)
	sw t0, -64(s0)
	li t1, 1
	lw t0, -64(s0)
	add t0, t0, t1
	sw t0, -56(s0)
	j while_loop_0
while_end_2:
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
