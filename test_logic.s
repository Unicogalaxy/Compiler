	.text
	.globl main
main:
	addi sp, sp, -68
	sw ra, 64(sp)
	sw s0, 60(sp)
	sw s1, 56(sp)
	sw s2, 52(sp)
	sw s3, 48(sp)
	sw s4, 44(sp)
	sw s5, 40(sp)
	sw s6, 36(sp)
	sw s7, 32(sp)
	sw s8, 28(sp)
	sw s9, 24(sp)
	sw s10, 20(sp)
	sw s11, 16(sp)
	li t0, 10
	sw t0, 8(sp)
	li t0, 20
	sw t0, 4(sp)
	li t0, 1
	sw t0, 0(sp)
	li t0, 0
	sw t0, -4(sp)
	lw t0, 8(sp)
	addi sp, sp, -4
	sw t0, 0(sp)
	li t0, 5
	lw t1, 0(sp)
	addi sp, sp, 4
	slt t0, t0, t1
	beq t0, zero, and_false_2
	lw t0, 4(sp)
	addi sp, sp, -4
	sw t0, 0(sp)
	li t0, 15
	lw t1, 0(sp)
	addi sp, sp, 4
	slt t0, t0, t1
	beq t0, zero, and_false_2
	li t0, 1
	j and_end_3
and_false_2:
	li t0, 0
and_end_3:
	beq t0, zero, endif_1
	lw t0, -4(sp)
	addi sp, sp, -4
	sw t0, 0(sp)
	li t0, 10
	lw t1, 0(sp)
	addi sp, sp, 4
	add t0, t1, t0
	sw t0, -4(sp)
	j endif_1
endif_1:
	lw t0, 8(sp)
	addi sp, sp, -4
	sw t0, 0(sp)
	li t0, 5
	lw t1, 0(sp)
	addi sp, sp, 4
	slt t0, t1, t0
	bne t0, zero, or_true_6
	lw t0, 4(sp)
	addi sp, sp, -4
	sw t0, 0(sp)
	li t0, 15
	lw t1, 0(sp)
	addi sp, sp, 4
	slt t0, t0, t1
	bne t0, zero, or_true_6
	li t0, 0
	j or_end_7
or_true_6:
	li t0, 1
or_end_7:
	beq t0, zero, endif_5
	lw t0, -4(sp)
	addi sp, sp, -4
	sw t0, 0(sp)
	li t0, 20
	lw t1, 0(sp)
	addi sp, sp, 4
	add t0, t1, t0
	sw t0, -4(sp)
	j endif_5
endif_5:
	lw t0, 8(sp)
	addi sp, sp, -4
	sw t0, 0(sp)
	lw t0, 4(sp)
	lw t1, 0(sp)
	addi sp, sp, 4
	sub t0, t1, t0
	slt t0, zero, t0
	xori t0, t0, 1
	slt t0, zero, t0
	beq t0, zero, endif_9
	lw t0, -4(sp)
	addi sp, sp, -4
	sw t0, 0(sp)
	li t0, 5
	lw t1, 0(sp)
	addi sp, sp, 4
	add t0, t1, t0
	sw t0, -4(sp)
	j endif_9
endif_9:
	lw t0, 8(sp)
	addi sp, sp, -4
	sw t0, 0(sp)
	li t0, 20
	lw t1, 0(sp)
	addi sp, sp, 4
	slt t0, t0, t1
	beq t0, zero, and_false_12
	lw t0, 0(sp)
	addi sp, sp, -4
	sw t0, 0(sp)
	li t0, 1
	lw t1, 0(sp)
	addi sp, sp, 4
	sub t0, t1, t0
	slt t0, zero, t0
	xori t0, t0, 1
	beq t0, zero, and_false_12
	li t0, 1
	j and_end_13
and_false_12:
	li t0, 0
and_end_13:
	beq t0, zero, else_10
	lw t0, -4(sp)
	addi sp, sp, -4
	sw t0, 0(sp)
	li t0, 100
	lw t1, 0(sp)
	addi sp, sp, 4
	add t0, t1, t0
	sw t0, -4(sp)
	j endif_11
else_10:
	lw t0, -4(sp)
	addi sp, sp, -4
	sw t0, 0(sp)
	li t0, 7
	lw t1, 0(sp)
	addi sp, sp, 4
	add t0, t1, t0
	sw t0, -4(sp)
endif_11:
	lw a0, -4(sp)
	j main_return
main_return:
	lw s0, 60(sp)
	lw s1, 56(sp)
	lw s2, 52(sp)
	lw s3, 48(sp)
	lw s4, 44(sp)
	lw s5, 40(sp)
	lw s6, 36(sp)
	lw s7, 32(sp)
	lw s8, 28(sp)
	lw s9, 24(sp)
	lw s10, 20(sp)
	lw s11, 16(sp)
	lw ra, 64(sp)
	addi sp, sp, 68
	jr ra
