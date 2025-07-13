	.text
	.globl _start
_start:
	jal main
	li a7, 93
	ecall
fib:
	addi sp, sp, -60
	sw ra, 56(sp)
	sw s0, 52(sp)
	sw s1, 48(sp)
	sw s2, 44(sp)
	sw s3, 40(sp)
	sw s4, 36(sp)
	sw s5, 32(sp)
	sw s6, 28(sp)
	sw s7, 24(sp)
	sw s8, 20(sp)
	sw s9, 16(sp)
	sw s10, 12(sp)
	sw s11, 8(sp)
	sw a0, 4(sp)
	lw t0, 4(sp)
	addi sp, sp, -4
	sw t0, 0(sp)
	li t0, 1
	lw t1, 0(sp)
	addi sp, sp, 4
	slt t0, t0, t1
	xori t0, t0, 1
	beq t0, zero, else_0
	lw a0, 4(sp)
	j fib_return
	j endif_1
else_0:
	addi sp, sp, -4
	sw ra, 0(sp)
	lw t0, 4(sp)
	addi sp, sp, -4
	sw t0, 0(sp)
	li t0, 1
	lw t1, 0(sp)
	addi sp, sp, 4
	sub t0, t1, t0
	addi sp, sp, -4
	sw t0, 0(sp)
	lw a0, 0(sp)
	addi sp, sp, 4
	jal fib
	lw ra, 0(sp)
	addi sp, sp, 4
	mv t0, a0
	sw t0, -4(sp)
	addi sp, sp, -4
	sw ra, 0(sp)
	lw t0, 4(sp)
	addi sp, sp, -4
	sw t0, 0(sp)
	li t0, 2
	lw t1, 0(sp)
	addi sp, sp, 4
	sub t0, t1, t0
	addi sp, sp, -4
	sw t0, 0(sp)
	lw a0, 0(sp)
	addi sp, sp, 4
	jal fib
	lw ra, 0(sp)
	addi sp, sp, 4
	mv t0, a0
	sw t0, -8(sp)
	lw t0, -4(sp)
	addi sp, sp, -4
	sw t0, 0(sp)
	lw t0, -8(sp)
	lw t1, 0(sp)
	addi sp, sp, 4
	add a0, t1, t0
	j fib_return
endif_1:
fib_return:
	lw s0, 52(sp)
	lw s1, 48(sp)
	lw s2, 44(sp)
	lw s3, 40(sp)
	lw s4, 36(sp)
	lw s5, 32(sp)
	lw s6, 28(sp)
	lw s7, 24(sp)
	lw s8, 20(sp)
	lw s9, 16(sp)
	lw s10, 12(sp)
	lw s11, 8(sp)
	lw ra, 56(sp)
	addi sp, sp, 60
	jr ra
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
	addi sp, sp, -4
	sw ra, 0(sp)
	li t0, 10
	addi sp, sp, -4
	sw t0, 0(sp)
	lw a0, 0(sp)
	addi sp, sp, 4
	jal fib
	lw ra, 0(sp)
	addi sp, sp, 4
	mv t0, a0
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
