	.text
	.globl _start
_start:
	jal main
	li a7, 93
	ecall
fib:
	addi sp, sp, -64
	sw ra, 60(sp)
	sw s0, 56(sp)
	sw s1, 52(sp)
	sw s2, 48(sp)
	sw s3, 44(sp)
	sw s4, 40(sp)
	sw s5, 36(sp)
	sw s6, 32(sp)
	sw s7, 28(sp)
	sw s8, 24(sp)
	sw s9, 20(sp)
	sw s10, 16(sp)
	sw s11, 12(sp)
	sw a0, 4(sp)
	addi sp, sp, -4
	sw t0, 0(sp)
	li t1, 1
	lw t0, 0(sp)
	addi sp, sp, 4
	addi sp, sp, -4
	sw t1, 0(sp)
	lw t0, 4(sp)
	lw t1, 0(sp)
	addi sp, sp, 4
	slt t0, t1, t0
	xori t0, t0, 1
	beq t0, zero, else_0
	lw a0, 4(sp)
	j fib_return
	j endif_1
else_0:
	addi sp, sp, -4
	sw ra, 0(sp)
	addi sp, sp, -4
	sw t0, 0(sp)
	addi sp, sp, -4
	sw t1, 0(sp)
	addi sp, sp, -4
	sw t2, 0(sp)
	addi sp, sp, -4
	sw t3, 0(sp)
	addi sp, sp, -4
	sw t4, 0(sp)
	addi sp, sp, -4
	sw t5, 0(sp)
	addi sp, sp, -4
	sw t6, 0(sp)
	addi sp, sp, -4
	sw a0, 0(sp)
	addi sp, sp, -4
	sw a1, 0(sp)
	addi sp, sp, -4
	sw a2, 0(sp)
	addi sp, sp, -4
	sw a3, 0(sp)
	addi sp, sp, -4
	sw a4, 0(sp)
	addi sp, sp, -4
	sw a5, 0(sp)
	addi sp, sp, -4
	sw a6, 0(sp)
	addi sp, sp, -4
	sw a7, 0(sp)
	addi sp, sp, -4
	sw t0, 0(sp)
	li t1, 1
	lw t0, 0(sp)
	addi sp, sp, 4
	addi sp, sp, -4
	sw t1, 0(sp)
	lw t0, 4(sp)
	lw t1, 0(sp)
	addi sp, sp, 4
	sub a0, t0, t1
	jal fib
	mv t0, a0
	lw a7, 0(sp)
	addi sp, sp, 4
	lw a6, 0(sp)
	addi sp, sp, 4
	lw a5, 0(sp)
	addi sp, sp, 4
	lw a4, 0(sp)
	addi sp, sp, 4
	lw a3, 0(sp)
	addi sp, sp, 4
	lw a2, 0(sp)
	addi sp, sp, 4
	lw a1, 0(sp)
	addi sp, sp, 4
	lw a0, 0(sp)
	addi sp, sp, 4
	lw t6, 0(sp)
	addi sp, sp, 4
	lw t5, 0(sp)
	addi sp, sp, 4
	lw t4, 0(sp)
	addi sp, sp, 4
	lw t3, 0(sp)
	addi sp, sp, 4
	lw t2, 0(sp)
	addi sp, sp, 4
	lw t1, 0(sp)
	addi sp, sp, 4
	lw t0, 0(sp)
	addi sp, sp, 4
	lw ra, 0(sp)
	addi sp, sp, 4
	sw t0, 0(sp)
	addi sp, sp, -4
	sw ra, 0(sp)
	addi sp, sp, -4
	sw t0, 0(sp)
	addi sp, sp, -4
	sw t1, 0(sp)
	addi sp, sp, -4
	sw t2, 0(sp)
	addi sp, sp, -4
	sw t3, 0(sp)
	addi sp, sp, -4
	sw t4, 0(sp)
	addi sp, sp, -4
	sw t5, 0(sp)
	addi sp, sp, -4
	sw t6, 0(sp)
	addi sp, sp, -4
	sw a0, 0(sp)
	addi sp, sp, -4
	sw a1, 0(sp)
	addi sp, sp, -4
	sw a2, 0(sp)
	addi sp, sp, -4
	sw a3, 0(sp)
	addi sp, sp, -4
	sw a4, 0(sp)
	addi sp, sp, -4
	sw a5, 0(sp)
	addi sp, sp, -4
	sw a6, 0(sp)
	addi sp, sp, -4
	sw a7, 0(sp)
	addi sp, sp, -4
	sw t0, 0(sp)
	li t1, 2
	lw t0, 0(sp)
	addi sp, sp, 4
	addi sp, sp, -4
	sw t1, 0(sp)
	lw t0, 4(sp)
	lw t1, 0(sp)
	addi sp, sp, 4
	sub a0, t0, t1
	jal fib
	mv t0, a0
	lw a7, 0(sp)
	addi sp, sp, 4
	lw a6, 0(sp)
	addi sp, sp, 4
	lw a5, 0(sp)
	addi sp, sp, 4
	lw a4, 0(sp)
	addi sp, sp, 4
	lw a3, 0(sp)
	addi sp, sp, 4
	lw a2, 0(sp)
	addi sp, sp, 4
	lw a1, 0(sp)
	addi sp, sp, 4
	lw a0, 0(sp)
	addi sp, sp, 4
	lw t6, 0(sp)
	addi sp, sp, 4
	lw t5, 0(sp)
	addi sp, sp, 4
	lw t4, 0(sp)
	addi sp, sp, 4
	lw t3, 0(sp)
	addi sp, sp, 4
	lw t2, 0(sp)
	addi sp, sp, 4
	lw t1, 0(sp)
	addi sp, sp, 4
	lw t0, 0(sp)
	addi sp, sp, 4
	lw ra, 0(sp)
	addi sp, sp, 4
	sw t0, -4(sp)
	addi sp, sp, -4
	sw t0, 0(sp)
	lw t1, -4(sp)
	lw t0, 0(sp)
	addi sp, sp, 4
	addi sp, sp, -4
	sw t1, 0(sp)
	lw t0, 0(sp)
	lw t1, 0(sp)
	addi sp, sp, 4
	add a0, t0, t1
	j fib_return
endif_1:
fib_return:
	lw s0, 56(sp)
	lw s1, 52(sp)
	lw s2, 48(sp)
	lw s3, 44(sp)
	lw s4, 40(sp)
	lw s5, 36(sp)
	lw s6, 32(sp)
	lw s7, 28(sp)
	lw s8, 24(sp)
	lw s9, 20(sp)
	lw s10, 16(sp)
	lw s11, 12(sp)
	lw ra, 60(sp)
	addi sp, sp, 64
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
	addi sp, sp, -4
	sw t0, 0(sp)
	addi sp, sp, -4
	sw t1, 0(sp)
	addi sp, sp, -4
	sw t2, 0(sp)
	addi sp, sp, -4
	sw t3, 0(sp)
	addi sp, sp, -4
	sw t4, 0(sp)
	addi sp, sp, -4
	sw t5, 0(sp)
	addi sp, sp, -4
	sw t6, 0(sp)
	addi sp, sp, -4
	sw a0, 0(sp)
	addi sp, sp, -4
	sw a1, 0(sp)
	addi sp, sp, -4
	sw a2, 0(sp)
	addi sp, sp, -4
	sw a3, 0(sp)
	addi sp, sp, -4
	sw a4, 0(sp)
	addi sp, sp, -4
	sw a5, 0(sp)
	addi sp, sp, -4
	sw a6, 0(sp)
	addi sp, sp, -4
	sw a7, 0(sp)
	li a0, 10
	jal fib
	mv t0, a0
	lw a7, 0(sp)
	addi sp, sp, 4
	lw a6, 0(sp)
	addi sp, sp, 4
	lw a5, 0(sp)
	addi sp, sp, 4
	lw a4, 0(sp)
	addi sp, sp, 4
	lw a3, 0(sp)
	addi sp, sp, 4
	lw a2, 0(sp)
	addi sp, sp, 4
	lw a1, 0(sp)
	addi sp, sp, 4
	lw a0, 0(sp)
	addi sp, sp, 4
	lw t6, 0(sp)
	addi sp, sp, 4
	lw t5, 0(sp)
	addi sp, sp, 4
	lw t4, 0(sp)
	addi sp, sp, 4
	lw t3, 0(sp)
	addi sp, sp, 4
	lw t2, 0(sp)
	addi sp, sp, 4
	lw t1, 0(sp)
	addi sp, sp, 4
	lw t0, 0(sp)
	addi sp, sp, 4
	lw ra, 0(sp)
	addi sp, sp, 4
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
