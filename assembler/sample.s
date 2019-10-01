# jump to entry point
	auipc	x31, 0
	jalr	x0, x31, 25
# fib.17:	
	addi	x5, x0, 1	
	bge	x5, x4, 21	
# blt:	
	addi	x5, x4, -1
	sw	x4, 0(x2)
	addi	x4, x5, 0	
	sw	x1, 1(x2)	
	addi	x2, x2, 2	
	jal	x1, -7	
	addi	x2, x2, -2	
	lw	x1, 1(x2)	
	lw	x5, 0(x2)	
	addi	x5, x5, -2	
	sw	x4, 1(x2)	
	addi	x4, x5, 0	
	sw	x1, 2(x2)	
	addi	x2, x2, 3	
	jal	x1, -16	
	addi	x2, x2, -3	
	lw	x1, 2(x2)	
	lw	x5, 1(x2)	
	add	x4, x5, x4	
	jalr	x0, x1, 0
# bge:	n
	jalr	x0, x1, 0
# program begins
	addi	x3, x0, 0	
	lui	x2, 32	
	addi	x4, x0, 30	
	sw	x1, 0(x2)	
	addi	x2, x2, 1	
	jal	x1, -28	
	addi	x2, x2, -1	
	lw	x1, 0(x2)
# program ends