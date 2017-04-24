.data
symbollist: .ascii  "0123456789ABCDEFG"

hard_board:
.half  65535     1  2048  1024 65535 65535   128 65535 65535 16384 65535 65535  4096 32768  8192 65535
.half  65535 16384 65535 65535  2048 65535 65535 65535 65535 65535 65535     4 65535 65535     2 65535
.half      4 65535 65535 65535    16 65535     2   256 32768   128 65535    64 65535 65535 65535   512
.half   8192     2 65535   256 65535 65535 65535 65535 65535 65535 65535 65535     8 65535    64 16384
.half   2048 65535 65535 16384 65535 65535  4096    16     8     4 65535 65535  1024 65535 65535   128
.half  65535 65535     2 65535  8192 65535     8     4  2048  1024 65535     1 65535    32 65535 65535
.half   1024 65535     1     4    32 65535 65535 65535 65535 65535 65535  4096 16384     8 65535  8192
.half  65535  8192     8 65535 65535   128 16384 65535 65535    32   256 65535 65535  4096  2048 65535
.half  65535    64 16384 65535 65535     8  1024 65535 65535 32768  2048 65535 65535   512     4 65535
.half      1 65535  1024  8192     4 65535 65535 65535 65535 65535 65535     8 32768     2 65535  2048
.half  65535 65535     4 65535   512 65535   256  8192  1024     2 65535 16384 65535    16 65535 65535
.half    128 65535 65535    16 65535 65535  2048    32    64  4096 65535 65535  8192 65535 65535     8
.half    256  1024 65535     2 65535 65535 65535 65535 65535 65535 65535 65535    16 65535     1  4096
.half     32 65535 65535 65535     1 65535  8192     8   256    16 65535  1024 65535 65535 65535     4
.half  65535    16 65535 65535    64 65535 65535 65535 65535 65535 65535   128 65535 65535    32 65535
.half  65535     4    64     1 65535 65535    16 65535 65535  2048 65535 65535   128  8192     8 65535

.text
main:
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)

	la	$a0, hard_board
	jal	solve
	la	$a0, hard_board
	jal	print_board

	lw	$ra, 0($sp)
	add	$sp, $sp, 4
	jr	$ra


has_single_bit_set:
	beq	$a0, 0, hsbs_ret_zero	# return 0 if value == 0
	sub	$a1, $a0, 1
	and	$a1, $a0, $a1
	bne	$a1, 0, hsbs_ret_zero	# return 0 if (value & (value - 1)) == 0
	li	$v0, 1
	jr	$ra
hsbs_ret_zero:
	li	$v0, 0
	jr	$ra


get_lowest_set_bit:
	li	$v0, 0			# i
	li	$t1, 1

glsb_loop:
	sll	$t2, $t1, $v0		# (1 << i)
	and	$t2, $t2, $a0		# (value & (1 << i))
	bne	$t2, $0, glsb_done
	add	$v0, $v0, 1
	blt	$v0, 16, glsb_loop	# repeat if (i < 16)

	li	$v0, 0			# return 0
glsb_done:
	jr	$ra


print_board:
	sub	$sp, $sp, 20
	sw	$ra, 0($sp)		# save $ra and free up 4 $s registers for
	sw	$s0, 4($sp)		# i
	sw	$s1, 8($sp)		# j
	sw	$s2, 12($sp)		# the function argument
	sw	$s3, 16($sp)		# the computed pointer (which is used for 2 calls)
	move	$s2, $a0

	li	$s0, 0			# i
pb_loop1:
	li	$s1, 0			# j
pb_loop2:
	mul	$t0, $s0, 16		# i*16
	add	$t0, $t0, $s1		# (i*16)+j
	sll	$t0, $t0, 1		# ((i*16)+j)*2
	add	$s3, $s2, $t0
	lhu	$a0, 0($s3)
	jal	has_single_bit_set		
	beq	$v0, 0, pb_star		# if it has more than one bit set, jump
	lhu	$a0, 0($s3)
	jal	get_lowest_set_bit	# 
	add	$v0, $v0, 1		# $v0 = num
	la	$t0, symbollist
	add	$a0, $v0, $t0		# &symbollist[num]
	lb	$a0, 0($a0)		#  symbollist[num]
	li	$v0, 11
	syscall
	j	pb_cont

pb_star:		
	li	$v0, 11			# print a "*"
	li	$a0, '*'
	syscall

pb_cont:	
	add	$s1, $s1, 1		# j++
	blt	$s1, 16, pb_loop2

	li	$v0, 11			# at the end of a line, print a newline char.
	li	$a0, '\n'
	syscall	
	
	add	$s0, $s0, 1		# i++
	blt	$s0, 16, pb_loop1

	lw	$ra, 0($sp)		# restore registers and return
	lw	$s0, 4($sp)
	lw	$s1, 8($sp)
	lw	$s2, 12($sp)
	lw	$s3, 16($sp)
	add	$sp, $sp, 20
	jr	$ra


get_square_begin:
	# round down to the nearest multiple of 4
	and	$v0, $a0, 0xfffffffc
	jr	$ra


## void 
## solve(unsigned short board[GRID_SQUARED][GRID_SQUARED]) {
##   bool changed;
##   do {
##     changed = rule1(board);
##     changed |= rule2(board);
##   } while (changed);
## }

solve:
	jr	$ra


## bool
## rule1(unsigned short board[GRID_SQUARED][GRID_SQUARED]) {
##   bool changed = false;
##   for (int i = 0 ; i < GRID_SQUARED ; ++ i) {
##     for (int j = 0 ; j < GRID_SQUARED ; ++ j) {
##       unsigned value = board[i][j];
##       if (has_single_bit_set(value)) {
##         for (int k = 0 ; k < GRID_SQUARED ; ++ k) {
##           // eliminate from row
##           if (k != j) {
##             if (board[i][k] & value) {
##               board[i][k] &= ~value;
##               changed = true;
##             }
##           }
##           // eliminate from column
##           if (k != i) {
##             if (board[k][j] & value) {
##               board[k][j] &= ~value;
##               changed = true;
##             }
##           }
##         }
## 
##         // elimnate from square
##         int ii = get_square_begin(i);
##         int jj = get_square_begin(j);
##         for (int k = ii ; k < ii + GRIDSIZE ; ++ k) {
##           for (int l = jj ; l < jj + GRIDSIZE ; ++ l) {
##             if ((k == i) && (l == j)) {
##               continue;
##             }
##             if (board[k][l] & value) {
##               board[k][l] &= ~value;
##               changed = true;
##             }
##           }
##         }
##       }
##     }
##   }
##   return changed;
## }

rule1:
	li	$v0, 0
	jr	$ra


## bool
## rule2(unsigned short board[GRID_SQUARED][GRID_SQUARED]) {
##   bool changed = false;
##   for (int i = 0 ; i < GRID_SQUARED ; ++ i) {
##     for (int j = 0 ; j < GRID_SQUARED ; ++ j) {
##       unsigned value = board[i][j];
##       if (has_single_bit_set(value)) {
##         continue;
##       }
##       
##       int jsum = 0, isum = 0;
##       for (int k = 0 ; k < GRID_SQUARED ; ++ k) {
##         if (k != j) {
##           jsum |= board[i][k];        // summarize row
##         }
##         if (k != i) {
##           isum |= board[k][j];         // summarize column
##         }
##       }
##       if (ALL_VALUES != jsum) {
##         board[i][j] = ALL_VALUES & ~jsum;
##         changed = true;
##         continue;
##       } else if (ALL_VALUES != isum) {
##         board[i][j] = ALL_VALUES & ~isum;
##         changed = true;
##         continue;
##       }
## 
##       // eliminate from square
##       int ii = get_square_begin(i);
##       int jj = get_square_begin(j);
##       unsigned sum = 0;
##       for (int k = ii ; k < ii + GRIDSIZE ; ++ k) {
##         for (int l = jj ; l < jj + GRIDSIZE ; ++ l) {
##           if ((k == i) && (l == j)) {
##             continue;
##           }
##           sum |= board[k][l];
##         }
##       }
## 
##       if (ALL_VALUES != sum) {
##         board[i][j] = ALL_VALUES & ~sum;
##         changed = true;
##       } 
##     }
##   }
##   return changed;
## }

rule2:
	li	$v0, 0
	jr	$ra
