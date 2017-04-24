.text

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

# input: $a0 = board, output: $v0 = changed bool
.globl rule1
rule1:
	sub     $sp, $sp, 24			# set up stack frame (6 var)
	sw      $ra, 0($sp)			# save return address
	sw      $a0, 4($sp)			# save $a0, board
	li      $t0, 0				# $t0 = changed = false
	sw      $t0, 8($sp)			# save $t0 bool
	li      $t1, 0				# $t1 = i = 0, multiple of 2
rule1_for_1:
	li      $t2, 0				# $t2 = j = 0, multiple of 2
rule1_for_2:
	sw      $t1, 12($sp)			# save $t1
	sw      $t2, 16($sp)			# save $t2
	lw      $a0, 4($sp)			# get $a0, board
	mul     $t1, $t1, 16			# $t1 = $t1 * GRID_SQUARED
	add     $a0, $a0, $t1			# $a0 = $a0 + $t1, row
	add     $a0, $a0, $t2			# $a0 = $a0 + $t2, col
	lhu     $t3, 0($a0)			# $t3 = value = board[i][j]
	sw      $t3, 20($sp)			# save $t3
	move    $a0, $t3			# $a0 = $t3
	jal     has_single_bit_set		# $v0 = single bit bool (0 | 1)
	beq     $v0, $0, rule1_end		# if not($v0 == 0)
	
	li      $t4, 0				# $t4 = k = 0
rule1_for_3:
	lw      $t1, 12($sp)			# get $t1, i
	lw      $t2, 16($sp)			# get $t2, j
	lw      $t3, 20($sp)			# get $t3, value
	
	## eliminate from row
	beq     $t4, $t2, rule1_k_eq_j		# if not(k == j)
	lw      $a0, 4($sp)			# get $a0, board
	mul     $t7, $t1, 16			# $t7 = $t1 * GRID_SQUARED
	add     $a0, $a0, $t7			# $a0 = $a0 + $t7, row
	add     $a0, $a0, $t4			# $a0 = $a0 + $t4, col
	lhu     $t5, 0($a0)			# $t5 = board[i][k]
	and     $t6, $t5, $t3			# $t6 = board[i][k] & value
	beq     $t6, $0, rule1_row_0		# if not($t6 == 0)
	not     $t8, $t3			# $t8 = ~$t3
	and     $t5, $t5, $t8			# $t5 = board[i][k] & ~value
	sh      $t5, 0($a0)			# board[i][k] = $t5
	li      $t0, 1				# $t0 = changed = true
	sw      $t0, 8($sp)			# save $t0 to stack
rule1_k_eq_j:
rule1_row_0:
	## eliminate from column
	beq     $t4, $t1, rule1_k_eq_i		# if not(k == i)
	lw      $a0, 4($sp)			# get $a0, board
	mul     $t7, $t4, 16			# $t7 = $t4 * GRID_SQUARED
	add     $a0, $a0, $t7			# $a0 = $a0 + $t7, row
	add     $a0, $a0, $t2			# $a0 = $a0 + $t2, col
	lhu     $t5, 0($a0)			# $t5 = board[k][j]
	and     $t6, $t5, $t3			# $t6 = board[k][j] & value
	beq     $t6, $0, rule1_col_0		# if not($t6 == 0)
	not     $t8, $t3			# $t8 = ~$t3
	and     $t5, $t5, $t8			# $t5 = board[k][j] & ~value
	sh      $t5, 0($a0)			# board[i][k] = $t5
	li      $t0, 1				# $t0 = changed = true
	sw      $t0, 8($sp)			# save $t0 to stack
rule1_k_eq_i:
rule1_col_0:
	## eliminate from square
	and     $t7, $t1, 0xfffffff8		# $t7 = k = ii, inline func
	and     $a0, $t2, 0xfffffff8		# $a0 = jj, inline func
	add     $a1, $t7, 8			# $a1 = ii + GRIDSIZE, k end
	add     $a2, $a0, 8			# $a2 = jj + GRIDSIZE, l end
	
rule1_for_4:
	move    $t8, $a0			# $t8 = l = jj
rule1_for_5:
	bne     $t7, $t1, rule1_k_ne_i		# if not(k != i)
	bne     $t8, $t2, rule1_l_ne_j		# if not(l != j)
	j       rule1_cont			# then continue	
	
rule1_k_ne_i:
rule1_l_ne_j:
	lw      $t6, 4($sp)			# $t6 = board
	mul     $t5, $t7, 16			# $t5 = k * GRID_SQUARED
	add     $t6, $t6, $t5			# $t6 = $t6 + $t5, row
	add     $t6, $t6, $t8			# $t6 = $t6 + $t8, col
	lhu     $t5, 0($t6)			# $t5 = board[k][l]
	and     $t0, $t5, $t3			# $t0 = board[k][l] & value
	beq     $t0, $0, rule1_square_0		# if not($t0 == 0)
	not     $t9, $t3			# $t9 = ~$t3
	and     $t0, $t5, $t9			# $t0 = board[k][l] & ~value
	sh      $t0, 0($t6)			# board[k][l] = $t0
	li      $t0, 1				# $t0 = changed = true
	sw      $t0, 8($sp)			# save $t0 in stack
	
rule1_square_0:
rule1_cont:
	add     $t8, $t8, 2			# $t8 = jj = $t8 + 2, next short
	bne     $t8, $a2, rule1_for_5		# loop until $t8 = $a2
	add     $t7, $t7, 2			# $t7 = ii = $t7 + 2, next short
	bne     $t7, $a1, rule1_for_4		# loop until $t7 = $a1
	add     $t4, $t4, 2			# $t4 = k = $t4 + 2, next short
	bne     $t4, 32, rule1_for_3		# loop until $t4 = 16 * 2
	
rule1_end:
	lw     $t1, 12($sp)			# get $t1, i
	lw     $t2, 16($sp)			# get $t2, j
	add    $t2, $t2, 2			# $t2 = $t2 + 2, next short
	bne    $t2, 32, rule1_for_2		# loop until $t2 = 16 * 2
	add    $t1, $t1, 2			# $t1 = $t1 + 2, next short
	bne    $t1, 32, rule1_for_1		# loop until $t1 = 16 * 2
	
	lw      $v0, 8($sp)			# return $v0 = changed
	lw      $ra, 0($sp)			# get return address
	add     $sp, $sp, 24			# take down stack frame (6 var)
	jr	$ra

