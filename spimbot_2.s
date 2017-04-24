.data

# spimbot constants
NUM_FLAGS   = 40	# maximum flags you can ever have on the board
BASE_RADIUS = 24
BASE_X = 15
BASE_Y = 150
MAX_FLAGS   = 5		# maximum flags you can have in hand (might not be optimal though)
FLAG_COST   = 7
INVIS_COST  = 25

# memory-mapped I/O
VELOCITY           = 0xffff0010
ANGLE              = 0xffff0014
ANGLE_CONTROL      = 0xffff0018
BOT_X              = 0xffff0020
BOT_Y              = 0xffff0024
FLAG_REQUEST       = 0xffff0050
PICK_FLAG          = 0xffff0054
FLAGS_IN_HAND      = 0xffff0058
GENERATE_FLAG      = 0xffff005c
ENERGY             = 0xffff0074
ACTIVATE_INVIS     = 0xffff0078 
PRINT_INT          = 0xffff0080
PRINT_FLOAT        = 0xffff0084
PRINT_HEX          = 0xffff0088
SUDOKU_REQUEST     = 0xffff0090
SUDOKU_SOLVED      = 0xffff0094
OTHER_BOT_X        = 0xffff00a0
OTHER_BOT_Y        = 0xffff00a4
COORDS_REQUEST     = 0xffff00a8
SCORE              = 0xffff00b0
ENEMY_SCORE        = 0xffff00b4

# interrupt memory-mapped I/O
TIMER              = 0xffff001c
BONK_ACKNOWLEDGE   = 0xffff0060
COORDS_ACKNOWLEDGE = 0xffff0064
TIMER_ACKNOWLEDGE  = 0xffff006c
TAG_ACKNOWLEDGE    = 0xffff0070
INVIS_ACKNOWLEDGE  = 0xffff007c

# interrupt masks
TAG_MASK           = 0x400
INVIS_MASK         = 0x800
BONK_MASK          = 0x1000
COORDS_MASK        = 0x2000
TIMER_MASK         = 0x8000

# syscall constants
PRINT_INTEGER = 1
PRINT_STRING = 4
PRINT_CHAR = 11

# program constants
sudoku				= 0x10001000
admode				= 0x10001200
opxpos				= 0x10001204
opypos				= 0x10001208
binvis				= 0x1000120C
arflag				= 0x10001210
singsp				= 0x10001350
single				= 0x10001354

three:	.float	3.0
five:	.float	5.0
PI:	.float	3.141592
F180:	.float  180.0

puzzle_data:
.half	0	0	0	0	0	0	0	0
.half	0	0	0	0	0	0	0	0
.half	0	0	0	0	0	0	0	0
.half	0	0	0	0	0	0	0	0
.half	0	0	0	0	0	0	0	0
.half	0	0	0	0	0	0	0	0
.half	0	0	0	0	0	0	0	0
.half	0	0	0	0	0	0	0	0

mode_data:
.word	1 # 2

# 0 - OFF
# 1 - DEFENSE
# 2 - OFFENSE
# more modes to come?

flag_data:
.space	320 # 80

flag_end:
.word	-1

.text

#==========
# MAIN
#==========

main:
	sub	$sp	$sp	4
	sw	$ra	0($sp)
	
	# initialize opp coords
	sw	$t0	COORDS_REQUEST
	lw	$t0	OTHER_BOT_X
	sw	$t0	opxpos
	lw	$t0	OTHER_BOT_Y
	sw	$t0	opypos
	
	# initialize interrupts
	li	$t0, TIMER_MASK		# timer interrupt enable bit
	or	$t0, $t0, COORDS_MASK	# coords interrupt bit
	or	$t0, $t0, 1		# global interrupt enable
	mtc0	$t0, $12		# set interrupt mask (Status register)
	
	sw		$0, VELOCITY
	jal		init
	
	li		$s6, 5
mainlp:
	la		$a0, sudoku
	sw		$a0, SUDOKU_REQUEST	# request for unsolved sudoku board
	
	la 		$a0, sudoku
	jal		scan				# scan for squares w/ 1 bit set
	
	la		$a0, sudoku
	jal 	rule1				# solve using rule 1
	
	la		$a0, sudoku
	sw		$a0, SUDOKU_SOLVED	# signal that puzzle is solved
	
	sw	$t0, GENERATE_FLAG
	sw 	$t0, GENERATE_FLAG
	sw  $t0, GENERATE_FLAG
	add	$s6, $s6, 3
	#blt	$s6, 40, mainlp

	# request timer interrupt
	lw	$t0, TIMER		# current time
	add	$t0, $t0, 50  
	sw	$t0, TIMER		# request timer in 50 cycles

main_loop:
	la	$t0	mode_data
	lw	$t0	0($t0)
	beq	$t0	$0	end_main

	li	$t1	1
	bne	$t0	$t1	main_not_mode1
	jal	defense_mode
	j	main_loop
main_not_mode1:

	li	$t1	2
	bne	$t0	$t1	main_not_mode2
	jal	offense_mode
	j	main_loop
main_not_mode2:
			#hopefully never gets here
	j	main_loop
end_main:
	lw	$ra	0($sp)
	add	$sp	$sp	4
	jr	$ra


#==========
# SUDOKU
#==========

request_sudoku:
	sub	$sp	$sp	4
	sw	$t0	0($sp)		#opposite convention, callee saved
	la	$t0	puzzle_data
	sw	$t0	SUDOKU_REQUEST
	lw	$t0	0($sp)
	add	$sp	$sp	4
	jr	$ra

solve_sudoku:
	sub	$sp	$sp	4
	sw	$ra	0($sp)
	la	$a0	puzzle_data
	jal	rule1
	lw	$ra	0($sp)
	add	$sp	$sp	4
	la	$t0	puzzle_data
	sw	$t0	SUDOKU_SOLVED
	jr	$ra

#----------
#applies rule 1
#----------
#rule1:
r1:
	sub	$sp	$sp	28	#allocate memory

	li	$t0	0		#bool changed = false
	li	$t1	0		#int i=0
r1_for1:				#GRID_SQUARED = 16
	bge	$t1	16	r1_afor1
	li	$t2	0		#int j=0;
r1_for2:
	bge	$t2	16	r1_afor2

	mul	$t4	$t1	16
	add	$t4	$t4	$t2
	mul	$t4	$t4	2
	add	$t4	$t4	$a0
	lhu	$t3	0($t4)		#value = board[i][j]

	sw	$t0	0($sp)		#save variables
	sw	$t1	4($sp)
	sw	$t2	8($sp)
	sw	$t3	12($sp)
	sw	$a0	16($sp)
	sw	$ra	20($sp)

	move	$a0	$t3		#load arguments
	jal	has_single_bit_set

	lw	$t0	0($sp)		#load variables
	lw	$t1	4($sp)
	lw	$t2	8($sp)
	lw	$t3	12($sp)
	lw	$a0	16($sp)
	lw	$ra	20($sp)

	beq	$v0	$0	r1_aif1	#if(has_single_bit_set(value))

	li	$t4	0		#int k=0
r1_for3:
	bge	$t4	16	r1_afor3

	beq	$t4	$t2	r1_aif2	#if(k != j)

	mul	$t7	$t1	16	#i*2*16 + k*2
	add	$t7	$t7	$t4
	mul	$t7	$t7	2
	add	$t7	$t7	$a0	#t7 = &board[i][k]
	lhu	$t5	0($t7)		#t5 = board[i][k]

	and	$t6	$t5	$t3	#t6 = board[i][k] & value
	beq	$t6	$0	r1_aif3
	nor	$t6	$t3	$t3	#~value
	and	$t6	$t6	$t5
	sh	$t6	0($t7)		#board[i][k]
	li	$t0	1
r1_aif3:
r1_aif2:

	beq	$t4	$t1	r1_aif4	#if(k != i)

	mul	$t7	$t4	16	#k*2*16 + j*2
	add	$t7	$t7	$t2
	mul	$t7	$t7	2
	add	$t7	$t7	$a0	#t7 = &board[k][j]
	lhu	$t5	0($t7)		#t5 = board[k][j]

	and	$t6	$t5	$t3	#t6 = board[k][j] & value
	beq	$t6	$0	r1_aif5
	nor	$t6	$t3	$t3	#~value
	and	$t6	$t6	$t5
	sh	$t6	0($t7)		#board[k][j]
	li	$t0	1
r1_aif5:
r1_aif4:

	addi	$t4	$t4	1
	j	r1_for3
r1_afor3:

	sw	$t0	0($sp)		#save variables
	sw	$t1	4($sp)
	sw	$t2	8($sp)
	sw	$a0	16($sp)
	sw	$ra	20($sp)
	sw	$t3	24($sp)

	move	$a0	$t1		#load arguments
	jal	get_square_begin

	move	$t3	$v0
	sw	$t3	12($sp)		#int ii = get_square_begin(i)

	move	$a0	$t2
	jal	get_square_begin
	move	$t4	$v0		#int jj = get_square_begin(j)

	lw	$t0	0($sp)		#load variables
	lw	$t1	4($sp)
	lw	$t2	8($sp)
	lw	$t3	12($sp)
	lw	$a0	16($sp)
	lw	$ra	20($sp)
	lw	$s0	24($sp)		#s0 = value

	move	$t5	$t3		#int k = ii
r1_for4:
	add	$t6	$t3	4	#t6 = ii + gridsize
	bge	$t5	$t6	r1_afor4

	move	$t6	$t4		#int l = jj
r1_for5:
	add	$t7	$t4	4	#t7 = jj + gridsize
	bge	$t6	$t7	r1_afor5

	seq	$t7	$t5	$t1
	seq	$t8	$t6	$t2
	and	$t7	$t7	$t8

	beq	$t7	$0	r1_aif6
	j	r1_for5_continue			#continue
r1_aif6:

	mul	$t7	$t5	16	#k*2*16 + l*2
	add	$t7	$t7	$t6
	mul	$t7	$t7	2
	add	$t7	$t7	$a0	#t7 = &board[k][l]
	lhu	$t8	0($t7)		#t8 = board[k][l]

	and	$t9	$t8	$s0	#t9 = board[k][l] & value
	beq	$t9	$0	r1_aif7
	nor	$t9	$s0	$s0	#~value
	and	$t9	$t8	$t9
	sh	$t9	0($t7)		#board[k][j]
	li	$t0	1
r1_aif7:

r1_for5_continue:
	addi	$t6	$t6	1
	j	r1_for5
r1_afor5:

	addi	$t5	$t5	1
	j	r1_for4
r1_afor4:

r1_aif1:

	addi	$t2	$t2	1	#j++
	j	r1_for2
r1_afor2:
	addi	$t1	$t1	1	#i++
	j	r1_for1
r1_afor1:
	move	$v0	$t0
	addi	$sp	$sp	28	#deallocate memory
	jr	$ra
#end of rule 1 function
#--------------------

get_square_begin:
	div	$v0, $a0, 4
	mul	$v0, $v0, 4
	jr	$ra

# input: $a0 = board, output: singsp, single
.globl scan
scan:
	sub		$sp, $sp, 4				# set up stack frame (1 var)
	sw 		$ra, 0($sp)				# save return address
	move	$t0, $a0				# $t0 = $a0 = board
	li		$t1, 0					# $t1 = row = 0, multiple of 2
	
scan_lp2:
	li		$t2, 0					# $t2 = col = 0, multiple of 2
	
scan_lp1:
	lhu		$t3, 0($t0)				# $t3 = value = board[i][j]
	jal		has_single_bit_set		# $v0 = single bit set bool (0 | 1)
	beq		$v0, $0, scan_not		# if not($v0 == 0)
	jal		push					# push $t1, $t2 onto stack
	
	#sw		$t1, PRINT_INT($0)		# print $t1 = row
	#sw		$t2, PRINT_INT($0)		# print $t2 = col
	
scan_not:
	add		$t0, $t0, 2				# move $t0 to next half word
	add		$t2, $t2, 2				# move $t2 to next column
	bne		$t2, 32, scan_lp1		# loop until $t2 = 16 * 2
	
	add		$t1, $t1, 2				# move $t1 to next row
	bne		$t1, 32, scan_lp2		# loop until $t1 = 16 * 2
	
	lw		$ra, 0($sp)				# get return address
	add		$sp, $sp, 4				# take down stack frame (1 var)
	jr		$ra						# return
	
# initialize single array stack pointer, output: singsp
.globl init
init: 
	la		$v0, single				# $v0 = pt to single array
	sw		$v0, singsp($0)			# set singsp to $v0
	jr		$ra						# return
	
# input: $t1 = row, $t2 = col, singsp, output: singsp, single
.globl push
push:
	lw		$v0, singsp($0)			# get $v0 = single stack pointer
	sw		$t1, 0($v0)				# store $t1 = row first
	sw		$t2, 4($v0)				# store $t2 = col next
	add		$v0, $v0, 8				# singsp = singsp + 8
	sw		$v0, singsp($0)			# store $v0 = single stack pointer
	jr		$ra						# return

# input: $t1 = row, $t4 = col, singsp, output: singsp, single
.globl push_ik
push_ik:
	lw		$v0, singsp($0)			# get $v0 = single stack pointer
	sw		$t1, 0($v0)				# store $t1 = row first
	sw		$t4, 4($v0)				# store $t4 = col next
	add		$v0, $v0, 8				# singsp = singsp + 8
	sw		$v0, singsp($0)			# store $v0 = single stack pointer
	jr		$ra						# return
	
# input: $t4 = row, $t2 = col, singsp, output: singsp, single
.globl push_kj
push_kj:
	lw		$v0, singsp($0)			# get $v0 = single stack pointer
	sw		$t4, 0($v0)				# store $t4 = row first
	sw		$t2, 4($v0)				# store $t2 = col next
	add		$v0, $v0, 8				# singsp = singsp + 8
	sw		$v0, singsp($0)			# store $v0 = single stack pointer
	jr		$ra						# return
	
# input: $t7 = row, $t9 = col, singsp, output: singsp, single
.globl push_iijj
push_iijj:
	lw		$v0, singsp($0)			# get $v0 = single stack pointer
	sw		$t7, 0($v0)				# store $t7 = row first
	sw		$t9, 4($v0)				# store $t9 = col next
	add		$v0, $v0, 8				# singsp = singsp + 8
	sw		$v0, singsp($0)			# store $v0 = single stack pointer
	jr		$ra						# return
	
# input: singsp, single, output: $t1 = row, $t2 = col, singsp
.globl pop
pop:
	lw		$v0, singsp($0)			# get $v0 = single stack pointer
	sub		$v0, $v0, 8				# singsp = singsp - 8
	lw		$t2, 4($v0)				# get $t2 = col
	lw		$t1, 0($v0)				# get $t1 = row
	sw		$v0, singsp($0)			# store $v0 = single stack pointer
	jr		$ra
	
# input: singsp, output: $v0 = empty bool
.globl empty
empty:
	lw		$v0, singsp($0)			# get $v0 = single stack pointer
	beq		$v0, single, empty_true	# if not($v0 == single), start of array
	move	$v0, $0					# then set $v0 to 0, false
	jr		$ra						# return
	
empty_true:							# else
	li		$v0, 1					# set $v0 to 1, true
	jr 		$ra						# return
	
# input: $a0 = board
.globl rule1
rule1:
	sub     $sp, $sp, 8				# set up stack frame (2 var)
	sw      $ra, 0($sp)				# save return address
	sw      $a0, 4($sp)				# save $a0, board
	
rule1_for:
	jal		empty					# check if single array is empty
	bne		$v0, $0, rule1_end		# if not($v0 != 0)
	
	jal		pop						# else, pop $t1 = row, $t2 = col
	lw      $a0, 4($sp)				# get $a0, board
	mul     $a1, $t1, 16			# $a1 = $t1 * GRID_SQUARED
	add     $a1, $a0, $a1			# $a0 = $a0 + $a1, row
	add     $a1, $a1, $t2			# $a0 = $a0 + $t2, col
	lhu     $t3, 0($a1)				# $t3 = value = board[i][j]
	not     $t8, $t3				# $t8 = ~$t3
	
	li      $t4, 0					# $t4 = k = 0
rule1_for_3:
	## eliminate from row
	beq     $t4, $t2, rule1_k_eq_j	# if not(k == j)
	mul     $a1, $t1, 16			# $a1 = $t1 * GRID_SQUARED
	add     $a1, $a0, $a1			# $a1 = $a0 + $a1, row
	add     $a1, $a1, $t4			# $a1 = $a1 + $t4, col
	lhu     $t5, 0($a1)				# $t5 = board[i][k]
	and     $t6, $t5, $t8			# $t6 = board[i][k] & ~value
	beq		$t5, $t6, rule1_row_0	# if $t5 not changed, then skip
	sh      $t6, 0($a1)				# board[i][k] = $t6
	jal		has_single_bit_set_6	# else check if $t6 has single bit set
	beq		$v0, $0, rule1_row_0	# if not($v0 == 0)
	jal		push_ik					# push $t1 = i, $t4 = k onto stack
	
rule1_k_eq_j:
rule1_row_0:
	## eliminate from column
	beq     $t4, $t1, rule1_k_eq_i	# if not(k == i)
	mul     $a1, $t4, 16			# $a1 = $t4 * GRID_SQUARED
	add     $a1, $a0, $a1			# $a1 = $a0 + $a1, row
	add     $a1, $a1, $t2			# $a1 = $a1 + $t2, col
	lhu     $t5, 0($a1)				# $t5 = board[k][j]
	and     $t6, $t5, $t8			# $t6 = board[k][j] & ~value
	beq		$t5, $t6, rule1_col_0	# if $t5 not changed, then skip
	sh      $t6, 0($a1)				# board[i][k] = $t6
	jal		has_single_bit_set_6	# else check if $t6 has single bit set
	beq		$v0, $0, rule1_col_0	# if not($v0 == 0)
	jal		push_kj					# push $t4 = k, $t2 = j onto stack
	
rule1_k_eq_i:
rule1_col_0:
	add     $t4, $t4, 2				# $t4 = k = $t4 + 2, next short
	bne     $t4, 32, rule1_for_3	# loop until $t4 = 16 * 2
	
	## eliminate from square
	and     $t7, $t1, 0xfffffff8	# $t7 = k = ii, inline func
	and     $a3, $t2, 0xfffffff8	# $a3 = jj, inline func
	add     $t4, $t7, 8				# $t4 = ii + GRIDSIZE, k end
	add     $a2, $a3, 8				# $a2 = jj + GRIDSIZE, l end
	
rule1_for_4:
	move    $t9, $a3				# $t9 = l = jj
rule1_for_5:
	bne     $t7, $t1, rule1_k_ne_i	# if not(k != i)
	bne     $t9, $t2, rule1_l_ne_j	# if not(l != j)
	j       rule1_cont				# then continue	
	
rule1_k_ne_i:
rule1_l_ne_j:
	mul     $t5, $t7, 16			# $t5 = k * GRID_SQUARED
	add     $t0, $a0, $t5			# $t0 = $a0 + $t5, row
	add     $t0, $t0, $t9			# $t0 = $t0 + $t9, col
	lhu     $t5, 0($t0)				# $t5 = board[k][l]
	and     $t6, $t5, $t8			# $t6 = board[k][l] & ~value
	beq		$t5, $t6, rule1_square_0# if $t5 not changed, then skip
	sh      $t6, 0($t0)				# board[k][l] = $t6
	jal		has_single_bit_set_6	# else check if $t6 has single bit set
	beq		$v0, $0, rule1_square_0	# if not($v0 == 0)
	jal		push_iijj				# push $t7 = k, $t9 = l onto stack
	
rule1_square_0:
rule1_cont:
	add     $t9, $t9, 2				# $t9 = jj = $t9 + 2, next short
	bne     $t9, $a2, rule1_for_5	# loop until $t9 = $a2
	add     $t7, $t7, 2				# $t7 = ii = $t7 + 2, next short
	bne     $t7, $t4, rule1_for_4	# loop until $t7 = $t4
	
	j		rule1_for				# loop until stack is empty
	
rule1_end:
	lw      $ra, 0($sp)				# get return address
	add     $sp, $sp, 8				# take down stack frame (2 var)
	jr		$ra						# return

# input: $t3 = value, output: $v0 = single bit set bool
has_single_bit_set_6:
	move	$t3, $t6				# set $t3 = $t6, fall through to routine
	
has_single_bit_set:
	beq		$t3, 0, hsbs_ret_zero	# return 0 if value == 0
	sub		$a1, $t3, 1
	and		$a1, $t3, $a1
	bne		$a1, 0, hsbs_ret_zero	# return 0 if (value & (value - 1)) == 0
	li		$v0, 1
	jr		$ra
hsbs_ret_zero:
	li		$v0, 0
	jr		$ra

	
#has_single_bit_set:
#	beq	$a0, 0, hsbs_ret_zero	# return 0 if value == 0
#	sub	$a1, $a0, 1
#	and	$a1, $a0, $a1
#	bne	$a1, 0, hsbs_ret_zero	# return 0 if (value & (value - 1)) == 0
#	li	$v0, 1
#	jr	$ra
#hsbs_ret_zero:
#	li	$v0, 0
#	jr	$ra


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


#==========
# MODES
#==========

update_mode:	#changes the value stored at mode_data
	jr	$ra

defense_mode:
	sub	$sp, $sp, 4
	sw	$ra, 0($sp)
	
	lw	$a2, opxpos
	lw	$t0, BOT_X
	sub	$a0, $a2, $t0		# $a0 = opxpos - BOT_X
	add	$a0, $a0, 5
	lw	$a3, opypos
	lw	$t1, BOT_Y
	sub	$a1, $a3, $t1		# $a1 = opypos - BOT_Y
	li	$v0, 150
	blt	$a2, $v0, defense_mode_chase
	
	# match opp y position
	blt	$a3, $t1, defense_mode_lt
	li		$a0, 90
	sw		$a0, ANGLE				# set angle to 90
	li		$a0, 1
	sw		$a0, ANGLE_CONTROL		# set absolute angle
	j		defense_mode_vel
	
defense_mode_lt:
	li		$a0, 270
	sw		$a0, ANGLE				# set angle to 270
	li		$a0, 1
	sw		$a0, ANGLE_CONTROL		# set absolute angle
	j		defense_mode_vel
	
defense_mode_chase:
	jal	arctan				# $v0 = arctan ($a1 / $a0)
	sw	$v0, ANGLE			# set angle to $v0
	li	$v0, 1
	sw	$v0, ANGLE_CONTROL	# set absolute angle
	
defense_mode_vel:
	li	$v0, 10
	sw	$v0, VELOCITY		# set velocity to 10
	
defense_mode_done:
	lw	$ra, 0($sp)
	add	$sp, $sp, 4
	jr	$ra

offense_mode:
	sub	$sp	$sp	16

	ori	$t0	$0	10
	sw	$t0	VELOCITY($0)	#set_velocity(10)

	la	$t4	flag_data
	sw	$t4	FLAG_REQUEST	#FLAG_REQUEST(flags)
	
	#li	$t0	0		#int i=0;
cf_for:	#li	$t3	NUM_FLAGS
	#mul	$t3	$t3	2
	#bge	$t0	$t3	cf_afr

cf_whl:	lw	$t1	FLAGS_IN_HAND
	li	$t2	MAX_FLAGS
	bge	$t1	$t2	cf_awl
	
	#li	$t3	NUM_FLAGS	#figure out where to save
	#mul	$t3	$t3	8
	#add	$t3	$t3	8
	#sub	$sp	$sp	$t3	#allocate memory
	sw	$t0	0($sp)		#save variables
	sw	$ra	4($sp)
	sw	$t4	8($sp)
	
	#mul	$t3	$t0	4	#figure out memory address
	#add	$t3	$t3	$t4	#address of array plus offset
	#lw	$a0	0($t3)
	#lw	$a1	4($t3)
	
	la		$s5, flag_data			# $t5 = pt to start of flag array
	lw		$a0, 0($s5)
	lw		$a1, 4($s5)
	
	lw		$s6, BOT_X				# $t6 = x coord of bot
	lw		$s7, BOT_Y				# $t7 = y coord of bot
	li		$s2, 9999				# $t2 = current dist
	move	$s3, $a0				# $t3 = current destination x coord
	move	$s4, $a1				# $t4 = current destination y coord
	
interrupt_inv_lp:
	lw		$a0, 0($s5)				# $a0 = x coord of flag
	lw		$a1, 4($s5)				# $a1 = y coord of flag
	add		$s5, $s5, 8				# move $t5 to next flag
	beq		$a0, -1, interrupt_inv_end
	sub		$v0, $s6, $a0			# $v0 = (x1 - x2), inline dist
	mul		$v0, $v0, $v0			# $v0 = (x1 - x2) ^ 2
	sub		$v1, $s7, $a1			# $v1 = (y1 - y2)
	mul		$v1, $v1, $v1			# $v1 = (y1 - y2) ^ 2
	add		$v0, $v0, $v1			# $v0 = (x1 - x2) ^ 2 + (y1 - y2) ^ 2 = dist
	blt		$s2, $v0, interrupt_inv_skip
	move	$s2, $v0				# if (not(cur dist < dist)), then set new dist
	move	$s3, $a0				# set new destination x coord
	move	$s4, $a1				# set new destination y coord
	
interrupt_inv_skip:
	j		interrupt_inv_lp		# loop until null flag reached
	
interrupt_inv_end:
	lw		$t0, 0($sp)
	move	$a0, $s3
	move	$a1, $s4
	
	jal	move_to_target		#execute function
	
	lw	$t0	0($sp)		#reload variables
	lw	$ra	4($sp)
	lw	$t4	8($sp)		#$t4 is array address
	
	sw	$0	PICK_FLAG	#PICK_UP_FLAG();
	#addi	$t0	$t0	2	#i += 2
	
	la	$t4	flag_data
	sw	$t4	FLAG_REQUEST	#FLAG_REQUEST(flags)
	
	j	cf_whl			#continue while loop
cf_awl:

	sw	$t0	0($sp)		#save variables
	sw	$ra	4($sp)
	li	$a0	BASE_X		#prepare arguments
	li	$a1	BASE_Y
	jal	move_to_target		#call function
	lw	$t0	0($sp)		#reload variables
	lw	$ra	4($sp)

	j	cf_for			#continue for loop
cf_afr:

	add	$sp	$sp	16
	jr	$ra			#return

# input: $t0 = x1, $t1 = y1, $a0 = x2, $a1 = y2, output: $v0 = dist, destroys: $v1
dist:
	sub		$v0, $t0, $a0			# $v0 = (x1 - x2)
	mul		$v0, $v0, $v0			# $v0 = (x1 - x2) ^ 2
	sub		$v1, $t1, $a1			# $v1 = (y1 - y2)
	mul		$v1, $v1, $v1			# $v1 = (y1 - y2) ^ 2
	add		$v0, $v0, $v1			# $v0 = (x1 - x2) ^ 2 + (y1 - y2) ^ 2
	jr		$ra						# return with $v0
	
# support function
#----------
move_to_target:
	sub	$sp	$sp	16	#allocate memory
	lw	$t0	BOT_X		#int my_x = SPIMBOT_X();
	
	#lw	$t7 TIMER
	#lw	$t8 BOT_X
	#lw  $t9 BOT_Y
	
	sw	$t0	0($sp)		#save variables
	sw	$ra	4($sp)
	sw	$a0	8($sp)
	sw	$a1	12($sp)
	move	$a1	$t0		#prepare arguments
	li	$a2	0
	jal	calculate_angle		#call function
	lw	$t0	0($sp)		#reload variables
	lw	$ra	4($sp)
	lw	$a0	8($sp)
	lw	$a1	12($sp)

	move	$t1	$v0		#int desired_angle = stuff
	sw	$t1	ANGLE($0)	#3 steps to set absolute angle
	li	$t2	1
	sw	$t2	ANGLE_CONTROL($0)
	
mtt_whilex:
	#--------------------
	la	$t2	mode_data
	lw	$t2	0($t2)
	bne	$t2	2	mtt_awhiley
	#--------------------
	lw	$t2	BOT_X
	sub	$t3	$t2	$a0
	bge	$t3	$0	mtt_temp1	#absolute value
	mul	$t3	$t3	-1
mtt_temp1:
	blt	$t3	2	mtt_awhilex
		#unclear
	j	mtt_whilex
mtt_awhilex:
	
	#--------------------
	la	$t2	mode_data
	lw	$t2	0($t2)
	bne	$t2	2	mtt_awhiley
	#--------------------
	lw	$t0	BOT_Y		#int my_y = SPIMBOT_Y();

	sw	$t0	0($sp)		#save variables
	sw	$ra	4($sp)
	sw	$a0	8($sp)
	sw	$a1	12($sp)
	move	$a0	$a1		#prepare arguments
	move	$a1	$t0
	li	$a2	1
	jal	calculate_angle		#call function
	lw	$t0	0($sp)		#reload variables
	lw	$ra	4($sp)
	lw	$a0	8($sp)
	lw	$a1	12($sp)

	move	$t1	$v0		#int desired_angle = stuff
	sw	$t1	ANGLE($0)	#3 steps to set absolute angle
	li	$t2	1
	sw	$t2	ANGLE_CONTROL($0)

mtt_whiley:
	lw	$t2	BOT_Y
	sub	$t3	$t2	$a1
	bge	$t3	$0	mtt_temp2	#absolute value
	mul	$t3	$t3	-1
mtt_temp2:
	blt	$t3	2	mtt_awhiley
		#unclear
	j	mtt_whiley
mtt_awhiley:
	
	#lw	$a0 TIMER
	#lw  $a1 BOT_X
	#lw  $a2 BOT_Y
	#sub $a0 $a0 $t7
	#abs $a0 $a0
	#sub $a1 $a1 $t8
	#abs $a1 $a1
	#sub $a2 $a2 $t9
	#abs $a2 $a2
	#add $a1 $a1 $a2
	#mul $a1 $a1 10000
	#div $a0 $a0 $a1
	#sw 	$a0 PRINT_INT
	#lw	$a0 FLAGS_IN_HAND
	#sw	$a0 PRINT_INT
	#li	$a0 8888
	#sw  $a0 PRINT_INT
	
	add	$sp	$sp	16	#deallocate memory
	jr	$ra

calculate_angle:
	bge	$a1	$a0	ca_else
	li	$t0	0
	j	ca_aif
ca_else:
	li	$t0	180
ca_aif:
	mul	$t1	$a2	90
	add	$v0	$t0	$t1
	jr	$ra


#==========
# SUPPORT
#==========

# -----------------------------------------------------------------------
# sb_arctan - computes the arctangent of y / x
# $a0 - x
# $a1 - y
# returns the arctangent
# -----------------------------------------------------------------------

arctan:
sb_arctan:
	li	$v0, 0		# angle = 0;

	abs	$t0, $a0	# get absolute values
	abs	$t1, $a1
	ble	$t1, $t0, no_TURN_90	  

	## if (abs(y) > abs(x)) { rotate 90 degrees }
	move	$t0, $a1	# int temp = y;
	neg	$a1, $a0	# y = -x;      
	move	$a0, $t0	# x = temp;    
	li	$v0, 90		# angle = 90;  

no_TURN_90:
	bgez	$a0, pos_x 	# skip if (x >= 0)

	## if (x < 0) 
	add	$v0, $v0, 180	# angle += 180;

pos_x:
	mtc1	$a0, $f0
	mtc1	$a1, $f1
	cvt.s.w $f0, $f0	# convert from ints to floats
	cvt.s.w $f1, $f1
	
	div.s	$f0, $f1, $f0	# float v = (float) y / (float) x;

	mul.s	$f1, $f0, $f0	# v^^2
	mul.s	$f2, $f1, $f0	# v^^3
	l.s	$f3, three	# load 5.0
	div.s 	$f3, $f2, $f3	# v^^3/3
	sub.s	$f6, $f0, $f3	# v - v^^3/3

	mul.s	$f4, $f1, $f2	# v^^5
	l.s	$f5, five	# load 3.0
	div.s 	$f5, $f4, $f5	# v^^5/5
	add.s	$f6, $f6, $f5	# value = v - v^^3/3 + v^^5/5

	l.s	$f8, PI		# load PI
	div.s	$f6, $f6, $f8	# value / PI
	l.s	$f7, F180	# load 180.0
	mul.s	$f6, $f6, $f7	# 180.0 * value / PI

	cvt.w.s $f6, $f6	# convert "delta" back to integer
	mfc1	$t0, $f6
	add	$v0, $v0, $t0	# angle += delta

	jr 	$ra
	

# -----------------------------------------------------------------------
# euclidean_dist - computes sqrt(x^2 + y^2)
# $a0 - x
# $a1 - y
# returns the distance
# -----------------------------------------------------------------------

euclidean_dist:
	mul	$a0, $a0, $a0	# x^2
	mul	$a1, $a1, $a1	# y^2
	add	$v0, $a0, $a1	# x^2 + y^2
	mtc1	$v0, $f0
	cvt.s.w	$f0, $f0	# float(x^2 + y^2)
	sqrt.s	$f0, $f0	# sqrt(x^2 + y^2)
	cvt.w.s	$f0, $f0	# int(sqrt(...))
	mfc1	$v0, $f0
	jr	$ra

#====================
.kdata				# interrupt handler data (separated just for readability)
chunkIH:	.space 8	# space for two registers
non_intrpt_str:	.asciiz "Non-interrupt exception\n"
unhandled_str:	.asciiz "Unhandled interrupt type\n"


#==========
# INTERRUPT
#==========

.ktext 0x80000180
interrupt_handler:
.set noat
	move	$k1, $at		# Save $at                               
.set at
	la	$k0, chunkIH
	sw	$a0, 0($k0)		# Get some free registers                  
	sw	$a1, 4($k0)		# by storing them to a global variable     

	mfc0	$k0, $13		# Get Cause register                       
	srl	$a0, $k0, 2                
	and	$a0, $a0, 0xf		# ExcCode field                            
	bne	$a0, 0, non_intrpt         

interrupt_dispatch:			# Interrupt:                             
	mfc0	$k0	$13		# Get Cause register, again                 
	beq	$k0	0	done	# handled all outstanding interrupts     

	and	$a0	$k0, COORDS_MASK	# is there a coords interrupt?                
	bne	$a0	0, coords_interrupt   

	and	$a0	$k0	TIMER_MASK	# is there a timer interrupt?
	bne	$a0	0	timer_interrupt

	# add dispatch for other interrupt types here.

	#li	$v0	PRINT_STRING	# Unhandled interrupt types
	#la	$a0	unhandled_str
	#syscall 
	j	done

coords_interrupt:
	sw	$0	COORDS_ACKNOWLEDGE	# acknowledge interrupt
	sw	$0	COORDS_REQUEST
	lw	$a0	OTHER_BOT_X
	beq	$a0, -1, coords_interrupt_invis
	sw	$a0	opxpos
	lw	$a0	OTHER_BOT_Y
	sw	$a0	opypos
	j	interrupt_dispatch	# see if other interrupts are waiting
	
coords_interrupt_invis:
	
	j	interrupt_dispatch
	
timer_interrupt:
	sw	$0	TIMER_ACKNOWLEDGE	# acknowledge interrupt

#==========
# TIMER
#----------
# Handles most status changes
# to do:
#  -check for score difference
#  -check state of opponent
#  -determine best mode to be in
#==========

	#nothing yet

#==========
	lw	$a0	TIMER		# current time
	add	$a0	$a0	2000  
	sw	$a0	TIMER		# request timer in 2000 cycles

	j	interrupt_dispatch	# see if other interrupts are waiting

non_intrpt:				# was some non-interrupt
	#li	$v0, PRINT_STRING
	#la	$a0, non_intrpt_str
	#syscall				# print out an error message
	# fall through to done

done:
	la	$k0, chunkIH
	lw	$a0, 0($k0)		# Restore saved registers
	lw	$a1, 4($k0)
.set noat
	move	$at, $k1		# Restore $at
.set at 
	eret


