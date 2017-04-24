.data

# spimbot constants
NUM_FLAGS   = 40	# maximum flags you can ever have on the board
BASE_RADIUS = 24
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
#PRINT_INT = 1
#PRINT_STRING = 4
#PRINT_CHAR = 11

three:	.float	3.0
five:	.float	5.0
PI:		.float	3.141592
F180:	.float  180.0

# program constants
sudoku				= 0x10001000	# sudoku puzzle
admode				= 0x10001200	# mode - 0 (M mode), 1 (I mode), 2 (E mode), 3 (C mode), 12 (Def mode), 13 (TV mode)
opxpos				= 0x10001204	# opp x position, updated by coords interrupt
opypos				= 0x10001208	# opp y position, updated by coords interrupt
opxvel				= 0x1000120C	# opp x velocity, updated by coords interrupt
opyvel				= 0x10001210	# opp y velocity, udpated by coords interrupt
bopinv				= 0x10001214	# boolean - opp is invisible
binvis				= 0x1000121C	# boolean - can go invisible
tinvis				= 0x10001220	# time of invisibility activation
nflags				= 0x10001224	# number of flags on field (not accurate)
bcarry				= 0x10001228	# boolean - is carrying flags
choice				= 0x1000122C	# boolean - true if bot has chosen destination (unused)
arflag				= 0x10001230	# array of flags, 0x140 bytes
alflag				= 0x1000136C	# ptr to last flag in array, -1 if field not full
atflag				= 0x10001370	# ptr to null terminating flag, set to -1
tunnelflags			= 0x10001378	# tunnel flags
tflags				= 0x10001378	# tunnel flags (alias)
singsp				= 0x10001400	# stack pointer to single stack
single				= 0x10001404	# stack of single squares in sudoku puzzle

.text
main:
	# the world is your oyster
	sub		$sp, $sp, 4			# set up stack frame (1 var)
	sw		$ra, 0($sp)			# save $ra
	
	# initialize variables
	or		$t0, $0, 1			# $t0 = 1
	sw		$0,  admode($0)		# set admode to 0 (M mode)
	sw		$t0, binvis($0)		# set binvis to 1 (initially can go invis)
	sw		$t0, COORDS_REQUEST	# request opp coords, next update in 20K cycles
	lw		$t0, OTHER_BOT_X
	sw		$t0, opxpos($0)		# set opxpos to 250
	lw		$t0, OTHER_BOT_Y
	sw		$t0, opypos($0)		# set opypos to 150
	sw		$0,  bopinv($0)		# set bopinv to 0 (opp is not invisible)
	li		$t0, 5
	sw		$t0, nflags($0)		# set nflags to 5
	li		$t0, -1
	sw		$t0, atflag($0)		# null terminate arflag with -1
	sw		$0,  bcarry($0)		# set bcarry to 0 (not carrying any flags)
	sw		$0,  choice($0)		# set choice to 0 (not chosen destination)
	jal		init				# initialize single stack pointer
	
	# enable interrupts
	li		$t4, TIMER_MASK		# timer interrupt enable bit
	or		$t4, $t4, INVIS_MASK# invisible interrupt enable bit
	or		$t4, $t4, COORDS_MASK# coords interrupt enable bit
	or		$t4, $t4, BONK_MASK	# bonk interrupt enable bit
	or		$t4, $t4, 1			# enable global interrupt
	mtc0	$t4, $12			# set interrupt mask
	
	# set velocity to max
	li		$t0, 10
	sw		$t0, VELOCITY		# set velocity to 10
	
	# set angle to 0, move to left
	or		$t0, $0, 1			# $t0 = 1
	sw		$0, ANGLE			# set angle to 0
	sw		$t0, ANGLE_CONTROL	# set absolute angle control
	
	# get array of flag coordinates (0x140 bytes long)
	la		$t0, arflag
	sw		$t0, FLAG_REQUEST($0)
	
	# request timer interrupt
	lw		$t0, TIMER			# read current time
	add		$t0, $t0, 10		# add 10 to current time
	sw		$t0, TIMER			# request timer interrupt in 10 cycles
	
	# solve sudoku puzzles
mainreq:
	la		$a0, sudoku
	sw		$a0, SUDOKU_REQUEST	# request for unsolved sudoku board
	
	la 		$a0, sudoku
	jal		scan				# scan for squares w/ 1 bit set
	
	la		$a0, sudoku
	jal 	rule1				# solve using rule 1
	
	la		$a0, sudoku
	sw		$a0, SUDOKU_SOLVED	# signal that puzzle is solved
	
	lw		$t2, nflags			# $t2 = num flags
	li		$t3, NUM_FLAGS		# $t3 = max flags, 40
mainflag:
	lw		$t1, ENERGY
	blt		$t1, 32, mainskip	# end loop if ENERGY < 32, save for invis
	beq		$t2, $t3, mainskip	# end loop if nflags == max flags
	lw		$t1, alflag			# $t1 = x coord of last flag
	bne		$t1, -1, mainskip	# end loop if last flag is not null
	sw		$t0, GENERATE_FLAG	# generate flag
	la		$t0, arflag
	sw		$t0, FLAG_REQUEST($0)# update flag coords
	add		$t2, $t2, 1			# $t2 ++
#	j		mainflag			# keep looping
	
mainskip:
	la		$t0, arflag
	sw		$t0, FLAG_REQUEST($0)# update flag coords
	sw		$t2, nflags			# store num flags
	
	j 		mainreq
	
	lw		$ra, 0($sp)			# restore $ra
	add		$sp, $sp, 4			# take down stack frame (1 var)
	jr		$ra
	
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
	
# ================================================================================
# INTERRUPT
# ================================================================================
	
.kdata								# interrupt handler data
chunkIH:	.space 64				# space for 16 registers

.ktext 0x80000180
interrupt_handler:
.set noat
	move	$k1, $at				# save $at
.set at
	la		$k0, chunkIH
	sw		$ra, 0($k0)				# save register $ra
	sw		$a0, 4($k0)				# save register $a0
	sw		$a1, 8($k0)				# save register $a1
	sw		$v0, 12($k0)			# save register $v0
	sw		$v1, 16($k0)			# save register $v1
	
	mfc0	$k0, $13				# get cause register
	srl 	$k0, $k0, 2				# $k0 = $k0 >> 2
	and		$k0, $k0, 0xF			# $k0 = $K0 & 0xF, get bottom 4 bits
	bne		$k0, $0, interrupt_non	# if not(($k0 >> 2) & 0xF != 0)
	
interrupt_dispatch:					# interrupt dispatch
	mfc0	$k0, $13				# get $k0 = cause register
	beq		$k0, $0, interrupt_done	# all interrupts taken care of
	
	and		$a0, $k0, TIMER_MASK	# checks for timer interrupt
	bne		$a0, $0, interrupt_timer
	
	and		$a0, $k0, INVIS_MASK	# checks for invis interrupt
	bne		$a0, $0, interrupt_invis
	
	and		$a0, $k0, COORDS_MASK	# checks for coords interrupt
	bne		$a0, $0, interrupt_coords
	
	and		$a0, $k0, BONK_MASK		# checks for bonk interrupt
	bne		$a0, $0, interrupt_bonk
	
	j		interrupt_done			# unhandled interrupt types aren't handled
	
# ================================================================================
# TIMER INTERRUPT
# ================================================================================
	
interrupt_timer:
	sw		$a0, TIMER_ACKNOWLEDGE	# acknowledge interrupt
	
	lw		$a0, admode				# $a0 = bot mode
	sw		$a0, PRINT_INT
	beq		$a0, $0, interrupt_mid
	li		$a1, 1
	beq		$a0, $a1, interrupt_inv
	li		$a1, 2
	beq		$a0, $a1, interrupt_inv_cont
	li		$a1, 3
	beq		$a0, $a1, interrupt_chase
	li		$a1, 12
	beq		$a0, $a1, interrupt_defense
	li		$a1, 13
	beq		$a0, $a1, interrupt_tunnelvision
	
	j		interrupt_end			# invalid mode, return to dispatch
	
# ================================================================================
# INTERRUPT_MID - middle mode
# ================================================================================
	
interrupt_mid:
	# M - go to middle, match opp y pos
	lw		$a0, TIMER
	li		$a1, 250000
	blt		$a0, $a1, interrupt_mid_ninv
	lw		$a0, binvis
	#beq		$a0, $0, interrupt_mid_ninv
	
	# if (binvis and not(TIME < 250K)), then switch to I mode
	li		$a1, 1
	sw		$a1, admode				# set admode to 1 (I mode)
	#sw		$a1, ACTIVATE_INVIS		# activate invisibility
	lw		$a1, TIMER				# $a1 = current time
	sw		$a1, tinvis				# set tinvis to time of invis activation
	#sw		$0, binvis				# set binvis to 0 (can't go invis)
	j		interrupt_end
	
interrupt_mid_ninv:
	lw		$a0, BOT_X
	li		$a1, 148
	blt		$a0, $a1, interrupt_mid_lt
	bgt		$a0, $a1, interrupt_mid_gt
	
interrupt_mid_eq:
	# if at middle, then match opp y pos
	lw		$a0, BOT_Y
	lw		$a1, opypos
	beq		$a0, $a1, interrupt_mid_yeq
	blt		$a0, $a1, interrupt_mid_ylt
	
interrupt_mid_ygt:
	# BOT_Y > OPP_Y, move south
	li		$a0, 10
	sw		$a0, VELOCITY			# set velocity to 10
	li		$a0, 270
	sw		$a0, ANGLE				# set angle to 270
	li		$a0, 1
	sw		$a0, ANGLE_CONTROL		# set absolute angle
	
	j		interrupt_end
	
interrupt_mid_yeq:
	# BOT_Y = OPP_Y, do nothing
	sw		$0, VELOCITY
	
	j 		interrupt_end
	
interrupt_mid_ylt:
	# BOT_Y < OPP_Y, move north
	li		$a0, 10
	sw		$a0, VELOCITY			# set velocity to 10
	li		$a0, 90
	sw		$a0, ANGLE				# set angle to 90
	li		$a0, 1
	sw		$a0, ANGLE_CONTROL		# set absolute angle
	
	j		interrupt_end
	
interrupt_mid_gt:
	# if BOT_X > 152, then move west
	li		$a0, 180
	sw		$a0, ANGLE				# set angle to 180
	li		$a0, 1
	sw		$a0, ANGLE_CONTROL		# set absolute angle
	
	j		interrupt_end		# return to dispatch
	
interrupt_mid_lt:
	# if BOT_X < 148, then move east
	sw		$0, ANGLE				# set angle to 0
	li		$a0, 1
	sw		$a0, ANGLE_CONTROL		# set absolute angle
	
	j		interrupt_end			# check other interrupts
	
# ================================================================================
# INTERRUPT_INV - invisibility mode
# ================================================================================
	
interrupt_inv:
	# I - use invisibility when possible, pick up nearest 4 flags
	# save $t0, $t1, $t2, $t3, $t4
	lw		$a0, tinvis				# $a0 = time of invis activation
	lw		$a1, TIMER				# $a1 = current time
	sub		$a1, $a1, $a0			# $a1 = time elapsed since activation
	blt		$a1, 25000, interrupt_inv_cont
	lw		$a1, bcarry
	bne		$a1, $0, interrupt_inv_cont
	
	# if (not(time elapsed < 25K) and bcarry == 0), then switch to M mode
	sw		$0, admode				# set admode to 0 (M mode)
	lw		$a0, nflags				# $a0 = num flags
	li		$a1, NUM_FLAGS			# $a1 = max flags, 40
interrupt_inv_flag:
	lw		$v0, ENERGY
	blt		$v0, 32, interrupt_inv_fskip	# skip if ENERGY < 32, save for invis
	beq		$a0, $a1, interrupt_inv_fskip	# skip if flags generated == max flags
	lw		$v0, alflag				# $v0 = x coord of last flag
	bne		$v0, -1, interrupt_inv_fskip	# skip if last flag is not null
	sw		$v0, GENERATE_FLAG		# generate flag
	la		$v0, arflag
	sw		$v0, FLAG_REQUEST($0)	# update flag coords
	add		$a0, $a0, 1				# $a0 ++
	j		interrupt_inv_flag		# keep looping
	
interrupt_inv_fskip:
	la		$v0, arflag
	sw		$v0, FLAG_REQUEST($0)# update flag coords
	sw		$a0, nflags			# store num flags
	j		interrupt_end
	
interrupt_inv_cont:
	la		$k0, chunkIH
	sw		$t0, 20($k0)			# $t0 goes after $v1
	sw		$t1, 24($k0)			# $t1 goes after $t0
	sw		$t2, 28($k0)			# $t2 goes after $t1
	sw		$t3, 32($k0)			# $t3 goes after $t2
	sw		$t4, 36($k0)			# $t4 goes after $t3
	
	# if (opxpos < 150 and BOT_X < 150 and dist to opp < 10^2 and FLAGS_IN_HAND < 3 and opp not invis), then switch to chase mode
	lw		$a0, opxpos
	li		$a1, 146
	blt		$a1, $a0, interrupt_inv_notc
	lw		$a0, BOT_X
	blt		$a1, $a0, interrupt_inv_notc
	lw		$v0, bopinv
	bne		$v0, $0, interrupt_inv_notc
	lw		$v0, FLAGS_IN_HAND
#	bge		$v0, 3, interrupt_inv_notc
	lw		$a1, BOT_Y	
	lw		$t0, opxpos
	lw		$t1, opypos
	jal		dist					# $v0 = sq dist btwn ($a0, $a1) and ($t0, $t1)
	bge		$v0, 2500, interrupt_inv_notc
	
	li		$a0, 3
	sw		$a0, admode				# set admode to 3 (C mode)
	j		interrupt_inv_done
	
interrupt_inv_notc:
	# if carrying flag and can go invis and opp is near and on other field, then switch to evasion mode
	lw		$t0, FLAGS_IN_HAND		# $t0 = num flags in hand
#	beq		$t0, $0, interrupt_inv_find
	blt		$t0, 2, interrupt_inv_find
	lw		$t0, binvis				# $t0 = binvis boolean
	beq		$t0, $0, interrupt_inv_find
	lw		$a0, opxpos
	lw		$a1, opypos
	lw		$t0, opxvel
	lw		$t1, opyvel
	add		$a0, $a0, $t0			# $a0 = opxpos + opxvel
	add		$a1, $a1, $t1			# $a1 = opypos + opyvel
	lw		$t0, BOT_X
	lw		$t1, BOT_Y
	blt		$t0, 150, interrupt_inv_find
	jal		dist					# $v0 = dist btwn bot and opp
	li		$v1, 100
	blt		$v1, $v0, interrupt_inv_find
	
	li		$a0, 2
	sw		$a0, admode				# set admode to 2 (E mode)
	sw		$a1, ACTIVATE_INVIS		# activate invisibility
	lw		$a1, TIMER				# $a1 = current time
	sw		$a1, tinvis				# set tinvis to time of invis activation
	sw		$0, binvis				# set binvis to 0 (can't go invis)
	j		interrupt_inv_done
	
interrupt_inv_find:
	lw		$t2, FLAGS_IN_HAND		# $t2 = number of flags in hand
	beq		$t2, $0, interrupt_inv_0
	lw		$t0, admode				# $t0 = admode
	li		$t1, 2
	beq		$t0, $t1, interrupt_inv_base
	bne		$t2, 4, interrupt_inv_lt ### MAX_FLAGS
	
	# if (not(FLAGS_IN_HAND != 4)), then go back to base
interrupt_inv_base:
	lw		$t0, BOT_X				# $t0 = x coord of bot
	li		$t2, 150
	blt		$t0, $t2, interrupt_inv_rht
	
	# if BOT_Y is close to opypos, then angle towards base
	lw		$t0, BOT_Y				# $t0 = y coord of bot
	lw		$a0, opypos
	sub		$a0, $a0, $t0
	abs		$a0, $a0				# $a0 = abs(BOT_Y - opypos)
	bgt		$a0, 10, interrupt_inv_xne
	
	lw		$a0, BOT_X
	lw		$a1, BOT_Y
	li		$t0, 150				# 50
	sub		$a0, $t0, $a0			# $a0 = 150 - BOT_X
	li		$t1, 150
	sub		$a1, $t1, $a1			# $a1 = 150 - BOT_Y
	beq		$a0, $0, interrupt_inv_xne
	jal		arctan					# $v0 = arctan ($a1 / $a0)
	sw		$v0, ANGLE				# set angle
	li		$v0, 1
	sw		$v0, ANGLE_CONTROL		# set absolute angle
	li		$v0, 10
	sw		$v0, VELOCITY			# set velocity to 10
	j		interrupt_inv_done
	
interrupt_inv_xne:
	# if bot on opp side of field, then go straight left
	li		$t0, 180
	sw		$t0, ANGLE				# set angle to 180
	li		$t0, 1
	sw		$t0, ANGLE_CONTROL		# set absolute angle
	li		$t0, 10
	sw		$t0, VELOCITY			# set velocity to 10
	j		interrupt_inv_done
	
interrupt_inv_rht:
	lw		$t1, BOT_Y				# $t1 = y coord of bot
	li		$t3, 25
	li		$t2, 125
	blt		$t1, $t2, interrupt_inv_top
	li		$t2, 174
	blt		$t2, $t1, interrupt_inv_top
	
interrupt_inv_mid:
	# else, go directly left back to base
	li		$t0, 180
	sw		$t0, ANGLE				# set angle to 180
	li		$t0, 1
	sw		$t0, ANGLE_CONTROL		# set absolute angle
	li		$t0, 10
	sw		$t0, VELOCITY			# set velocity to 10
	j 		interrupt_inv_done
	
interrupt_inv_top:
	# if (BOT_Y < 125), then go to top right corner
	sub		$a0, $t3, $t0			# $a0 = 25 - $t0, assumes $t0 = BOT_X, $t3 = 25
	sub		$a1, $t2, $t1			# $a1 = 125 - $t1, assumes $t1 = BOT_Y, $t2 = 125
	beq		$a0, $0, interrupt_inv_done
	jal		arctan					# $v0 = arctan ($a1 / $a0)
	sw		$v0, ANGLE				# set angle
	li		$t0, 1
	sw		$t0, ANGLE_CONTROL		# set absolute angle
	li		$t0, 10
	sw		$t0, VELOCITY			# set velocity to 10
	j		interrupt_inv_done
	
interrupt_inv_0:
	# if carrying 0 flags, then set bcarry to 0, continue
	sw		$0, bcarry				# set bcarry to 0, not carrying any flags
	
	lw		$t0, SCORE				# $t0 = bot score
	lw		$t1, ENEMY_SCORE		# $t1 = opp score
	sub		$t0, $t1, $t0			# $t0 = opp score - bot score

	blt		$t0, -4, interrupt_inv_not_behind
	
interrupt_inv_behind:
	# if behind in score, then switch to offensive mode
	li		$t0, 1
	sw		$t0, admode				# set admode to 1 (I mode)
	j		interrupt_inv_lt
	
interrupt_inv_not_behind:
	# if ahead in score, then switch to defensive mode
	li		$t0, 12
	sw		$t0, admode				# set admode to 12 (Def mode)
	j		interrupt_inv_done
	
interrupt_inv_lt:
	# find the nearest flag, go to (150, 150) if no flags found
#	lw		$t0, choice				# $t0 = choice boolean
#	bne		$t0, $0, interrupt_inv_done	# skip if choice already made
	la		$k0, arflag				# $k0 = pt to start of flag array
	li		$t2, 999999				# $t2 = current dist
	li		$t3, 150				# $t3 = current destination x coord
	li		$t4, 150				# $t4 = current destination y coord
	
interrupt_inv_lp:
	lw		$a0, 0($k0)				# $a0 = x coord of flag
	lw		$a1, 4($k0)				# $a1 = y coord of flag
	beq		$a0, -1, interrupt_inv_end
	lw		$t0, opxpos				# $t0 = x coord of opp
	lw		$t1, opypos				# $t1 = y coord of opp
	jal		dist					# $v0 = dist, $v1 = x diff
	blt		$v0, 1000, interrupt_inv_skip	# skip if opp too close to flag
	lw		$t0, BOT_X				# $t0 = x coord of bot
	lw		$t1, BOT_Y				# $t1 = y coord of bot
	jal		dist					# $v0 = dist, $v1 = x diff
	blt		$t2, $v0, interrupt_inv_skip
#	bgt		$v1, 2500, interrupt_inv_skip
	move	$t2, $v0				# if (not(cur dist < dist)), then set new dist
	move	$t3, $a0				# set new destination x coord
	move	$t4, $a1				# set new destination y coord
	
interrupt_inv_skip:
	add		$k0, $k0, 8				# move $k0 to next flag
	j		interrupt_inv_lp		# loop until null flag reached
	
interrupt_inv_end:
	#bne		$t3, $t0, interrupt_inv_ne
	#bne		$t4, $t1, interrupt_inv_ne
	bgt		$t2, 4, interrupt_inv_ne
	
	sw		$t0, PICK_FLAG			# pick flag at current location
	la		$t0, arflag
	sw		$t0, FLAG_REQUEST		# update flag array
	li		$t0, 1
	sw		$t0, bcarry				# set bcarry to 1 (carrying flag)
	lw		$t0, nflags
	sub		$t0, $t0, 1
	sw		$t0, nflags				# nflags --
	j		interrupt_inv_done
	
interrupt_inv_ne:
	sub		$a0, $t3, $t0			# $a0 = dest x - bot x
	sub		$a1, $t4, $t1			# $a1 = dest y - bot y
	jal		arctan					# $v0 = arctan ($a1 / $a0)
	sw		$v0, ANGLE				# set angle to $v0
	li		$v1, 1
	sw		$v1, ANGLE_CONTROL		# set absolute angle
	li		$v0, 10
	sw		$v0, VELOCITY			# set velocity to 10
#	sw		$v1, choice				# set choice to 1 (destination chosen)
	
interrupt_inv_done:
	la		$k0, chunkIH
	lw		$t4, 36($k0)			# restore $t4
	lw		$t3, 32($k0)			# restore $t3
	lw		$t2, 28($k0)			# restore $t2
	lw		$t1, 24($k0)			# restore $t1
	lw		$t0, 20($k0)			# restore $t0
	
	# fall through to interrupt end
	
interrupt_end:
	lw		$a0, TIMER				# $a0 = current time
	add		$a0, $a0, 1000			# $a0 += 1000, 1K cycles to move 1 px
	sw		$a0, TIMER				# request interrupt in 1K cycles
	
	j 		interrupt_dispatch		# check other interrupts
	
# ================================================================================
# INTERRUPT_CHASE - chase mode
# ================================================================================
	
interrupt_chase:
	# C - chase after opp using estimated future position, stop when cross line or invis
	la		$k0, chunkIH
	sw		$t0, 20($k0)			# save $t0
	sw		$t1, 24($k0)			# save $t1
	sw		$t2, 28($k0)			# save $t2
	
	lw		$a0, bopinv
	bne		$a0, $0, interrupt_chase_I
	lw		$a0, opxpos
	lw		$a1, opypos
	sub		$t0, $a0, 150
	abs		$t0, $t0				# $t0 = abs(opxpos - 150)
	blt		$t0, 3, interrupt_chase_I
	bgt		$a0, 148, interrupt_chase_I
	j		interrupt_chase_C
	
interrupt_chase_I:
	li		$a0, 1
	sw		$a0, admode				# set admode to 1 (I mode)
	j 		interrupt_chase_done
	
interrupt_chase_C:
	lw		$t0, BOT_X
	sub		$a0, $a0, $t0			# $a0 -= BOT_X
	lw		$t0, opxvel
#	mul		$t0, $t0, 2
	add		$a0, $a0, $t0			# $a0 += opxvel
#	add		$a0, $a0, 5				# $a0 += 5
	lw		$t1, BOT_Y
	sub		$a1, $a1, $t1			# $a1 -= BOT_Y
	lw		$t1, opyvel
#	mul		$t1, $t1, 2
	add		$a1, $a1, $t1			# $a1 += opyvel
	jal		arctan					# $v0 = arctan ($a1 / $a0)
	sw		$v0, ANGLE				# set angle to $v0
	li		$v0, 1
	sw		$v0, ANGLE_CONTROL		# set absolute angle
	li		$v0, 10
	sw		$v0, VELOCITY			# set velocity to 10
	
interrupt_chase_done:
	la		$k0, chunkIH
	lw		$t2, 28($k0)			# restore $t2
	lw		$t1, 24($k0)			# restore $t1
	lw		$t0, 20($k0)			# restore $t0
	
	j		interrupt_end
	
# ================================================================================
# INTERRUPT_DEFENSE - defense mode
# ================================================================================
	
interrupt_defense:
	lw	$v1	OTHER_BOT_Y
	bne	$v1	-1	interrupt_def_not_invis

	#other bot is invisible, can't defend against that.
	li	$v1	1
	sw	$v1	admode
	j	interrupt_end

interrupt_def_not_invis:
	#need to save more registers to use existing functions
	la	$k0, chunkIH
	sw	$t0, 20($k0)			# $t0 goes after $v1
	sw	$t1, 24($k0)			# $t1 goes after $t0
	sw	$t2, 28($k0)			# $t2 goes after $t1
	sw	$t3, 32($k0)			# $t3 goes after $t2
	sw	$t4, 36($k0)			# $t4 goes after $t3

	bge	$v1	150	interrupt_def_not_attacking
	#other bot is on our side, get him!
	lw	$t3	OTHER_BOT_X
	move	$t4	$v1
	lw	$t0	BOT_X
	lw	$t1	BOT_Y

	#thanks for this
	sub	$a0, $t3, $t0			# $a0 = dest x - bot x
	sub	$a1, $t4, $t1			# $a1 = dest y - bot y
	lw		$t0, opxvel
	add		$a0, $a0, $t0			# $a0 += opxvel
#	add		$a0, $a0, 5				# $a0 += 5
	lw		$t1, opyvel
	add		$a1, $a1, $t1			# $a1 += opyvel
	
	jal	arctan					# $v0 = arctan ($a1 / $a0)
	sw	$v0, ANGLE				# set angle to $v0
	li	$v0, 1
	sw	$v0, ANGLE_CONTROL		# set absolute angle
	li	$v0, 10
	sw	$v0, VELOCITY			# set velocity to 10
	j	interrupt_def_done

interrupt_def_not_attacking:
	#they aren't invis and aren't on our side, maybe there is a flag they cannot defend
	#look for closest flag, see if we can get to it before they can

interrupt_def_lt:
	la	$k0, arflag				# $k0 = pt to start of flag array
	
interrupt_def_lp:
	lw	$t0, BOT_X				# $t0 = x coord of bot
	lw	$t1, BOT_Y				# $t1 = y coord of bot

	lw	$a0, 0($k0)				# $a0 = x coord of flag
	lw	$a1, 4($k0)				# $a1 = y coord of flag
	add	$k0, $k0, 8				# move $k0 to next flag
	beq	$a0, -1, interrupt_def_guarded
	jal	dist					# $v0 = dist, $v1 = x diff
	move	$t2	$v0
	lw	$t0, OTHER_BOT_X				# $t3 = current destination x coord
	lw	$t1, OTHER_BOT_Y
	jal	dist

	ble	$v0	$t2	interrupt_def_lp	#are we closer to the flag
	#there is an unprotected flag, after it!
	sw	$a0	tunnelflags($0)
	la	$a0	tunnelflags
	sw	$a1	4($a0)
	li	$a0	13
	sw	$a0	admode					# set tunnel vision mode
	j	interrupt_def_done
	
interrupt_def_guarded:
	#no unprotected flags, what next?

	lw	$t0	SCORE
	lw	$t1	ENEMY_SCORE
	sub	$t0	$t1	$t0

	blt	$t0	$0	interrupt_def_not_behind
	#we are not ahead in score, can't afford to be defensive

	li	$t0	1	#set mode to offensive
	sw	$t0	admode
	j	interrupt_def_done

interrupt_def_not_behind:
	#ahead in score, get comfortable

	lw	$a0	BOT_X
	beq	$a0	148	interrupt_def_at_middle

	li	$t3	148
	lw	$t4	OTHER_BOT_Y
	lw	$t0	BOT_X
	lw	$t1	BOT_Y

	#thanks for this
	sub	$a0, $t3, $t0			# $a0 = dest x - bot x
	sub	$a1, $t4, $t1			# $a1 = dest y - bot y
	jal	arctan					# $v0 = arctan ($a1 / $a0)
	sw	$v0, ANGLE				# set angle to $v0
	li	$v0, 1
	sw	$v0, ANGLE_CONTROL		# set absolute angle
	li	$v0, 10
	sw	$v0, VELOCITY			# set velocity to 10
	j	interrupt_def_done

interrupt_def_at_middle:
	#already at middle

	lw	$t0	BOT_Y
	lw	$t1	OTHER_BOT_Y

	bne	$t0	$t1	interrupt_def_no_sit

	#we are happy where we are, take a break
	sw	$0	VELOCITY
	j	interrupt_def_done

interrupt_def_no_sit:
	#can't sit here, keep moving

	li	$t3	148
	lw	$t4	OTHER_BOT_Y
	lw	$t0	BOT_X
	lw	$t1	BOT_Y

	#thanks for this
	sub	$a0, $t3, $t0			# $a0 = dest x - bot x
	sub	$a1, $t4, $t1			# $a1 = dest y - bot y
	
	jal	arctan					# $v0 = arctan ($a1 / $a0)
	sw	$v0, ANGLE				# set angle to $v0
	li	$v0, 1
	sw	$v0, ANGLE_CONTROL		# set absolute angle
	li	$v0, 10
	sw	$v0, VELOCITY			# set velocity to 10
#	j	interrupt_def_done

interrupt_def_done:
	la	$k0, chunkIH
	lw	$t0, 20($k0)			# $t0 goes after $v1
	lw	$t1, 24($k0)			# $t1 goes after $t0
	lw	$t2, 28($k0)			# $t2 goes after $t1
	lw	$t3, 32($k0)			# $t3 goes after $t2
	lw	$t4, 36($k0)			# $t4 goes after $t3
	j	interrupt_end
	
# ================================================================================
# INTERRUPT_TUNNELVISION - tunnel vision mode
# ================================================================================
	
interrupt_tunnelvision:
	la	$k0, chunkIH
	sw	$t0, 20($k0)			# $t0 goes after $v1
	sw	$t1, 24($k0)			# $t1 goes after $t0
	sw	$t2, 28($k0)			# $t2 goes after $t1
	sw	$t3, 32($k0)			# $t3 goes after $t2
	sw	$t4, 36($k0)			# $t4 goes after $t3
	
	lw	$t0	BOT_X
	lw	$t1	BOT_Y
	lw	$a0	tunnelflags($0)
	la	$a1	tunnelflags
	lw	$a1	4($a1)

	jal	dist
	bge	$v0	3	interrupt_tv_far

	sw	$0	PICK_FLAG
	#==========
	# RETURN TO BASE
	#----------

	li	$a0	2		#return to base mode here
	sw	$a0	admode	
	j	interrupt_tv_end

	#==========

interrupt_tv_far:
	#too far to pick up flag, keep trucking
	#thanks for this
	lw	$t3	tunnelflags($0)
	la	$t4	tunnelflags
	lw	$t4	4($t4)
	sub	$a0, $t3, $t0			# $a0 = dest x - bot x
	sub	$a1, $t4, $t1			# $a1 = dest y - bot y
	jal	arctan					# $v0 = arctan ($a1 / $a0)
	sw	$v0, ANGLE				# set angle to $v0
	li	$v0, 1
	sw	$v0, ANGLE_CONTROL		# set absolute angle
	li	$v0, 10
	sw	$v0, VELOCITY			# set velocity to 10
	
interrupt_tv_end:
	la	$k0, chunkIH
	lw	$t0, 20($k0)			# $t0 goes after $v1
	lw	$t1, 24($k0)			# $t1 goes after $t0
	lw	$t2, 28($k0)			# $t2 goes after $t1
	lw	$t3, 32($k0)			# $t3 goes after $t2
	lw	$t4, 36($k0)			# $t4 goes after $t3
	j	interrupt_end
	
# ================================================================================
# FUNCTIONS
# ================================================================================
	
# input: $t0 = x1, $t1 = y1, $a0 = x2, $a1 = y2, output: $v0 = dist, $v1 = x diff
dist:
	sub		$v1, $t0, $a0			# $v1 = (x1 - x2)
	mul		$v1, $v1, $v1			# $v1 = (x1 - x2) ^ 2
	sub		$v0, $t1, $a1			# $v0 = (y1 - y2)
	mul		$v0, $v0, $v0			# $v0 = (y1 - y2) ^ 2
	add		$v0, $v1, $v0			# $v0 = (x1 - x2) ^ 2 + (y1 - y2) ^ 2
	jr		$ra						# return with $v0
	
# input: $a0 = x, $a1 = y, $v0 = arctan (y / x)
arctan:								# compute arctangent of y / x
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
	
# ================================================================================
# INVIS INTERRUPT
# ================================================================================
	
interrupt_invis:
	sw		$a0, INVIS_ACKNOWLEDGE	# acknowledge interrupt
	
	li		$a0, 1					# $a0 = 1
	sw		$a0, binvis($0)			# set binvis to 1, now can go invis
	sw		$a0, PRINT_INT
	
	j		interrupt_dispatch		# check other interrupts
	
# ================================================================================
# COORDS INTERRUPT
# ================================================================================
	
interrupt_coords:
	sw		$a0, COORDS_ACKNOWLEDGE	# acknowledge interrupt
	
	# request opponent coordinates
	sw		$a0, COORDS_REQUEST
	
	# set opxpos, opypos
	lw		$a0, OTHER_BOT_X		# $a0 = x coord of other bot
	beq		$a0, -1, interrupt_coords_inv
	
	lw		$v0, opxpos				# $v0 = previous x coord
	sub		$v0, $a0, $v0			# $v0 = change in x coord
	sw		$v0, opxvel				# set opxvel to $v0
	sw		$a0, opxpos				# set opxpos to $a0
	lw		$a0, OTHER_BOT_Y		# get y coord of other bot
	lw		$v0, opypos				# $v0 = previous x coord
	sub		$v0, $a0, $v0			# $v0 = change in x coord
	sw		$v0, opyvel				# set opyvel to $v0
	sw		$a0, opypos				# set opypos variable
	sw		$0,  bopinv				# set bopinv to 0 (opp not invisible)
	
	j 		interrupt_dispatch		# check other interrupts
	
interrupt_coords_inv:				# opp is invisible, don't update coords
	li		$a0, 1
	sw		$a0, bopinv				# set bopinv to 1 (opp is invisible)
	
	j 		interrupt_dispatch		# check other interrupts
	
# ================================================================================
# INTERRUPT_BONK
# ================================================================================

interrupt_bonk:
	sw		$a0, BONK_ACKNOWLEDGE	# acknowledge interrupt
	
	# reverse angle by 180 degrees
	li		$a0, 180
	sw		$a0, ANGLE				# set angle to 180
	sw		$0, ANGLE_CONTROL		# set relative angle
	
	j 		interrupt_dispatch		# check other interrupts
	
interrupt_non:
	# non-interrupt
	# fall through to done
	
interrupt_done:
	la		$k0, chunkIH
	lw		$ra, 0($k0)				# restore $ra
	lw		$a0, 4($k0)				# restore $a0
	lw		$a1, 8($k0)				# restore $a1
	lw		$v0, 12($k0)			# restore $v0
	lw		$v1, 16($k0)			# restore $v1
	
.set noat
	move 	$at, $k1				# restore $at
.set at
	eret							# return
	