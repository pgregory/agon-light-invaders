; Assembly control flags
DEBUG: equ 0
USE_SPRITES: equ 1

MAX_ARGS:	equ 1
	include "include/crt.inc"
	include "include/vdp.inc"
	include "include/math.inc"
	include "include/utils.inc"	
	include "include/player.inc"

_main:
	call vdp_init

	; Initialise the invader sprites
	call init_enemies
	call init_player
	
	ld a, BMP_SHIELD
	ld bc, 32
	ld de, 140
	call draw_bitmap
	ld bc, 32+22+23
	call draw_bitmap
	ld bc, 32+22+23+22+23
	call draw_bitmap
	ld bc, 32+22+23+22+23+22+23
	call draw_bitmap

	ld b, 13
	ld hl, 8 
@line_loop:
	push bc
	ld a, BMP_LINE
	ld c, l
	ld b, h
	ld de, 180
	call draw_bitmap
	ld a, 16
	ld b, 0
	ld c, a
	add hl, bc
	pop bc
	djnz @line_loop
	
	;ld a, 8
	;call draw_line
	
	
	if USE_SPRITES
	ld a, (next_sprite_index)
	call activate_sprites
	endif

@loop:
	call delay 
	call update_enemies

	if USE_SPRITES
	call update_sprites
	endif

	call process_player

	ld a, (keycode)

	if DEBUG
	; Print keycode for identification
    push af
	ld l, a
	ld h, 0
	call home_cursor
	call print_Hex16
    pop af
	endif

	cp 27
	jr z, exit

	jr @loop
exit:	
	call vdp_close
	ret

delay:
	ld b, 1
@@:	call vsync
	djnz @p
	ret

	
init_enemies:
	push af
	push hl
	push bc
	push de
	
	ld a, (next_sprite_index) ; Sprite index
	ld b, 55 ; Number of enemies
@inv_def_loop:
	if USE_SPRITES
	call get_inv_type
	call def_sprite
	endif

	ld hl, invaders
	ld e, a
	ld d, 0
	add hl, de
	ld a, (hl)	; Get invader state variable
	and a
	ld a, e
	jp z, @done

	if USE_SPRITES
	call show_sprite
	endif

@done:
	add a, 1
	djnz @inv_def_loop
	
	ld (inv_explode_spr), a
	ld hl, inv_explosion_sprite_data
	call def_sprite
	call hide_sprite

	add a, 1
	ld (next_sprite_index), a

	pop de
	pop bc
	pop hl
	pop af
	ret
	


update_enemies:
	push af
	push bc
	push hl
	
	ld a, (inv_exploding)
	and a
	jp nz, @inv_exploding_countdown
	
	ld a, (invader_cursor)
@try:
	ld hl, invaders				; Point to invaders status table
	ld c, a						; Store invader cursor
	ld b, 0						; Clear MSB
	add hl, bc					; Point to this invader status
	ld a, (hl)					; Get invader state variable
	and a						; If not zero, killed.
	ld a, (invader_cursor)
	jp nz, @hide
	
	call get_inv_coords			; Get invader coords BC, HL
	ld (last_invader_x_pos), bc	; Store X for boundary check

	if USE_SPRITES
	call move_sprite			; Move sprite into position
	call next_frame_sprite		; Animate sprite frame
	else
	call clear_invader_bmp
	call draw_invader
	endif

	ld hl, (last_invader_x_pos)	; Check boundary conditions
	ld a, l
	cp 10
	jp c, @do_swap
	cp 200 
	jp c, @1

@do_swap:
	;ld hl, (last_invader_x_pos)	; Print coord triggering swap
	;call home_cursor
	;call print_Hex16

	ld a, 1						; Flag a swap at the end of this rack move
	ld (should_swap), a
	jp @1

@hide:
	call hide_sprite			; Hide the current invader
	inc a						; Check the next one...
	ld (invader_cursor), a
	cp 55
	jp c, @try					; Try again if more invaders left.
	
@1:
	ld a, (invader_cursor)		; Step cursor to next invader
	inc a
	ld (invader_cursor), a
	cp 55
	jp c, @next					; Carry on if more to go

	xor a						; Reset cursor
	ld (invader_cursor), a

	ld a, (should_swap)			; If move has hit boundary, swap
	and a						; direction
	jp z, @move
	call swap_rack_direction
	xor a
	ld (should_swap), a

@move:
	ld a, (invader_direction)	; Move rack in current direction
	ld b, a
	ld a, (invader_off_x)
	add a, b
	ld (invader_off_x), a
	jp @next

@inv_exploding_countdown:
	ld hl, inv_explode_time
	dec (hl)
	jp nz, @next
	ld a, (inv_explode_spr)
	call hide_sprite
	
	ld a, 0
	ld (inv_exploding), a

@next:
	pop hl
	pop bc
	pop af
	ret
	
	
; A - index
; Returns BC - X, DE - Y 
get_inv_coords:
	push af
	ld c, 0 ; X ref
	ld e, 0 ; Y ref
@row_loop:
	cp 11
	jp m, @at_row
	sbc a, 11
	ld d, a
	ld a, e
	add a, 16
	ld e, a
	ld a, d
	jp @row_loop
@at_row:
@col_loop:
	and a
	jp z, @at_col
	ld d, a
	ld a, c
	add a, 16
	ld c, a
	ld a, d
	dec a
	jp @col_loop
@at_col:
	ld b, 0
	ld d, 0
	ld a, (invader_off_x)
	add a, 24
	add a, c
	ld c, a
	ld a, 80 
	sub a, e
	ld e, a
	pop af
	ret
  
; BC - X
; DE - Y 
; Returns A - index
get_inv_at:
	push bc
	push de
	push hl
	ld a, (invader_ref_y)
	add a, 16 ; Bottom pixel row or invader rank
	cp e
	; Check if below the bottom row 
	jp c, @none_y
	sub e
	srl a
	srl a
	srl a
	srl	a; Divide by 16 to get row
	ld l, a
	ld h, 11
	mlt hl
	ld a, (invader_off_x)
	add a, 24
	ld e, a
	ld a, c
	sub e
	srl a
	srl a
	srl a
	srl a; Divide by 16 to get column
	add a, l
	jp @done
	
@none_y:
	ld a, $AA
	jp @done

@none_x:
	ld a, $BB

@done:
	pop hl
	pop de
	pop bc
	ret




; A - index
; Returns HL - sprite def
get_inv_type:
	push af
	push bc
	ld bc, 0
@row_loop:
	cp 11
	jp m, @at_row
	sbc a, 11
	inc bc
	jp @row_loop
@at_row:
	ld a, c
	and 0xfe
	rlca
	ld c, a
	ld b, 0
	ld hl, invader1
	add hl, bc
	pop bc
	pop af
	ret

swap_rack_direction:
	ld a, (invader_direction)
	or a
	jp m, @left
	ld a, -2
	jp @change_dir
@left:
	ld a, 2
@change_dir:
	ld (invader_direction), a
	ret


invader1:
	db 2, 0, 1,     0
	db 2, 2, 3,     0
	db 2, 4, 5,     0

animation_frame:
	db 0
	
	
inv_explosion_sprite_data:
	db 1, BMP_INV_EXPLOSION
	
invaders:
	db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
	db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
	db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
	db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
	db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

	; Test set with some invaders killed
	db 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
	db 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
	db 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0
	db 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0
	db 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1


invader_cursor:
	db 0

invader_ref_y:
	db 80
invader_off_x:
	db 0
invader_direction:
	db 2
last_invader_x_pos:
	dw 0, 0
should_swap:
	db 0
	
inv_exploding:
	db 0
inv_explode_time:
	db 0
	
inv_explode_spr:
	db 0

bottom_line:
	dw 8
	dw 180
	dw 168
	dw 180


	if USE_SPRITES
	else
; A - bitmap
; BC - X
; DE - Y
draw_invader_bmp:
	push af
	call draw_bitmap
	pop af
	ret
	
	
; BC - X
; DE - Y
clear_invader_bmp:
	push af
	ld a, BMP_BLANK
	call draw_bitmap
	pop af
	ret
	endif
	

; A - index
draw_invader:
	if USE_SPRITES
	; Sprite mode
	call show_sprite
	push bc
	call get_inv_coords
	call move_sprite
	pop bc

	else
	
	; Bitmap mode
	push bc
	call get_inv_type
	ld c, 1
	ld b, 0
	add hl, bc
	call get_inv_coords
	ld a, (hl)
	call draw_invader_bmp
	pop bc
	
	endif

	ret