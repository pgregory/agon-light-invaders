
init_invaders:
	push af
	push hl
	push bc
	push de
	
	ld a, (next_sprite_index) ; Sprite index
	ld b, 55 ; Number of invaders
@inv_def_loop:
	ld hl, invaders
	ld.l de, 0
	ld e, a
	add hl, de
	ld a, (hl)	; Get invader state variable
	and a
	ld a, e
	jp z, @done

@done:
	add a, 1
	djnz @inv_def_loop
	
	ld (inv_explode_spr), a
	ld hl, inv_explosion_sprite_data
	call def_sprite
	call hide_sprite

	add a, 1

	ld (inv_shot_spr), a
	ld hl, inv_shot_sprite_data
	call def_sprite
	;call hide_sprite
	call show_sprite
	
	add a, 1
	ld (next_sprite_index), a

	pop de
	pop bc
	pop hl
	pop af
	ret
	

update_invaders:
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
	
	call get_curr_inv_coords	; Get invader coords BC, HL
	ld (last_invader_x_pos), bc	; Store X for boundary check

	call clear_invader
	call draw_invader

	ld hl, (last_invader_x_pos)	; Check boundary conditions
	ld a, l
	cp 10
	jp c, @do_swap
	cp 200 
	jp c, @step

@do_swap:
	;ld hl, (last_invader_x_pos)	; Print coord triggering swap
	;call home_cursor
	;call print_Hex16

	ld a, 1						; Flag a swap at the end of this rack move
	ld (should_swap), a
	jp @step

@hide:
	call hide_sprite			; Hide the current invader
	inc a						; Check the next one...
	ld (invader_cursor), a
	cp 55
	jp c, @try					; Try again if more invaders left.
	
@step:
	ld a, (invader_cursor)		; Step cursor to next invader
	inc a
	ld (invader_cursor), a
	cp 55
	jp c, @next					; Carry on if more to go

	ld a, (invader_anim_frame)
	add a, 1
	and 1
	ld (invader_anim_frame), a
	ld a, (invader_ref_y)
	ld (last_invader_ref_y), a
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
	ld (last_invader_off_x), a
	add a, b
	ld (invader_off_x), a
	jp @next

@inv_exploding_countdown:
	ld hl, inv_explode_time
	dec (hl)
	jp nz, @next
	
	ld a, (exploding_inv)
	call remove_invader
	
	ld a, 0
	ld (inv_exploding), a

@next:
	pop hl
	pop bc
	pop af
	ret


update_invader_shots:
	push af
	push bc
	push hl

	;ld a, (shot_active)
	;and a
	;jp z, @done
	
    ld a, (shot_pos_y)
    add a, 1
	cp $F0
    jp nc, @shot_exited

    ld (shot_pos_y), a

    ld a, (shot_pos_x)
    ld c, a
    ld b, 0
    ld a, (shot_pos_y)
    ld e, a
    ld d, 0
    ld a, (inv_shot_spr)
    call move_sprite
	call next_frame_sprite		; Animate sprite frame

    ld a, (shot_pos_x)
	add a, 1
    ld c, a
    ld b, 0
    ld a, (shot_pos_y)
	add a, 8
    ld e, a
    ld d, 0
    call get_pixel_color
    
    ld hl, (pixel_data_ptr)
    ld hl, (hl)
    ld a, h
    and a
    jp z, @done

    ; print pixel color 
	; push hl
    ; push af
	; call home_cursor
	; call print_Hex8
    ; pop af
	; pop hl

@check_hits:
    ld a, (shot_pos_y)
    add a, 8
    cp 140
    jp c, @done

@check_player:
    cp PLAYER_YPOS
    jp c, @check_shields
    cp PLAYER_YPOS + 8
    jp nc, @shot_exited
    
    call explode_player

    jp @shot_exited
    
@check_shields:
    ; Check if the hit is in the range of the shields
    cp 140
    jp c, @done

    ld a, (shot_pos_x)
    sub 3
    ld c, a
    ld b, 0
    ld a, (shot_pos_y)
	add a, 4
    ld e, a
    ld d, 0
    ld a, BMP_PLYR_B_EXPL
    call draw_bitmap
    
@shot_exited:
	call invader_shoot

@done:
	pop hl
	pop bc
	pop af
	ret
	
	
; A - column
; Returns A - invader index
bottom_invader_in_column:
	push hl
	push bc

@loop:
	ld hl, invaders
	ld b, 0
	ld c, a
	add hl, bc
	ld a, (hl)
	or a 
	ld a, c
	jp z, @found
	
	add a, 11 	; next row
	cp 55
	jp c, @loop
	
	ld a, $AA	; not found
	jp @done

@found:

	;push af
	;ld l, a	
	;call home_cursor
	;call print_Hex16
	;pop af

@done:
	pop bc
	pop hl
	ret

	
; A - index
; Returns BC - X, DE - Y  relative to reference alien
get_relative_inv_pos:
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
	ret


get_prev_inv_coords:
	push af
	call get_relative_inv_pos
	ld b, 0
	ld d, 0
	ld a, (last_invader_off_x)
	add a, 24
	add a, c
	ld c, a
	ld a, (last_invader_ref_y) 
	sub a, e
	ld e, a
	pop af
	ret

get_curr_inv_coords:
	push af
	call get_relative_inv_pos
	ld b, 0
	ld d, 0
	ld a, (invader_off_x)
	add a, 24
	add a, c
	ld c, a
	ld a, (invader_ref_y) 
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

    ld hl, invaders				; Point to invaders status table
    ld c, a						; Store invader cursor
    ld b, 0						; Clear MSB
    add hl, bc					; Point to this invader status
	ld a, (hl)
	or a
	jp nz, @dead
	ld a, c

	jp @done

@dead:
	ld a, $CC
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
	
	ld a, (invader_ref_y)
	add a, 8
	ld (invader_ref_y), a
	ret

	

invader_shoot:
	push af
	push bc
	push hl
	
	ld hl, invader_col_fire_table
	ld a, (invader_col_pos_1)
	cp $10
	jp c, @do_shot

	; Loop through column table
	ld a, (invader_col_pos_1_init)
	ld (invader_col_pos_1), a
	
@do_shot:
	ld c, a
	ld b, 0
	add hl, bc
	add a, 1
	ld (invader_col_pos_1), a
	ld a, (hl)

	call bottom_invader_in_column
	cp $AA
	jp z, @done
	call get_curr_inv_coords
	ld a, c
	add a, 7
	ld (shot_pos_x), a
	ld a, e
	add a, 11
	ld (shot_pos_y), a
	
@done:
	pop hl
	pop bc
	pop af
	ret


; A - index
draw_invader:
	push bc
	push af
	call get_inv_type
	push af
	ld a, (invader_anim_frame)
	add a, 1
	ld c, a
	ld b, 0
	add hl, bc
	pop af
	call get_curr_inv_coords
	ld a, (hl)
	call draw_bitmap
	pop af
	pop bc
	
	ret

; A - index	
remove_invader:
	push bc
	push af
	; call get_inv_type
	; ld c, 1
	; ld b, 0
	; add hl, bc
	call get_curr_inv_coords
	ld a, BMP_BLANK
	call draw_bitmap
	pop af
	pop bc
	
	ret

; A - index	
clear_invader:
	push bc
	push af
	; call get_inv_type
	; ld c, 1
	; ld b, 0
	; add hl, bc
	call get_prev_inv_coords
	ld a, BMP_BLANK
	call draw_bitmap
	pop af
	pop bc
	
	ret
	
; A - index
kill_invader:
    ld hl, invaders				; Point to invaders status table
    ld c, a						; Store invader cursor
    ld b, 0						; Clear MSB
    add hl, bc					; Point to this invader status
    ld (hl), 1
	ld (exploding_inv), a
	call remove_invader
    call get_curr_inv_coords
    ld a, BMP_INV_EXPLOSION
	call draw_bitmap

    ld a, 2
    ld (inv_exploding), a
    ld a, $10
    ld (inv_explode_time), a
	
	ret

; --------------------------------------

invader1:
	db 2, 0, 1,     0
	db 2, 2, 3,     0
	db 2, 4, 5,     0

animation_frame:
	db 0
	
	
inv_explosion_sprite_data:
	db 1, BMP_INV_EXPLOSION
	
inv_shot_sprite_data:
	db 4, BMP_INV_SHOT_F1, BMP_INV_SHOT_F2, BMP_INV_SHOT_F3, BMP_INV_SHOT_F4
	
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

	
invader_col_fire_table:
	db $1, $7, $1, $1, $1, $4, $B, $1, $6, $3, $1, $1, $1, $B, $9, $2, $8
	db $2, $B, $4, $7, $A
	
invader_col_pos_1_init:
	db 6
invader_col_pos_1:
	db 6

invader_cursor:
	db 0
	
invader_anim_frame:
	db 0

invader_ref_y:
	db 80
invader_off_x:
	db 0

last_invader_ref_y:
	db 80
last_invader_off_x:
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
exploding_inv:
	db 0
	
inv_explode_spr:
	db 0

inv_shot_spr:
	db 0


shot_pos_x:
    db 0
shot_pos_y:
    db 0
shot_active:
    db 0
