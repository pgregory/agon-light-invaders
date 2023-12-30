
init_invaders:
	push af
	push hl
	push bc
	push de
	push ix
	
	ld a, (next_sprite_index) ; Sprite index
	ld (inv_shot_spr), a
	ld hl, inv_shot_sprite_data_r
	call def_sprite
	call hide_sprite
	
	inc a

	ld (inv_shot_spr2), a
	ld hl, inv_shot_sprite_data_p
	call def_sprite
	call hide_sprite

	inc a

	ld (inv_shot_spr3), a
	ld hl, inv_shot_sprite_data
	call def_sprite
	call hide_sprite

	inc a

	ld (saucer_spr), a
	ld hl, saucer_sprite_data
	call def_sprite
	call hide_sprite

	inc a

	ld (saucer_exp_spr), a
	ld hl, saucer_exp_sprite_data
	call def_sprite
	call hide_sprite
	
	inc a
	ld (next_sprite_index), a
	
	pop ix
	pop de
	pop bc
	pop hl
	pop af
	ret
	
reset_invaders:
	push af
	push bc
	push de
	push hl
	ld a, 0
	ld b, 55 ; Number of invaders
@inv_def_loop:
	ld hl, invaders
	ld.l de, 0
	ld e, a
	add hl, de
	ld (hl), 0
	add a, 1
	djnz @inv_def_loop
	
	ld hl, inv_state_start
	ex de, hl
	ld hl, inv_state_start
	inc de
	ld (hl), 0
	ld bc, inv_state_end-inv_state_start
	ldir

	ld a, 80
	ld (invader_ref_y), a
	ld (last_invader_ref_y), a
	ld a, 2
	ld (invader_direction), a
	ld a, $34
	ld (fleet_sound_timer), a

	xor a
	ld (saucer_active), a
	ld (saucer_exploding), a
	
	ld hl, SAUCER_DELAY
	ld (saucer_timer), hl

	pop hl
	pop de
	pop bc
	pop af
	
	ret
	

update_invaders:
	push af
	push bc
	push hl
	
	ld a, (inv_exploding)
	and a
	jp nz, @inv_exploding_countdown

	ld a, (player_exploding)
	and a
	jp nz, @next
	
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

	call get_inv_bitmaps
	ld a, (hl)
	ld hl, invader_anim_frame
	add a, (hl)
	call draw_bitmap

	ld hl, (last_invader_x_pos)	; Check boundary conditions
	ld a, l
	cp 10
	jp c, @do_swap
	cp 200 
	jp c, @step

@do_swap:
	ld a, 1						; Flag a swap at the end of this rack move
	ld (should_swap), a
	jp @step

@hide:
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
	call remove_explosion
	
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
	
	call saucer_timer
	
	ld a, (player_exploding)
	and a
	jp nz, @done

	ld bc, sb_end - sb_start ; Step between shot data blocks

	ld ix, shot_data
	ld a, (shot_timer)	; 0 - shot1, 1 - shot2, 2 - shot3

	cp 0
	jp z, @shot1
	add ix, bc
	cp 1
	jp z, @shot2
	add ix, bc
	cp 2
	jp z, @shot3
	dec a
	ld (shot_timer), a
	jp @done

; Squiggly shot/saucer
@shot3:
	ld a, (saucer_start)
	and a
	jp z, @do_shot3

	ld a, (shot3_step)
	and a
	jp nz, @do_shot3

	ld a, (saucer_active)
	and a
	jp nz, @process_saucer

@launch_saucer:
	ld a, (saucer_spr)
	call show_sprite
	ld a, 1
	ld (saucer_active), a
	ld a, 0
	ld (saucer_pos), a

    ld a, -8
    ld b, a
    ld a, CHAN_SAUCER
    ld de, 65535
    call play_channel

@process_saucer:
	ld a, (shot_timer)
	dec a
	ld (shot_timer), a
	ld a, (saucer_exploding)
	and a
	jp z, @move_saucer
	cp 2
	jp nc, @saucer_exp_delay
	ld a, (saucer_spr)
	call hide_sprite
	ld de, SAUCER_Y_POS
	ld a, (saucer_pos)
	ld c, a
	ld b, 0
	ld a, (saucer_exp_spr)
	call move_sprite
	call show_sprite

    ld a, -9
    ld b, a
    ld a, CHAN_SAUCER
    ld de, 1200
    call play_channel

@saucer_exp_delay:
	ld hl, saucer_exploding
	inc (hl)
	ld a, (hl)
	cp $20
	jp c, @done
	ld a, (saucer_exp_spr)
	call hide_sprite
	jp @saucer_exited

@move_saucer:
	ld de, SAUCER_Y_POS
	ld a, (saucer_pos)
	ld c, a
	ld b, 0
	ld a, (saucer_spr)
	call move_sprite
	ld a, (saucer_pos)
	ld c, a
	ld a, (saucer_delta)
	add a, c
	cp $d0
	jp nc, @saucer_exited
	ld (saucer_pos), a
	jp @done
	
@saucer_exited:
	ld hl, SAUCER_DELAY
	call reset_saucer
	xor a
	ld (saucer_active), a
	ld (saucer_start), a
	ld (saucer_exploding), a
	ld a, (saucer_spr)
	call hide_sprite
	xor a
	ld c, a
	ld a, CHAN_SAUCER
	call set_volume
	jp @done

@do_shot3:	
	ld a, (shot_timer)
	dec a
	ld (shot_timer), a
	ld a, (ix + 5)	; column offset
	cp $15
	jp c, @col3_ok
	ld (ix + 5), 6
@col3_ok:
	ld a, (shot1_step)
	ld (other1), a
	ld a, (shot2_step)
	ld (other2), a
	jr @doit

; Plunger shot
@shot2:
	dec a
	ld (shot_timer), a
	ld a, (ix + 5)	; column offset
	cp $10
	jp c, @col2_ok
	ld (ix + 5), 0
@col2_ok:
	ld a, (shot1_step)
	ld (other1), a
	ld a, (shot3_step)
	ld (other2), a
	jr @doit
	
; Rolling shot
@shot1:
	ld a, (shot2_step)
	ld (other1), a
	ld a, (shot3_step)
	ld (other2), a
	ld a, 2
	ld (shot_timer), a
	
@doit:
	ld a, (ix + 0)
	or a
	jp z, @check_shoot

	; Increment counter
	inc (ix + 4)

	ld a, (shot_speed)
	ld c, a
    ld a, (ix + 2) ; Y pos
    add a, c

	cp $F0
    jp nc, @shot_exited

    ld (ix + 2), a

    ld a, (ix + 1)
    ld c, a
    ld b, 0
    ld a, (ix + 2)
    ld e, a
    ld d, 0
    ld a, (ix + 3)
    call move_sprite
	call next_frame_sprite		; Animate sprite frame

    ld a, (ix + 1)
	add a, 1
    ld c, a
    ld b, 0
    ld a, (ix + 2)
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
    ld a, (ix + 2)
    add a, 8
    cp 140
    jp c, @done

@check_player:
    cp PLAYER_YPOS
    jp c, @check_shields
    cp PLAYER_YPOS + 8
    jp nc, @shot_exited
    
    call explode_player

    jp @shot_destroyed
    
@check_shields:
    ; Check if the hit is in the range of the shields
    cp 140
    jp c, @done

    ld a, (ix + 1)
    sub 3
    ld c, a
    ld b, 0
    ld a, (ix + 2)
	add a, 4
    ld e, a
    ld d, 0
    ld a, BMP_PLYR_B_EXPL
    call draw_bitmap
	
@shot_destroyed:
	ld (ix + 4), 0
	jr @shot_exited
    
@check_shoot:
	call invader_shoot
	jr @done

@shot_exited:
	ld a, 0
	ld (ix + 0), a
    ld a, (ix + 3)
    call hide_sprite

@done:
	pop hl
	pop bc
	pop af
	ret
	
	
; IX - shot data block
invader_shoot:
	push af
	push bc
	push hl

	; Check reload rate
	ld hl, shot_rate
	ld a, (other1)
	and a
	jp z, @next
	cp (hl)
	jp c, @done 

@next:
	ld a, (other2)
	and a
	jp z, @ok
	cp (hl)
	jp c, @done 

@ok:	
	; Reset counter
	ld (ix + 4), 0
	
	ld a, (ix + 6)	; check if target player
	or a
	jp z, @column

	; Get column above player
	ld a, (player_position)
	ld c, a
	ld b, 0
	call get_column_at
	cp $AA
	jp z, @done
	call bottom_invader_in_column
	cp $AA
	jp z, @done
	jr @shoot_from_alien

@column:	
	ld hl, invader_col_fire_table
	ld a, (ix + 5)
	inc (ix + 5)
	ld c, a
	ld b, 0
	add hl, bc
	ld a, (hl)

	call bottom_invader_in_column
	cp $AA
	jp z, @done

@shoot_from_alien:
	call get_curr_inv_coords
	ld a, c
	add a, 7
	ld (ix + 1), a
	ld a, e
	add a, 11
	ld (ix + 2), a
	ld a, 1
	ld (ix + 0), a
	ld a, (ix + 3)
	call show_sprite
	inc (ix + 4)
	
@done:
	pop hl
	pop bc
	pop af
	ret
	

saucer_timer:
	ld hl, 0
	ld ix, time_to_saucer
	ld a, (ix + 0)
	ld l, a
	ld a, (ix + 1)
	ld h, a
	dec hl
	
	ld a, l
	or h
	jp nz, reset_saucer 

	ld a, 1
	ld (saucer_start), a
	ld hl, SAUCER_DELAY
	
; HL - saucer time delay 
reset_saucer:
	ld a, l
	ld (ix + 0), a
	ld a, h
	ld (ix + 1), a

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
get_curr_inv_coords:
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

	pop af
	push af
	push bc
	ld c, a
	ld a, (invader_cursor)
	cp c
	pop bc
	jr c, @prev

	ld a, (invader_off_x)
	add a, 24
	add a, c
	ld c, a
	ld a, (invader_ref_y) 
	sub a, e
	jr @done
	
@prev:
	ld a, (last_invader_off_x)
	add a, 24
	add a, c
	ld c, a
	ld a, (last_invader_ref_y) 
	sub a, e

@done:
	; Move up by 4 to account for black space for wiping
	sub a, 4
	ld e, a
	ld b, 0
	ld d, 0

	pop af
	ret

; BC - X
; Returns A - column index
get_column_at:
	push bc
	push de
	push hl
	ld a, (invader_off_x)
	add a, 24
	ld e, a
	ld a, c
	sub e
	srl a
	srl a
	srl a
	srl a; Divide by 16 to get column
	
	cp 11
	jp c, @done

@none:
	ld a, $AA

@done:
	pop hl
	pop de
	pop bc
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
; Returns A - type
get_inv_type:
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
	rrca
	pop bc
	ret

; A - index
; Returns HL - bitmap list
get_inv_bitmaps:
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
	ld c, a
	ld b, 0
	ld hl, invader_bitmaps
	add hl, bc
	pop bc
	pop af
	ret

; A - index
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

	

; A - index	
remove_explosion:
	push bc
	push af
    call get_curr_inv_coords
	ld a, BMP_BLANK
	call draw_bitmap
	pop af
	pop bc
	
	ret

	
; A - index
kill_invader:
	push af
    ld hl, invaders				; Point to invaders status table
    ld c, a						; Store invader cursor
    ld b, 0						; Clear MSB
    add hl, bc					; Point to this invader status
    ld (hl), 1
	ld (exploding_inv), a
    call get_curr_inv_coords
    ld a, BMP_INV_EXPLOSION
	call draw_bitmap

    ld a, 2
    ld (inv_exploding), a
    ld a, $10
    ld (inv_explode_time), a
	
	ld a, -7
	ld b, a
	ld a, CHAN_ALIEN
	ld de, 420
	call play_channel
	
	pop af
	call get_inv_type
	ld c, a

	ld de, 10
	ld hl, 10
	dec c
	jp m, @done
	add hl, de
	dec c
	jp m, @done
	add hl, de
@done:
	call add_to_score
	
	ret
	
	
;
count_invaders:
	ld hl, invaders
	ld.s bc, $3700
@loop:
	ld a, (hl)
	and a
	jr nz, @next
	inc c
@next:
	inc hl
	dec b
	jr nz, @loop
	ld a, c
	ld (invader_count), a
	ret
	

play_fleet_sound:
	ld hl, fleet_sound_timer
	dec (hl)
	jr nz, @done	
	
	ld a, (saucer_active)
	and a,
	jp nz, @done
	
	; Choose from the 4 sounds
	ld hl, fleet_sound
	inc (hl)

	ld a, (inv_exploding)
	and a
	jr nz, @next
	
	ld a, (hl)
	cp 4
	jr c, @play_sound
	ld a, 0
	ld (hl), a

@play_sound:
	inc a	
	inc a	
	neg
	
	; Play sound and reset timer
	ld b, a
	ld a, CHAN_RANK
	ld de, 73
	call play_channel

@next:
	ld hl, fleet_sound_alien_counts
	ld de, fleet_sound_delays
	ld a, (invader_count)
@loop:
	cp (hl)
	jr nc, @store_delay
	inc hl
	inc de
	jr @loop

@store_delay:
	ld a, (de)
	ld (fleet_sound_timer), a
	
@done:
	; Should turn fleet sound off.
	ret
	

; --------------------------------------

invader_bitmaps:
	db BMP_INVADER1_1, BMP_INVADER1_2
	db BMP_INVADER2_1, BMP_INVADER2_2
	db BMP_INVADER3_1, BMP_INVADER3_2

animation_frame:
	db 0
	
inv_shot_sprite_data:
	db 4, BMP_INV_SHOT_F1, BMP_INV_SHOT_F2, BMP_INV_SHOT_F3, BMP_INV_SHOT_F4

inv_shot_sprite_data_p:
	db 4, BMP_INV_SHOT_P1, BMP_INV_SHOT_P2, BMP_INV_SHOT_P3, BMP_INV_SHOT_P4

inv_shot_sprite_data_r:
	db 4, BMP_INV_SHOT_R1, BMP_INV_SHOT_R2, BMP_INV_SHOT_R3, BMP_INV_SHOT_R4
	
invaders:
	db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
	db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
	db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
	db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
	db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0


; Invader shoot calculation
invader_col_fire_table:
	db $0, $6, $0, $0, $0, $3, $A, $0, $5, $2, $0, $0, $0, $A, $8, $1, $7
	db $1, $A, $3, $6, $9

invader_count:
	db 0

inv_state_start:
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

saucer_start:
	db 0
saucer_active:
	db 0
saucer_exploding:
	db 0
saucer_pos:
	db 0

inv_state_end:	
	db 0


; Shot data blocks
shot_timer:
	db 0	; frame timer
shot_data:

	; Shot 1 "Rolling"
sb_start:
shot_1:
	db 0	; status
    db 0, 0	; x, y
inv_shot_spr:
	db 0
shot1_step:
	db 0	; counter
	db 0	; column offset
	db 1	; target player
sb_end:

	; Shot 2 "Plunger"
shot_2:
	db 0	; status
    db 0, 0	; x, y
inv_shot_spr2:
	db 0
shot2_step:
	db 0	; counter
	db 0	; column offset
	db 0	; target player

	; Shot 3 "Squiggly"
shot_3:
	db 0	; status
    db 0, 0	; x, y
inv_shot_spr3:
	db 0
shot3_step:
	db 0	; counter
	db 6	; column offset
	db 0	; target player
	
saucer_delta:
	db 2

other1:
	db 0
other2:
	db 0
	
saucer_spr:
	db 0
saucer_exp_spr:
	db 0

saucer_sprite_data:
	db 1, BMP_SAUCER
saucer_exp_sprite_data:
	db 1, BMP_SAUCER_EXPL
	
SAUCER_Y_POS: equ 8
SAUCER_Y_BASE: equ 16
time_to_saucer:
	dw $200
SAUCER_DELAY: equ $600



fleet_sound_timer:
	db $01
fleet_sound:
	db 0
	db 0

fleet_sound_alien_counts:
	db $32, $2B, $24, $1C, $16, $11, $0D, $0A, $08, $07, $06, $05, $04, $03, $02, $01
fleet_sound_delays:
	db $34, $2E, $27, $22, $1C, $18, $15, $13, $10, $0E, $0D, $0C, $0B, $09, $07, $05

	
shot_speed:
	db 4
shot_rate:
	db $30
	
shot_rates:
	db $30, $10, $0B, $08
	db $07
shot_score_tab:
	db $02, $10, $20, $30