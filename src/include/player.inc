init_player:
	ld a, (next_sprite_index)
    ld (player_spr), a
	ld hl, player_sprite_data	
	call def_sprite
	call show_sprite
    add a, 1

    ld (bullet_spr), a
	ld hl, bullet_sprite_data	
	call def_sprite
    add a, 1
    
    ld (player_exp_spr), a
    ld hl, player_exp_sprite_data
    call def_sprite
    add a, 1

    ld (next_sprite_index), a

    ret
    

reset_player:
    push bc
    push de
    push hl
    
    ld hl, player_state
    ex de, hl
    ld hl, player_state
    ld (hl), 0
    ld bc, player_state_end-player_state
    ldir

    ld a, (player_initial_position)
    ld (player_position), a
    
    ld a, 3
    ld (player_lives), a
    
    ld hl, 0
    ld (player_score), hl

    ld a, (player_exp_spr)
    call hide_sprite

    call cancel_player_shot

    pop hl
    pop de
    pop bc
    
    ret
    
cancel_player_shot:
    ld a, (bullet_spr)
    call hide_sprite
    xor a
    ld (bullet_active), a
    ret



process_player:
    push af
    push de
    push hl
    push bc
    
    ld a, (player_exploding)
    and a
    jp nz, @player_exploding_countdown

    ld hl, KEY_RIGHT
    call check_key
    jp nz, @move_right

    ld hl, KEY_LEFT
    call check_key
    jp nz, @move_left
    
    jp @update_sprite
    
@move_right:
    ld a, (player_position)
    add a, 1
    cp 0xc8
    jp nc, @update_sprite

    ld (player_position), a
    jp @update_sprite
    
@move_left:
    ld a, (player_position)
    sub a, 1
    cp 0x8
    jp c, @update_sprite

    ld (player_position), a
@update_sprite:
    ld a, (player_position)
    ld c, a
    ld b, 0
    ld a, (player_spr)
    ld de, PLAYER_YPOS
    call move_sprite
    
    jp @done
    

@player_exploding_countdown:
    ld hl, player_explode_time
    dec (hl)
    jp nz, @done

    ld a, (player_exp_spr)
    call next_frame_sprite
    ld (hl), $05
    ld hl, player_explode_counter
    dec (hl)
    jp nz, @done
    
	ld a, (player_exp_spr)
	call hide_sprite
    ld a, (player_spr)
    call show_sprite
	
	ld a, 0
	ld (player_exploding), a
    ld a, (player_initial_position)
    ld (player_position), a

    ld a, (player_lives)
    dec a
    ld (player_lives), a
    
@check_invaded:
    ld a, (invaded)
    and a
    jp z, @done
    
    xor a
    ld (player_lives), a

@done:
    pop bc
    pop hl
    pop de
    pop af
    ret
    

update_player_shot:
    push af
    push bc
    push hl
    push de
    ld a, (inv_exploding)
    and a
    jp nz, @move_bullet

    ld a, (bullet_active)
    and a
    jp nz, @move_bullet

    ld a, (shoot_bounce)
    and a
    jp nz, @shoot_debounce

    ld hl, KEY_FIRE
    call check_key
    jp z, @done
    
    ld a, 1
    ld (shoot_bounce), a
    
    ld a, -1
    ld b, a
    ld a, CHAN_PLAYER
    ld de, 310
    call play_channel
    
    ld a, (player_position)
    add a, 8
    ld (bullet_pos_x), a
    ld a, BULLET_START_YPOS
    ld (bullet_pos_y), a

    ld a, (bullet_spr)
    ld (bullet_active), a
    call show_sprite
    jp @update_bullet
    
@move_bullet:
    ld a, (bullet_active)
    and a
    jp z, @done
    
    ld a, (bullet_pos_y)
    sbc a, 4
    ld (bullet_pos_y), a
    jp nc, @check_collision

    ; clear pixel color 
    if DEBUG
    push af
    push bc
    push hl
    ld a, 31
    PRT_CHR
    ld a, 6
    PRT_CHR
    ld a, 0
    PRT_CHR
    ld a, '-'
    PRT_CHR
    PRT_CHR
    PRT_CHR
    PRT_CHR
    PRT_CHR
    PRT_CHR
    ld a, ' '
    PRT_CHR
    PRT_CHR
    PRT_CHR
    pop hl
    pop bc
    pop af
    endif

    jp @bullet_exited

@check_collision:
    sub a, 4
    ld h, 4
    ld e, a
    ld d, 0
    ld bc, 0
    ld a, (bullet_pos_x)
    ld c, a
    ld ix, (pixel_data_ptr)
@coll_loop:
    call get_pixel_color
    ld a, (ix + 0)
    or a, (ix + 1)
    or a, (ix + 2)
    and a
    jr nz, @collide
    inc e
    dec h
    jr nz, @coll_loop
    jp @update_bullet
    
@collide:
    ld a, e
    ld (bullet_col_pos), a

    ; print pixel color 
    if DEBUG
    push af
    push bc
    push hl
    ld a, 31
    PRT_CHR
    ld a, 6
    PRT_CHR
    ld a, 0
    PRT_CHR
    ld a, (ix + 0)
	call print_Hex8
    ld a, (ix + 1)
	call print_Hex8
    ld a, (ix + 2)
	call print_Hex8
    ld a, ' '
    PRT_CHR
    ld a, (bullet_col_pos)
    call print_Hex8
    pop hl
    pop bc
    pop af
    endif

@check_inv:
    ; Check if hit an invader
    ld a, (bullet_pos_x)
    ld c, a
    ld b, 0
    ld a, (bullet_col_pos)
    ld e, a
    ld d, 0
    call get_inv_at
    
    ; print invader index hit
    ;push af
	;ld h, a
    ;ld a, (bullet_pos_y)
	;ld l, a
	;call home_cursor
	;call print_Hex16
    ;pop af

    cp 0x55 
    jp nc, @check_shields

    call kill_invader
    
    jp @bullet_exited

@check_shields:
    ; Check if the hit is in the range of the shields
    ld a, (bullet_col_pos)
    cp $80
    jp c, @check_saucer

    ld a, (bullet_pos_x)
    sub 4
    ld c, a
    ld b, 0
    ld a, (bullet_col_pos)
    sub a, 4
    ld e, a
    ld d, 0
    ld a, BMP_PLYR_B_EXPL
    call draw_bitmap

    jp @bullet_exited
    
@check_saucer:
    cp SAUCER_Y_BASE
    jp nc, @bullet_exited

    ld a, 1
    ld (saucer_exploding), a
    
@bullet_exited:
    xor a
    ld (bullet_active), a
    ld a, (bullet_spr)
    call hide_sprite
    ld hl, (player_shots)
    inc hl
    ld (player_shots), hl

    jr @done

@shoot_debounce:
    ld hl, KEY_FIRE
    call check_key
    jp nz, @done
    xor a
    ld (shoot_bounce), a
    
@update_bullet:
    ld a, (bullet_pos_x)
    ld c, a
    ld b, 0
    ld a, (bullet_pos_y)
    ld e, a
    ld d, 0
    ld a, (bullet_spr)
    call move_sprite

@done:
    pop de
    pop hl
    pop bc
    pop af
    ret
; -----------

explode_player:
    push af
    push de
    push bc
    push hl
    ld a, 2
    ld (player_exploding), a
    ld a, $05
    ld (player_explode_time), a
    ld a, $0C
    ld (player_explode_counter), a
    ld a, (player_spr)
    call hide_sprite

    ld a, (player_exp_spr)
    call show_sprite

    ld a, (player_position)
    ld c, a
    ld b, 0
    ld a, (player_exp_spr)
    ld de, PLAYER_YPOS
    call move_sprite
    
    ld a, -6
    ld b, a
    ld a, CHAN_PLAYER
    ld de, 1200
    call play_channel
    
    pop hl
    pop bc
    pop de
    pop af
    
    ret

; DE - score to add
add_to_score:
    push ix
	ld ix, player_score
	ld hl, (ix+0)
	add hl, de
	ld (ix+0), hl
    pop ix
    
    ret

    
KEY_LEFT: equ -26
KEY_RIGHT: equ -122
KEY_FIRE: equ -99
PLAYER_YPOS: equ 165
BULLET_START_YPOS: equ 160


player_sprite_data:
	db 1, BMP_PLAYER
bullet_sprite_data:
	db 1, BMP_BULLET
player_exp_sprite_data:
    db 2, BMP_PLAYER_EXP1, BMP_PLAYER_EXP2

player_spr:
    db 0
bullet_spr:
    db 0
player_exp_spr:
    db 0
player_initial_position:
    db 25

player_state:
player_position: 
    db 35
    
bullet_pos_x:
    db 0
bullet_pos_y:
    db BULLET_START_YPOS
bullet_active:
    db 0
bullet_col_pos:
    db 0
shoot_bounce:
    db 0

player_exploding:
	db 0
player_explode_time:
	db 0
player_explode_counter:
    db 0
player_score:
    dw 0
    db 0    ; Allow for 24 bit addressing
player_lives:
    db 0
player_shots: 
    dw 0
player_state_end:
    db 0
    
    
