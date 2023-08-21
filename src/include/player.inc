
KEY_LEFT: equ 8
KEY_RIGHT: equ 21
KEY_FIRE: equ 32
PLAYER_YPOS: equ 165
BULLET_START_YPOS: equ 160


player_spr:
    db 0

bullet_spr:
    db 0

player_position: 
    db 35
    
bullet_pos_x:
    db 0
bullet_pos_y:
    db BULLET_START_YPOS
bullet_active:
    db 0
    
player_sprite_data:
	db 1, BMP_PLAYER
bullet_sprite_data:
	db 1, BMP_BULLET

init_player:
    if USE_SPRITES
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
    ld (next_sprite_index), a
    endif

    ret



process_player:
    push af
    push de
    push hl
    push bc
    
    ld a, (keycode)

    cp KEY_RIGHT
    jp z, @move_right
    
    cp KEY_LEFT
    jp z, @move_left
    
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
    
@check_fire:
    ld a, (keycode)
    cp KEY_FIRE
    jp nz, @move_bullet
    
    ld a, (bullet_active)
    and a
    jp nz, @move_bullet
    
    ld a, (bullet_spr)
    ld (bullet_active), a
    call show_sprite

    ld a, (player_position)
    add a, 8
    ld (bullet_pos_x), a
    ld a, BULLET_START_YPOS
    ld (bullet_pos_y), a
    
@move_bullet:
    ld a, (bullet_active)
    and a
    jp z, @done
    
    ld a, (bullet_pos_y)
    sbc a, 2
    jp c, @bullet_exited

    ld (bullet_pos_y), a

    ld a, (bullet_pos_x)
    ld c, a
    ld b, 0
    ld a, (bullet_pos_y)
    ld e, a
    ld d, 0
    ld a, (bullet_spr)
    call move_sprite
    
    ld a, (bullet_pos_y)
    sub a, 4
    ld e, a
    ld d, 0
    call get_pixel_color
    
    ld hl, (pixel_data_ptr)
    ld hl, (hl)
    ld a, h
    and a
    jp z, @done
    
    ; print pixel color 
    ;push af
	;call home_cursor
	;call print_Hex16
    ;pop af

@check_inv:
    ; Check if hit an invader
    ld a, (bullet_pos_x)
    ld c, a
    ld b, 0
    ld a, (bullet_pos_y)
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

    ld hl, invaders				; Point to invaders status table
    ld c, a						; Store invader cursor
    ld b, 0						; Clear MSB
    add hl, bc					; Point to this invader status
    ld (hl), 1
    call hide_sprite
    
    call get_inv_coords
    ld a, (inv_explode_spr)
    call move_sprite
    call show_sprite

    ld a, 1
    ld (inv_exploding), a
    ld a, $10
    ld (inv_explode_time), a
    
    jp @bullet_exited

@check_shields:
    ; Check if the hit is in the range of the shields
    ld a, (bullet_pos_y)
    cp $80
    jp c, @bullet_exited

    ld a, (bullet_pos_x)
    sub 4
    ld c, a
    ld b, 0
    ld a, (bullet_pos_y)
    sub 4
    ld e, a
    ld d, 0
    ld a, BMP_PLYR_B_EXPL
    call draw_bitmap

    jp @bullet_exited
    
@bullet_exited:
    xor a
    ld (bullet_active), a
    ld a, (bullet_spr)
    call hide_sprite

@done:
    pop bc
    pop hl
    pop de
    pop af
    ret
    
