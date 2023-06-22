SPR_PLAYER: equ 60
SPR_BULLET: equ 61

KEY_LEFT: equ 8
KEY_RIGHT: equ 21
KEY_FIRE: equ 32
PLAYER_YPOS: equ 165
BULLET_START_YPOS: equ 160


player_position: 
    db 35
    
bullet_pos_x:
    db 0
bullet_pos_y:
    db BULLET_START_YPOS
bullet_active:
    db 0
    

init_player:
	ld a, SPR_PLAYER
	ld hl, player_sprite_data	
	call def_sprite
	call show_sprite

	ld a, SPR_BULLET
	ld hl, bullet_sprite_data	
	call def_sprite
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
    ld a, SPR_PLAYER
    ld de, PLAYER_YPOS
    call move_sprite
    
@check_fire:
    ld a, (keycode)
    cp KEY_FIRE
    jp nz, @move_bullet
    
    ld a, (bullet_active)
    and a
    jp nz, @move_bullet
    
    ld a, SPR_BULLET
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
    ld a, SPR_BULLET
    call move_sprite
    call get_pixel_color
    
    ld hl, (pixel_data_ptr)
    ld hl, (hl)
    ld a, h
    and a
    jp z, @done
    
@bullet_hit:
@bullet_exited:
    xor a
    ld (bullet_active), a
    ld a, SPR_BULLET
    ;call hide_sprite

@done:
    pop bc
    pop hl
    pop de
    pop af
    ret