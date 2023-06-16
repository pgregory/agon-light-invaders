
KEY_LEFT: equ 8
KEY_RIGHT: equ 21
PLAYER_YPOS: equ 165


player_position: 
    db 35


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
    
@done:
    pop bc
    pop hl
    pop de
    pop af
    ret