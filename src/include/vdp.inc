;; All sprites 16x16. Prepares VDP routines and includes binary

	macro DEFBMP arg, w, h, file
	db 23, 27, 0
	db arg
	db 23, 27, 1
	dw w, h
	incbin file
	endmacro

BMP_INVADER1_1:	equ $00
BMP_INVADER1_2:	equ $01
BMP_INVADER2_1:	equ $02
BMP_INVADER2_2:	equ $03

BMP_INVADER3_1:	equ $04
BMP_INVADER3_2:	equ $05

BMP_SAUCER:		equ $06

BMP_SHIELD:		equ $07
BMP_LINE:		equ $08
BMP_PLAYER:		equ $09
BMP_BULLET:		equ $0A

BMP_BLANK:		equ $0B

BMP_INV_EXPLOSION: equ $0C

BMP_PLYR_B_EXPL: equ $0D

BMP_INV_SHOT_F1: equ $0E
BMP_INV_SHOT_F2: equ $0F
BMP_INV_SHOT_F3: equ $10
BMP_INV_SHOT_F4: equ $11


keycode_ptr:	dl	0
frame_ptr:	dl	0
vk_code:	dl	0
key_up:		dl	0

keycode:	db	0
vk:		db	0

c_up:	db	0
c_dn:	db	0
c_lf:	db	0
c_rt:	db	0

VK_U: equ 150
VK_D: equ 152
VK_L: equ 154
VK_R: equ 156

pixel_data_ptr:
	dl 0
screen_width_ptr:
	dl 0
screen_height_ptr:
	dl 0
	
next_sprite_index:
	db 0

vk_down:
	ld hl, (vk_code)
	ld a, (hl)

	ld hl, c_up
	cp VK_U
	jr z, @write

	inc hl
	cp VK_D
	jr z, @write
	
	inc hl
	cp VK_L
	jr z, @write

	inc hl
	cp VK_R
	jr z, @write
	ret
@write:
	ld a, 1
	ld (hl), a
	ret

vk_up:
	ld hl, (vk_code)
	ld a, (hl)

	ld hl, c_up
	cp VK_U
	jr z, @write

	inc hl
	cp VK_D
	jr z, @write
	
	inc hl
	cp VK_L
	jr z, @write

	inc hl
	cp VK_R
	jr z, @write
	ret
@write:
	xor a
	ld (hl), a
	ret


get_key:
	xor a
	ld (keycode), a

	ld hl, (key_up)
	ld a, (hl)
	and a
	jr z, vk_up	

	call vk_down

	ld hl, (keycode_ptr)
	ld a, (hl)
	ld (keycode), a
	ret

vsync:
	ld hl, (frame_ptr)
	ld a, (hl)
@wait:	push af
	push hl
	call get_key
	pop hl
	pop af
	cp (hl)
	jr z, @wait
	ret

;; Inits screen
vdp_init:
	MOSCALL mos_sysvars
	lea hl, ix+5
	ld (keycode_ptr), hl
	lea hl, ix+$17
	ld  (vk_code), hl
	lea hl, ix+$18
	ld (key_up), hl
	lea hl, ix+$0a
	ld (pixel_data_ptr), hl
	lea hl, ix+$0f
	ld (screen_width_ptr), hl
	lea hl, ix+$11
	ld (screen_height_ptr), hl

	ld (frame_ptr), ix

	ld hl, @cmd
	ld bc, @end-@cmd
	rst.lil $18

	ret
@cmd:
	db 22, 2 ;; Mode 1
	db 23, 1, 0 ;; Hide cursor
	db "Video mode established", 13, 10
	db "Setting keyboard mode", 13,10
	db 23, 0, $88
	dw 250, 33
	db 255	
	db 23, 0, $86    ; Get screen dimensions
	db 23, 27, 16    ; Reset and clear sprites
	db 23, 0, $C0, 0 ; Turn off logical screen coordinates.
	db "Sending bitmaps to VDP", 13, 10

	DEFBMP BMP_INVADER1_1, 16, 16, "imgs/invader1_1.rgba"
	DEFBMP BMP_INVADER1_2, 16, 16, "imgs/invader1_2.rgba"
	DEFBMP BMP_INVADER2_1, 16, 16, "imgs/invader2_1.rgba"
	DEFBMP BMP_INVADER2_2, 16, 16, "imgs/invader2_2.rgba"
	DEFBMP BMP_INVADER3_1, 16, 16, "imgs/invader3_1.rgba"
	DEFBMP BMP_INVADER3_2, 16, 16, "imgs/invader3_2.rgba"
	DEFBMP BMP_SAUCER, 16, 16, "imgs/saucer.rgba"
	DEFBMP BMP_SHIELD, 22, 16, "imgs/shield.rgba"
	DEFBMP BMP_LINE, 16, 1, "imgs/line.rgba"
	DEFBMP BMP_PLAYER, 16, 8, "imgs/player.rgba"
	DEFBMP BMP_BULLET, 1, 4, "imgs/bullet.rgba"
	DEFBMP BMP_BLANK, 16, 16, "imgs/blank_invader.rgba"
	DEFBMP BMP_INV_EXPLOSION, 16, 16, "imgs/alien_explosion.rgba"
	DEFBMP BMP_PLYR_B_EXPL, 8, 8, "imgs/plyr_b_expl.rgba"
	DEFBMP BMP_INV_SHOT_F1, 3, 8, "imgs/invader_shot_f1.rgba"
	DEFBMP BMP_INV_SHOT_F2, 3, 8, "imgs/invader_shot_f2.rgba"
	DEFBMP BMP_INV_SHOT_F3, 3, 8, "imgs/invader_shot_f3.rgba"
	DEFBMP BMP_INV_SHOT_F4, 3, 8, "imgs/invader_shot_f4.rgba"
	db 12
@end:	

;; Clear screen
vdp_cls:
	ld a, 12
	jp putc

; A - number
bmp_select:
	ld (@bmp), a
	ld hl, @cmd
	ld bc, @end-@cmd
	rst.lil $18
	ret
@cmd:
	db 23, 27, 0
@bmp:
	db 0
@end:


cursor_on:
	ld hl, @cmd
	ld bc, @end-@cmd
	rst.lil $18
	ret
@cmd:
	db 23, 1, 1
@end:

cursor_off:	
	ld hl, @cmd
	ld bc, @end-@cmd
	rst.lil $18
	ret
@cmd:
	db 23, 1, 0
@end:

;; DE - xy
goto_xy:
	push de
	ld a, 31
	call putc
	pop de
	push de
	ld a, e
	call putc
	pop de
	ld a, d
	call putc
	ret

; A - color
set_fg:
	and 63
set_color:	
	push af
	ld a, 17
	call putc
	pop af
	call putc
	ret

; A - color
set_bg:
	or $80
	jr set_color

vdp_close:
	ld hl, @cmd
	ld bc, @end-@cmd
	rst.lil $18
	ret
@cmd:
	db 22, 1
	db 17, 7, 17, $80
	db 23, 1, 1
	db 12
@end:

; A - sprite number
; HL - sprite data
def_sprite:
	push af
	push bc
	push de
	push hl

	ld (@spr), a
	ld hl, @cmd
	ld bc, @end-@cmd
	rst.lil $18
	pop hl
	push hl
	ld a, (hl)
	ld b, a
	ld de, 1
	add hl, de
@frame_loop:
	ld a, (hl)
	ld (@add_bmp), a
	push bc
	push hl
	ld hl, @add_cmd
	ld bc, @add_end-@add_cmd
	rst.lil $18
	pop hl
	pop bc
	ld de, 1
	add hl, de
	djnz @frame_loop

	pop hl
	pop de
	pop bc
	pop af
	ret
@cmd:
	db 23, 27, 4 ; Select sprite
@spr:
	db 0

	db 23, 27, 5 ; Clear sprite
@end:
@add_cmd:
	db 23, 27, 6 ; Add bitmap n to sprite
@add_bmp:
	db 0
@add_end:


; A - sprite
show_sprite:
	push af
	push hl
	push bc
	ld (@spr), a
	ld hl, @cmd
	ld bc, @end-@cmd
	rst.lil $18
	pop bc
	pop hl
	pop af
	ret
@cmd:
	db 23, 27, 4 ; Select sprite
@spr:
	db 0

	db 23, 27, 11 ; show sprite
@end:


; A - sprite
hide_sprite:
	push af
	push hl
	push bc
	ld (@spr), a
	ld hl, @cmd
	ld bc, @end-@cmd
	rst.lil $18
	pop bc
	pop hl
	pop af
	ret
@cmd:
	db 23, 27, 4 ; Select sprite
@spr:
	db 0

	db 23, 27, 12 ; hide sprite
@end:


; A - sprite
; B - bmp
add_to_sprite:
	push af
	push hl
	push bc
	ld (@spr), a
	ld a, b
	ld (@bmp), a
	ld hl, @cmd
	ld bc, @end-@cmd
	rst.lil $18
	pop bc
	pop hl
	pop af
	ret
@cmd:
	db 23, 27, 4 ; Select sprite
@spr:
	db 0

	db 23, 27, 6 ; Add bitmap n to sprite
@bmp:
	db 0
@end:



; A - sprite
; BC - X
; DE - Y
move_sprite:
	push af
	push hl
	push bc
	push de
	ld (@spr), a
	ld a, c
	ld (@c_x), a
	ld a, b
	ld (@c_x + 1), a
	ld a, e
	ld (@c_y), a
	ld a, d
	ld (@c_y + 1), a
	ld hl, @cmd
	ld bc, @end-@cmd
	rst.lil $18
	pop de
	pop bc
	pop hl
	pop af
	ret
@cmd:
	db 23, 27, 4 ; Select sprite
@spr:
	db 0
	db 23, 27, 13 ; Move sprite
@c_x:
	dw 160
@c_y:
	dw 100
@end:


next_frame_sprite:
	push af
	push hl
	push bc
	ld (@spr), a
	ld hl, @cmd
	ld bc, @end-@cmd
	rst.lil $18
	pop bc
	pop hl
	pop af
	ret
@cmd:
	db 23, 27, 4
@spr:
	db 0
	db 23, 27, 8
@end:


activate_sprites:
	push af
	push hl
	push bc
	ld (@count), a
	ld hl, @cmd
	ld bc, @end-@cmd
	rst.lil $18
	pop bc
	pop hl
	pop af
	ret
@cmd:
	db 23, 27, 7
@count:
	db 0
@end:


update_sprites:
	push hl
	push bc
	ld hl, @cmd
	ld bc, @end-@cmd
	rst.lil $18
	pop bc
	pop hl
	ret
@cmd:
	db 23, 27, 15
@end:


; A - bitmap id
; BC - X
; DE - Y
draw_bitmap:
	push af
	push hl
	push bc
	push de
	ld (@bitmap), a
	ld (@x), bc
	ld (@y), de
	ld hl, @cmd
	ld bc, @end-@cmd
	rst.lil $18
	pop de
	pop bc
	pop hl
	pop af
	ret
@cmd:
	db 23, 27, 0
@bitmap:
	db 0
	db 23, 27, 3
@x:
	dw 0 
@y:
	dw 0
@end:


; A - color
; IX - line data
draw_line:
	push af
	push hl
	push bc
	push de
	ld (@color), a
	; ld bc, (ix)
	; ld (@x1), bc
	; ld bc, (ix + 2)
	; ld (@y1), bc
	; ld bc, (ix + 4)
	; ld (@x2), bc
	; ld bc, (ix + 6)
	; ld (@y2), bc
	ld hl, @cmd
	ld bc, @end-@cmd
	rst.lil $18
	pop de
	pop bc
	pop hl
	pop af
	ret
@cmd:
	db 18, 0
@color:
	db 0
	db 25, 4
@x1:
	dw 8 
@y1:
	dw 180
	db 25, 5
@x2:
	dw 168 
@y2:
	dw 180 
@end:



; BC - X
; DE - Y
get_pixel_color:
	push hl
	push bc
	push de
	ld (@x), bc
	ld (@y), de
	ld hl, @cmd
	ld bc, @end-@cmd
	rst.lil $18
	pop de
	pop bc
	pop hl
	ret
@cmd:
	db 23, 0, $84
@x:
	dw 0 
@y:
	dw 0
@end:
