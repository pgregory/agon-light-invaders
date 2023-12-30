; Print character in A
    macro  PRT_CHR
    rst.lil $10
    endmacro

; Print LF+CR
    macro  PRT_CRLF
    ld      a, '\n'
    PRT_CHR
    ld      a, '\r'
    PRT_CHR
    endmacro


absA:
    or a
    ret p
    neg
    ret

; Print a 16-bit HEX number
; HL: Number to print
;
print_Hex16:    
    ld      a, h
    call    print_Hex8
    ld      a, l

; Print an 8-bit HEX number
; A: Number to print
;
print_Hex8:    
    ld      c,a
    rra
    rra
    rra
    rra
    call    @prtnbl
    ld      a,c
@prtnbl:         
    and 0fH
    add a, 0x90
    daa
    adc a, 0x40
    daa
    PRT_CHR
    ret

home_cursor:
    ld a, 30
    PRT_CHR
    
    ret
    
; B - row
; C - col
set_cursor:
    ld a, c
    ld (@col), a
    ld a, b
    ld (@row), a
    ld hl, @cmd
    ld bc, @cmd_end-@cmd
    rst.lil $18
    ret

@cmd:
	db 31
@col:
    db 30
@row:
    db 1
@cmd_end:


; HL - Number to print
print_Dec16:    
    ld bc, -10000
    call @num1
    ld bc, -1000
    call @num1
    ld bc, -100
    call @num1
    ld c, -10
    call @num1
    ld c, -1
@num1:
    ld a, '0'-1
@num2:
    inc a
    add hl, bc
    jr c, @num2
    sbc hl, bc
    PRT_CHR
    ret
    

; HL - Number to print
print_Dec8:    
    ld bc, -10
    call @num1
    ld c, -1
@num1:
    ld a, '0'-1
@num2:
    inc a
    add hl, bc
    jr c, @num2
    sbc hl, bc
    PRT_CHR
    ret
    