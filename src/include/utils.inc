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