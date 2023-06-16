add_signed_a_hl:
    or a
    jp p, @positive
    dec h
@positive:
add_unsigned_a_hl:
    add a, l
    ld l, a
    adc a, h
    sub l
    ld h, a
    ret
    
cmpgte:
    ld a, h
    xor d
    jp m, @cmpgte2
    sbc hl, de
    jr nc, @cmpgte3
@cmpgte1:
    scf
    ret
@cmpgte2:
    bit 7, d
    jr z, @cmpgte1
@cmpgte3:
    or a
    ret