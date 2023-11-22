clearScreen macro
    cld
    xor ax, ax
    xor di, di
    mov cx, (320 * 200) / 2
    rep stosw
endm

.model compact, c
ballRadius equ 20
.data
drawCircle proto, centerX: word, centerY: word
circleWidth \
dw 9, 15, 19, 23, 25, 27, 29, 31, 33, 35, 35, 37, 37, 39, 39, 39, 41, 41, 41, 41, 41, 41, 41, 41, 41
dw 39, 39, 39, 37, 37, 35, 35, 33, 31, 29, 27, 25, 23, 19, 15, 9
ballX dw 160
ballY dw 100
ballVx dw 2
ballVy dw 2
backBuffer segment
db 64000 dup(0)
backBuffer ends
.stack 1000h
.code
main proc
    .startup
    
    ; video mode, 320 * 200, 256 colors
    mov ax, 13h
    int 10h
    
    ; back buffer
    mov ax, backBuffer
    mov es, ax
    
    refresh:
        
        xor ax, ax
        xor di, di
        mov cx, (320 * 200) / 2
        rep stosw
        
        mov ax, 0c0ch
        mov di, 320 * 5
        mov cx, 320 * 10 / 2
        rep stosw
        
        invoke drawCircle, ballX, ballY
        
        
        ; backbuffer -> video memory
        push ds
        push es
        mov ax, backBuffer
        mov ds, ax
        mov ax, 0a000h
        mov es, ax
        xor si, si
        xor di, di
        mov cx, (320 * 200) / 2
        rep movsw
        pop es
        pop ds
        
        
        ; iterating
        mov ax, ballVx
        ; add ballVy, 1
        mov bx, ballVy
        add ballX, ax
        add ballY, bx
        
        .if ballX <= ballRadius || ballX >= 320 - ballRadius
            neg ballVx
        .endif
        .if ballY <= ballRadius || ballY >= 200 - ballRadius
            neg ballVy
        .endif
    
        ; delay
        ; mov ah, 86h
        ; xor cx, cx
        ; mov dx, 5000
        ; int 15h
        
        ;  mov dx, 3dah
        ; _waitForEnd:
        ; in al, dx
        ; test al, 08h
        ; jnz _waitForEnd
        ; _waitForNew:
        ; in al, dx
        ; test al, 08h
        ; jz _waitForNew
        
        ; read key
        mov ah, 06h
        mov dl, 0ffh
        int 21h
        jz refresh
    
    exit:
        mov ax, 03h
        int 10h
        .exit
main endp

drawCircle proc, centerX: word, centerY: word
    local siEnd: word

    ; di = (centerY - ballRadius) * 320 + (centerX - circleWidth[0] >> 1)
    mov ax, centerY
    sub ax, ballRadius
    mov bx, 320
    mul bx
    add ax, centerX
    ; lea si, circleWidth
    ; mov bx, [si]
    mov bx, circleWidth[0]
    shr bx, 1
    sub ax, bx
    mov di, ax
    
    ; si end = &circleWidth[0] + sizeof circleWidth
    lea si, circleWidth
    mov siEnd, si
    add siEnd, (ballRadius * 2 + 1) * 2 ; 2bytes * (2r + 1)
    
    mov al, 00fh ; color white
    .while si != siEnd
        mov cx, [si]
        rep stosb
        
        ; di += 320 - circleWidth[i] / 2 - circleWidth[i + 1] / 2
        ;     = (640 - circleWidth[i] - circleWidth[i + 1]) >> 1
        mov cx, 640
        sub cx, [si]
        add si, 2
        sub cx, [si]
        shr cx, 1
        add di, cx
    .endw
    
    ret
drawCircle endp
end