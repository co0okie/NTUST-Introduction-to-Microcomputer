boxWidth equ 160
boxHeight equ 120
initLeft equ 640 / 2 - boxWidth / 2
initTop equ 480 / 2 - boxHeight / 2

.model small
.data
boxLeft dw initLeft
boxRight dw initLeft + boxWidth
boxTop dw initTop
boxBottom dw initTop + boxHeight
vx dw 5
vy dw 5
color db 1111b
.stack 1000h
.code
main proc
    .startup
    
    ; video mode, 640 * 480, 16 colors
    mov ax, 12h
    int 10h
    
    ; vga memory address
    mov ax, 0a000h
    mov es, ax
    
    ; write bitplane
    mov dx, 03c4h
    mov al, 02h
    out dx, al
    
    l1:
        call draw
        
        mov ax, vx
        mov bx, vy
        add boxLeft, ax
        add boxRight, ax
        add boxTop, bx
        add boxBottom, bx
        
        .if boxLeft <= 0 || boxRight >= 640
            inc color
            neg vx
        .endif
        .if boxTop <= 0 || boxBottom >= 480
            inc color
            neg vy
        .endif
        
        ; delay
        mov ah, 86h
        xor cx, cx
        mov dx, 60000
        int 15h
        
        mov ah, 06h
        mov dl, 0ffh
        int 21h
        jz l1
        
    exit:
        mov ax, 03h
        int 10h
        .exit
main endp

draw proc
    local middleWidth: word, topLeftIndex: word
    
    ; clear screen
    mov dx, 03c5h
    mov al, 1111b
    out dx, al
    xor ax, ax
    xor di, di
    mov cx, 640 * 480 / 16
    rep stosw
    
    ; boxLeft = 14 = 1*8 + 6, boxRight = 29 = 3*8 + 5
    ;                             |                             |
    ; |0.1.2.3.4.5.6.7|0.1.2.3.4.5.6.7|0.1.2.3.4.5.6.7|0.1.2.3.4.5.6.7|
    ;                             |                             |
    
    ; select all plane
    mov dx, 03c5h
    mov al, color
    out dx, al
    
    ; top left index = top * (640 / 8) + left >> 3
    ; middle width = right index - left index - 1
    mov ax, 640 / 8
    mul boxTop
    mov di, ax
    mov ax, boxLeft
    mov cl, 3
    shr ax, cl
    mov middleWidth, -1
    sub middleWidth, ax
    add di, ax
    mov topLeftIndex, di
    ; left side data = 11111111b >> (left & 7)
    mov cx, boxLeft
    and cx, 7
    mov al, 11111111b
    shr al, cl
    ; draw left side
    mov cx, boxHeight
    leftSide:
        mov es:[di], al
        add di, 640 / 8
        loop leftSide
        
    ; right index = right >> 3
    mov cl, 3
    mov ax, boxRight
    shr ax, cl
    add middleWidth, ax
    mov di, topLeftIndex
    add di, middleWidth
    inc di
    ; right side data = 11111111b << (8 - right & 7)
    mov cx, boxRight
    and cx, 7
    neg cl
    add cl, 8
    mov al, 11111111b
    shl al, cl
    ; draw right side
    mov cx, boxHeight
    rightSide:
        mov es:[di], al
        add di, 640 / 8
        loop rightSide
    
    mov di, topLeftIndex
    inc di
    mov ax, 0ffh
    mov cx, boxHeight
    middle:
        push cx
        mov cx, middleWidth
        rep stosb
        add di, 640 / 8 ; lf
        sub di, middleWidth ; cr
        pop cx
        loop middle
    
    ret
draw endp
end main