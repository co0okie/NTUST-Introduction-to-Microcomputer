printNewline macro
    push ax
    push dx
    
    mov ah, 02h
    mov dl, 13
    int 21h
    mov dl, 10
    int 21h
    
    pop dx
    pop ax
endm

.model small, c
.data
printUint16 proto, number: word
triangleX dw 100
triangleY dw 100
side dw 40
color db 0110b
.stack 1000h
.code
main proc
    .startup
    
    mov ax, 12h
    int 10h
    
    
    call drawTriangle
    
    getChar:
        mov ah, 06h
        mov dl, 0ffh
        int 21h
        cmp al, 1bh
        je exit
        cmp al, 'c'
        je changeColor
        
        mov ax, 03h
        int 33h
        mov triangleX, cx
        mov triangleY, dx
        test bx, 001b
        jnz leftClick
        test bx, 010b
        jnz rightClick
        jmp draw
        changeColor:
            inc color
            jmp draw
        leftClick:
            cmp side, 20
            jle draw
            sub side, 10
            jmp draw
        rightClick:
            cmp side, 80
            jae draw
            add side, 10
            jmp draw
        draw:
            call drawTriangle
            mov ah, 86h
            mov cx, 1
            mov dx, 0
            int 15h
        
        jmp getChar

    exit:
        mov ax, 03h
        int 10h
        mov ax, 4c00h
        int 21h
main endp

drawTriangle proc
    local beginX: word, endX: word, endY: word
    
    mov ax, 12h
    int 10h
    
    invoke printUint16, triangleX
    printNewline
    invoke printUint16, triangleY
    
    mov ax, 07h
    mov cx, 40
    mov dx, 600
    int 33h
    mov ax, 08h
    mov cx, 0
    mov dx, 440
    int 33h
    mov ah, 0bh
    mov bh, 00h
    mov bl, 0001b
    int 10h
    
    ; tx, ty
    ; tx - 1 ~ tx + 1, ty + 1
    ; tx - 2 ~ tx + 2, ty + 2
    ; ...
    ; tx - side ~ tx + side, ty + side
    
    mov dx, triangleY
    mov ax, dx
    add ax, side
    mov endY, ax ; endY = beginY + side
    mov cx, triangleX
    mov beginX, cx
    mov endX, cx
    mov ah, 0ch
    mov bh, 0
    mov al, color
    l1:
        l2:
            int 10h
            
            inc cx
            cmp cx, endX
            jle l2
        
        dec beginX
        inc endX
        mov cx, beginX
        inc dx
        cmp dx, endY
        jle l1
    
    ret
drawTriangle endp


printUint16 proc uses ax cx dx ds si, number: word
    local outputString[6]: byte
    
    mov si, 5
    mov outputString[si], '$'
    
    mov cx, 10
    mov ax, number
    test ax, ax
    jne divide10
        ; print '0' if number == 0
        mov dl, '0'
        mov ah, 02h
        int 21h
        ret
        
    divide10:
        xor dx, dx
        div cx
        or dl, 30h
        dec si
        mov outputString[si], dl
        
        test ax, ax
        jne divide10
    
    mov ax, ss
    mov ds, ax
    mov ah, 09h
    lea dx, outputString[si]
    int 21h
    
    ret
printUint16 endp

end main