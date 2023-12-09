printNewline macro
    mov ah, 02h
    mov dl, 13
    int 21h
    mov dl, 10
    int 21h
endm

waitKey macro
    mov ah, 10h
    int 16h
endm

.model small, c
.data
printUint16 proto, number: word
printUint32 proto, number: dword
triangleX dw 640 / 2 - 40 / 2
triangleY dw 480 / 2 - 40 / 2
color db 0110b
.stack 1000h
.code
main proc
    mov ax, @data
    mov ds, ax
        
    mov ax, 12h
    int 10h
    
    getChar:
        call draw
        
        waitKey
        cmp al, 1bh ; esc
        je exit
        cmp ah, 72 ; up
        je up
        cmp al, '8' ; up
        je up
        cmp ah, 80 ; down
        je down
        cmp al, '2' ; down
        je down
        cmp ah, 75 ; left
        je left
        cmp al, '4' ; left
        je left
        cmp ah, 77 ; right
        je right
        cmp al, '6' ; right
        je right
        cmp ah, 76 ; 5
        je changeColor
        cmp al, '5'
        je changeColor
        jmp getChar
        up:
            sub triangleY, 4
            jmp getChar
        down:
            add triangleY, 4
            jmp getChar
        left:
            sub triangleX, 4
            jmp getChar
        right:
            add triangleX, 4
            jmp getChar
        changeColor:
            inc color
            jmp getChar

    exit:
        mov ax, 03h
        int 10h
        
        mov ax, 4c00h
        int 21h
main endp

draw proc
    local endX: word, endY: word
    
    mov ax, 12h
    int 10h
    
    mov ah, 0bh
    mov bh, 00h
    mov bl, 1011b
    int 10h
    
    ; x = tx, y = ty
    ; x = tx+1, y = ty ty+1
    ; ...
    ; x = tx+38, y = ty ... ty+38
    ; x = tx+39, y = ty ... ty+39
    
    mov dx, triangleY
    mov endY, dx
    add endY, 40
    mov ax, triangleX
    mov endX, ax
    mov ah, 0ch
    mov bh, 0
    mov al, color
    l1:
        inc endX
        mov cx, triangleX
        l2:
            int 10h
            
            inc cx
            cmp cx, endX
            jne l2
        
        inc dx
        cmp dx, endY
        jne l1
        
    ret
draw endp

end main