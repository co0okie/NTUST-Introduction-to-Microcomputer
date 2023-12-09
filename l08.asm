.model small
.data
.stack 1000h
.code
main proc
    mov ax, @data
    mov ds, ax

readChar:
        mov ah, 10h
        int 16h
        
        cmp al, 1bh ; esc
        je exit
        
        cmp al, 0e0h
        jne normalChar
        
        cmp ah, 48h ; up
        je up
        cmp ah, 50h ; down
        je down
        cmp ah, 4bh ; left
        je left
        cmp ah, 4dh ; right
        je right
        jmp normalChar
    up:
        mov dl, "u"
        jmp print
    down:
        mov dl, "d"
        jmp print
    left:
        mov dl, "l"
        jmp print
    right:
        mov dl, "r"
        jmp print
        
    normalChar:
        mov dl, al
        
    print:
        mov ah, 02h
        int 21h
        jmp readChar

exit:
    mov ax, 4c00h
    int 21h
main endp
end main