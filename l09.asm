include ./Masm615/INCLUDE/Irvine16.inc

.model small
.data
charAttr db 0fh
.stack 1000h
.code
main proc
    mov ax, @data
    mov ds, ax

    userInput:
        mov ah, 10h
        int 16h
        cmp al, 1bh
        je endProgram
        cmp ax, 3b00h
        je f1
        cmp ax, 3c00h
        je f2
        cmp ax, 3d00h
        je f3
        cmp ax, 3e00h
        je f4
        cmp al, 0e0h
        jne print
        jmp userInput
        f1:
            mov ah, 03h
            mov bx, 0
            int 10h
            dec ch
            and ch, 00000111b
            mov ah, 01h
            mov cl, 07h
            int 10h
            jmp userInput
        f2:
            mov bl, charAttr
            add bl, 00010000b
            mov charAttr, bl
            jmp userInput
        f3:
            mov bl, charAttr
            and bl, 00001111b
            inc bl
            and charAttr, 11110000b
            xor charAttr, bl
            jmp userInput
        f4:
            mov ax, 26
            call RandomRange
            add al, 'A'
        print:
            mov ah, 09h
            mov bh, 0
            mov bl, charAttr
            mov cx, 2
            int 10h
            
            mov ah, 03h
            mov bh, 0
            int 10h
            mov ah, 02h
            mov bh, 0
            inc dh
            add dl, 2
            int 10h
            jmp userInput
        endProgram:
            mov ax, 4c00h
            int 21h
main endp
end main