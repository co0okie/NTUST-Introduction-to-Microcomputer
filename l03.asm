;file name :ch2_ex1.asm
.model small
.data
result db 4 dup(?), '$'
value dw 0ffffh, 0003h, 7fffh, 8123h
.stack
.code
main proc
    mov ax,@data
    mov ds,ax
    mov ax,0
    add ax,value[0]
    add ax,value[0]
    add ax,value[2]
    add ax,value[4]
    add ax,value[6]
    
    mov bx, ax
    
    mov dl, bh
    mov cl, 4
    shr dl, cl
    add dl, 48
    mov result[0], dl

    mov dl, bh
    mov cl, 4
    shl dl, cl
    shr dl, cl
    add dl, 48
    mov result[1], dl

    mov dl, bl
    mov cl, 4
    shr dl, cl
    add dl, 48
    mov result[2], dl
    
    mov dl, bl
    mov cl, 4
    shl dl, cl
    shr dl, cl
    add dl, 48
    mov result[3], dl
    
    mov dx, offset result
    mov ah, 09h
    int 21h

    mov ax,4c00h
    int 21h
main endp
end main