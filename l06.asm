.model small
.data
array dw 0001h, 0012h, 0123h, 1234h, 2345h
.stack 1000H
.code
main:
    mov ax, @data
    mov ds, ax
    
    mov ax, lengthof array ; bp + 6
    push ax
    lea ax, array ; bp + 4
    push ax
    call sum

    mov ax, 4c00h
    int 21h

sum proc
    push bp
    mov bp, sp
    
    mov ax, 0
    mov bx, [bp + 4]
    mov cx, [bp + 6]
    l1:
        add ax, [bx]
        
        add bx, 2
    loop l1
    
    mov sp, bp
    pop bp
    ret 4
sum endp

end main