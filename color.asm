.model small
.data
color db 0
blockRowDi dw 0
blockColumnDi dw 0
.stack 1000h
.code
main proc
    .startup
    
    ; 320 * 200, 256 colors
    mov ax, 13h
    int 10h
    mov ax, 0a000h
    mov es, ax
    
    ; (10 * 16)^2
    mov ax, 0 ; color
    mov cx, 16
    blockRow:
        push cx
        mov di, blockRowDi
        mov blockColumnDi, di
        
        mov cx, 16
        blockColumn:
            push cx
            mov di, blockColumnDi
            
            mov cx, 10
            pixelRow:
                push cx
                
                mov cx, 5
                rep stosw
                
                add di, 320 - 10
                pop cx
                loop pixelRow
            
            inc ah
            inc al
            add blockColumnDi, 10
            pop cx
            loop blockColumn
        
        add blockRowDi, 320 * 10
        pop cx
        loop blockRow
    
    mov ah, 00h
    int 16h

    exit:
        mov ax, 03h
        int 10h
        mov ax, 4c00h
        int 21h
main endp
end