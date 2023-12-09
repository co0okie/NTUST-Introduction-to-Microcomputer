.model small
.data
array byte 12h, 11h, 99h, 28h, 70h, 16h
.stack 100H
.code
main proc
    mov ax, @data
    mov ds, ax
    
    ; i = length - 1 ~ 1
    mov cx, lengthof array
    dec cx
    L1:
        push cx
        ; j = i ~ 1
        ; index = length - 1 ~ length - i
        mov bx, lengthof array
        L2:
            dec bx
            mov al, array[bx]
            cmp al, array[bx - 1]
            jae smaller
            ; exchange if array[bx] < array[bx - 1]
                xchg al, array[bx - 1]
                xchg al, array[bx]
            smaller:
                push cx
                push bx
                mov cx, lengthof array
            print:
                push cx
                ; bx = length - cx
                mov ax, lengthof array
                sub ax, cx
                mov bx, ax
                ; print left
                mov dl, array[bx]
                mov cl, 4
                shr dl, cl
                add dl, 48
                mov ah, 02h
                int 21h
                ; print right
                mov dl, array[bx]
                shl dl, cl
                shr dl, cl
                add dl, 48
                mov ah, 02h
                int 21h
                ; print space
                mov dl, 20h
                int 21h
                
                pop cx
                loop print
            
            ; print newline
            mov dl, 13
            int 21h
            mov dl, 10
            int 21h
            
            pop bx
            pop cx
            loop L2

        pop cx
        loop L1
    
    mov ax, 4c00h
    int 21h
main endp
end main