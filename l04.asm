.model small
.data
string db 'A*B', 13, 10, '$'
A dw 340dh
B dw 5h
D dw 0ff00h
answer dw 2 dup(?)
result db 8 dup(?), 13, 10, '$'
answer2 dw ?
result2 db 4 dup(?), '$'
.stack
.code
main proc
    mov ax, @data
    mov ds, ax
    
    ; print A*B
    mov dx, offset string
    mov ah, 09h
    int 21h
    
    mov ax, A
    mul B
    mov answer[0], ax
    mov answer[2], dx
    
    ; 1
    mov ax, answer[2]
    mov cl, 4
    shr ah, cl
    add ah, 48
    mov result[0], ah
    
    ; 2
    mov ax, answer[2]
    mov cl, 4
    shl ah, cl
    shr ah, cl
    add ah, 48
    mov result[1], ah
    
    ; 3
    mov ax, answer[2]
    mov cl, 4
    shr al, cl
    add al, 48
    mov result[2], al
    
    ; 4
    mov ax, answer[2]
    mov cl, 4
    shl al, cl
    shr al, cl
    add al, 48
    mov result[3], al
    
    ; 5
    mov ax, answer[0]
    mov cl, 4
    shr ah, cl
    add ah, 48
    mov result[4], ah
    
    ; 6
    mov ax, answer[0]
    mov cl, 4
    shl ah, cl
    shr ah, cl
    add ah, 48
    mov result[5], ah
    
    ; 7
    mov ax, answer[0]
    mov cl, 4
    shr al, cl
    add al, 48
    mov result[6], al
    
    ; 8
    mov ax, answer[0]
    mov cl, 4
    shl al, cl
    shr al, cl
    add al, 48
    mov result[7], al
    
    mov dx, offset result
    mov ah, 09h
    int 21h
    
    ; A * B / D
    mov ax, A
    imul B
    idiv D
    mov answer2, dx
    
    ; 1
    mov ax, answer2
    mov cl, 4
    shr ah, cl
    add ah, 48
    mov result2[0], ah
    
    ; 2
    mov ax, answer2
    mov cl, 4
    shl ah, cl
    shr ah, cl
    add ah, 48
    mov result2[1], ah
    
    ; 3
    mov ax, answer2
    mov cl, 4
    shr al, cl
    add al, 48
    mov result2[2], al
    
    ; 4
    mov ax, answer2
    mov cl, 4
    shl al, cl
    shr al, cl
    add al, 48
    mov result2[3], al
    
    mov dx, offset result2
    mov ah, 09h
    int 21h
    
    mov ax, 4c00h
    int 21h
main endp
end main