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

waitKey macro
    mov ah, 10h
    int 16h
endm

clearScreen macro
    cld
    mov ax, 0a000h
    mov es, ax
    mov ax, 0
    xor di, di
    mov cx, (640 * 480) / 16
    rep stosw
endm

.model small, c
.586
.stack 1000h
.data
.code
printUint8 proc uses ax dx ds si, number: byte
    local outputString[4]: byte
    
    mov si, 3
    mov outputString[si], '$'
    
    mov dl, 10
    mov al, number
    test al, al
    jne divide10
        ; print '0' if number == 0
        mov dl, '0'
        mov ah, 02h
        int 21h
        ret
        
    divide10:
        xor ah, ah
        div dl
        or ah, 30h
        dec si
        mov outputString[si], ah
        
        test al, al
        jne divide10
    
    mov ax, ss
    mov ds, ax
    mov ah, 09h
    lea dx, outputString[si]
    int 21h
    
    ret
printUint8 endp
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
printUInt32 proc uses ax cx dx ds si, number: dword
    local outputString[11]: byte
    
    mov si, 10
    mov outputString[si], '$'
    
    mov cx, 10
    cmp word ptr number, 0
    jne divide10
    cmp word ptr number + 2, 0
    jne divide10
        ; print '0' if number == 0
        mov dl, '0'
        mov ah, 02h
        int 21h
        ret
        
    divide10:
        ; dx:ax(1234:5678) / 10
        ; (1234(2^16) + 5678) / 10
        ; 1234(2^16) / 10 + 5678 / 10
        ; (123 + 4 / 10)(2^16) + 5678 / 10
        ; 123(2^16) + (4(2^16) + 5678) / 10
        ; 123:(4:5678 / 10)
        
        ; dx:ax(1234) / cx(10) = ax(123) ... dx(4)
        xor dx, dx
        mov ax, word ptr number + 2
        div cx
        mov word ptr number + 2, ax
        
        ; dx:ax(4:5678) / cx(10) = ax ... dx
        mov ax, word ptr number
        div cx
        mov word ptr number, ax
        
        or dl, 30h
        dec si
        mov outputString[si], dl
        
        cmp word ptr number, 0
        jne divide10
        cmp word ptr number + 2, 0
        jne divide10
        
    mov ax, ss
    mov ds, ax
    mov ah, 09h
    lea dx, outputString[si]
    int 21h
        
    ret
printUInt32 endp
printInt8 proc uses ax dx ds si, number: sbyte
    local outputString[5]: byte
    
    mov si, 4
    mov outputString[si], '$'
    
    mov dl, 10
    mov al, number
    test al, al
    jns notNegative
        neg al
        jmp divide10
    notNegative:
    jne divide10
        ; print '0' if number == 0
        mov dl, '0'
        mov ah, 02h
        int 21h
        ret
        
    divide10:
        xor ah, ah
        div dl
        or ah, 30h
        dec si
        mov outputString[si], ah
        
        test al, al
        jne divide10
    
    mov al, number
    test al, al
    jns notNegative2
        dec si
        mov outputString[si], '-'
    notNegative2:
    mov ax, ss
    mov ds, ax
    mov ah, 09h
    lea dx, outputString[si]
    int 21h
    
    ret
printInt8 endp
printInt16 proc uses ax cx dx ds si, number: sword
    local outputString[7]: byte
    
    mov si, 6
    mov outputString[si], '$'
    
    mov cx, 10
    mov ax, number
    test ax, ax
    jns notNegative
        neg ax
        jmp divide10
    notNegative:
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
    
    mov ax, number
    test ax, ax
    jns notNegative2
        dec si
        mov outputString[si], '-'
    notNegative2:
    mov ax, ss
    mov ds, ax
    mov ah, 09h
    lea dx, outputString[si]
    int 21h
    
    ret
printInt16 endp
printInt32 proc uses eax cx dx ds si, number: sdword
    local outputString[12]: byte
    
    mov si, 11
    mov outputString[si], '$'
    
    mov ecx, 10
    mov eax, number
    test eax, eax
    jns notNegative
        neg eax
        jmp divide10
    notNegative:
    jne divide10
        ; print '0' if number == 0
        mov dl, '0'
        mov ah, 02h
        int 21h
        ret
        
    divide10:
        xor edx, edx
        div ecx
        or dl, 30h
        dec si
        mov outputString[si], dl
        
        test eax, eax
        jne divide10
        
    mov eax, number
    test eax, eax
    jns notNegative2
        dec si
        mov outputString[si], '-'
    notNegative2:
    mov ax, ss
    mov ds, ax
    mov ah, 09h
    lea dx, outputString[si]
    int 21h
    
    ret
printInt32 endp
