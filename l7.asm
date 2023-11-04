printNewLine macro
    mov ah, 02h
    mov dl, 0dh
    int 21h
    mov dl, 0ah
    int 21h
endm

printString macro string
    mov ah, 09h
    lea dx, string
    int 21h
endm

.model small, c

.data
; function prototype
getNumber proto, pNumber: ptr
getGCD proto, x: word, y: word
printUint16 proto, number: word
printUint32 proto, number: dword

; string
enterFirstNumber db "Enter first number: $"
enterSecondNumber db "Enter second number: $"
gcdString db "GCD: $"
lcmString db "LCM: $"

number1 dw ?
number2 dw ?
gcd dw ?
lcm dd ?

.stack 1000h

.code
main proc
    mov ax, @data
    mov ds, ax

    start:
        ; enter number1
        printString enterFirstNumber
        invoke getNumber, offset number1
        
        ; enter number2
        printString enterSecondNumber
        invoke getNumber, offset number2
        
        invoke getGCD, number1, number2
        mov gcd, ax
        
        ; lcm = a * b / gcd = a / gcd * b
        mov ax, number1
        xor dx, dx
        div gcd
        mul number2
        mov word ptr lcm + 2, dx
        mov word ptr lcm, ax
        
        ; print result
        printString gcdString
        invoke printUint16, gcd
        printNewLine
        printString lcmString
        invoke printUInt32, lcm
        printNewLine
        
        jmp start
        
    exit:
        mov ax, 4c00h
        int 21h
main endp

getNumber proc, pNumber: ptr
    local char: byte
    
    mov si, pNumber
    mov word ptr [si], 0 ; clear number
    
    getChar:
        ; get char
        mov ah, 00h
        int 16h
        xor ah, ah
        mov char, al
        
        cmp al, 1bh ; esc
        je exit
        cmp al, 0dh ; enter(\n)
        je nextNumber
        cmp al, 08h ; backspace
        je backspace
        cmp al, '0'
        je isZero ; == 0
        jl getChar ; < 0
        cmp al, '9'
        ja getChar ; > 9
        jmp readChar ; 0 ~ 9
        exit:
            mov ax, 4c00h
            int 21h
        nextNumber:
            ; prevent number == 0
            cmp word ptr [si], 0
            je getChar
            jmp return
        backspace:
            ; ignore if number == 0
            cmp word ptr [si], 0
            je getChar
            ; print backspace, space, backspace
            mov ah, 02h
            mov dl, 08h
            int 21h
            mov dl, ' '
            int 21h
            mov dl, 08h
            int 21h
            ; *pNumber /= 10
            mov ax, [si]
            xor dx, dx
            mov cx, 10
            div cx
            mov [si], ax
            jmp getChar
        isZero:
            ; prevent leading 0
            cmp word ptr [si], 0
            je getChar
        readChar:
            ; *pNumber *= 10
            mov ax, 10
            mul word ptr [si]
            jc getChar ; skip if overflow
            ; *pNumber += (char - 30h)
            mov cl, char
            xor ch, ch
            sub cx, '0'
            add ax, cx
            jc getChar ; skip if overflow
            mov [si], ax
            ; print char
            mov dl, char
            mov ah, 02h
            int 21h
        
            jmp getChar
        
    return:
        printNewLine
        ret
getNumber endp

getGCD proc, x: word, y: word
        mov ax, x
        mov bx, y
    divide:
        ; x / y = q ... r
        xor dx, dx
        div bx
        ; x := y, y := r
        mov ax, bx
        mov bx, dx
        ; r != 0: loop
        cmp dx, 0
        jne divide
    ret
getGCD endp

printUint16 proc, number: word
    mov cx, 0
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
        mov bx, 10
        div bx
        push dx
        inc cx
        
        test ax, ax
        jne divide10
    
    printDigit:
        pop dx
        add dl, '0'
        mov ah, 02h
        int 21h
        loop printDigit
        
    ret
printUint16 endp

printUInt32 proc, number: dword
    mov cx, 0
    ; check number == 0
    cmp word ptr number, 0
    jne divide10
    cmp word ptr number + 2, 0
    jne divide10
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
        
        ; dx:ax(1234) / bx(10) = ax(123) ... dx(4)
        xor dx, dx
        mov ax, word ptr number + 2
        mov bx, 10
        div bx
        mov word ptr number + 2, ax
        
        ; dx:ax(4:5678) / bx(10) = ax ... dx
        mov ax, word ptr number
        div bx
        mov word ptr number, ax
        push dx
        inc cx
        
        cmp word ptr number, 0
        jne divide10
        cmp word ptr number + 2, 0
        jne divide10
        
    printDigit:
        pop dx
        add dl, 30h
        mov ah, 02h
        int 21h
        loop printDigit
        
    ret
printUInt32 endp
end main