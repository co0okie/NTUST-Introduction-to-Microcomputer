; get second per cpu cycle first
; and use cpu cycle as unit

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

.model small, c
.586


.data
drawCircle proto, centerX: word, centerY: word
printUInt8 proto, number: byte
printUInt16 proto, number: word
printUInt32 proto, number: dword

msPer1MClock dd 0
lastTimerCount dd ?
ballRadius equ 20
circleWidth \
dw 9, 15, 19, 23, 25, 27, 29, 31, 33, 35, 35, 37, 37, 39, 39, 39, 41, 41, 41, 41, 41, 41, 41, 41, 41
dw 39, 39, 39, 37, 37, 35, 35, 33, 31, 29, 27, 25, 23, 19, 15, 9


ballIntX dw 160
ballIntY dw 100
ballX dd 160.0
ballY dd 100.0
ballVx dw 3
ballVy dw 3
loopCounter dd 0

backBuffer segment
db 320 * 200 dup(0)
backBuffer ends

.stack 1000h
.code
main proc
    .startup
    
    ; video mode, 320 * 200, 256 colors
    mov ax, 13h
    int 10h
    
    ; get cpu cycle
    ; clock: rdtsc (cpu clock)
    ; 1 tick = 1 / 1.193MHz / 2 = 419 ns/tick = 419 / 10^6 ms/tick
    ; msPer1MClock = tickPer1MClock    * ms/tick
    ;              = tickPer32768Clock * 32 * 419 / 10^6
    ; clock * msPer1MClock / 1M = ms
    rdtsc
    mov ecx, eax
    in al, 40h
    mov bl, al
    in al, 40h
    mov bh, al
    .repeat
        rdtsc
        sub eax, ecx
    .until eax > 32768 ; clock > 32768
    in al, 40h
    mov cl, al
    in al, 40h
    mov ch, al
    sub bx, cx ; tick per 32768 clock
    xor eax, eax
    mov ax, bx
    mov ebx, 32 * 419
    mul ebx
    mov ebx, 1000000
    div ebx
    mov msPer1MClock, eax
    invoke printUInt32, eax
    printNewline
    
    
    ; rdtsc
    ; push eax
    ; in al, 40h
    ; mov bl, al
    ; in al, 40h
    ; mov bh, al
    ; mov cx, 8000h
    ; calculateCPUCycle:
    ;     loop calculateCPUCycle
    ; rdtsc
    ; push eax
    ; in al, 40h
    ; mov cl, al
    ; in al, 40h
    ; mov ch, al
    ; sub bx, cx
    ; pop eax
    ; pop edx
    ; sub eax, edx
    ; invoke printUInt16, bx
    ; printNewline
    ; invoke printUInt32, eax
    ; printNewline
    
    mov ah, 00h
    int 16h
    
    refresh:
        ; clear backbuffer
        mov ax, backBuffer
        mov es, ax
        xor eax, eax
        xor di, di
        mov cx, (320 * 200) / 4
        rep stosd
        
        ; move text cursor
        mov ah, 02h
        mov bh, 0
        mov dh, 0
        mov dl, 0
        int 10h
        
        ; get dt
        rdtsc
        mov ebx, eax
        sub eax, lastTimerCount
        mov lastTimerCount, ebx
        mov ecx, eax
        
        invoke printUInt32, lastTimerCount
        printNewline
        invoke printUInt32, eax
        
        ; step
        
        mov ax, ballVx
        ; add ballVy, 1
        mov bx, ballVy
        add ballIntX, ax
        add ballIntY, bx
        
        .if ballIntX <= ballRadius || ballIntX >= 320 - ballRadius
            neg ballVx
        .endif
        .if ballIntY <= ballRadius || ballIntY >= 200 - ballRadius
            neg ballVy
        .endif
        
        ; draw to backbuffer
        invoke drawCircle, ballIntX, ballIntY
        
        ; VSync
        mov dx, 03dah
        waitVerticalRetraceEnd:
            in al, dx
            bt ax, 3
            jc waitVerticalRetraceEnd
        waitVerticalRetraceStart:
            in al, dx
            bt ax, 3
            jnc waitVerticalRetraceStart
        
        ; backbuffer -> video memory
        push ds
        mov ax, backBuffer
        mov ds, ax
        mov ax, 0a000h
        mov es, ax
        xor si, si
        xor di, di
        mov cx, (320 * 200) / 4
        rep movsd
        pop ds
        
        
    
        ; delay
        ; mov ah, 86h
        ; mov cx, 0
        ; mov dx, 40000
        ; int 15h
        
        inc loopCounter
        ; read key
        mov ah, 06h
        mov dl, 0ffh
        int 21h
        jz refresh
    
    exit:
        mov ax, 03h
        int 10h
        .exit
main endp

drawCircle proc, centerX: word, centerY: word
    ; di = (centerY - ballRadius) * 320 + (centerX - circleWidth[0] >> 1)
    mov ax, centerY
    sub ax, ballRadius
    mov bx, 320
    mul bx
    add ax, centerX
    mov bx, circleWidth[0]
    shr bx, 1
    sub ax, bx
    mov di, ax
    
    ; si end = &circleWidth[0] + sizeof circleWidth
    lea si, circleWidth
    mov bx, si
    add bx, (ballRadius * 2 + 1) * 2 ; 2bytes * (2r + 1)
    
    mov al, 00fh ; color white
    .while si != bx
        mov cx, [si]
        rep stosb
        
        ; di += 320 - circleWidth[i] / 2 - circleWidth[i + 1] / 2
        ;     = (640 - circleWidth[i] - circleWidth[i + 1]) >> 1
        mov cx, 640
        sub cx, [si]
        add si, 2
        sub cx, [si]
        shr cx, 1
        add di, cx
    .endw
    
    ret
drawCircle endp
printUInt8 proc uses ax dx ds si, number: byte
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
printUInt8 endp
printUInt16 proc uses ax cx dx ds si, number: word
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
printUInt16 endp
printUInt32 proc uses eax cx dx ds si, number: dword
    local outputString[11]: byte
    
    mov si, 10
    mov outputString[si], '$'
    
    mov ecx, 10
    mov eax, number
    test eax, eax
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
    
    mov ax, ss
    mov ds, ax
    mov ah, 09h
    lea dx, outputString[si]
    int 21h
    
    ret
printUInt32 endp
end main