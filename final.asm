; http://www.brackeen.com/vga/unchain.html
; https://wanker742126.neocities.org/
; https://www.ctyme.com/intr/int.htm
; https://www.website.masmforum.com/tutorials/fptute/
; https://stackoverflow.com/questions/13450894/struct-or-class-in-assembly

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
fcomi_ macro i ; see fcomi in https://www.website.masmforum.com/tutorials/fptute/fpuchap7.htm
    db 0dbh, 0f0h + i
endm
fcomip_ macro i ; see fcomip in https://www.website.masmforum.com/tutorials/fptute/fpuchap7.htm
    db 0dfh, 0f0h + i
endm

; https://learn.microsoft.com/en-us/cpp/assembler/masm/dot-model?view=msvc-170
; compact: multiple data segment
; c: c calling convention
; farstack: independent stack (SS != DS)
.model compact, c, farstack
.586


screenWidth equ 320.0
screenHeight equ 200.0

.data
step proto, pBall: ptr circle
drawCircle proto, centerX: word, centerY: word
printUint8 proto, number: byte
printUint16 proto, number: word
printUInt32 proto, number: dword
printInt8 proto, number: byte
printInt16 proto, number: word
printInt32 proto, number: dword

circlePixelRadius equ 20
circleRadius real8 20.0
circleWidth \
dw 9, 15, 19, 23, 25, 27, 29, 31, 33, 35, 35, 37, 37, 39, 39, 39, 41, 41, 41, 41, 41, 41, 41, 41, 41
dw 39, 39, 39, 37, 37, 35, 35, 33, 31, 29, 27, 25, 23, 19, 15, 9
circle struct
    x real8 ?
    y real8 ?
    vx real8 ?
    vy real8 ?
circle ends
wallRebound proto, pBall: ptr circle
ball1 circle <120.0, 100.0, -300.0, -300.0>
ball2 circle <200.0, 100.0, 100.0, 100.0>

lastTimerCount dd ?
fpuTemp dword ?
fpuTemp2 dword ?
widthMinusRadius real8 screenWidth
heightMinusRadius real8 screenHeight
frictionCoefficient real8 30.0

debugCounter dw 0

.fardata
db 320 * 200 dup(?) ; video backbuffer

.stack 1000h
.code
main proc
    .startup
    finit ; init fpu
    
    ; calculate floating-point constant
    fld [circleRadius] ; st: [radius]
    fld [widthMinusRadius] ; st: [width][radius]
    fsub st, st(1) ; st: [width - radius][radius]
    fstp [widthMinusRadius] ; st: [radius]
    fsubr [heightMinusRadius] ; st: [height - radius]
    fstp [heightMinusRadius] ; st: []
    
    
    ; video mode, 320 * 200, 256 colors
    mov ax, 13h
    int 10h
    
    ; 1 tick = 1 / (1193182 * 2) s = 1 / 2.39M s
    ; clock = rdtsc cpu clock
    ; clockPeriod
    ;     = time / clock 
    ;     = time/tick * tick/clock
    ;     = tickIn32768Clock / 32768 / 2.39M
    xor al, al
    out 43h, al
    rdtsc
    mov ecx, eax
    in al, 40h
    mov bl, al
    in al, 40h
    mov bh, al
    .repeat
        rdtsc
        sub eax, ecx
    .until eax > 1024 ; clock > 1024
    push eax
    xor al, al
    out 43h, al
    in al, 40h
    mov cl, al
    in al, 40h
    mov ch, al
    pop eax
    invoke printUInt32, eax
    printNewline
    invoke printUInt16, bx
    printNewline
    invoke printUInt16, cx
    printNewline
    sub bx, cx ; bx = tick in 32768 clock
    invoke printUInt16, bx
    printNewline
    movzx ebx, bx
    invoke printUInt32, ebx
    printNewline
    mov [fpuTemp], ebx
    fild [fpuTemp] ; st: [tickIn32768Clock]
    mov [fpuTemp], 1024
    fidiv [fpuTemp] ; st: [tickIn32768Clock / 32768]
    mov [fpuTemp], 1193182 * 2 ; 2.39MHz
    fidiv [fpuTemp] ; st: [tickIn32768Clock / 32768 / 2.39M] = [clockPeriod]
    
    mov ah, 00h
    int 16h
    
    
    rdtsc
    mov [lastTimerCount], eax
    
    refresh:
        ; clear backbuffer
        mov ax, @fardata
        mov es, ax
        xor eax, eax
        xor di, di
        mov cx, (320 * 200) / 4
        rep stosd
        
        ; get dt
        rdtsc
        mov ebx, eax
        sub eax, [lastTimerCount]
        mov [lastTimerCount], ebx
        mov [fpuTemp], eax
        fild [fpuTemp] ; st: [dClock][clockPeriod]
        fmul st, st(1) ; st: [dt][clockPeriod]
        ; fix dt mode
        ; fstp st
        ; mov [fpuTemp], 28571
        ; fild [fpuTemp]
        ; mov [fpuTemp], 1000000
        ; fdiv [fpuTemp]
        
        ; step
        invoke step, addr ball1
        invoke step, addr ball2
        
        
        ; collision detect
        fld [heightMinusRadius] ; st: [h - r][dt][clockPeriod]
        fld [widthMinusRadius] ; st: [w - r][h - r][dt][clockPeriod]
        fld [circleRadius] ; st: [r][w - r][h - r][dt][clockPeriod]
        invoke wallRebound, addr ball1
        invoke wallRebound, addr ball2
        fstp st ; st: [w - r][h - r][dt][clockPeriod]
        fstp st ; st: [h - r][dt][clockPeriod]
        fstp st ; st: [dt][clockPeriod]
        
        
        ; invoke printUint16, [debugCounter]
        ; printNewline
        
        ; draw to backbuffer
        mov ax, @fardata
        mov es, ax
        fld [ball1.x] ; st: [x][dt][clockPeriod]
        fistp word ptr [fpuTemp + 2] ; st: [dt][clockPeriod]
        fld [ball1.y] ; st: [y][dt][clockPeriod]
        fistp word ptr [fpuTemp] ; st: [dt][clockPeriod]
        invoke drawCircle, word ptr [fpuTemp + 2], word ptr [fpuTemp]
        fld [ball2.x] ; st: [x][dt][clockPeriod]
        fistp word ptr [fpuTemp + 2] ; st: [dt][clockPeriod]
        fld [ball2.y] ; st: [y][dt][clockPeriod]
        fistp word ptr [fpuTemp] ; st: [dt][clockPeriod]
        invoke drawCircle, word ptr [fpuTemp + 2], word ptr [fpuTemp]
        
        ; VSync
        mov dx, 03dah
        @@:
            in al, dx
            bt ax, 3
            jc @b
        @@:
            in al, dx
            bt ax, 3
            jnc @b
        
        ; backbuffer -> video memory
        push ds
        mov ax, @fardata
        mov ds, ax
        mov ax, 0a000h
        mov es, ax
        xor si, si
        xor di, di
        mov cx, (320 * 200) / 4
        rep movsd
        pop ds
        
        ; move text cursor
        mov ah, 02h
        mov bh, 0
        mov dh, 0
        mov dl, 0
        int 10h
        
        ; debug information
        mov [fpuTemp], 1000000
        fild [fpuTemp] ; st: [100000][dt][clockPeriod]
        fmul st, st(1) ; st: [100000 * dt][dt][clockPeriod]
        fistp [fpuTemp] ; st: [dt][clockPeriod]
        invoke printUint32, [fpuTemp]
        printNewline
        
        fld [ball1.x]
        fistp [fpuTemp]
        invoke printInt32, [fpuTemp]
        printNewline
        fld [ball1.y]
        fistp [fpuTemp]
        invoke printInt32, [fpuTemp]
        printNewline
        fld [ball1.vx]
        fistp [fpuTemp]
        invoke printInt32, [fpuTemp]
        printNewline
        fld [ball1.vy]
        fistp [fpuTemp]
        invoke printInt32, [fpuTemp]
        printNewline
        
        ; delay
        ; mov ah, 86h
        ; mov cx, 0
        ; mov dx, 40000
        ; int 15h
        
        
        fstp st ; st: [clockPeriod]
        
        ; mov ah, 00h
        ; int 16h
        ; cmp al, 1bh
        ; jne refresh
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

step proc, pBall: ptr circle ; st: [dt][<=5]
    mov si, word ptr [pBall]
    
    ; newX = x + vx * dt
    fld [(circle ptr [si]).vx] ; st: [vx][dt][clockPeriod]
    fmul st, st(1) ; st: [vx * dt][dt][clockPeriod]
    fadd [(circle ptr [si]).x] ; st: [x + vx * dt][dt][clockPeriod] = [newX][dt][clockPeriod]
    fstp [(circle ptr [si]).x] ; st: [dt][clockPeriod]
    ; newY = y + vy * dt
    fld [(circle ptr [si]).vy] ; st: [vy][dt][clockPeriod]
    fmul st, st(1) ; st: [vy * dt][dt][clockPeriod]
    fadd [(circle ptr [si]).y] ; st: [y + vy * dt][dt][clockPeriod] = [newY1][dt][clockPeriod]
    fstp [(circle ptr [si]).y] ; st: [dt][clockPeriod]
    
    ; friction:
    ;   f = m * a = k_f * n * (-v / ||v||), m = mass, a = accelleration, k_f = friction coefficient, n = normal force
    ;   a = -k_f * n / m * (-v / ||v||) = -k * v / ||v||
    ;   v = v0 + a0 * dt = v0 - k * v0 / ||v0|| * dt 
    ;     = v0 * (1 - k * dt / ||v0||), k > 0, 1 - k * dt / ||v0|| >= 0
    fld [(circle ptr [si]).vx] ; st: [vx][dt][clockPeriod]
    fmul st, st ; st: [vx^2][dt][clockPeriod]
    fld [(circle ptr [si]).vy] ; st: [vy][vx^2][dt][clockPeriod]
    fmul st, st ; st: [vy^2][vx^2][dt][clockPeriod]
    faddp st(1), st ; st: [vx^2 + vy^2][dt][clockPeriod]
    fsqrt ; st: [||v||][dt][clockPeriod]
    fdivr st, st(1) ; st: [dt / ||v||][dt][clockPeriod]
    fmul [frictionCoefficient] ; st: [k * dt / ||v||][dt][clockPeriod]
    fld1 ; st: [1][k * dt / ||v||][dt][clockPeriod]
    fsubrp st(1), st ; st: [1 - k * dt / ||v||][dt][clockPeriod]
    fldz ; st: [0][1 - k * dt / ||v||][dt][clockPeriod]
    fcomip_ 1 ; st: [1 - k * dt / ||v||][dt][clockPeriod]
    jbe @f ; 0 <= 1 - k * dt / ||v||
        fstp st ; st: [dt][clockPeriod]
        fldz ; st: [0][dt][clockPeriod]
    @@:
    fld [(circle ptr [si]).vx] ; st: [vx][1 - k * dt / ||v||][dt][clockPeriod]
    fmul st, st(1) ; st: [vx * (1 - k * dt / ||v||)][1 - k * dt / ||v||][dt][clockPeriod]
    fstp [(circle ptr [si]).vx] ; st: [1 - k * dt / ||v||][dt][clockPeriod]
    fld [(circle ptr [si]).vy] ; st: [vy][1 - k * dt / ||v||][dt][clockPeriod]
    fmul ; st: [vy * (1 - k * dt / ||v||)][dt][clockPeriod]
    fstp [(circle ptr [si]).vy] ; st: [dt][clockPeriod]
    
    ret
step endp
wallRebound proc, pBall: ptr circle ; st: [radius][width- radius][height - radius][<=4]
    mov si, word ptr [pBall]
    ; r = radius, w = width, h = height
    
    ; left edge
    fld [(circle ptr [si]).x] ; st: [x][r][w - r][h - r][...]
    fcomi_ 1 ; cmp x, r
    ja @f ; x > r
    bt word ptr [(circle ptr [si]).vx + 7], 7 ; sign bit of float
    jnc @f ; vx > 0
        ; x <= r && vx < 0
        and byte ptr [(circle ptr [si]).vx + 7], 01111111b ; neg -> pos
    @@:
    
    ; right edge
    fcomip_ 2 ; cmp x, w - r ; st: [r][w - r][h - r][...]
    jb @f ; x < w - r
    bt word ptr [(circle ptr [si]).vx + 7], 7 ; sign bit of float
    jc @f ; vx < 0
        ; x >= w - r && vx > 0
        or byte ptr [(circle ptr [si]).vx + 7], 10000000b ; pos -> neg
    @@:
    
    ; top edge
    fld [(circle ptr [si]).y] ; st: [y][r][w - r][h - r][...]
    fcomi_ 1 ; cmp y, r
    ja @f ; y > r
    bt word ptr [(circle ptr [si]).vy + 7], 7 ; sign bit of float
    jnc @f ; vy > 0
        ; y <= r && vy < 0
        and byte ptr [(circle ptr [si]).vy + 7], 01111111b ; neg -> pos
    @@:
    
    ; bottom edge
    fcomip_ 3 ; cmp y, h - r ; st: [r][w - r][h - r][...]
    jb @f ; y < h - r
    bt word ptr [(circle ptr [si]).vy + 7], 7 ; sign bit of float
    jc @f ; vy < 0
        ; y >= h - r && vy > 0
        or byte ptr [(circle ptr [si]).vy + 7], 10000000b ; pos -> neg
    @@:
    
    ret
wallRebound endp
drawCircle proc, centerX: word, centerY: word
    ; di = (centerY - circleRadius) * 320 + (centerX - circleWidth[0] >> 1)
    mov ax, [centerY]
    sub ax, circlePixelRadius
    mov bx, 320
    mul bx
    add ax, [centerX]
    mov bx, [circleWidth + 0]
    shr bx, 1
    sub ax, bx
    mov di, ax
    
    ; bx = si end = &circleWidth[0] + sizeof circleWidth
    lea si, [circleWidth]
    mov bx, si
    add bx, (circlePixelRadius * 2 + 1) * 2 ; 2bytes * (2r + 1)
    
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
printInt8 proc uses ax dx ds si, number: byte
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
printInt16 proc uses ax cx dx ds si, number: word
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
printInt32 proc uses eax cx dx ds si, number: dword
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
end main