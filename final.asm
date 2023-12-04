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
matrix_x_vector macro m11, m21, m12, m22, v1, v2 ; index of fpu stack, result stored in v1, v2
    ; /m11 m12\ /v1\ = /m11 * v1 + m12 * v2\
    ; \m21 m22/ \v2/   \m21 * v1 + m22 * v2/
    fld st(v1) ; st: [v1]...[v1][v2]
    fld st(v2 + 1) ; st: [v2][v1]...[v1][v2]
    fmul st, st(m12 + 2) ; st: [m12 * v2][v1]...[v1][v2]
    fxch st(v1 + 2) ; st: [v1][v1]...[m12 * v2][v2]
    fmul st, st(m11 + 2) ; st: [m11 * v1][v1]...[m12 * v2][v2]
    faddp st(v1 + 2), st ; st: [v1]...[m11 * v1 + m12 * v2][v2]
    fmul st, st(m21 + 1) ; st: [m21 * v1]...[m11 * v1 + m12 * v2][v2]
    fxch st(v2 + 1) ; st: [v2]...[m11 * v1 + m12 * v2][m21 * v1]
    fmul st, st(m22 + 1) ; st: [m22 * v2]...[m11 * v1 + m12 * m1v22][m21 * v1]
    faddp st(v2 + 1), st ; st: ...[m11 * v1 + m12 * v2][m21 * v1 + m22 * v2]
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

circlePixelRadius equ 16
circleRadius real8 16.0
circleWidth \
dw 9, 13, 17, 21, 23, 25, 27, 27, 29, 29, 31, 31, 33, 33, 33, 33, 33, 33, 33, 33, 33, 31, 31, 29, 29
dw 27, 27, 25, 23, 21, 17, 13, 9
circle struct
    x real8 ?
    y real8 ?
    vx real8 ?
    vy real8 ?
    integerX dw ?
    integerY dw ?
circle ends
wallRebound proto, pBall: ptr circle
ball1 circle <120.0, 100.0, 100.0, 200.0>
ball2 circle <200.0, 100.0, 300.0, 400.0>

clockPeriod real8 ?
lastTimerCount dd ?
deltaT real8 ?
fpuTemp dword ?
fpuTempTbyte dt ?
widthMinusRadius real8 screenWidth
heightMinusRadius real8 screenHeight
frictionCoefficient real8 50.0

mouseButtonStatus db ?

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
    fld [ball1.x] ; st: [x]
    fistp [ball1.integerX] ; st: []
    fld [ball1.y] ; st: [x]
    fistp [ball1.integerY] ; st: []
    fld [ball2.x] ; st: [x]
    fistp [ball2.integerX] ; st: []
    fld [ball2.y] ; st: [x]
    fistp [ball2.integerY] ; st: []
    
    
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
    fidiv [fpuTemp] ; st: [tickIn32768Clock / 32768 / 2.39M = clockPeriod]
    fstp [clockPeriod] ; st: []
    
    mov ah, 00h
    int 16h
    
    mov ax, 03h
    int 33h
    mov mouseButtonStatus, bl
    
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
        
        ; move text cursor to top left
        mov ah, 02h
        xor bh, bh
        xor dh, dh
        xor dl, dl
        int 10h
        
        ; get dt
        rdtsc
        mov ebx, eax
        sub eax, [lastTimerCount]
        mov [lastTimerCount], ebx
        mov [fpuTemp], eax
        fild [fpuTemp] ; st: [dClock]
        fmul [clockPeriod] ; st: [dt]
        fstp [deltaT] ; st: []
        
        mov [fpuTemp], 1000000
        fild [fpuTemp] ; st: [1000000]
        fmul [deltaT] ; st: [1000000 * dt]
        fistp [fpuTemp] ; st: []
        invoke printUint32, [fpuTemp]
        printNewline
        
        ; step
        fld [deltaT] ; st: [dt]
        invoke step, addr ball1
        invoke step, addr ball2
        fstp st ; st: []
        
        mov ax, 03h
        int 33h
        shr cx, 1
        invoke printUint16, cx
        printNewline
        invoke printUint16, dx
        printNewline
        ; wait for negative edge
        test mouseButtonStatus, 001b
        jz @f ; not pressed
        test bl, 001b
        jnz @f ; pressed but not released
            mov word ptr [fpuTemp], cx
            fild word ptr [fpuTemp] ; st: [mouseX]
            fsub [ball1.x] ; st: [mouseX - x1]
            fldpi ; st: [pi][mouseX - x1]
            fmul ; st: [pi * (mouseX - x1)]
            ; fsqrt
            fstp [ball1.vx] ; st: []
            mov word ptr [fpuTemp], dx
            fild word ptr [fpuTemp] ; st: [mouseY]
            fsub [ball1.y] ; st: [mouseY - y1]
            fldpi ; st: [pi][mouseY - y1]
            fmul ; st: [pi * (mouseY - y1)]
            ; fsqrt
            fstp [ball1.vy] ; st: []
        @@:
        mov mouseButtonStatus, bl
        
        ; wall collision
        fld [heightMinusRadius] ; st: [h - r]
        fld [widthMinusRadius] ; st: [w - r][h - r]
        fld [circleRadius] ; st: [r][w - r][h - r]
        invoke wallRebound, addr ball1
        invoke wallRebound, addr ball2
        fstp st ; st: [w - r][h - r]
        fstp st ; st: [h - r]
        fstp st ; st: []
        
        ; ball collision
        ; n = (s1 - s2) / |n| = (nx, ny), unit normal vector
        ; t = (tx, ty) = (-ny, nx), unit tangent vector
        ; |n|^2 <= (2 * r)^2: collision occur
        ; /nx tx\   /nx -ny\ , NT coordinate => XY coordinate
        ; \ny ty/ = \ny  nx/ , orthonormal basis
        ; /nx -ny\-1  /nx -ny\T  / nx ny\ , XY coordinate => NT coordinate
        ; \ny  nx/  = \ny  nx/ = \-ny nx/
        fld [ball1.y] ; st: [y1]
        fsub [ball2.y] ; st: [y1 - y2 = dy]
        fld [ball1.x] ; st: [x1][dy]
        fsub [ball2.x] ; st: [x1 - x2 = dx][dy]
        fld st(1) ; st: [dy][dx][dy]
        fmul st, st ; st: [dy^2][dx][dy]
        fld st(1) ; st: [dx][dy^2][dx][dy]
        fmul st, st ; st: [dx^2][dy^2][dx][dy]
        fadd ; st: [dx^2 + dy^2 = |n|^2][dx][dy]
        fld [circleRadius] ; st: [r][|n|^2][dx][dy]
        fadd st, st ; st: [2 * r][|n|^2][dx][dy]
        fmul st, st ; st: [(2 * r)^2][|n|^2][dx][dy]
        fcomip_ 1 ; st: [|n|^2][dx][dy]
        jb @f ; (2 * r)^2 < |n|^2: no collision
            fsqrt ; st: [|n|][dx][dy]
            fdiv st(2), st ; st: [|n|][dx][dy / |n| = ny]
            fdivp st(1), st ; st: [dx / |n| = nx][ny]
            fld st(1) ; st: [ny][nx][ny]
            fchs ; st: [-ny][nx][ny]
            fld [ball1.vy] ; st: [v1y][-ny][nx][ny]
            fld [ball1.vx] ; st: [v1x][v1y][-ny][nx][ny]
            ; / nx ny\ /v1x\ = /v1n\
            ; \-ny nx/ \v1y/   \v1t/
            matrix_x_vector 3, 2, 4, 3, 0, 1 ; st: [v1n][v1t][-ny][nx][ny]
            fstp [fpuTempTbyte] ; st: [v1t][-ny][nx][ny]
            fld [ball2.vy] ; st: [v2y][v1t][-ny][nx][ny]
            fld [ball2.vx] ; st: [v2x][v2y][v1t][-ny][nx][ny]
            ; / nx ny\ /v2x\ = /v2n\
            ; \-ny nx/ \v2y/   \v2t/
            matrix_x_vector 4, 3, 5, 4, 0, 1 ; st: [v2n][v2t][v1t][-ny][nx][ny]
            ; /nx -ny\ /v2n\ = v1': v1 after collision
            ; \ny  nx/ \v1t/
            matrix_x_vector 4, 5, 3, 4, 0, 2 ; st: [v1x'][v2t][v1y'][-ny][nx][ny]
            fstp [ball1.vx] ; st: [v2t][v1y'][-ny][nx][ny]
            fld [fpuTempTbyte] ; st: [v1n][v2t][v1y'][-ny][nx][ny]
            ; /nx -ny\ /v1n\ = v2': v2 after collision
            ; \ny  nx/ \v2t/
            matrix_x_vector 4, 5, 3, 4, 0, 1 ; st: [v2x'][v2y'][v1y'][-ny][nx][ny]
            fstp [ball2.vx] ; st: [v2y'][v1y'][-ny][nx][ny]
            fstp [ball2.vy] ; st: [v1y'][-ny][nx][ny]
            fstp [ball1.vy] ; st: [-ny][nx][ny]
        @@:
        finit ; st: []
        
        
        ; invoke printUint16, [debugCounter]
        ; printNewline
        
        ; draw to backbuffer
        mov ax, @fardata
        mov es, ax
        ; fld [ball1.x] ; st: [x]
        ; fistp word ptr [fpuTemp + 2] ; st: []
        ; fld [ball1.y] ; st: [y]
        ; fistp word ptr [fpuTemp] ; st: []
        ; invoke drawCircle, word ptr [fpuTemp + 2], word ptr [fpuTemp]
        ; fld [ball2.x] ; st: [x]
        ; fistp word ptr [fpuTemp + 2] ; st: []
        ; fld [ball2.y] ; st: [y]
        ; fistp word ptr [fpuTemp] ; st: []
        ; invoke drawCircle, word ptr [fpuTemp + 2], word ptr [fpuTemp]
        invoke drawCircle, [ball1.integerX], [ball1.integerY]
        invoke drawCircle, [ball2.integerX], [ball2.integerY]
        
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
        
        ; delay
        ; mov ah, 86h
        ; mov cx, 0
        ; mov dx, 40000
        ; int 15h
        
        inc [debugCounter]
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

step proc, pBall: ptr circle ; st: [dt]
    mov si, word ptr [pBall]
    
    ; newX = x + vx * dt
    fld [(circle ptr [si]).vx] ; st: [vx][dt]
    fmul st, st(1) ; st: [vx * dt][dt]
    fadd [(circle ptr [si]).x] ; st: [x + vx * dt][dt] = [newX][dt]
    fist [(circle ptr [si]).integerX]
    fstp [(circle ptr [si]).x] ; st: [dt]
    ; newY = y + vy * dt
    fld [(circle ptr [si]).vy] ; st: [vy][dt]
    fmul st, st(1) ; st: [vy * dt][dt]
    fadd [(circle ptr [si]).y] ; st: [y + vy * dt][dt] = [newY1][dt]
    fist [(circle ptr [si]).integerY]
    fstp [(circle ptr [si]).y] ; st: [dt]
    
    ; friction:
    ;   f = m * a = k_f * n * (-v / ||v||), m = mass, a = accelleration, k_f = friction coefficient, n = normal force
    ;   a = -k_f * n / m * (-v / ||v||) = -k * v / ||v||
    ;   v = v0 + a0 * dt = v0 - k * v0 / ||v0|| * dt 
    ;     = v0 * (1 - k * dt / ||v0||), k > 0, 1 - k * dt / ||v0|| >= 0
    fld [(circle ptr [si]).vx] ; st: [vx][dt]
    fmul st, st ; st: [vx^2][dt]
    fld [(circle ptr [si]).vy] ; st: [vy][vx^2][dt]
    fmul st, st ; st: [vy^2][vx^2][dt]
    faddp st(1), st ; st: [vx^2 + vy^2][dt]
    fsqrt ; st: [||v||][dt]
    fdivr st, st(1) ; st: [dt / ||v||][dt]
    fmul [frictionCoefficient] ; st: [k * dt / ||v||][dt]
    fld1 ; st: [1][k * dt / ||v||][dt]
    fsubrp st(1), st ; st: [1 - k * dt / ||v||][dt]
    fldz ; st: [0][1 - k * dt / ||v||][dt]
    fcomip_ 1 ; st: [1 - k * dt / ||v||][dt]
    jbe @f ; 0 <= 1 - k * dt / ||v||
        fstp st ; st: [dt]
        fldz ; st: [0][dt]
    @@:
    fld [(circle ptr [si]).vx] ; st: [vx][1 - k * dt / ||v||][dt]
    fmul st, st(1) ; st: [vx * (1 - k * dt / ||v||)][1 - k * dt / ||v||][dt]
    fstp [(circle ptr [si]).vx] ; st: [1 - k * dt / ||v||][dt]
    fld [(circle ptr [si]).vy] ; st: [vy][1 - k * dt / ||v||][dt]
    fmul ; st: [vy * (1 - k * dt / ||v||)][dt]
    fstp [(circle ptr [si]).vy] ; st: [dt]
    
    ret
step endp
wallRebound proc, pBall: ptr circle ; st: [radius][width- radius][height - radius]
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