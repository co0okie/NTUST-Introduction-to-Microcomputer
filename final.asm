; Reference:
;   https://wanker742126.neocities.org/
;   https://www.ctyme.com/intr/int.htm
;   https://www.website.masmforum.com/tutorials/fptute/
;   https://stackoverflow.com/questions/13450894/struct-or-class-in-assembly
;   https://en.wikipedia.org/wiki/Linear_congruential_generator#Parameters_in_common_use
;   https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle#The_modern_algorithm
;   https://masm32.com/board/index.php?topic=7837.0
;   https://stackoverflow.com/questions/67066755/assembly-x86-16-bit-vsync-screen-tearing
;   https://learn.microsoft.com/en-us/cpp/assembler/masm/dot-model?view=msvc-170

moveTextCursor macro x, y
    mov ah, 02h
    xor bh, bh
    mov dh, y
    mov dl, x
    int 10h
endm
printString macro string
    mov ah, 09h
    lea dx, string
    int 21h
endm
printStringWithAttribute macro string, color, length, x, y
    push bp
    mov ax, @data
    mov es, ax
    mov ah, 13h
    mov al, 0
    mov bh, 0
    mov bl, color
    mov cx, length
    mov dh, y
    mov dl, x
    lea bp, string
    int 10h
    pop bp
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
fcomi_ macro i ; https://www.website.masmforum.com/tutorials/fptute/fpuchap7.htm#fcomi
    db 0dbh, 0f0h + i
endm
fcomip_ macro i ; https://www.website.masmforum.com/tutorials/fptute/fpuchap7.htm#fcomip
    db 0dfh, 0f0h + i
endm
nextRandomNumber macro
    ; https://en.wikipedia.org/wiki/Linear_congruential_generator#Parameters_in_common_use
    mov eax, 1664525
    mul [randomNumber]
    add eax, 1013904223
    mov [randomNumber], eax
endm

; https://learn.microsoft.com/en-us/cpp/assembler/masm/dot-model?view=msvc-170
; compact: multiple data segment
; c: c calling convention
; farstack: independent stack segment (SS != DS)
.model compact, c, farstack
.586

.data
; procedure prototype
step proto, pBall: near ptr circle
wallCollision proto, pBall: near ptr circle
drawRectangle proto, left: word, top: word, rectWidth: word, height: word, color: byte
drawCircle proto, centerX: word, centerY: word, color: byte
drawLine proto, x0: word, y0: word, x1: word, y1: word
printScore proto, score: byte, color: byte, x: byte, y: byte

; string
enterToStartMessage db "Press Enter to start"
hForHelpMessage db "Press H for help"
gameIntroduction \
    db "This is a two-player battle game where", 13, 10, " "
    db "red and blue take turns. Click the", 13, 10, " "
    db "left mouse button to shoot the ball", 13, 10 ," "
    db "towards the cursor; the dashed line", 13, 10 ," "
    db "length indicates the shots power.", 13, 10 ," "
    db "First to reach 10 points wins, but -10", 13, 10 ," "
    db "means loss. Hitting walls of different", 13, 10 ," "
    db "colors triggers various effects,", 13, 10 ," "
    db "including:$"
normalWallMessage db "no effect$"
increaseScoreMessage db "score +1$"
increase3ScoreMessage db "score +3$"
decreaseScoreMessage db "score -1$"
swapBallsMessage db "swap balls$"
extraTurnMessage db "gain extra turn$"
winMessage db " win !$"
playAgainMessage db "Press Enter to play again$"
exitMessage db "Press ESC to exit$" 

circleIntegerRadius equ 12
circleRadius real4 12.0
circleWidth dw 7, 11, 15, 17, 19, 21, 21, 23, 23, 25, 25, 25, 25, 25, 25, 25, 23, 23, 21, 21, 19, 17, 15, 11, 7
circle struct
    id db 5 dup(?)
    x real4 ?
    y real4 ?
    vx real4 ?
    vy real4 ?
    integerX dw ?
    integerY dw ?
    score db ?
circle ends
redBall circle <"Red$", , , , , , , >
blueBall circle <"Blue$", , , , , , , >

clockPeriod real4 ?
lastTimerCount dd ?
deltaT real4 ?
fpuTemp dt ? ; 10 bytes
widthMinusRadius real4 308.0 ; 320 - 12
heightMinusRadius real4 188.0 ; 200 - 12
frictionCoefficient real4 50.0

mouseButtonStatus db ?

gameStatus db 00000000b
; [0] 1: round start, 0: round end
; [1] who's turn? 0: red, 1: blue
; [2] who's next? 0: red, 1: blue

; https://masm32.com/board/index.php?topic=7837.0
wallEffectProto typedef proto
wallEffectPtr typedef near ptr wallEffectProto ; array for functions addr, the data inside is wallEffectProto
wallEffect wallEffectPtr 26 dup(?)
wallColor db 26 dup(?)

dashedlineColor db 08h, 08h, 08h, 08h, 08h, 00h, 00h, 00h
dashedlineFlow dw 0

randomNumber dd ?

.fardata? backBuffer
db 320 * 200 dup(?) ; video backbuffer

.stack 1000h
.code
main proc
    .startup
    
    ; video mode, 320 * 200, 256 colors
    mov ax, 13h
    int 10h
    
    ; random number seed
    rdtsc
    mov [randomNumber], eax
    
    call getCPUClockPeriod
    
    .while 1 ; game cycle
        ; cover image
        push ds
        mov ax, coverImage
        mov ds, ax
        mov ax, 0a000h
        mov es, ax
        xor si, si
        xor di, di
        mov cx, (320 * 200) / 4
        rep movsd
        pop ds
        
        printStringWithAttribute enterToStartMessage, 28h, lengthof enterToStartMessage, 2, 17
        printStringWithAttribute hForHelpMessage, 37h, lengthof hForHelpMessage, 4, 19
        
        
        .while 1 ; wait user input
            mov ah, 00h
            int 16h
            .if al == 1bh ; esc
                call exitGame
            .elseif al == 'h' || al == 'H'
                call helpPage
                .break
            .elseif al == 0dh ; enter
                call gameStart
                .break
            .endif
        .endw
    .endw
main endp
getCPUClockPeriod proc
    ; 1 tick = 1 / 1193182 s
    ; clock = rdtsc cpu clock
    ; clockPeriod
    ;     = time / clock 
    ;     = time/tick * tick/clock
    ;     = tickIn4096Clock / 32768 / 1193182
    mov al, 00110000b
    out 43h, al
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
    .until eax > 4096 ; clock > 4096
    push eax
    xor al, al
    out 43h, al
    in al, 40h
    mov cl, al
    in al, 40h
    mov ch, al
    pop eax
    sub bx, cx ; bx = tick in 4096 clock
    mov word ptr [fpuTemp], bx
    fild word ptr [fpuTemp] ; st: [tickIn4096Clock]
    mov word ptr [fpuTemp], 4096
    fidiv word ptr [fpuTemp] ; st: [tickIn4096Clock / 4096]
    mov dword ptr [fpuTemp], 1193182
    fidiv dword ptr [fpuTemp] ; st: [tickIn4096Clock / 4096 / 1193182 = clockPeriod]
    fstp [clockPeriod] ; st: []
    
    ret
getCPUClockPeriod endp

gameStart proc
    finit
    
    ; ball initialization
    mov [redBall.integerX], 50
    fild [redBall.integerX] ; st: [x]
    fstp [redBall.x] ; st: []
    mov [redBall.integerY], 50
    fild [redBall.integerY] ; st: [y]
    fstp [redBall.y] ; st: []
    mov [blueBall.integerX], 320 - 50
    fild [blueBall.integerX] ; st: [x]
    fstp [blueBall.x] ; st: []
    mov [blueBall.integerY], 200 - 50
    fild [blueBall.integerY] ; st: [y]
    fstp [blueBall.y] ; st: []
    mov [redBall.vx], 0
    mov [redBall.vy], 0
    mov [blueBall.vx], 0
    mov [blueBall.vy], 0
    mov [redBall.score], 0
    mov [blueBall.score], 0
    
    call generateWall
    
    mov ax, 03h
    int 33h
    mov [mouseButtonStatus], bl
    
    rdtsc
    mov [lastTimerCount], eax
    
    ; reset game status and choose random player to start
    mov [gameStatus], 0
    nextRandomNumber
    .if al & 10000000b
        or [gameStatus], 00000010b ; blue first
        and [gameStatus], 11111011b ; next is red
    .else
        and [gameStatus], 11111101b ; red first
        or [gameStatus], 00000100b ; next is blue
    .endif
    
    .while 1
        ; clear backbuffer
        mov ax, backBuffer
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
        mov dword ptr [fpuTemp], eax
        fild dword ptr [fpuTemp] ; st: [dClock]
        fmul [clockPeriod] ; st: [dt]
        fstp [deltaT] ; st: []
        
        ; calculate next position and velocity
        fld [deltaT] ; st: [dt]
        invoke step, addr redBall
        invoke step, addr blueBall
        fstp st ; st: []
        
        call clickHandler

        ; wall collision
        fld [heightMinusRadius] ; st: [h - r]
        fld [widthMinusRadius] ; st: [w - r][h - r]
        fld [circleRadius] ; st: [r][w - r][h - r]
        invoke wallCollision, addr [redBall]
        invoke wallCollision, addr [blueBall]
        fstp st ; st: [w - r][h - r]
        fstp st ; st: [h - r]
        fstp st ; st: []
        
        call ballCollsion

        ; round not over yet && all velocity == 0: end of round
        mov eax, 7fffffffh ; 01111...111b, float has +0 and -0
        .if [gameStatus] & 1b && !([redBall.vx] & eax) && !([redBall.vy] & eax) && !([blueBall.vx] & eax) && !([redBall.vy] & eax)
            xor [gameStatus], 00000001b ; end round
            .if [gameStatus] & 00000100b ; who's next
                or [gameStatus], 00000010b ; blue's turn
                and [gameStatus], 11111011b ; next is red
            .else
                and [gameStatus], 11111101b ; red's turn
                or [gameStatus], 00000100b ; next is blue
            .endif
        .endif
        
        call draw
        
        ; VSync
        ; https://stackoverflow.com/questions/67066755/assembly-x86-16-bit-vsync-screen-tearing
        mov dx, 03dah
        @@:
            in al, dx
            test al, 1000b
            jnz @b
        @@:
            in al, dx
            test al, 1000b
            jz @b
        
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
        
        invoke printScore, [redball.score], 28h, 19, 1
        invoke printScore, [blueBall.score], 37h, 19, 23
        
        ; game over judgement
        .if sbyte ptr [redBall.score] >= 10
            lea si, [redBall]
            .break
        .elseif sbyte ptr [blueBall.score] >= 10
            lea si, [blueBall]
            .break
        .elseif sbyte ptr [redBall.score] <= -10
            lea si, [blueBall]
            .break
        .elseif sbyte ptr [blueBall.score] <= -10
            lea si, [redball]
            .break
        .endif
        
        mov ah, 06h
        mov dl, 0ffh
        int 21h
        .continue .if zero?
        .if al == 1bh ; esc
            ret
        .elseif al == 'h' || al == 'H'
            call helpPage
            rdtsc
            mov [lastTimerCount], eax
        .endif
    .endw
    
    ; print win message
    moveTextCursor 15, 12
    printString (circle ptr [si]).id
    printString winMessage
    ; print play again, exit message
    moveTextCursor (40 - (lengthof playAgainMessage - 1)) / 2, 16
    printString [playAgainMessage]
    moveTextCursor (40 - (lengthof exitMessage - 1)) / 2, 18
    printString [exitMessage]
    
    .repeat
        mov ah, 00h
        int 16h
        .if al == 1bh ; esc
            call exitGame
        .endif
    .until al == 0dh ; enter
        
    ret
gameStart endp
generateWall proc
    mov [wallEffect + 0 * 2], increaseScore ; at least one increase score
    mov [wallColor + 0 * 1], 2fh ; green
    mov esi, 1
    .while esi != 26
        ;  increaseScore: 15% = ~ 644245094
        ; increase3Score: 2% = ~ 730144440
        ;  decreaseScore: 5% = ~ 944892805
        ;      swapBalls: 2% = ~ 1030792151
        ;      extraTurn: 2% = ~ 1116691497
        ;       noEffect: remain% ~ 4294967295
        nextRandomNumber
        .if [randomNumber] <= 644245094
            mov [wallEffect + esi * 2], increaseScore
            mov byte ptr [wallColor + esi * 1], 2fh ; green
        .elseif [randomNumber] <= 730144440
            mov [wallEffect + esi * 2], increase3Score
            mov byte ptr [wallColor + esi * 1], 34h ; aqua
        .elseif [randomNumber] <= 944892805
            mov [wallEffect + esi * 2], decreaseScore
            mov byte ptr [wallColor + esi * 1], 28h ; red
        .elseif [randomNumber] <= 1030792151
            mov [wallEffect + esi * 2], swapBalls
            mov byte ptr [wallColor + esi * 1], 22h ; purple
        .elseif [randomNumber] <= 1116691497
            mov [wallEffect + esi * 2], extraTurn
            mov byte ptr [wallColor + esi * 1], 2ch ; yellow
        .else
            mov [wallEffect + esi * 2], noEffect
            mov byte ptr [wallColor + esi * 1], 0fh ; white
        .endif
        
        inc esi
    .endw
    ; shuffle wall
    ; https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle#The_modern_algorithm
    mov bl, lengthof wallEffect ; 26, divisor
    mov ecx, lengthof wallEffect - 1 ; 25
    @@:
        nextRandomNumber
        xor ah, ah ; avoid division overflow
        div bl ; random / bl = al ... ah, 0 <= ah <= cx
        movzx edi, ah
        ; exchange wallEffect[ecx], wallEffect[edi]
        mov ax, [wallEffect + ecx * 2]
        xchg ax, [wallEffect + edi * 2]
        mov [wallEffect + ecx * 2], ax
        ; exchange wallColor[ecx], wallColor[edi]
        mov al, [wallColor + ecx * 1]
        xchg al, [wallColor + edi * 1]
        mov [wallColor + ecx * 1], al
        
        dec bl
        loop @b
    
    ret
generateWall endp
step proc, pBall: near ptr circle ; st: [dt]
    mov si, [pBall]
    assume si: near ptr circle
    
    ; newX = x + vx * dt
    fld [si].vx ; st: [vx][dt]
    fmul st, st(1) ; st: [vx * dt][dt]
    fadd [si].x ; st: [x + vx * dt][dt] = [newX][dt]
    fist [si].integerX
    fstp [si].x ; st: [dt]
    ; newY = y + vy * dt
    fld [si].vy ; st: [vy][dt]
    fmul st, st(1) ; st: [vy * dt][dt]
    fadd [si].y ; st: [y + vy * dt][dt] = [newY1][dt]
    fist [si].integerY
    fstp [si].y ; st: [dt]
    
    ; friction:
    ;   f = m * a = k * n * (-v / ||v||), m = mass = 1, a = accelleration,
    ;   k = friction coefficient, n = normal force = 1
    ;   a = -k * n / m * (-v / ||v||) = -k * v / ||v||
    ;   v = v0 + a0 * dt = v0 - k * v0 / ||v0|| * dt 
    ;     = v0 * (1 - k * dt / ||v0||), k > 0, 1 - k * dt / ||v0|| >= 0
    fld [si].vx ; st: [vx][dt]
    fmul st, st ; st: [vx^2][dt]
    fld [si].vy ; st: [vy][vx^2][dt]
    fmul st, st ; st: [vy^2][vx^2][dt]
    faddp st(1), st ; st: [vx^2 + vy^2][dt]
    fsqrt ; st: [||v||][dt]
    fdivr st, st(1) ; st: [dt / ||v||][dt]
    fmul [frictionCoefficient] ; st: [k * dt / ||v||][dt]
    fld1 ; st: [1][k * dt / ||v||][dt]
    fsubrp st(1), st ; st: [1 - k * dt / ||v||][dt]
    ftst ; if v < 0: v = 0
    fstsw ax
    fwait
    sahf
    jae @f ; 1 - k * dt / ||v|| >= 0
        fstp st ; st: [dt]
        fldz ; st: [0][dt]
    @@:
    fld [si].vx ; st: [vx][1 - k * dt / ||v||][dt]
    fmul st, st(1) ; st: [vx * (1 - k * dt / ||v||)][1 - k * dt / ||v||][dt]
    fstp [si].vx ; st: [1 - k * dt / ||v||][dt]
    fmul [si].vy ; st: [vy * (1 - k * dt / ||v||)][dt]
    fstp [si].vy ; st: [dt]
        
    assume si: nothing
    ret
step endp
clickHandler proc
    mov ax, 03h
    int 33h
    shr cx, 1 ; X max: 640 -> 320
    ; wait for negative edge
    test [mouseButtonStatus], 001b
    jz @f ; not pressed
    test bl, 001b
    jnz @f ; pressed but not released
    test [gameStatus], 00000001b
    jnz @f ; round not over yet
        .if [gameStatus] & 00000010b
            lea si, [blueBall]
        .else
            lea si, [redBall]
        .endif
        assume si: near ptr circle
        or [gameStatus], 00000001b ; new round start
        mov word ptr [fpuTemp], cx
        fild word ptr [fpuTemp] ; st: [mouseX]
        fsub [si].x ; st: [mouseX - x1]
        fadd st, st ; st: [(mouseX - x1) * 2]
        fstp [si].vx ; st: []
        mov word ptr [fpuTemp], dx
        fild word ptr [fpuTemp] ; st: [mouseY]
        fsub [si].y ; st: [mouseY - y1]
        fadd st, st ; st: [(mouseY - y1) * 2]
        fstp [si].vy ; st: []
        assume si: nothing
    @@:
    mov [mouseButtonStatus], bl
    
    ret
clickHandler endp
wallCollision proc, pBall: near ptr circle ; st: [radius][width- radius][height - radius]
    ; 40 pixels per section
    ; 
    ;     0  1  2  3  4  5  6  7
    ;    ┌───────────────────────┐
    ; 25 │                       │ 8
    ; 24 │                       │ 9
    ; 23 │                       │ 10
    ; 22 │                       │ 11
    ; 21 │                       │ 12
    ;    └───────────────────────┘
    ;     20 19 18 17 16 15 14 13
    
    mov si, [pBall]
    assume si: near ptr circle
    ; r = radius, w = width, h = height
    
    ; left wall
    fld [si].x ; st: [x][r][w - r][h - r]
    fcomi_ 1 ; cmp x, r
    ja @f ; x > r
    bt [si].vx, 31 ; sign bit of float
    jnc @f ; vx > 0
        ; x <= r && vx < 0
        btr [si].vx, 31 ; neg -> pos
        ; x = x + 2(r - x) = 2r - x
        fld st(1) ; st: [r][x][r][w - r][h - r]
        fadd st, st ; st: [2r][x][r][w - r][h - r]
        fsub st, st(1) ; st: [2r - x][x][r][w - r][h - r]
        fst [si].x
        fistp [si].integerX ; st: [x][r][w - r][h - r]
        
        ; wall effect index = 2 * (wall section)
        ;   = 2 * (25 - floor(y / 40))
        mov ax, [si].integerY 
        mov bl, 40
        div bl ; y / 40 = al ... ah, floor(y / 40) = al
        xor ah, ah
        neg ax
        add ax, 25
        shl ax, 1
        mov bx, ax
        cmp [wallEffect + bx], noEffect
        je topBottomCollision
        invoke [wallEffect + bx]
        jmp topBottomCollision
    @@:
    ; st: [x][r][w - r][h - r]
    ; right wall
    fcomi_ 2 ; cmp x, w - r
    jb @f ; x < w - r
    bt [si].vx, 31 ; sign bit of float
    jc @f ; vx < 0
        ; x >= w - r && vx > 0
        bts [si].vx, 31 ; pos -> neg
        ; x = x - 2(x - (w - r)) = 2(w - r) - x
        fld st(2) ; st: [w - r][x][r][w - r][h - r]
        fadd st, st ; st: [2(w - r)][x][r][w - r][h - r]
        fsub st, st(1) ; st: [2(w - r) - x][x][r][w - r][h - r]
        fst [si].x
        fistp [si].integerX ; st: [x][r][w - r][h - r]
        
        ; wall effect index = 2 * (wall section)
        ;   = 2 * (8 + floor(y / 40))
        mov ax, [si].integerY
        mov bl, 40
        div bl ; y / 40 = al ... ah, floor(y / 40) = al
        xor ah, ah
        add ax, 8
        shl ax, 1
        mov bx, ax
        cmp [wallEffect + bx], noEffect
        je topBottomCollision
        invoke [wallEffect + bx]
    @@:
    
    topBottomCollision:
    ; top wall
    fld [si].y ; st: [y][x][r][w - r][h - r]
    fcomi_ 2 ; cmp y, r
    ja @f ; y > r
    bt [si].vy, 31 ; sign bit of float
    jnc @f ; vy > 0
        ; y <= r && vy < 0
        btr [si].vy, 31 ; neg -> pos
        ; y = y + 2(r - y) = 2r - y
        fld st(2) ; st: [r][y][x][r][w - r][h - r]
        fadd st, st ; st: [2r][y][x][r][w - r][h - r]
        fsub st, st(1) ; st: [2r - y][y][x][r][w - r][h - r]
        fst [si].y
        fistp [si].integerY ; st: [y][x][r][w - r][h - r]
        
        ; wall effect index = 2 * (wall section)
        ;   = 2 * floor(x / 40)
        mov ax, [si].integerX
        mov bl, 40
        div bl ; x / 40 = al ... ah, floor(x / 40) = al
        xor ah, ah
        shl ax, 1
        mov bx, ax
        cmp [wallEffect + bx], noEffect
        je return
        invoke [wallEffect + bx]
        jmp return
    @@:
    
    ; bottom wall
    fcomi_ 4 ; cmp y, h - r
    jb @f ; y < h - r
    bt [si].vy, 31 ; sign bit of float
    jc @f ; vy < 0
        ; y >= h - r && vy > 0
        bts [si].vy, 31 ; pos -> neg
        ; y = y - 2(y - (h - r)) = 2(h - r) - y
        fld st(4) ; st: [h - r][y][x][r][w - r][h - r]
        fadd st, st ; st: [2(h - r)][y][x][r][w - r][h - r]
        fsub st, st(1) ; st: [2(h - r) - y][y][x][r][w - r][h - r]
        fst [si].y
        fistp [si].integerY ; st: [y][x][r][w - r][h - r]
        
        ; wall effect index = 2 * (wall section)
        ;   = 2 * (20 - floor(x / 40))
        mov ax, [si].integerX
        mov bl, 40
        div bl ; x / 40 = al ... ah, floor(x / 40) = al
        xor ah, ah
        neg ax
        add ax, 20
        shl ax, 1
        mov bx, ax
        cmp [wallEffect + bx], noEffect
        je return
        invoke [wallEffect + bx]
    @@:
    
    return:
        fstp st ; st: [x][r][w - r][h - r]
        fstp st ; st: [r][w - r][h - r]
        assume si: nothing
        ret
wallCollision endp
ballCollsion proc
    ; n = (s1 - s2) / |n| = (nx, ny), unit normal vector
    ; t = (tx, ty) = (-ny, nx), unit tangent vector
    ; |n|^2 <= (2 * r)^2: collision occur
    ; /nx tx\   /nx -ny\ , NT coordinate => XY coordinate
    ; \ny ty/ = \ny  nx/ , orthonormal basis
    ; /nx -ny\-1  /nx -ny\T  / nx ny\ , XY coordinate => NT coordinate
    ; \ny  nx/  = \ny  nx/ = \-ny nx/
    fld [redBall.y] ; st: [y1]
    fsub [blueBall.y] ; st: [y1 - y2 = dy]
    fld [redBall.x] ; st: [x1][dy]
    fsub [blueBall.x] ; st: [x1 - x2 = dx][dy]
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
        fld [redBall.vy] ; st: [v1y][-ny][nx][ny]
        fld [redBall.vx] ; st: [v1x][v1y][-ny][nx][ny]
        ; / nx ny\ /v1x\ = /v1n\
        ; \-ny nx/ \v1y/   \v1t/
        matrix_x_vector 3, 2, 4, 3, 0, 1 ; st: [v1n][v1t][-ny][nx][ny]
        fstp [fpuTemp] ; st: [v1t][-ny][nx][ny]
        fld [blueBall.vy] ; st: [v2y][v1t][-ny][nx][ny]
        fld [blueBall.vx] ; st: [v2x][v2y][v1t][-ny][nx][ny]
        ; / nx ny\ /v2x\ = /v2n\
        ; \-ny nx/ \v2y/   \v2t/
        matrix_x_vector 4, 3, 5, 4, 0, 1 ; st: [v2n][v2t][v1t][-ny][nx][ny]
        ; /nx -ny\ /v2n\ = v1': v1 after collision
        ; \ny  nx/ \v1t/
        matrix_x_vector 4, 5, 3, 4, 0, 2 ; st: [v1x'][v2t][v1y'][-ny][nx][ny]
        fstp [redBall.vx] ; st: [v2t][v1y'][-ny][nx][ny]
        fld [fpuTemp] ; st: [v1n][v2t][v1y'][-ny][nx][ny]
        ; /nx -ny\ /v1n\ = v2': v2 after collision
        ; \ny  nx/ \v2t/
        matrix_x_vector 4, 5, 3, 4, 0, 1 ; st: [v2x'][v2y'][v1y'][-ny][nx][ny]
        fstp [blueBall.vx] ; st: [v2y'][v1y'][-ny][nx][ny]
        fstp [blueBall.vy] ; st: [v1y'][-ny][nx][ny]
        fstp [redBall.vy] ; st: [-ny][nx][ny]
    @@:
    finit ; st: []
    
    ret
ballCollsion endp
printScore proc, score: byte, color: byte, x: byte, y: byte
    moveTextCursor x, y
    mov ah, 0eh
    xor bh, bh
    mov bl, color
    .if sbyte ptr [score] < 0
        neg [score]
        mov al, '-'
        int 10h
    .elseif sbyte ptr [score] == 0
        mov al, '0'
        int 10h
        ret
    .endif
    
    mov al, [score]
    xor ecx, ecx
    mov dl, 10
    .repeat
        xor ah, ah
        div dl
        or ah, 30h
        push ax
        inc ecx
    .until al == 0
    
    mov ah, 0eh
    @@:
        pop dx
        mov al, dh
        int 10h
        loop @b
    
    ret
printScore endp
draw proc
    mov ax, backBuffer
    mov es, ax
    ; draw line
    mov ax, 03h
    int 33h
    shr cx, 1
    mov si, [dashedlineFlow]
    dec [dashedlineFlow]
    and [dashedlineFlow], 0111b ; cycle from 0 to 7
    .if !([gameStatus] & 00000001b) ; if round not start yet
        .if [gameStatus] & 00000010b ; if player2's turn
            invoke drawLine, [blueBall.integerX], [blueBall.integerY], cx, dx
        .else
            invoke drawLine, [redBall.integerX], [redBall.integerY], cx, dx
        .endif
    .endif
    invoke drawCircle, [redBall.integerX], [redBall.integerY], 28h
    invoke drawCircle, [blueBall.integerX], [blueBall.integerY], 37h
    ; draw wall
    lea si, wallColor
    xor di, di
    .repeat
        mov al, [si]
        mov cx, 40
        rep stosb
        inc si
    .until si == offset [wallColor + 8]
    dec di
    .repeat
        mov al, [si]
        mov cx, 40
        @@:
            stosb
            add di, 320 - 1 ; width - stosb increment
            loop @b
        inc si
    .until si == offset [wallColor + 8 + 5]
    sub di, 320
    std
    .repeat
        mov al, [si]
        mov cx, 40
        rep stosb
        inc si
    .until si == offset [wallColor + 8 + 5 + 8]
    inc di
    .repeat
        mov al, [si]
        mov cx, 40
        @@:
            stosb
            add di, -(320 - 1) ; width - stosb increment
            loop @b
        inc si
    .until si == offset [wallColor + 8 + 5 + 8 + 5]
    cld
    
    ret
draw endp
drawCircle proc, centerX: word, centerY: word, color: byte
    ; di = (centerY - circleRadius) * 320 + (centerX - circleWidth[0] >> 1)
    mov ax, [centerY]
    sub ax, circleIntegerRadius
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
    add bx, (circleIntegerRadius * 2 + 1) * 2 ; 2bytes * (2r + 1)
    
    mov al, [color]
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
drawLine proc, x0: word, y0: word, x1: word, y1: word ; si would be the flashing clock
    local sx: word, sy: word, sydi: word, error: word, deltax: word, deltay: word
    mov ax, x1
    sub ax, x0
    .if sign? ; x1 - x0 < 0, check sign flag
        neg ax
        mov deltax, ax
        mov sx, -1
    .else
        mov deltax, ax
        mov sx, 1
    .endif
    mov bx, y1
    sub bx, y0
    .if sign? ; y1 - y0 < 0
        mov deltay, bx
        mov sy, -1
        mov sydi, -320
    .else
        neg bx
        mov deltay, bx
        mov sy, 1
        mov sydi, 320
    .endif
    add ax, bx
    mov error, ax ; error = dx + dy
    
    mov ax, 320
    mul y0
    add ax, x0
    mov di, ax
    
    ; cx = x0, dx = y0, bx = e2
    mov cx, x0
    mov dx, y0
    .while 1
        mov al, [dashedlineColor + si]
        mov es:[di], al
        inc si
        and si, 0111b ; clear si, if si > 7
        .break .if cx == x1 && dx == y1
        mov bx, error
        add bx, bx
        .if sword ptr bx >= deltay
            .break .if cx == x1
            mov ax, deltay
            add error, ax
            add cx, sx
            add di, sx
        .endif
        .if sword ptr bx <= deltax
            .break .if dx == y1
            mov ax, deltax
            add error, ax
            add dx, sy
            add di, sydi
        .endif
    .endw
    
    ret
drawLine endp

helpPage proc
    ; clear screen
    mov ax, 0a000h
    mov es, ax
    xor eax, eax
    xor di, di
    mov cx, (320 * 200) / 4
    rep stosd
    
    moveTextCursor 1, 1
    printString [gameIntroduction]
    
    invoke drawRectangle, 2 * 8, 12 * 8, 8, 8, 0fh ; white
    moveTextCursor 4, 12
    printString [normalWallMessage]
    invoke drawRectangle, 2 * 8, 14 * 8, 8, 8, 2fh ; green
    moveTextCursor 4, 14
    printString [increaseScoreMessage]
    invoke drawRectangle, 2 * 8, 16 * 8, 8, 8, 34h ; aqua
    moveTextCursor 4, 16
    printString [increase3ScoreMessage]
    invoke drawRectangle, 2 * 8, 18 * 8, 8, 8, 28h ; red
    moveTextCursor 4, 18
    printString [decreaseScoreMessage]
    invoke drawRectangle, 2 * 8, 20 * 8, 8, 8, 22h ; purple
    moveTextCursor 4, 20
    printString [swapBallsMessage]
    invoke drawRectangle, 20 * 8, 12 * 8, 8, 8, 2ch ; yellow
    moveTextCursor 22, 12
    printString [extraTurnMessage]
    
    mov ah, 00h
    int 16h
    
    ret
helpPage endp
drawRectangle proc, left: word, top: word, rectWidth: word, height: word, color: byte
    mov ax, 320
    mul top
    add ax, left
    mov di, ax
    mov bx, 320
    sub bx, rectWidth
    mov al, color
    mov cx, height
    @@:
        push cx
        mov cx, rectWidth
        rep stosb
        add di, bx
        pop cx
        loop @b
    ret
drawRectangle endp

exitGame proc
    mov ax, 03h
    int 10h
    .exit
exitGame endp

; wall effect function
noEffect proc
    nop
noEffect endp
increaseScore proc
    inc (circle ptr [si]).score
    ret
increaseScore endp
increase3Score proc
    add (circle ptr [si]).score, 3
    ret
increase3Score endp
decreaseScore proc
    dec (circle ptr [si]).score
    ret
decreaseScore endp
swapBalls proc
    mov eax, redBall.x
    xchg eax, blueBall.x
    mov redBall.x, eax
    mov eax, redBall.y
    xchg eax, blueBall.y
    mov redBall.y, eax
    mov eax, redBall.vx
    xchg eax, blueBall.vx
    mov redBall.vx, eax
    mov eax, redBall.vy
    xchg eax, blueBall.vy
    mov redBall.vy, eax
    mov ax, redBall.integerX
    xchg ax, blueBall.integerX
    mov redBall.integerX, ax
    mov ax, redBall.integerY
    xchg ax, blueBall.integerY
    mov redBall.integerY, ax
    
    ret
swapBalls endp
extraTurn proc
    .if si == offset [redball]
        and [gameStatus], 11111011b ; next is red
    .else
        or [gameStatus], 00000100b ; next is blue
    .endif
    ret
extraTurn endp

.fardata coverImage
image \
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 17,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,21,25,28,29,30,31,28,17,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,17,24,31,31,31,31,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,21,30,31,31,31
    db 31,31,31,31,30,17,16,16,16,16,16,16,16,16,16,16,16,16,16,16,17,30,31,31,31,31,27,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,24,31,31,31,31,31,31,31,31,31,31,24,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,17,31,31,31,31,31,21,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,23,31,31,31,31,31,28,31,31,31,31,31,17,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,30,31,31,31,31,22,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,25,31,31,31,31,31,19,16
    db 28,31,31,31,31,25,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,30,31,31,31,31,23,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,21,31,31,31,31,31,25,16,16,23,31,31,31,31,31,17,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,25,31,31,31,31,24,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,21,31,31,31,31,31,23,16,16,17,31,31,31,31,31,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,31,31,31,31,26,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,22,27,27,21,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,17,17,17,17,17,17,17,17
    db 16,16,16,16,16,16,16,16,17,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,17,31,31,31,31,31,21,16,16
    db 18,31,31,31,31,21,16,16,16,16,16,21,25,28,28,23,16,16,16,16,16,25,31,31,31,30,19,16,16,16
    db 16,16,22,26,28,28,22,16,16,16,16,17,27,31,31,31,27,16,18,29,28,19,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,19,21,21,20,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,23,16
    db 17,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,21,31,31,31,31,31,17,16,16,23,31,31,30,22,16,16,16,16,17
    db 26,31,31,31,31,31,31,17,16,16,16,28,31,31,31,29,17,16,16,16,18,27,31,31,31,31,31,25,16,16
    db 16,28,31,31,31,31,20,16,31,31,31,31,17,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,22,26
    db 27,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,28,19,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,26,31,31,31,31,31,24,16,16,19,21,19,16,16,16,16,16,18,30,31,31,31,31,31,30,29,17,16,16
    db 16,26,31,31,31,31,25,16,16,19,31,31,31,31,31,31,29,25,16,16,16,27,31,31,31,31,22,27,31,31
    db 31,31,20,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,17,25,25,25,28,29,29,29,30,30,30,31,31
    db 31,31,31,31,31,31,31,31,31,27,20,17,17,16,16,16,17,17,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,28,31,31,31,31,31,28,16,16
    db 16,16,16,16,16,16,16,16,25,31,31,31,31,30,19,16,16,16,16,16,16,26,31,31,31,31,23,16,16,29
    db 31,31,31,31,28,18,16,16,16,16,16,21,31,31,31,31,31,31,31,31,31,31,21,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,21,21,19,20,20,20,19,22,29,28,27,30,31,31,31,31,30,30,31,31,31,31,27
    db 26,27,27,27,27,27,27,27,27,17,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,28,31,31,31,31,31,16,16,16,16,16,16,16,16,16,16,16,27,31
    db 31,31,31,18,16,22,29,30,27,16,16,26,31,31,31,31,22,16,17,31,31,31,31,30,16,16,24,30,29,25
    db 16,17,31,31,31,31,31,31,31,31,31,31,20,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,22,21,19
    db 20,19,19,19,22,28,27,26,29,31,30,30,28,28,27,29,31,31,31,31,31,31,30,30,30,30,30,30,30,31
    db 21,18,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,27,31,31,31,31,31,21,16,16,16,16,16,16,16,16,16,17,31,31,31,31,23,16,24,31,31,31,31,16
    db 16,25,31,31,31,31,21,16,23,31,31,31,31,17,16,26,31,31,31,31,16,19,31,31,31,31,31,31,27,22
    db 25,30,18,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,22,21,19,20,20,20,19,22,28,23,20,21,21
    db 21,21,26,27,26,28,31,31,31,31,31,31,31,31,31,31,31,31,31,31,28,20,18,17,17,17,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,27,31,31,31,31,31,26,16,16
    db 16,16,16,16,16,16,16,25,31,31,31,31,16,16,31,31,31,31,31,23,16,26,31,31,31,31,20,16,30,31
    db 31,31,25,16,18,31,31,31,31,31,19,19,31,31,31,31,31,21,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,22,21,19,20,20,20,19,22,28,22,19,19,19,19,19,25,21,21,21,22,21,21,26
    db 27,28,28,30,31,31,31,31,31,31,30,30,30,30,30,29,17,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,22,31,31,31,31,31,24,16,16,16,16,16,16,16,16,16,30,31,31
    db 31,29,16,16,21,31,31,31,31,30,16,30,31,31,31,31,19,17,31,31,31,31,24,16,16,23,31,31,31,31
    db 27,17,31,31,31,31,24,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,22,21,19
    db 20,20,20,19,22,28,22,19,20,20,20,20,25,18,19,19,19,19,18,24,26,27,28,30,31,31,31,31,31,31
    db 31,31,31,31,31,30,19,17,17,18,17,19,19,20,20,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,30,31,31,31,31,24,16,16,16,16,20,21,16,16,17,31,31,31,31,29,16,16,23,31,31,31,31,27
    db 16,30,31,31,31,31,19,19,31,31,31,31,22,16,16,25,31,31,31,31,24,17,31,31,31,31,26,17,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,22,21,19,20,20,20,19,22,28,22,19,20,20
    db 20,20,25,19,20,20,20,20,19,25,27,23,23,24,24,24,24,24,24,27,29,29,29,31,31,31,30,30,29,29
    db 29,29,29,29,29,28,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,24,31,31,31,31,31,23,16,16
    db 16,16,28,31,25,16,16,31,31,31,31,31,20,16,26,31,31,31,31,19,16,31,31,31,31,31,20,18,31,31
    db 31,31,28,17,16,28,31,31,31,31,17,16,31,31,31,31,31,21,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,21,21,19,20,20,20,19,22,28,22,19,20,20,20,20,25,19,20,20,20,20,19,25
    db 28,20,19,19,19,19,19,19,19,23,28,27,27,31,31,31,31,31,31,31,31,31,31,31,31,30,22,16,17,17
    db 16,16,16,17,17,17,17,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,28,31,31,31,31,31,26,16,16,16,17,31,31,31,24,16,30,31,31
    db 31,31,28,16,18,31,31,31,31,27,16,21,31,31,31,31,21,18,31,31,31,31,31,21,16,20,31,31,31,31
    db 23,17,31,31,31,31,31,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,21,21,19
    db 20,20,20,19,22,28,22,19,20,20,20,20,25,19,20,20,20,20,19,25,27,21,19,20,20,20,20,20,19,24
    db 25,23,24,26,26,26,27,29,29,29,31,31,31,31,31,30,22,26,25,29,29,29,29,29,29,29,22,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,19,31,31,31,31,31,31,23,16,16,30,31,31,31,31,18,26,31,31,31,31,22,16,18,31,31,31,31,25
    db 16,22,31,31,31,31,24,16,31,31,31,31,31,17,16,19,31,31,31,31,20,18,31,31,31,31,31,18,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,21,21,19,20,20,20,19,23,28,22,19,20,20
    db 20,20,25,19,20,20,20,20,19,24,27,21,19,20,20,20,20,20,20,25,22,19,20,19,19,19,22,27,27,28
    db 31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,30,19,17,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,29,31,31,31,31,31,31,26
    db 29,31,31,31,31,31,17,24,31,31,31,31,27,16,21,31,31,31,31,21,16,28,31,31,31,31,21,16,31,31
    db 31,31,31,21,16,24,31,31,31,31,17,17,31,31,31,31,31,19,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,21,21,19,20,20,20,19,22,28,22,19,20,20,20,20,25,19,20,20,20,20,19,24
    db 27,21,19,20,20,20,20,20,20,25,22,19,20,20,20,19,23,26,23,25,27,27,27,29,29,29,30,31,31,31
    db 31,31,31,31,31,31,30,28,28,18,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,20,31,31,31,31,31,31,31,31,31,31,31,31,23,16,18,23,31
    db 31,31,31,26,30,31,31,31,31,17,16,21,31,31,31,31,19,16,20,23,31,31,31,31,26,31,31,31,31,30
    db 16,16,29,31,31,31,31,18,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,22,20,18
    db 19,19,19,18,22,28,22,19,20,20,20,20,25,19,20,20,20,20,19,24,27,21,19,20,20,20,20,20,20,25
    db 22,19,20,20,20,20,24,24,17,18,18,18,17,25,25,26,27,31,31,31,31,31,31,31,31,31,31,31,31,29
    db 19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,26,31,31,31,31,31,31,31,31,31,31,27,16,16,16,19,31,31,31,31,31,31,31,31,31,19,16
    db 16,28,31,31,31,31,29,16,16,21,31,31,31,31,31,31,31,31,31,18,16,19,31,31,31,31,31,19,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,21,21,19,20,20,20,20,22,27,22,19,20,20
    db 20,20,25,19,20,20,20,20,19,24,27,21,19,20,20,20,20,20,20,25,22,19,20,20,20,20,24,24,18,20
    db 20,20,19,26,26,26,27,30,29,29,29,30,30,30,30,30,30,30,30,29,27,18,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,24,31,31,31,31,31
    db 30,25,28,25,16,16,16,16,19,27,31,31,31,31,31,30,29,21,16,16,17,31,31,31,31,31,31,21,16,21
    db 30,31,31,31,31,31,30,26,18,16,16,22,31,31,31,31,31,23,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,21,27,27,27,27,27,27,26,22,19,19,19,19,20,25,19,20,20,20,20,19,24
    db 27,21,19,20,20,20,20,20,20,25,22,19,20,20,20,20,24,24,18,20,20,20,20,26,24,20,20,20,20,20
    db 20,28,28,26,27,27,27,27,26,28,31,31,20,18,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,17,23,26,27,25,22,19,22,18,16,16,16,16,16,18
    db 30,31,31,31,25,16,16,16,16,16,17,26,30,31,31,31,22,17,16,16,16,25,29,31,31,24,16,16,16,16
    db 16,23,31,31,31,31,29,17,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,19,28
    db 29,29,29,29,28,26,22,19,19,19,19,19,25,19,20,20,20,20,19,24,27,20,19,19,19,19,19,19,19,25
    db 22,19,20,20,20,20,24,24,18,20,20,20,20,26,23,19,19,19,19,19,19,28,28,26,27,27,27,27,26,28
    db 31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,18,25,28,29,31,31,31,31,21,16,16,17,25,31,31,31,31,21,16,16,16,16,16,16
    db 16,16,16,21,21,17,16,16,16,16,16,16,16,17,17,16,16,16,16,16,16,25,31,31,31,31,28,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,20,22,21,22,21,22,21,23,27,25,25,25,25
    db 25,22,25,19,19,19,19,19,19,24,27,20,19,19,19,19,19,19,19,25,22,19,20,20,20,20,23,24,18,20
    db 20,20,20,26,23,19,19,19,19,19,19,28,28,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,25,31,31,31,31
    db 31,31,31,31,31,21,16,24,31,31,31,31,31,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,29,31,31,31,31,28,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,22,20,18,20,19,19,19,22,27,27,29,28,29,29,27,25,18,19,19,19,19,18,24
    db 27,26,26,26,26,26,26,27,26,27,21,19,20,20,20,20,23,24,19,20,20,20,20,26,24,20,19,19,20,19
    db 19,28,28,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,29,31,31,31,31,31,31,31,31,31,31,22,16,22,31,31
    db 31,31,30,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,24,31,31,31,31,24,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,22,20,19
    db 20,20,20,19,22,28,24,22,22,22,22,23,26,24,24,25,25,25,24,26,26,29,30,30,30,30,30,30,29,28
    db 21,19,19,19,19,19,23,24,19,20,20,20,20,26,24,20,20,20,20,19,19,28,28,26,27,27,27,27,26,28
    db 31,31,27,20,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,18,30,31,31,31,31,27,19,29,31,31,31,31,18,16,20,31,31,31,31,31,17,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,21,31,31,31,31,21,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,22,21,19,20,20,20,19,22,28,22,18,19,19
    db 19,19,26,27,27,28,28,29,29,27,26,24,23,23,23,23,23,24,23,26,21,18,19,19,19,19,22,24,19,20
    db 20,20,20,26,24,20,20,20,20,19,19,28,28,26,27,27,27,27,26,28,31,31,27,20,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,17,30,31,31,31,31,29,16,16
    db 26,31,31,31,31,30,16,19,31,31,31,31,31,17,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,21,31,31,31,31,27,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,23,21,19,20,20,20,19,22,28,23,19,20,20,20,20,25,22,22,23,23,24,24,25
    db 27,20,19,19,19,19,19,19,19,24,25,24,24,24,24,24,25,24,19,20,20,20,20,26,24,20,20,20,20,19
    db 19,28,28,26,27,27,27,27,26,28,31,31,27,20,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,23,31,31,31,31,31,24,16,16,20,31,31,31,31,31,16,16,27,31
    db 31,31,31,17,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,20,31,31,31,31,24,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,23,21,19
    db 20,20,20,19,22,28,23,19,20,20,20,20,25,19,20,19,19,19,19,24,27,21,19,20,20,20,20,20,19,23
    db 29,31,31,31,31,31,29,23,18,19,19,19,19,26,24,20,20,20,20,20,19,27,28,26,27,27,27,27,26,28
    db 31,31,27,20,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,19,31,31,31,31,31,23,16,16,17,31,31,31,31,29,16,16,26,31,31,31,31,18,16,16,16,16,16,17
    db 20,23,20,16,16,16,16,16,16,16,16,16,16,21,24,26,26,23,16,16,16,24,31,31,31,31,21,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,23,21,19,20,20,20,19,22,28,23,19,20,20
    db 20,20,25,19,20,20,20,20,19,24,27,21,19,20,20,20,20,20,19,24,26,27,27,27,27,27,26,25,22,22
    db 22,22,23,26,24,20,20,20,20,20,19,28,27,25,26,26,26,26,25,28,31,31,27,20,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,19,31,31,31,31,31,19,16,16
    db 21,31,31,31,29,17,16,18,31,31,31,31,24,16,16,16,16,23,27,31,31,31,31,26,23,18,16,16,16,16
    db 16,21,27,31,31,31,31,31,23,16,16,25,31,31,31,31,18,16,25,28,28,25,19,16,16,16,21,29,30,22
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,17,16,16
    db 16,16,16,16,16,16,16,24,21,19,20,20,20,19,22,28,23,19,20,20,20,20,25,19,20,20,20,20,19,24
    db 27,21,19,20,20,20,20,20,19,25,22,19,20,20,20,20,22,28,30,30,30,30,30,27,24,19,19,19,19,19
    db 18,28,25,21,23,23,23,23,22,26,31,31,27,20,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,24,31,31,31,31,31,17,16,16,24,30,29,25,16,16,16,18,31,31
    db 31,31,26,16,16,16,22,31,31,31,31,31,31,31,31,31,22,16,16,21,29,31,31,31,31,31,31,31,31,17
    db 16,22,31,31,31,31,16,23,31,31,31,31,31,18,16,21,31,31,31,31,17,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,17,27,27,27,27,27,17,17,17,18,27,27,27,27,27,27,27,27,27,17,17,24,21,19
    db 20,20,20,19,22,28,23,19,20,20,20,20,25,19,20,20,20,20,19,24,27,21,19,20,20,20,20,20,19,25
    db 22,19,20,20,20,20,22,27,26,26,26,26,27,27,24,20,20,20,20,20,19,28,26,22,24,24,24,24,22,26
    db 31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,27,31,31,31,31,31,28,16,16,16,16,16,16,16,16,16,17,31,31,31,31,31,18,16,17,31,31,31,31
    db 31,31,31,31,31,31,31,17,16,31,31,31,31,31,31,30,28,31,31,18,16,20,31,31,31,31,25,30,31,31
    db 31,31,31,30,16,25,31,31,31,31,26,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,17,19,26,26,26,26
    db 26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,27,21,19,19,19,19,19,22,28,23,19,20,20
    db 20,20,25,19,20,20,20,20,19,24,27,21,20,20,20,20,20,20,19,25,22,19,20,20,20,20,23,24,19,20
    db 20,20,20,26,26,28,27,27,27,27,26,29,30,29,29,29,29,29,29,30,31,31,27,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,28,31,31,31,31,31,24,16,16
    db 16,16,16,16,16,16,16,17,31,31,31,31,31,17,16,17,30,26,19,18,20,24,31,31,31,31,30,16,19,31
    db 31,31,31,31,23,16,16,20,21,16,16,20,31,31,31,31,31,31,31,31,31,31,31,31,16,25,31,31,31,31
    db 24,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,19,27,26,26,26,26,26,26,26,26,26,26,26,26,26,26
    db 26,26,26,26,26,26,25,26,21,19,19,19,19,19,22,28,23,19,20,20,20,20,25,19,20,20,20,20,19,24
    db 27,21,20,20,20,20,20,20,19,25,22,19,20,20,20,20,23,24,18,20,20,20,20,26,26,28,28,28,28,28
    db 27,29,31,31,31,31,31,31,31,31,31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,28,31,31,31,31,30,16,16,16,16,16,16,16,16,16,16,17,31,31
    db 31,31,30,16,16,16,16,16,16,16,16,16,27,31,31,31,27,16,24,31,31,31,31,19,16,16,16,16,16,16
    db 16,21,31,31,31,31,31,31,25,27,31,31,31,31,16,17,28,31,31,23,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,18,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,25,25,23,25
    db 26,26,26,26,26,27,22,19,19,19,19,19,25,19,20,20,20,20,19,24,27,21,20,20,20,20,20,20,19,25
    db 22,19,20,20,20,20,23,24,19,20,20,20,20,26,24,21,21,21,21,21,20,27,29,26,26,26,26,26,26,28
    db 31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,27,31,31,31,31,31,24,16,16,16,16,16,16,16,16,16,16,31,31,31,31,29,16,16,16,16,16,23,27
    db 28,27,29,31,31,31,30,16,26,31,31,31,31,24,28,30,28,23,16,16,16,23,31,31,31,31,31,22,16,27
    db 31,31,31,31,18,16,16,19,18,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,17,23,26,26,26,26
    db 26,26,26,26,26,26,26,26,26,26,26,26,26,27,27,27,27,27,23,27,29,29,29,29,28,27,22,17,18,18
    db 18,19,25,19,20,20,20,20,19,24,27,21,20,20,20,20,20,20,19,25,22,19,20,20,20,20,23,24,19,20
    db 20,20,20,26,23,19,19,19,19,19,18,27,28,26,26,26,26,26,25,27,31,31,27,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,27,31,31,31,31,31,25,16,16
    db 16,16,16,16,16,16,16,19,31,31,31,31,30,16,16,16,21,29,31,31,31,31,31,31,31,31,30,16,28,31
    db 31,31,31,31,31,31,31,31,26,16,16,19,31,31,31,31,29,16,23,31,31,31,31,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,17,19,19,19,19,19,19,19,20,20,20,20,20,27,27
    db 27,27,27,27,27,27,27,28,21,21,23,23,23,23,24,27,25,25,25,25,25,26,25,19,20,20,20,20,19,24
    db 27,21,20,20,20,20,20,20,20,25,22,19,20,20,20,20,23,24,19,20,20,20,20,26,23,19,20,20,20,20
    db 19,27,28,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,18,30,31,31,31,31,25,16,16,16,16,16,16,16,16,16,23,31,31
    db 31,31,30,16,16,24,31,31,31,31,31,30,31,31,31,31,27,16,25,31,31,31,31,31,31,31,31,31,31,24
    db 16,16,29,31,31,31,25,16,22,31,31,31,31,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,23,20,18
    db 19,19,19,19,22,28,26,25,25,25,25,26,25,19,19,19,19,19,18,24,27,21,19,20,20,20,20,20,20,25
    db 22,19,20,20,20,20,23,24,19,20,20,20,20,26,23,19,19,20,20,19,19,27,29,26,27,27,27,27,26,28
    db 31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,30,31,31,31,31,24,16,16,16,16,23,26,16,16,16,22,31,31,31,31,31,16,16,31,31,31,31,30
    db 22,17,30,31,31,31,28,16,16,20,22,30,28,26,31,31,31,31,31,31,17,20,31,31,31,31,25,16,19,31
    db 31,31,31,30,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,22,21,19,20,20,20,19,22,28,22,20,20,20
    db 20,21,26,24,25,25,25,25,25,26,27,21,19,20,20,20,20,20,20,25,22,19,20,20,20,20,24,24,19,20
    db 20,20,20,26,23,19,20,20,20,19,19,27,29,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,26,31,31,31,31,31,24,16,16
    db 16,16,29,31,29,19,16,20,30,31,31,31,31,16,20,31,31,31,31,26,16,16,31,31,31,31,31,16,16,16
    db 16,16,16,16,25,30,31,31,31,31,19,21,31,31,31,31,25,16,23,31,31,31,31,28,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,22,21,19,20,20,20,19,22,27,21,19,20,20,20,20,26,29,31,31,31,31,31,28
    db 26,21,19,19,19,19,19,19,19,25,22,19,20,20,20,20,23,24,18,20,20,20,20,26,23,19,20,20,19,19
    db 19,27,29,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,24,31,31,31,31,31,30,16,16,16,23,31,31,31,31,17,16,25,31
    db 31,31,31,16,24,31,31,31,31,23,16,16,30,31,31,31,31,16,16,16,16,16,16,16,16,27,31,31,31,31
    db 16,21,31,31,31,31,27,16,23,31,31,31,31,27,16,16,16,20,21,17,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,22,21,19
    db 20,20,20,19,22,27,22,19,20,20,20,20,26,25,26,26,26,26,26,26,26,21,20,20,20,20,20,20,20,25
    db 22,19,20,20,20,20,23,24,19,20,20,20,20,26,23,19,20,20,19,20,19,27,29,26,27,27,27,27,26,28
    db 31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,17,31,31,31,31,31,31,27,16,20,31,31,31,31,31,18,17,31,31,31,31,31,16,27,31,31,31,31,29
    db 23,29,31,31,31,31,17,16,16,28,28,16,16,16,22,31,31,31,31,31,16,21,31,31,31,31,28,16,16,29
    db 31,31,31,27,16,16,28,31,31,30,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,22,21,19,20,20,20,19,22,27,22,19,20,20
    db 20,20,25,19,20,19,20,20,19,24,27,25,24,24,24,25,25,25,25,26,21,19,20,20,20,20,23,24,19,20
    db 20,20,20,26,23,19,19,20,20,20,19,27,28,26,26,26,26,26,25,28,31,31,27,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,26,31,31,31,31,31,31,30
    db 31,31,31,31,31,31,16,16,29,31,31,31,29,16,24,31,31,31,31,31,31,31,31,31,31,31,19,16,17,31
    db 31,30,27,30,31,31,31,31,31,23,16,22,31,31,31,31,21,16,22,31,31,31,31,26,16,23,31,31,31,31
    db 21,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,23,21,19,20,20,20,19,22,27,22,19,20,20,20,20,25,19,20,20,20,20,19,25
    db 27,27,27,27,27,27,27,27,26,27,21,19,19,19,19,19,23,24,19,20,20,20,20,26,23,19,19,19,19,19
    db 19,28,26,23,24,24,24,24,22,26,31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,17,31,31,31,31,31,31,31,31,31,31,31,31,20,16,16,29,31
    db 31,31,30,16,16,24,31,31,31,31,31,31,31,31,31,31,31,16,28,31,31,31,31,31,31,31,31,31,21,16
    db 16,22,31,31,31,31,21,16,28,31,31,31,31,27,16,24,31,31,31,31,31,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,17,19,18,17,16,17,16,16,16,16,17,18,27,27,27,27,27,27,27,27,27,27,27,27,28,21,19
    db 20,20,20,19,22,27,22,19,20,20,20,20,25,19,20,20,20,20,19,25,27,20,19,19,19,19,19,19,19,25
    db 22,19,20,20,20,20,23,24,19,20,20,20,20,25,23,20,20,20,20,20,18,28,24,21,23,23,23,23,20,24
    db 31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,20,31,31,31,31,31,31,31,31,31,31,20,16,16,21,31,31,31,31,31,26,16,21,31,31,31,31
    db 29,22,31,31,31,31,31,20,28,31,31,31,31,31,31,31,31,31,19,16,16,27,31,31,31,31,24,16,29,31
    db 31,31,31,31,17,16,31,31,31,31,21,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,27,26,26,26,27,27,17
    db 16,16,16,27,26,26,26,26,26,26,26,26,26,26,26,26,25,26,20,19,20,20,20,19,22,27,22,19,20,20
    db 20,20,25,19,20,20,20,20,19,25,27,20,19,19,19,19,19,19,19,24,29,29,29,29,29,28,26,24,18,19
    db 19,19,19,25,25,27,27,26,25,24,22,28,26,22,23,23,23,23,21,25,31,31,27,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,27,31,31,31,31
    db 25,21,25,20,16,16,16,24,31,31,31,31,31,27,16,16,22,31,31,28,16,16,31,31,31,31,31,19,16,29
    db 31,31,31,31,31,31,30,20,16,16,20,31,31,31,31,25,17,16,21,31,31,31,31,31,24,16,22,28,27,17
    db 16,16,16,16,16,16,16,16,16,19,20,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,27,26,26,26,27,27,18,16,16,16,27,25,26,26,26,26,26
    db 26,26,26,26,26,26,25,25,20,19,20,20,20,19,22,27,22,19,20,20,20,20,25,19,20,20,20,20,19,24
    db 27,21,20,20,20,20,20,20,19,24,30,31,31,30,31,30,28,23,18,20,20,20,20,25,26,29,29,29,29,28
    db 27,29,30,29,29,30,29,29,29,30,31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,19,23,28,31,23,20,19,22,25,25,20,16,16,16,18,24,28
    db 31,30,23,16,16,16,16,17,20,16,16,16,19,30,31,31,31,25,16,16,22,31,31,31,31,26,16,16,23,30
    db 31,31,21,22,17,16,16,16,16,18,28,30,30,27,18,16,16,16,16,16,16,16,16,16,16,16,16,21,29,31
    db 31,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,26,26,26,26,27,27,18,16,16,16,27,25,26,26,26,26,26,26,26,26,26,26,26,25,25,21,19
    db 20,20,20,19,22,27,22,19,20,20,20,20,25,19,20,20,20,20,19,24,27,21,19,20,20,20,20,20,20,25
    db 23,21,21,21,21,21,25,27,28,28,28,28,28,26,24,21,21,21,21,21,20,28,30,29,29,29,29,29,29,30
    db 31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,18,26,31,31,31,31,19,17,28,31,31,31,31,26,17,16,16,16,16,17,16,16,16,16,16,16,16,16,16
    db 16,16,23,30,31,31,31,28,16,21,28,31,31,31,31,17,16,20,31,31,31,30,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,31,31,31,31,19,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,23,27,27,22,19,20,17
    db 16,16,16,23,27,27,27,27,27,27,27,27,27,27,27,27,27,28,21,19,20,20,20,19,22,27,22,19,20,20
    db 20,20,25,19,20,20,20,20,19,24,27,21,19,20,20,20,20,20,20,25,21,18,19,19,18,19,24,29,31,31
    db 31,31,31,27,23,19,19,19,19,19,18,27,29,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,27,31,31,31,31,31,31,31,31
    db 31,31,31,31,31,26,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,31,31,31,31,31,23,16,28
    db 31,31,31,31,28,16,16,23,31,31,31,31,26,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,22
    db 28,29,28,17,16,16,18,31,31,31,31,30,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,17,17,16,16,16,16,16,16,16,16,17,16,17,17,17,18
    db 17,18,18,18,17,18,17,26,21,19,20,20,20,19,22,28,22,19,20,20,20,20,25,19,20,20,20,20,19,24
    db 27,21,19,20,20,20,20,20,20,25,22,19,20,20,19,20,25,25,24,24,24,24,24,26,23,20,20,20,20,20
    db 19,27,28,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,19,31,31,31,31,31,31,31,31,31,31,31,31,31,19,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,28,31,31,31,31,23,16,24,31,31,31,31,27,16,16,23,31,31
    db 31,31,27,16,16,16,16,16,16,16,16,16,16,16,16,16,16,19,29,31,31,31,31,16,16,16,19,31,31,31
    db 31,31,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,23,20,19
    db 20,20,20,19,22,28,22,19,20,20,20,20,25,19,20,20,20,20,19,24,27,21,20,20,18,18,19,20,20,26
    db 22,18,19,19,19,19,25,23,18,19,19,19,19,25,23,20,20,20,20,20,19,27,28,26,27,27,27,27,26,28
    db 31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,23,31,31,31,31,31,24,23,31,31,31,31,31,29,18,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,28,31,31,31,31,24,16,23,31,31,31,31,29,16,16,16,24,31,31,30,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,23,31,31,31,31,28,16,16,16,16,21,30,31,31,19,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,17,16,16,16,16,16,16,16,16,17,17,16,16,16,17,17,16,16,16,16
    db 16,16,16,16,16,16,16,16,17,17,17,17,17,17,17,16,16,24,20,18,19,19,19,18,21,28,23,19,20,20
    db 20,20,25,19,20,20,20,20,19,24,27,21,19,20,22,23,22,19,20,25,25,24,24,24,24,24,27,23,19,20
    db 20,20,20,25,23,20,20,20,20,20,19,28,28,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,22,31,31,31,31,31,16,16
    db 27,31,31,31,31,31,20,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,27,31,31,31,31,24,16,23
    db 31,31,31,31,29,16,16,16,16,17,21,17,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,18,31,31
    db 31,31,26,16,16,16,16,16,16,20,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,19,20,17,16,16,16,16
    db 21,19,16,16,16,16,16,16,19,27,27,27,27,27,27,27,27,20,20,20,20,20,20,20,21,21,27,27,27,27
    db 27,27,27,27,27,27,27,28,22,21,22,22,22,22,24,28,23,19,19,19,19,20,25,19,20,20,20,20,19,24
    db 27,21,19,20,29,31,27,19,20,24,29,31,30,30,30,30,28,23,19,20,20,20,20,26,23,20,20,20,20,20
    db 19,28,28,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,17,31,31,31,31,31,31,18,16,28,31,31,31,31,31,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,18,31,31,31,31,26,16,16,26,31,31,31,31,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,17,16,16,16,16,16,16,31,31,31,31,23,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,18,23,26,26,26,16,16,16,24,26,26,21,16,16,16,16,17,27,26
    db 26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,27,26,26,27,29
    db 29,29,29,30,29,26,23,18,19,19,19,19,25,19,20,20,20,20,19,24,27,21,19,20,28,30,27,19,20,24
    db 29,29,29,29,29,29,27,23,19,20,20,20,20,26,23,20,20,20,20,20,19,28,28,26,27,27,27,27,26,28
    db 31,31,27,20,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,29,31,31,31,31,31,18,16,29,31,31,31,31,25,16,16,16,16,16,17,22,24,27,28,23,17,16,16
    db 16,16,21,31,31,31,31,22,16,18,29,31,31,31,27,16,16,16,16,21,23,23,22,16,16,16,16,16,22,26
    db 29,30,30,27,16,16,16,16,30,31,31,31,25,17,22,19,16,16,20,23,23,22,17,16,16,16,16,16,19,23
    db 25,25,23,20,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,21,27,25,25,27,16,16,16,27,26,26,27,17,16,16,16,20,26,26,26,26,26,26,26,26,26,26,26,26
    db 26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,25,27,28,27,28,29,28,27,24,21,21,21
    db 21,22,25,19,20,20,20,20,19,24,27,21,19,20,25,27,24,19,20,24,27,27,26,27,27,27,26,23,19,20
    db 20,20,20,26,23,20,20,20,20,20,19,28,28,26,27,27,27,27,26,28,31,31,27,20,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,25,31,31,31,31,27,16,20
    db 30,31,31,31,28,16,16,16,16,16,24,31,31,31,31,31,31,31,26,18,16,16,26,31,31,31,29,16,16,22
    db 31,31,31,31,18,16,16,20,29,31,31,31,31,16,16,16,21,28,31,31,31,31,31,31,24,16,20,28,31,31
    db 31,31,31,31,31,25,17,26,31,31,31,31,20,16,16,16,24,29,31,31,31,31,31,31,22,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,20,27,26,26,27,16,16,16,27
    db 26,26,25,17,16,16,16,19,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26
    db 26,26,26,26,26,26,26,26,20,19,20,20,20,20,23,27,29,30,30,30,30,30,26,19,20,20,20,20,19,24
    db 27,21,19,20,20,20,20,19,20,25,22,19,20,20,20,20,23,24,19,20,20,20,20,26,23,20,20,20,20,20
    db 19,28,28,26,27,27,27,27,26,28,31,31,27,26,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,26,31,31,31,31,27,22,31,31,31,31,26,16,16,16,16,16,18
    db 31,31,31,31,31,31,31,31,31,31,20,16,25,31,31,31,31,24,16,20,31,31,31,31,29,16,16,31,31,31
    db 31,31,27,16,16,28,31,31,31,31,31,31,31,31,31,16,25,31,31,31,31,31,31,31,31,18,25,31,31,31
    db 31,31,16,16,16,25,31,31,31,31,31,31,31,31,22,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,17,20,27,27,20,16,16,16,22,27,27,19,16,16,16,16,16,27,27
    db 27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,28,20,18
    db 20,19,19,19,22,28,27,28,28,28,28,28,26,18,19,19,19,19,18,24,27,21,19,20,19,19,19,19,20,25
    db 21,19,20,20,20,19,23,24,19,20,20,20,20,26,23,20,20,20,20,20,19,28,28,26,26,27,27,26,25,28
    db 31,31,27,20,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,25,31,31,31,31,31,31,31,31,31,26,16,16,16,16,16,16,27,31,31,29,29,31,31,31,31,31,31
    db 27,16,24,31,31,31,31,25,16,20,31,31,31,31,30,16,16,27,31,31,31,31,24,16,21,31,31,31,31,31
    db 31,24,24,30,27,16,29,31,31,31,31,31,31,31,27,16,23,31,31,31,31,28,16,16,22,31,31,31,31,25
    db 23,31,31,31,20,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,17,16,16,16,16,27,27,27,27,27,27
    db 27,27,27,16,16,16,16,16,16,16,16,16,16,16,16,16,16,24,21,19,20,20,20,19,22,28,23,20,21,21
    db 21,22,25,18,18,18,18,18,17,24,27,21,19,19,19,20,20,19,20,25,22,19,20,20,20,19,23,24,19,20
    db 20,20,20,26,23,19,19,19,19,19,18,28,26,23,24,23,23,24,22,26,31,31,27,20,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,25,31,31,31,31,31,31,31
    db 31,31,25,25,31,19,16,16,16,22,24,18,16,16,16,24,31,31,31,31,20,16,24,31,31,31,31,24,16,20
    db 31,31,31,31,28,16,16,22,31,31,31,31,21,16,26,31,31,31,31,26,16,16,16,16,16,16,30,31,31,31
    db 31,29,24,26,16,16,17,31,31,31,31,25,16,16,27,31,31,31,27,16,21,31,31,31,26,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,17,17,17,16,16,16,19,27,23,20,16,16,17,17,17
    db 16,16,16,16,16,16,16,22,20,19,20,20,20,19,22,28,22,19,19,19,19,20,26,25,26,26,26,26,26,26
    db 26,24,21,20,20,21,21,19,20,25,22,19,20,20,20,19,23,24,19,20,20,20,20,26,23,19,19,18,18,18
    db 17,28,25,21,22,22,22,22,21,25,31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,25,31,31,31,31,31,31,31,31,31,31,31,31,31,17,16,16,16
    db 16,16,16,20,17,20,31,31,31,31,20,16,24,31,31,31,31,22,16,19,31,31,31,31,27,16,16,25,31,31
    db 31,31,19,16,29,31,31,31,30,16,21,23,18,16,16,16,16,20,31,31,31,29,19,16,16,16,21,31,31,31
    db 31,25,16,16,30,31,31,31,22,16,30,31,31,31,31,17,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,20,27,27,25,16,16,16,16,16,16,16,16,16,16,16,16,22,21,19
    db 20,20,20,19,22,28,23,19,19,20,20,20,26,22,22,22,22,22,22,25,26,27,27,27,26,28,25,19,20,25
    db 22,19,20,20,20,19,23,24,19,20,20,20,20,26,25,26,26,25,25,26,23,28,29,28,28,28,28,28,28,29
    db 31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,28,31,31,31,31,31,30,27,31,31,31,31,31,31,25,16,16,16,16,24,30,31,31,31,31,31,31,31
    db 24,16,23,31,31,31,31,22,16,19,31,31,31,31,26,16,16,28,31,31,31,28,16,16,31,31,31,31,31,30
    db 31,31,31,27,16,16,16,18,31,31,31,31,31,16,16,16,24,31,31,31,30,16,16,21,31,31,31,31,21,16
    db 24,31,31,26,17,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,20,26,26,26,16,16,16,16,16,16,16,16,16,16,16,16,22,21,19,20,20,20,19,22,29,23,19,20,20
    db 20,21,26,18,19,19,19,18,18,24,27,28,29,27,30,31,26,18,20,25,22,19,20,20,20,19,23,24,19,20
    db 20,20,20,26,26,30,30,30,31,31,29,30,31,31,31,31,31,31,31,31,31,31,27,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,25,31,31,31,31,31,20,16
    db 27,31,31,31,31,31,31,16,16,21,30,31,31,31,31,31,31,31,31,31,20,16,28,31,31,31,31,22,16,24
    db 31,31,31,31,27,16,16,27,31,31,31,30,18,16,31,31,31,31,31,31,31,31,31,31,27,16,16,20,31,31
    db 31,31,29,16,16,16,22,31,31,31,31,21,16,25,31,31,31,31,25,16,16,18,19,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,18,26,26,24,17,16,16,16,16
    db 16,16,16,16,16,16,16,22,21,19,19,19,19,19,20,23,21,18,19,19,19,19,21,19,19,19,19,20,19,24
    db 27,24,23,22,29,29,28,25,25,26,21,19,20,20,20,20,23,24,18,20,20,20,20,26,25,24,24,24,24,24
    db 23,29,29,28,29,29,29,29,28,29,31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,29,31,31,31,31,16,16,28,31,31,31,31,31,31,17,19,31
    db 31,31,31,31,27,27,31,31,31,31,17,16,30,31,31,31,31,23,16,26,31,31,31,31,28,16,16,26,31,31
    db 31,31,23,16,25,30,31,31,31,31,31,31,31,31,31,24,16,21,31,31,31,31,28,16,16,16,21,31,31,31
    db 31,28,16,28,31,31,31,31,22,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,19,20,18,16,16,16,16,16,16,16,16,16,16,16,16,22,21,19
    db 19,18,18,18,18,17,18,18,18,18,18,18,17,18,18,18,19,20,19,24,27,21,19,20,29,29,30,31,30,28
    db 21,19,20,20,20,20,23,24,18,20,20,20,20,26,23,19,19,19,19,19,18,27,28,26,26,26,26,27,25,28
    db 31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,21,30,31,31,31,31,18,16,20,30,31,31,31,31,31,17,24,31,31,31,31,23,16,23,31,31,31,31
    db 21,16,31,31,31,31,31,24,16,26,31,31,31,31,28,16,16,25,31,31,31,31,22,16,16,16,18,24,21,24
    db 31,31,31,31,31,31,17,16,29,31,31,31,29,16,16,16,20,31,31,31,31,25,16,28,31,31,31,31,22,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,22,21,19,18,18,18,18,18,18,18,18,18,18
    db 18,18,18,18,18,18,18,20,19,24,27,21,19,20,29,29,28,26,26,27,20,18,19,19,19,18,22,24,18,20
    db 20,20,20,26,23,20,20,20,20,20,19,27,28,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,28,31,31,31,31,31,17,16
    db 16,30,31,31,31,31,31,16,27,31,31,31,31,17,16,23,31,31,31,31,24,16,22,31,31,31,31,24,16,19
    db 28,31,31,31,29,16,16,28,31,31,31,31,21,16,16,16,16,16,16,16,18,30,31,31,31,31,16,17,31,31
    db 31,31,28,16,16,16,23,31,31,31,31,25,16,26,31,31,31,31,31,21,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,19,19,20,20,28,21,19,18,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,18,20,19,25
    db 28,21,19,20,29,29,25,19,20,25,24,22,22,22,22,22,24,23,18,20,20,20,20,26,23,20,20,20,20,20
    db 19,27,28,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,26,31,31,31,31,31,16,16,21,31,31,31,31,31,28,16,30,31
    db 31,31,31,16,16,27,31,31,31,31,20,16,18,31,31,31,31,25,16,16,27,31,31,31,31,16,16,22,31,31
    db 31,31,21,16,16,20,17,16,16,16,16,30,31,31,31,30,16,21,31,31,31,31,17,16,16,16,19,30,31,31
    db 31,25,16,23,31,31,31,31,31,19,16,18,17,20,26,18,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,17,17,16,16,16,16,16,16,16,16,16,24,25,25,25,26,20,19
    db 21,20,20,21,21,20,20,20,20,20,20,20,20,21,21,21,20,19,19,23,25,20,19,20,29,29,25,19,20,24
    db 30,31,31,31,31,31,29,23,18,20,20,20,20,26,23,20,20,20,20,20,19,27,28,26,27,27,27,27,26,28
    db 31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,27,31,31,31,31,31,22,22,31,31,31,31,31,27,17,17,31,31,31,31,31,28,29,31,31,31,31,22
    db 16,16,27,31,31,31,31,25,16,22,31,31,31,31,30,16,16,19,31,31,31,31,20,16,16,31,30,18,18,22
    db 29,31,31,31,31,26,16,21,31,31,31,31,27,18,16,16,16,29,31,31,31,24,16,21,31,31,31,31,31,29
    db 30,31,31,31,31,18,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,20,27,28,25,16,16,16,16,16,16,20,18,16,17,17,17,17,17,17,29,31,31,31,31,31,31,31,31,30
    db 30,30,30,30,31,31,25,17,18,17,17,18,17,18,29,29,26,20,22,25,29,30,30,30,30,29,28,23,18,19
    db 19,19,19,26,24,20,20,20,20,20,19,27,28,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,29,31,31,31,31,31,31,31
    db 31,31,31,31,31,24,16,17,25,31,31,31,31,31,31,31,31,31,31,28,18,16,20,31,31,31,31,21,16,16
    db 31,31,31,31,25,16,16,29,31,31,31,31,19,16,24,31,31,31,31,31,31,31,31,31,25,18,16,21,31,31
    db 31,31,31,19,16,16,24,31,31,31,31,24,16,17,23,29,31,31,31,31,31,31,31,31,31,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,27,24,23,26,20,16,16,16,16
    db 16,21,27,17,17,17,17,17,17,17,29,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,25,17,18,18
    db 18,18,17,18,28,29,28,28,28,29,29,27,26,21,20,20,23,23,17,19,19,19,18,26,24,20,20,20,20,20
    db 19,28,29,27,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,21,31,31,31,31,31,31,31,31,31,31,31,31,31,22,16,16,16,31
    db 31,31,31,31,31,29,31,31,31,31,24,16,25,31,31,31,31,26,16,20,31,31,31,31,30,18,16,29,31,31
    db 31,31,20,16,31,31,31,31,31,31,31,31,31,31,17,16,16,24,31,31,31,31,31,26,16,16,26,31,31,31
    db 31,27,16,16,16,31,31,31,31,31,31,31,31,31,21,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,27,23,22,25,21,16,16,16,16,16,22,28,17,18,18,18,18,18,17
    db 29,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,24,17,18,18,18,18,17,18,29,29,29,29,29,29
    db 29,27,26,20,19,19,21,28,29,29,29,29,29,27,23,20,20,20,20,20,19,27,30,28,28,28,28,28,28,29
    db 31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,20,31,31,31,31,31,31,31,31,31,31,31,29,19,16,16,16,16,23,31,31,31,31,18,25,31,31,31,31
    db 30,16,30,31,31,31,31,31,17,25,31,31,31,31,31,29,16,30,31,31,31,31,31,16,28,31,31,31,31,31
    db 31,31,31,28,18,16,16,26,31,31,31,31,31,31,24,16,28,31,31,31,31,31,20,16,16,23,31,31,31,31
    db 31,31,31,25,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 17,27,21,21,25,22,16,16,16,16,16,22,28,17,18,18,18,18,18,19,29,31,31,31,31,31,31,31,31,31
    db 31,31,31,31,31,31,25,17,18,18,18,18,18,18,29,29,29,29,29,29,29,27,26,21,19,19,21,29,31,31
    db 31,31,31,27,23,19,19,19,19,19,18,28,26,23,24,24,24,24,23,27,31,31,27,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,18,26,31,31,25,20,25,31
    db 31,29,27,19,16,16,16,16,16,22,30,31,31,18,16,25,31,31,31,28,23,16,29,31,31,31,31,27,17,24
    db 31,31,31,31,30,22,16,29,31,31,31,31,26,16,16,26,31,31,31,30,31,30,22,16,16,16,16,18,29,31
    db 31,31,31,29,17,16,29,31,31,31,31,27,19,16,16,16,20,29,31,31,28,27,24,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,18,25,25,19,17,16,16,16,16
    db 16,23,28,17,18,17,23,28,27,28,30,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,28,23,24,24
    db 24,24,24,22,21,21,21,22,23,23,23,26,27,21,19,19,22,25,22,23,22,23,23,26,23,18,18,18,18,18
    db 17,28,24,19,21,21,21,20,19,25,31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,18,24,21,21,25,30,24,19,29,30,28,16,16,18,29,31
    db 31,31,31,16,16,16,19,23,19,16,16,16,17,21,25,26,22,16,16,16,19,23,26,24,17,16,16,17,23,26
    db 22,17,16,16,16,16,18,21,18,16,20,16,16,16,16,16,16,16,16,21,27,25,21,16,18,27,31,31,31,31
    db 23,16,16,16,16,16,16,16,20,17,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,18,27,17,16,16,16,16,16,16,23,28,17,18,17,25,31,31,31
    db 31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,29,25,26,26,26,26,26,24,18,17,17,18,20,19
    db 19,25,27,21,19,19,23,23,18,19,19,19,19,25,26,25,25,25,25,25,24,29,28,27,27,27,27,27,26,29
    db 31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,21,28,31,31,31,31,30,30,31,31,31,16,16,26,31,31,31,31,31,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,26,31,31,31,31,31,19,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,17,25,26,26,26,16,16,16,24,26,26,21,16,17,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26
    db 26,26,26,26,26,26,26,26,26,17,16,23,28,17,18,17,25,31,31,31,31,31,31,31,31,31,31,31,31,31
    db 31,31,31,31,31,31,29,25,26,26,26,26,26,24,18,18,17,19,20,19,20,27,28,21,19,19,24,24,18,20
    db 19,19,19,26,26,27,27,27,26,26,25,29,31,31,31,31,31,31,31,31,31,31,27,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,20,31,31,31,31,31,31
    db 31,31,31,31,29,16,16,22,31,31,31,31,28,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,18,31,31,31,31,28,17,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,20,27,25,25,27,16,16,16,27
    db 26,26,26,18,17,26,25,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,19
    db 17,26,28,16,17,16,24,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,28,25,26,26
    db 26,26,26,24,18,17,17,18,20,19,21,28,29,21,19,19,23,24,19,20,20,20,19,26,24,21,21,21,21,21
    db 20,28,29,28,28,28,28,28,28,29,31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,29,31,31,31,31,31,29,30,31,31,31,30,16,16,18,31,31
    db 31,31,21,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,30,31,31,31,18
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,19,27,25,25,27,17,16,16,27,26,26,27,27,27,26,26,26,26,26
    db 26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,27,28,22,23,22,27,30,30,30
    db 31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,29,25,26,26,26,26,26,25,21,22,21,21,20,20
    db 19,27,29,24,23,23,25,23,19,20,20,20,19,26,24,19,19,19,19,19,19,27,28,26,27,26,26,27,25,28
    db 31,31,28,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,21,31,31,31,31,31,26,16,16,23,31,31,31,19,16,18,31,31,31,31,28,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,19,31,31,31,31,29,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,18,26,26,26,25,16,16,16,23,26,26,21,18,17,27,26,26,26,26,26,26,26,26,26,26,26,26,26,26
    db 26,26,26,26,26,26,26,26,26,22,21,21,24,31,31,31,30,29,29,29,30,31,31,31,31,31,31,31,31,31
    db 31,31,31,31,31,31,29,25,26,26,26,26,26,26,27,27,28,24,18,19,16,25,30,29,29,29,28,23,18,19
    db 19,20,20,26,24,20,20,20,20,20,19,27,28,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,29,31,31,31,31,27,16,16
    db 16,16,21,31,25,16,16,18,31,31,31,31,28,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,18,31,31,31,31,31,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,17,26,19
    db 17,17,19,30,29,29,29,29,29,29,30,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,29,25,26,26
    db 26,26,26,26,26,26,26,23,17,18,16,26,30,29,29,28,27,24,21,22,22,20,19,26,24,20,20,20,20,20
    db 19,27,28,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,20,31,31,31,31,31,24,17,16,16,16,16,18,16,16,16,21,31,31
    db 31,31,24,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,17,16,16,16,16,16,16,16,16,16,17,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,21,26,28,27,23,17,16,16,16,16,16,16,31,31,31,31,27
    db 16,16,16,16,16,16,16,17,17,16,16,16,16,16,16,16,16,17,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,18,24,27,27,25,19,16,16,21,24,21,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,17,27,19,18,17,19,30,29,29,29,29,29,29
    db 30,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,29,25,26,26,26,26,26,26,26,26,26,23,17,18
    db 16,26,30,29,29,29,26,27,29,29,29,22,19,26,24,20,20,20,20,20,19,28,28,26,27,27,27,27,26,28
    db 31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,28,31,31,31,31,31,31,31,28,18,16,16,16,16,16,16,23,31,31,31,31,22,16,21,26,27,24,17,16
    db 16,16,16,16,16,22,27,30,31,29,22,16,16,16,16,16,23,30,31,27,17,16,16,16,16,16,16,16,16,16
    db 16,16,23,31,31,31,31,31,17,16,16,19,24,26,28,31,31,31,31,24,16,16,16,16,18,26,29,31,31,25
    db 16,16,16,16,16,21,29,31,29,21,16,16,16,16,16,16,16,16,16,16,16,18,31,31,31,31,31,22,18,29
    db 31,31,31,22,16,22,26,29,29,18,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,17,26,19,17,17,19,30,29,29,29,29,29,29,31,31,31,31,31,31,31,31,31,31
    db 31,31,31,31,31,31,29,25,26,26,26,26,26,26,26,26,26,23,17,18,16,25,29,28,28,28,26,27,30,29
    db 29,21,19,26,24,20,20,20,20,20,19,28,28,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,27,31,31,31,31,31,31,31,31
    db 31,22,16,16,16,16,16,20,31,31,31,31,19,17,31,31,31,31,30,17,16,16,16,16,25,31,31,31,31,31
    db 31,23,16,16,16,24,31,31,31,31,19,16,16,16,19,26,30,28,26,18,16,16,24,31,31,31,31,25,16,16
    db 21,31,31,31,31,31,31,31,31,26,16,16,16,20,31,31,31,31,31,31,27,16,16,16,18,31,31,31,31,26
    db 16,16,16,16,23,29,29,27,21,16,16,18,31,31,31,31,31,16,27,31,31,31,31,19,19,31,31,31,31,31
    db 19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,20,22,26,26,24,19
    db 18,17,20,29,28,27,28,29,29,29,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,29,25,26,25
    db 29,30,29,29,29,28,29,24,17,18,17,19,20,20,20,21,24,28,30,29,27,19,17,25,24,20,20,20,20,20
    db 19,28,28,26,27,26,26,27,26,28,31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,17,31,31,31,31,31,31,31,31,31,31,29,22,16,16,16,18,31,31
    db 31,31,22,25,31,31,31,31,31,29,16,16,16,25,31,31,31,31,31,31,28,22,16,16,23,31,31,31,31,31
    db 17,16,16,25,31,31,31,31,31,22,16,16,20,31,31,31,31,18,16,25,31,31,31,31,31,31,31,31,31,29
    db 16,16,20,31,31,31,31,31,31,29,24,16,16,18,31,31,31,31,31,23,16,16,20,30,31,31,31,31,30,16
    db 16,16,29,31,31,31,26,16,23,31,31,31,31,19,29,31,31,31,31,31,30,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,20,24,19,19,19,18,24,26,25,25,27,27,26,28,30,29,29
    db 31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,29,25,26,25,30,31,31,31,31,31,31,25,17,18
    db 18,18,18,18,18,19,23,28,29,29,28,27,26,26,24,19,19,19,19,19,19,28,25,22,23,23,23,23,21,25
    db 31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,20,31,31,31,31,31,31,31,31,31,31,31,19,16,16,17,31,31,31,31,31,31,31,31,31,31,31,31
    db 19,16,18,31,31,31,31,31,23,16,16,16,16,16,24,31,31,31,31,31,17,16,17,31,31,31,31,31,31,23
    db 16,16,22,31,31,31,31,16,23,31,31,31,31,29,23,31,31,31,31,27,16,16,31,31,31,31,31,27,17,16
    db 16,16,16,18,31,31,31,31,31,23,16,16,27,31,31,31,31,31,29,16,16,17,30,31,31,31,22,16,19,31
    db 31,31,31,29,31,31,31,31,31,31,30,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,22,23,17,17,18,16,25,28,27,27,27,27,26,28,30,29,29,31,31,31,31,31,31,31,31,31,31
    db 31,31,31,31,31,31,29,25,26,25,30,31,31,31,31,31,31,25,17,18,18,18,18,18,19,20,23,27,30,29
    db 30,31,31,27,24,19,19,19,19,19,19,28,25,22,23,23,23,23,21,25,31,31,27,20,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,18,21,30,31,31,31,31
    db 31,31,31,31,31,18,16,18,31,31,31,31,31,31,27,25,31,31,31,31,20,16,22,31,31,31,31,22,16,21
    db 29,30,30,19,16,30,31,31,31,31,24,16,18,31,31,31,31,31,31,25,16,16,27,31,31,31,20,16,31,31
    db 31,31,31,16,16,31,31,31,31,27,16,19,31,31,31,31,25,16,16,27,30,30,25,16,23,31,31,31,31,28
    db 16,16,27,31,31,31,31,31,31,16,16,20,31,31,31,24,16,16,18,31,31,31,31,31,31,25,27,31,31,31
    db 28,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,21,23,17,18,18,16,25
    db 27,27,27,27,27,26,28,30,29,29,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,29,25,26,25
    db 30,31,31,31,31,31,31,25,17,18,18,18,18,19,26,26,27,27,27,27,25,24,24,26,26,27,27,27,27,27
    db 25,29,29,28,29,29,29,29,28,29,31,31,27,20,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,30,31,31,31,31,31,31,31,31,31,25,16,19,31,31
    db 31,31,31,28,16,22,31,31,31,31,20,16,29,31,31,31,27,16,21,31,31,31,31,20,16,31,31,31,31,31
    db 21,16,16,29,31,31,31,31,31,31,16,16,28,31,31,31,20,17,31,31,31,31,31,16,16,31,31,31,31,26
    db 16,26,31,31,31,29,16,16,30,31,31,31,30,16,26,31,31,31,31,27,16,16,25,31,31,31,31,31,31,22
    db 16,22,31,31,31,28,16,16,17,31,31,31,31,31,25,16,25,31,31,31,29,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,20,23,17,18,18,16,26,28,28,28,27,27,26,28,30,29,29
    db 31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,29,25,26,25,30,31,31,31,31,31,31,25,17,18
    db 18,18,18,19,30,29,29,27,25,26,22,19,20,26,27,30,30,30,30,30,28,29,31,31,31,31,31,31,31,31
    db 31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,20,23,27,31,31,31,31,31,31,31,29,16,18,31,31,31,31,31,16,18,31,31,31,31,26
    db 17,18,31,31,31,31,20,16,27,31,31,31,31,28,16,29,31,31,31,31,20,16,24,31,31,31,31,31,31,27
    db 16,17,31,31,31,31,18,18,31,31,31,31,28,16,16,31,31,31,31,25,16,31,31,31,31,25,16,20,31,31
    db 31,31,31,18,22,31,31,31,31,26,16,18,31,31,31,31,31,31,31,18,16,26,31,31,31,24,16,16,19,31
    db 31,31,31,30,16,17,31,31,31,30,17,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,18,18,18,18,17,17,17,17,17,18,18,18
    db 18,18,19,23,23,17,18,18,17,24,26,25,25,27,27,26,28,29,29,29,28,28,28,28,28,28,28,28,28,28
    db 28,28,28,28,28,28,27,25,26,26,28,28,28,28,28,28,29,24,17,18,18,18,18,19,29,29,29,28,27,28
    db 25,24,24,26,24,23,23,23,23,23,23,29,29,28,28,28,28,28,28,29,31,31,27,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,22,31
    db 31,31,31,31,31,31,16,16,25,31,31,31,29,16,20,31,31,31,31,18,16,25,31,31,31,31,20,16,16,30
    db 31,31,31,31,18,21,31,31,31,31,21,16,30,31,31,31,31,31,31,28,16,17,31,31,31,30,16,16,29,31
    db 31,31,21,16,23,31,31,31,31,26,18,31,31,31,31,25,16,16,24,31,31,31,31,26,16,31,31,31,31,28
    db 16,22,31,31,31,31,31,31,31,19,16,26,31,31,31,16,16,16,19,31,31,31,31,27,16,21,31,31,31,30
    db 21,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,27,27,27,27,27,27,27,27,27,26,27,27,27,27,27,28,23,17,18,18,17,21
    db 23,22,22,27,27,26,28,29,29,29,26,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,26,26,26,26
    db 26,25,25,25,25,25,27,23,17,18,18,18,18,19,29,29,29,29,29,29,28,28,29,27,23,19,19,19,19,19
    db 18,28,27,25,26,26,26,26,25,28,31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,18,19,16,16,16,16,23,31,31,31,31,31,31,16,17,29,31
    db 31,31,27,16,17,31,31,31,31,30,16,25,31,31,31,31,18,16,18,30,31,31,31,29,17,20,31,31,31,31
    db 21,16,31,31,31,31,31,31,31,31,16,17,30,31,31,26,16,23,31,31,31,31,26,16,21,31,31,31,31,25
    db 18,31,31,31,31,24,16,16,26,31,31,31,31,22,16,30,31,31,31,27,16,27,31,31,31,31,31,31,31,22
    db 16,26,31,31,28,16,16,16,16,29,31,31,31,27,16,17,29,31,31,31,30,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,17,26,25
    db 26,26,26,26,26,26,26,26,26,26,26,26,26,26,23,17,18,18,17,22,23,22,22,27,27,26,28,29,29,29
    db 26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,27,24,17,18
    db 18,18,18,19,29,29,29,29,29,29,29,29,29,27,23,20,20,20,20,19,19,28,28,26,27,27,27,27,26,28
    db 31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,23,31,25,16,16,16,16,16,29,31,31,31,31,31,16,19,31,31,31,31,28,16,19,31,31,31,31,31
    db 16,25,31,31,31,31,28,16,19,31,31,31,31,27,16,18,31,31,31,31,24,18,31,31,31,29,31,31,31,31
    db 16,21,31,31,31,21,16,28,31,31,31,31,27,16,17,31,31,31,29,16,19,31,31,31,31,29,18,16,28,31
    db 31,31,31,18,16,29,31,31,31,28,18,31,31,31,29,31,31,31,31,19,16,31,31,31,24,16,16,16,20,31
    db 31,31,31,30,16,16,28,31,31,31,26,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,17,26,25,26,26,26,26,26,26,26,26,26,26
    db 26,26,26,26,23,17,18,18,17,22,23,22,22,27,27,26,28,30,29,29,26,26,26,26,26,26,26,26,26,26
    db 26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,27,24,17,18,18,18,18,19,29,29,29,29,29,29
    db 29,29,29,27,23,20,20,20,20,19,19,28,28,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,23,31,31,29,16,16,16,16,16
    db 30,31,31,31,31,31,16,19,31,31,31,31,28,16,22,31,31,31,31,30,16,23,31,31,31,31,31,17,16,27
    db 31,31,31,31,17,16,31,31,31,31,31,31,31,31,22,17,31,31,31,31,22,26,31,31,31,19,16,22,31,31
    db 31,31,30,19,26,31,31,31,29,16,18,31,31,31,31,31,20,16,20,31,31,31,31,25,16,30,31,31,31,31
    db 31,31,31,30,17,30,31,31,31,25,24,31,31,31,22,16,16,16,21,31,31,31,31,30,16,16,31,31,31,31
    db 28,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,17,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,27,23,17,18,18,17,22
    db 23,22,22,27,27,26,28,30,29,30,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26
    db 26,26,26,26,26,26,27,24,17,18,18,18,18,19,29,29,29,28,27,27,28,29,29,27,23,20,20,20,20,19
    db 19,28,28,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,24,31,31,31,23,16,16,16,28,31,31,31,31,31,25,16,20,31,31
    db 31,31,31,16,16,27,31,31,31,29,16,22,31,31,31,31,28,16,16,28,31,31,31,30,16,16,20,31,31,31
    db 31,31,31,31,17,16,31,31,31,31,31,31,31,31,31,16,16,16,28,31,31,31,31,31,31,31,31,31,31,23
    db 16,31,31,31,31,31,16,16,21,31,31,31,31,21,16,19,28,31,31,31,31,31,31,25,16,31,31,31,31,31
    db 31,31,31,31,21,16,16,16,19,31,31,31,31,24,16,18,31,31,31,31,31,17,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,27,27,27,27,27,27,27,27,27,27,28,23,17,18,18,17,22,23,22,22,27,27,27,27,27,27,27
    db 26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,27,23,17,18
    db 18,18,18,19,29,29,30,26,20,21,25,29,29,27,23,20,20,20,20,19,19,28,28,26,27,27,27,27,26,28
    db 31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,22,31,31,31,31,30,26,28,31,31,31,31,31,29,16,16,20,31,31,31,31,26,16,17,28,31,31,31,28
    db 16,19,31,31,31,31,31,16,16,31,31,31,31,28,16,16,17,31,31,31,31,31,31,31,16,16,30,31,31,31
    db 31,31,31,31,29,16,16,21,31,31,31,31,31,31,31,31,31,31,31,24,16,31,31,31,31,31,20,16,25,31
    db 31,31,31,18,16,16,29,31,31,31,31,31,31,21,16,28,31,31,31,31,31,31,31,31,19,16,16,16,18,31
    db 31,31,31,24,16,19,31,31,31,31,27,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,17,17,17,17,17,17,16,17,17
    db 17,17,17,28,23,17,18,18,17,22,23,22,22,27,27,27,27,27,27,27,26,26,26,26,26,26,26,26,26,26
    db 26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,27,23,17,18,18,18,18,19,29,29,30,26,20,20
    db 25,29,29,27,23,20,20,20,20,20,19,28,28,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,20,31,31,31,31,31,31,31,31
    db 31,31,31,30,16,16,16,19,31,31,31,31,23,16,25,31,31,31,31,29,16,16,19,27,31,31,31,30,29,31
    db 31,31,31,20,16,16,19,31,31,31,31,31,31,25,16,16,21,31,31,31,31,31,31,31,26,16,16,18,31,31
    db 31,31,31,31,24,30,31,31,31,28,16,18,22,31,31,31,31,28,31,31,31,31,26,16,16,17,31,31,31,31
    db 31,31,31,16,16,18,31,31,31,31,31,31,31,31,17,16,16,16,16,31,31,31,31,28,16,16,22,31,31,31
    db 29,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,17,28,23,17,18,18,17,22
    db 23,22,22,27,27,27,27,27,27,27,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26
    db 26,26,26,26,26,26,27,23,17,18,18,18,18,19,29,29,30,26,20,20,25,29,29,27,23,20,20,20,20,20
    db 19,28,28,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,17,31,31,31,31,31,31,31,31,31,31,31,19,16,16,16,23,31,31
    db 31,31,29,16,27,31,31,31,31,31,17,16,16,31,31,31,31,31,31,31,31,31,22,16,16,16,16,30,31,31
    db 31,31,31,22,16,16,24,31,31,31,31,31,31,27,17,16,16,16,24,31,31,31,31,23,16,31,31,31,31,31
    db 17,16,24,31,31,31,31,31,31,31,31,27,16,16,16,16,26,31,31,31,31,31,29,16,16,21,31,31,31,31
    db 31,31,31,20,16,16,16,16,20,31,31,31,31,28,16,16,26,31,31,31,31,19,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,17,16,16,16,16,16,17,28,23,17,18,18,17,22,23,22,22,27,27,27,27,27,27,27
    db 26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,27,23,17,18
    db 18,18,18,19,29,29,30,26,21,21,25,29,29,27,23,20,20,20,20,20,19,28,29,27,28,27,27,27,26,28
    db 31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,23,30,31,31,31,31,31,28,31,31,20,16,16,16,16,30,31,31,31,30,21,16,22,31,31,31,31,31
    db 27,16,16,24,31,31,31,31,31,31,30,23,16,16,16,16,16,27,31,31,31,31,28,18,16,16,23,31,31,31
    db 31,31,27,16,16,16,16,16,16,26,31,31,31,16,19,31,31,31,31,31,27,16,20,30,31,31,31,31,31,30
    db 26,16,16,16,16,16,21,31,31,31,31,30,22,16,16,20,31,31,31,31,31,30,20,16,16,16,16,16,25,31
    db 31,31,31,30,16,17,31,31,31,31,31,27,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,17,26,26,26,26,17,16,16,19,26
    db 26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26
    db 26,26,26,26,22,17,18,18,17,22,23,23,23,24,24,23,25,27,27,27,25,25,25,25,25,25,25,25,25,25
    db 25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,26,22,18,18,18,18,18,19,29,29,29,28,26,27
    db 28,29,29,26,23,19,19,19,19,19,19,28,27,24,25,24,24,24,23,27,31,31,27,20,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,20,23,25,26,25,16
    db 20,19,16,16,16,16,16,24,27,27,23,16,16,16,16,21,31,31,31,31,23,16,16,16,21,28,30,31,30,18
    db 16,16,16,16,16,16,16,16,21,24,27,23,16,16,16,16,16,20,25,27,27,20,16,16,16,16,16,16,16,16
    db 20,23,22,16,16,29,31,31,31,31,23,16,16,17,25,29,31,31,22,16,16,16,16,16,16,16,16,19,23,27
    db 25,16,16,16,16,16,18,23,27,28,22,16,16,16,16,16,16,16,23,31,31,31,27,21,16,16,31,31,31,31
    db 31,27,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,19,27,25,25,26,18,17,16,21,26,26,26,26,26,26,26,26,26,26,26
    db 26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,25,21,17,18,18,17,22
    db 23,23,23,22,22,22,25,27,27,27,25,25,25,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24
    db 24,24,24,24,24,24,26,22,18,18,18,18,18,19,29,29,29,29,29,29,29,29,29,26,23,19,19,19,19,18
    db 17,28,25,21,22,21,21,21,20,25,31,31,27,20,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,17,21,21,19,16,16,16,16,16,16,16,17,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,23,25,24,18
    db 16,16,16,16,16,16,17,17,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,17,20,19,16,16,16,16,21,25,27,25,21,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 19,27,25,25,26,27,17,17,22,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26
    db 26,26,26,26,26,26,26,26,26,26,26,26,26,25,22,17,18,18,17,22,23,23,23,23,23,22,25,28,27,27
    db 25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,26,22,18,18
    db 18,18,18,19,29,29,29,29,28,29,28,29,29,26,25,25,25,25,25,25,23,29,28,26,27,27,27,26,26,29
    db 31,31,27,20,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,17,26,26,26,27,17,17,16,19,26
    db 26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26
    db 26,26,26,27,23,17,18,18,17,22,23,23,23,23,23,22,26,28,27,27,25,24,25,25,24,24,24,24,24,24
    db 24,24,24,24,24,24,25,25,25,25,25,25,25,25,25,25,26,22,18,18,18,18,18,19,31,30,29,29,30,30
    db 30,30,30,26,24,25,25,25,25,25,24,28,31,30,30,30,30,30,29,30,31,31,27,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,17,17,16,16,16,16,16,17,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,23,23,17,18,18,17,22
    db 23,23,23,23,23,23,24,25,25,25,25,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,25,25,25,25
    db 25,25,25,25,25,25,26,22,17,18,18,18,18,19,26,26,27,27,26,27,27,27,27,26,23,19,19,19,19,19
    db 18,27,29,26,27,27,27,26,26,28,31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,21,23,17,18,18,17,22,23,23,23,23,23,23,23,22,22,22
    db 26,27,27,27,27,27,27,27,27,27,27,27,27,27,27,28,26,24,25,25,25,25,25,25,25,25,26,22,17,18
    db 18,18,18,18,18,18,23,24,18,19,19,19,19,25,23,20,20,20,20,20,19,28,29,26,27,27,27,27,26,28
    db 31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,21,23,17,18,18,17,22,23,23,23,23,23,23,23,23,23,23,27,27,27,27,27,27,27,27,27,27
    db 27,27,27,27,27,27,26,24,25,25,25,25,25,25,25,25,26,22,18,18,18,18,18,18,18,18,23,25,20,21
    db 21,21,21,26,23,20,20,20,20,20,19,28,28,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,20,23,17,18,18,17,22
    db 23,23,23,23,23,23,23,23,23,23,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,26,24,25,25
    db 25,25,25,25,25,25,26,22,17,18,18,18,18,18,21,21,23,28,30,29,29,29,30,26,23,20,20,20,20,20
    db 19,28,28,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,20,23,18,18,19,18,22,23,23,23,23,23,23,23,23,23,23
    db 27,27,26,26,26,26,26,26,26,26,26,26,26,26,26,27,26,24,25,25,25,25,25,25,25,25,26,22,17,18
    db 18,18,18,19,28,28,26,28,30,30,30,30,30,26,23,20,20,20,20,20,19,28,28,26,27,27,27,27,26,28
    db 31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,17,26,27,26,24,21,21,21,21,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23
    db 23,23,23,23,23,24,25,25,25,25,25,25,25,25,25,25,26,22,17,18,18,18,18,19,29,29,28,24,19,20
    db 20,20,19,25,24,20,20,20,20,20,19,28,28,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,19,19,19,28,20
    db 18,18,19,23,23,23,23,23,23,23,22,22,22,22,22,22,22,23,23,22,23,23,23,23,23,22,24,25,25,25
    db 25,25,25,25,25,25,26,22,17,18,18,19,19,20,29,29,28,23,18,19,19,19,19,25,24,20,20,20,20,20
    db 19,27,28,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,28,19,17,17,18,23,23,23,23,23,23,23
    db 23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,22,23,25,25,25,25,25,25,25,25,25,26,22,17,18
    db 17,24,27,26,29,29,28,23,19,20,20,20,20,26,23,20,20,20,20,20,19,27,28,26,27,27,27,27,26,28
    db 31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,27,19,18,18,19,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23
    db 23,23,23,23,23,22,23,25,25,25,25,25,25,25,25,25,26,22,17,18,17,27,30,29,29,29,28,23,19,20
    db 20,20,20,26,23,19,19,19,19,19,18,28,25,23,24,24,24,24,23,26,31,31,27,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,17,17,16,16,16,16,27,19
    db 17,18,18,22,21,21,22,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,22,23,25,25,25
    db 25,25,25,24,23,23,24,21,17,18,16,26,30,29,30,30,28,23,19,20,20,20,20,26,23,19,19,19,19,19
    db 18,28,24,21,22,22,22,22,20,25,31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,24,25,25,25,18,16,17,27,19,17,18,18,19,19,18,21,24,23,23
    db 23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,22,23,25,25,25,25,25,25,22,19,19,19,19,18,18
    db 17,23,25,24,24,24,25,23,18,20,20,20,20,26,25,23,23,23,23,23,22,29,28,26,27,27,27,27,26,28
    db 31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 17,27,22,22,25,24,17,16,27,19,18,18,18,18,18,17,21,24,23,23,23,23,23,23,23,23,23,23,23,23
    db 23,23,23,23,23,22,23,25,25,25,25,25,25,22,18,18,18,18,18,18,18,18,18,18,18,18,22,24,18,20
    db 20,20,20,26,27,30,30,30,30,30,28,29,31,31,31,31,31,31,31,31,31,31,28,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,17,27,22,22,26,28,17,16,18,19
    db 17,17,17,18,18,17,21,24,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,22,23,25,25,25
    db 25,25,25,22,18,18,18,18,18,18,18,18,18,18,18,18,22,24,19,20,20,20,20,26,26,26,26,26,26,26
    db 24,28,30,30,30,30,30,30,30,30,31,31,28,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,27,24,23,26,20,17,16,18,21,20,20,20,18,18,17,20,23,21,22
    db 23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,21,18,18,18,18,18,18
    db 18,18,18,18,18,18,22,24,19,20,20,20,20,26,22,19,20,20,20,19,19,27,28,26,27,27,27,27,26,28
    db 31,31,28,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,18,19,18,16,16,16,16,17,20,28,29,27,17,18,18,18,18,18,18,23,23,23,23,23,23,23,23,23,23
    db 23,23,23,23,23,23,20,16,17,17,17,17,17,18,18,18,18,18,18,18,18,18,18,18,18,18,23,24,19,20
    db 20,20,20,26,23,20,20,20,20,20,19,27,28,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,17
    db 17,24,28,17,18,18,18,18,18,18,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,20,17,17,17
    db 17,17,17,18,18,18,18,18,18,18,18,18,18,18,18,18,23,24,18,20,20,20,20,26,23,20,20,20,20,20
    db 19,28,28,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,20,27,17,18,18,18,18,18,18
    db 23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,20,17,18,18,18,18,18,18,18,18,18,18,18,18
    db 18,18,18,18,19,19,23,24,18,20,20,20,20,26,23,20,20,20,20,20,19,28,28,26,27,27,27,27,26,28
    db 31,31,28,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,20,27,17,18,18,18,18,18,18,23,23,23,23,23,23,23,23,23,23
    db 23,23,23,23,23,23,20,17,18,18,18,18,18,18,18,18,18,18,18,18,18,17,18,18,19,19,23,24,18,20
    db 20,20,20,26,23,20,20,20,20,20,19,27,28,26,27,27,27,27,26,28,31,31,28,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,17,20,26,26,26,26,27,21,18,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
    db 19,19,19,19,18,18,18,18,18,18,20,27,28,27,28,27,27,23,18,20,20,20,19,26,23,20,20,20,20,20
    db 19,27,28,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,17,28,28,28,29,30,21,18
    db 18,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,18,20,19,20,20,20,20,20,18,18,18,18,18,18
    db 21,27,28,27,27,26,27,23,17,19,18,19,19,26,23,20,20,20,20,20,19,27,28,26,27,27,27,27,26,28
    db 31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,17,17,16,16,16,16,16,17,17,16,16,16,17,18,17,17,17,18,18,18,18,18,18,18,18,18,18
    db 17,17,17,18,17,17,17,17,17,17,17,17,17,27,27,27,27,28,21,18,18,18,18,18,18,18,18,18,18,18
    db 18,18,18,18,18,18,18,19,19,20,20,20,20,20,18,18,18,18,18,18,20,21,21,21,21,21,24,26,25,25
    db 25,25,25,26,23,19,19,19,19,19,19,28,26,24,25,25,25,25,24,27,31,31,27,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,21,27,26,22,16,16,16,21
    db 26,26,19,17,16,27,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,27,26,26,26,26,26,27,27
    db 27,27,26,26,26,26,26,27,20,18,17,17,17,17,17,17,17,18,18,18,18,18,17,17,17,17,18,19,19,20
    db 20,19,20,20,18,18,18,18,18,18,19,20,20,20,20,19,22,29,31,31,31,31,31,27,22,19,19,19,19,19
    db 18,28,24,21,22,22,22,22,20,25,31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,20,27,26,26,27,17,16,16,27,25,26,27,27,27,26,26,26,26,26
    db 26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,25,25,22,22
    db 22,22,22,22,22,22,19,17,18,18,18,18,20,18,18,18,19,19,19,20,20,19,19,19,18,18,18,18,18,18
    db 19,20,20,20,20,19,22,27,26,27,27,27,27,27,24,23,23,23,23,22,21,28,27,25,26,26,26,26,25,27
    db 31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,20,27,25,26,27,17,16,16,27,25,26,27,27,27,26,25,26,26,26,26,26,26,26,26,26,26,26,26,26
    db 26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,25,24,26,29,30,30,30,30,28,27,24,22,23,23
    db 23,23,25,19,19,19,19,19,19,20,20,18,18,18,18,18,18,18,18,18,19,20,20,20,20,19,23,24,18,19
    db 19,19,19,25,27,29,29,29,29,29,27,29,31,31,31,31,31,31,31,31,31,31,27,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,18,27,26,26,27,16,16,16,27
    db 26,26,23,18,18,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26
    db 26,26,26,26,26,26,26,26,23,26,27,27,27,27,27,25,28,31,31,31,31,31,25,19,19,19,19,19,19,19
    db 19,18,18,18,18,18,18,19,18,19,20,20,20,20,20,20,24,24,18,20,20,20,20,26,26,26,27,27,26,26
    db 25,29,30,30,30,30,30,30,30,30,31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,17,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,27,27,27,27,27,27,27,27,27,27,27,27,27,28,20,19
    db 20,20,20,20,22,26,26,29,28,28,28,28,26,20,21,21,21,21,21,21,20,19,18,18,18,18,19,20,19,23
    db 21,19,20,20,20,20,24,24,18,20,20,20,20,26,22,20,20,20,20,20,19,27,28,26,27,27,27,27,26,28
    db 31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,17,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,22,20,19,20,20,20,19,22,27,22,20,20,20
    db 20,20,26,29,30,30,30,30,30,27,26,20,18,18,18,18,19,20,19,24,22,19,20,20,20,20,24,24,18,20
    db 20,20,20,26,22,19,20,20,19,19,19,27,28,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,22,20,19,20,20,20,19,22,27,22,19,20,20,20,19,26,30,31,31,31,31,31,28
    db 26,20,18,19,20,20,20,19,19,25,22,19,20,20,20,20,24,24,18,20,20,20,20,26,22,20,20,20,20,20
    db 19,28,28,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,22,20,19
    db 20,20,20,19,22,27,22,19,20,20,20,20,25,21,22,22,22,22,21,25,27,20,18,19,20,20,20,20,19,25
    db 22,19,20,20,20,20,24,24,18,20,20,20,20,26,22,20,20,20,20,20,19,28,28,26,27,27,27,27,26,28
    db 31,31,27,20,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,22,20,19,20,20,20,19,22,27,22,19,20,20
    db 20,20,25,19,19,19,19,19,19,24,27,20,18,19,20,19,20,20,19,25,22,19,20,20,20,20,24,24,18,20
    db 20,20,20,26,22,20,20,20,20,20,19,28,28,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,22,20,19,20,20,20,19,22,28,22,19,20,20,20,20,25,19,20,20,20,20,19,24
    db 27,21,19,19,20,20,20,20,19,25,22,19,20,20,20,20,23,24,18,20,20,20,20,26,23,20,20,20,20,20
    db 19,28,28,26,27,27,27,27,26,28,31,31,27,20,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,19,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,22,20,19
    db 20,20,20,19,22,28,22,19,20,20,20,20,25,19,20,20,20,20,19,25,27,21,20,20,20,19,20,20,19,25
    db 22,19,20,20,20,20,23,24,18,20,20,20,20,26,23,20,20,20,20,20,19,28,28,26,27,27,27,27,26,28
    db 31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,17,23,25,25,25,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,18,18,18,18,18,18,18,18,18,18,18,18,18,17,22,20,19,20,20,20,19,22,28,22,19,20,20
    db 20,20,25,19,20,20,20,20,19,25,27,21,19,20,20,19,20,20,19,25,22,19,20,20,20,20,23,24,18,20
    db 20,20,20,26,23,20,20,20,20,20,19,28,28,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,19,25,22,23,27,18,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,19,27,27,27,27,27,27,27
    db 27,27,27,27,27,27,27,28,21,19,20,20,20,19,22,28,22,19,20,20,20,20,25,19,20,20,20,20,19,25
    db 27,21,19,19,18,18,18,18,18,25,22,19,20,20,20,19,23,24,18,20,20,20,20,26,23,20,20,20,20,20
    db 19,28,28,26,27,27,27,27,26,28,31,31,27,20,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,20,25,21,21,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,18,27,25,26,26,26,26,26,26,26,26,26,26,26,26,26,26,21,19
    db 20,20,20,19,22,28,22,19,20,20,20,20,25,19,20,20,20,20,19,24,27,21,19,19,22,22,22,23,23,26
    db 21,19,19,19,19,19,23,24,18,20,20,20,20,25,23,20,20,20,20,20,19,28,28,26,27,27,27,27,26,28
    db 31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,19,25,22,22,26,18,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,18,27,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,21,19,20,20,20,19,22,28,22,19,20,20
    db 20,20,25,19,20,20,20,20,19,25,27,21,19,20,30,30,30,30,29,28,22,21,22,22,22,22,24,24,18,20
    db 20,20,20,25,23,20,20,20,20,20,19,28,28,26,27,27,27,27,26,28,31,31,27,20,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,17,27,26,26,26,26,26,26,26
    db 26,26,26,26,26,26,26,28,21,18,19,19,19,19,21,28,23,19,20,20,20,20,25,19,20,20,20,20,19,24
    db 27,21,19,19,24,24,23,23,21,24,28,29,30,31,31,31,28,23,18,19,19,19,19,25,23,20,20,20,20,20
    db 19,28,28,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,28,21,19
    db 20,20,20,20,22,28,23,19,20,20,20,20,25,19,20,20,20,20,19,24,27,21,19,20,20,20,20,20,19,23
    db 29,29,29,29,29,29,27,23,19,19,19,19,19,25,23,20,20,20,20,20,19,28,28,26,27,27,27,27,26,28
    db 31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,18,27,27,27,27,27,27,16,16,27,27,27,27,27,27,26
    db 27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,26,27,27,27,27,27,27,27,23,19,20,20
    db 20,20,25,19,20,20,20,20,19,24,27,21,19,20,20,20,20,20,19,25,23,20,20,20,20,20,23,28,30,30
    db 30,30,30,27,23,20,20,20,20,20,19,28,27,25,26,26,26,26,25,28,31,31,27,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,19,27,26,26,26,26,26,26,17,17,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26
    db 26,26,26,26,26,25,25,25,25,28,28,28,28,29,28,27,22,19,19,19,19,20,25,19,20,20,20,20,19,24
    db 27,21,19,20,20,20,20,20,19,25,22,19,19,19,19,19,21,29,31,31,31,31,31,27,24,20,20,20,19,20
    db 19,28,24,21,23,23,23,23,22,26,31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,19,27,26,26,26,26,26
    db 26,18,17,26,25,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,25,25,25,26,21,21
    db 22,22,22,22,23,27,22,18,19,19,19,19,25,19,20,20,20,20,19,24,27,21,19,20,20,20,20,20,19,25
    db 22,19,20,20,20,19,23,25,22,22,22,22,22,26,24,19,19,19,19,19,18,28,23,21,23,22,23,22,20,26
    db 31,31,27,20,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,18,22,27,26,26,26,26,27,16,16,27,26,26,26,26,26,26
    db 26,26,26,26,26,27,27,27,27,27,27,27,27,27,26,26,26,28,20,19,20,19,19,19,22,27,26,25,26,26
    db 25,25,25,19,19,19,19,19,19,24,27,21,20,20,20,20,20,20,19,25,22,19,20,20,20,20,23,24,18,19
    db 19,19,19,26,25,25,25,25,25,25,23,28,29,28,29,29,29,28,28,29,31,31,27,20,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,17,20,20,20,20,20,19,16,16,18,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27
    db 27,27,27,27,27,27,27,28,20,19,20,20,20,19,22,27,28,29,28,29,28,28,25,18,17,17,17,17,17,23
    db 27,21,20,20,20,20,20,20,19,25,22,19,20,20,20,20,24,24,19,20,20,20,20,26,26,30,30,30,29,29
    db 27,29,31,31,31,31,31,31,31,31,31,31,27,20,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,17,16,16,17,17,17,17,17,17,16,16,16,16,16,17,17,16,16,17,16,16,16,22,20,19
    db 20,20,20,19,22,27,24,23,23,23,23,23,26,24,24,24,24,24,24,26,27,20,19,19,19,19,19,19,19,25
    db 22,19,20,20,20,20,24,24,19,20,20,20,20,26,24,24,24,24,24,24,23,28,30,28,29,29,29,29,29,30
    db 31,31,27,20,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,21,20,19,20,20,20,19,22,27,21,19,20,20
    db 20,20,25,25,26,25,26,26,26,27,27,22,21,21,21,21,21,21,20,25,22,19,20,20,20,20,24,24,19,20
    db 20,20,20,26,22,19,19,19,19,19,19,27,28,26,27,27,27,27,26,28,31,31,27,20,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,22,20,19,20,20,20,19,22,27,22,19,20,20,20,20,25,19,19,19,19,19,18,24
    db 27,26,26,26,26,26,26,26,25,26,22,19,20,20,20,19,23,24,18,20,20,20,20,26,23,20,20,20,20,20
    db 19,28,28,26,27,27,27,27,26,28,31,31,27,20,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,22,20,19
    db 20,20,20,19,22,27,22,19,20,20,20,20,25,19,20,20,20,20,19,24,27,25,25,25,25,25,25,25,25,27
    db 21,18,19,19,19,19,23,24,18,20,20,20,20,26,23,20,20,20,20,19,19,28,28,26,27,27,27,27,26,28
    db 31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,22,20,19,20,20,20,19,22,27,22,19,20,20
    db 20,20,25,19,20,20,20,20,19,24,28,20,18,19,19,19,19,19,18,24,23,21,21,21,21,21,24,24,18,20
    db 20,20,20,26,23,20,20,20,20,20,19,27,28,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,22,20,19,20,20,20,19,22,27,22,19,20,20,20,20,25,19,20,20,20,20,19,24
    db 27,21,19,20,20,20,20,20,19,24,29,30,30,30,30,29,27,23,18,19,19,19,19,26,23,20,20,20,20,20
    db 19,28,29,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,21,20,19
    db 20,20,20,19,22,28,22,19,20,20,20,20,25,19,20,20,20,20,19,24,27,21,20,20,20,20,20,20,19,23
    db 29,30,29,30,30,30,29,23,19,20,19,20,19,26,23,20,20,20,20,20,19,27,29,26,27,27,27,27,26,28
    db 31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,22,20,19,20,20,20,19,22,28,22,19,20,20
    db 20,20,25,19,20,20,20,20,19,24,27,21,20,20,20,20,20,20,19,25,23,21,21,21,21,21,24,27,26,26
    db 26,26,26,26,23,20,20,20,20,20,19,27,29,27,27,27,27,27,27,28,31,31,27,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,22,20,18,19,19,19,19,22,28,22,19,20,20,20,20,25,19,20,20,20,20,19,24
    db 27,21,20,20,20,20,20,20,20,25,22,19,19,19,19,19,22,27,28,28,28,28,28,26,23,19,19,19,19,19
    db 19,28,26,23,24,24,24,24,23,26,31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,18,18,17,17,17,17,18
    db 17,17,17,16,17,18,18,18,18,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,18,17,18,25,21,18
    db 19,19,19,19,22,28,22,19,20,20,20,20,25,19,20,20,20,20,19,24,27,21,20,20,20,20,20,20,20,25
    db 22,19,20,20,20,20,23,25,21,22,22,22,22,26,23,18,17,17,17,17,17,28,23,19,20,19,20,20,18,24
    db 31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,21,22,20,21,21,21,21,21,21,22,22,22,25,22,21,21,21,21,21
    db 21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,22,19,18,19,19,18,18,19,22,20,19,19,19
    db 19,20,25,19,20,20,20,20,19,24,27,21,20,20,20,20,20,20,20,25,22,19,20,20,20,20,24,24,18,19
    db 19,19,19,26,25,24,24,24,24,24,24,29,28,26,26,26,26,26,26,28,31,31,27,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,20,19,19,19,20,20,20,19,19
    db 18,30,26,16,17,17,17,17,17,17,19,18,20,23,19,16,17,17,17,17,17,17,17,17,17,17,17,17,17,17
    db 17,17,17,17,17,17,17,17,18,18,18,18,18,18,18,17,19,19,19,19,19,20,26,19,20,20,20,20,19,24
    db 27,21,20,20,20,20,20,20,20,26,22,19,20,20,20,20,24,24,19,20,20,20,20,26,24,24,24,24,24,24
    db 23,28,31,31,31,31,31,31,30,31,31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,17,16,17,17,17,17
    db 16,17,17,16,16,16,16,16,16,18,19,22,22,22,22,22,23,23,22,22,22,23,22,18,18,18,18,18,18,18
    db 20,20,21,24,20,17,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
    db 18,18,18,18,18,18,18,19,19,19,19,19,22,19,20,20,20,20,19,24,27,21,20,20,20,20,20,20,20,26
    db 22,19,20,20,20,20,24,24,19,20,20,20,20,26,22,19,19,19,19,18,17,27,29,27,28,28,28,28,27,28
    db 31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,17,26,27,27,27,27,27,27,27,27,27,27,27,27,27,27,28
    db 24,16,17,17,17,17,17,17,17,17,17,16,17,18,18,18,18,18,18,18,19,19,20,22,19,18,18,18,18,18
    db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
    db 18,18,17,19,19,19,19,19,19,25,28,21,20,20,20,20,20,20,20,26,22,19,20,20,20,20,24,24,19,20
    db 20,20,20,26,22,20,20,20,20,20,19,28,28,26,27,26,26,27,26,28,31,31,27,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,18,18,18
    db 18,18,24,24,22,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,22,18,18,18,18,18,18,18,18,18
    db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
    db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,19,19,19,19,19,19,22
    db 24,20,20,20,20,20,20,20,20,26,22,19,20,20,20,20,24,24,19,20,20,20,20,26,22,20,20,20,20,20
    db 19,28,28,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,23,25,25,25,25,26,20,17,18,18,18,18,18
    db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
    db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
    db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,17,19,20,20,20,20,20,20,20,26
    db 22,19,20,20,20,20,24,24,19,20,20,20,20,26,23,20,20,20,20,20,19,28,28,26,27,27,27,27,26,28
    db 31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,20,17,17,17,17,17,17,17,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
    db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
    db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
    db 18,18,18,18,18,18,18,18,18,18,18,19,19,19,19,19,19,19,20,26,22,19,20,20,20,20,24,24,19,20
    db 20,20,20,26,23,20,20,20,20,20,19,28,28,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,19,19,19,19
    db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
    db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
    db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
    db 18,19,19,19,19,19,19,18,19,25,22,19,20,20,20,20,24,24,19,20,20,20,20,26,23,20,20,20,20,20
    db 19,28,28,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,25,29,30,25,17,18,18,18,18,18,18,18,18
    db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
    db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
    db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
    db 19,19,19,20,19,19,24,24,19,20,20,20,20,26,24,20,20,20,20,20,19,28,28,26,27,27,27,27,26,28
    db 31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,21,28,30,25,17,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
    db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
    db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
    db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,17,19,19,19,19,19,19,24,24,19,20
    db 20,20,20,26,24,20,20,20,20,20,19,28,28,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,17,18
    db 20,23,24,20,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
    db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
    db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
    db 18,18,18,18,18,18,18,18,18,18,18,19,18,18,18,18,21,21,19,20,20,20,20,26,24,20,20,20,20,20
    db 19,28,28,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,24,20,17,18,18,18,18,18
    db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
    db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
    db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
    db 18,18,18,18,18,18,18,19,19,19,19,19,19,26,25,20,20,20,20,20,19,28,28,26,27,27,27,27,26,28
    db 31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,17,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,18,24,24,24,24,25,25,25,25,25,25,25,25,25,25,25,18,18
    db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
    db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
    db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,19,19
    db 19,19,19,27,26,20,20,20,20,20,19,28,28,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,18,24,26,26,29,29,29,29,29,29,29,29,29,30,29,18,18,18,18,18,18,18,18,18,18,18,18
    db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
    db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
    db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,19,19,19,19,18,23,22,19,19,19,20,19
    db 19,27,28,26,27,27,27,27,26,28,31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,17,16,16,16,16,18,23,22,22,22,22,22,22,22,22,22,21,21,21,21,21,21,22,21,21,20,21,21
    db 19,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
    db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
    db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,19,19,19,19,19,18,27,28,26,27,27,27,27,26,28
    db 31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,17,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,19,20
    db 20,19,18,18,18,18,18,17,17,17,17,18,18,18,18,27,27,23,18,17,19,18,18,18,18,18,19,19,19,19
    db 19,19,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
    db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
    db 18,18,18,18,18,19,19,19,19,19,18,27,27,25,26,26,26,26,24,26,31,31,27,19,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,17,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,17,18,16,19,18,17,17,17,17,17,18,18,18,18,18,18,17,17,17,17,17,17,17,17
    db 17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17
    db 17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17
    db 17,27,24,21,22,22,22,22,20,24,31,31,27,19,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,19
    db 19,18,18,19,19,18,19,19,19,19,19,19,19,19,19,19,19,18,18,18,18,18,18,18,18,18,18,18,18,18
    db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
    db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,19,24,21,22,22,22,22,20,24
    db 31,30,18,18,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,18,27,27,27,27,27,27,27,27,27
    db 27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,26,27,27,27,27,27,27,27,27,27,27,26,26,27,27
    db 27,27,27,27,27,27,27,27,27,27,26,26,26,27,27,26,26,27,27,26,26,26,27,26,26,26,26,26,26,26
    db 26,26,26,26,27,27,26,26,27,26,18,16,16,17,27,27,27,27,18,17,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,17,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17
    db 17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17
    db 17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17
    db 17,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,17,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,17,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,17,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16
    db 16,16,16,16,16,16,16,16,16,16
end