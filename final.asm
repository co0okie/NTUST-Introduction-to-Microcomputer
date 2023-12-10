; http://www.brackeen.com/vga/unchain.html
; https://wanker742126.neocities.org/
; https://www.ctyme.com/intr/int.htm
; https://www.website.masmforum.com/tutorials/fptute/
; https://stackoverflow.com/questions/13450894/struct-or-class-in-assembly
; https://en.wikipedia.org/wiki/Linear_congruential_generator#Parameters_in_common_use
; https://masm32.com/board/index.php?topic=7837.0

printNewline macro
    mov ah, 02h
    mov dl, 13
    int 21h
    mov dl, 10
    int 21h
endm
moveTextCursor macro x, y
    mov ah, 02h
    xor bh, bh
    mov dh, y
    mov dl, x
    int 10h
endm
printChar macro char, color, times
    mov ah, 09h
    mov al, char
    mov bh, 0
    mov cx, times
    mov bl, color
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
fcomi_ macro i ; see fcomi in https://www.website.masmforum.com/tutorials/fptute/fpuchap7.htm
    db 0dbh, 0f0h + i
endm
fcomip_ macro i ; see fcomip in https://www.website.masmforum.com/tutorials/fptute/fpuchap7.htm
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
; farstack: independent stack (SS != DS)
.model compact, c, farstack
.586


screenWidth equ 320.0
screenHeight equ 200.0

.data
; procedure prototype
step proto, pBall: near ptr circle
drawRectangle proto, left: word, top: word, squareWidth: word, height: word, color: byte
drawCircle proto, centerX: word, centerY: word, color: byte
drawLine proto, x0: word, y0: word, x1: word, y1: word
printUint8 proto, number: byte
printUint16 proto, number: word
printUint32 proto, number: dword
printInt8 proto, number: byte
printInt16 proto, number: word
printInt32 proto, number: dword
printBinary proto, number: byte
printHex32 proto, number: dword

; string
enterToStartMessage db "Press Enter to start"
hForHelpMessage db "Press H for help"
gameIntroduction \
    db "This is a two-player battle game where", 13, 10, " "
    db "red and blue take turns. Click the", 13, 10, " "
    db "left mouse button to shoot the ball", 13, 10 ," "
    db "towards the mouse pointer; the dashed", 13, 10 ," "
    db "line length indicates the shooting", 13, 10 ," "
    db "power. The first to reach 10 points", 13, 10 ," "
    db "wins. Hitting walls of different", 13, 10 ," "
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


circlePixelRadius equ 12
circleRadius real4 12.0
circleWidth \
dw 7, 11, 15, 17, 19, 21, 21, 23, 23, 25, 25, 25, 25, 25, 25, 25, 23, 23, 21, 21, 19, 17, 15, 11, 7
circle struct
    id db 5 dup(?)
    x real4 ?
    y real4 ?
    vx real4 ?
    vy real4 ?
    integerX dw ?
    integerY dw ?
    score dw ?
circle ends
wallCollision proto, pBall: near ptr circle
redBall circle <"Red$", , , , , , , >
blueBall circle <"Blue$", , , , , , , >

clockPeriod real4 ?
lastTimerCount dd ?
deltaT real4 ?
fpuTemp dt ?
widthMinusRadius real4 screenWidth
heightMinusRadius real4 screenHeight
frictionCoefficient real4 50.0

mouseButtonStatus db ?

gameStatus db 00000000b
; [0] 1: round start, 0: round end
; [1] who's turn? 0: red, 1: blue
; [2] who's next? 0: red, 1: blue

wallEffectProto typedef proto
wallEffectPtr typedef near ptr wallEffectProto
wallEffect wallEffectPtr 26 dup(?)
wallColor db 26 dup(?)

dashedlineColor db 08h, 08h, 08h, 08h, 08h, 00h, 00h, 00h
dashedlineFlow dw 0

randomNumber dd ?

debugVariable dw 0

.fardata? backBuffer
db 320 * 200 dup(?) ; video backbuffer

.stack 1000h
.code
main proc
    .startup
    
    ; video mode, 320 * 200, 256 colors
    mov ax, 13h
    int 10h
    
    ; calculate constant
    fld [circleRadius] ; st: [radius]
    fld [widthMinusRadius] ; st: [width][radius]
    fsub st, st(1) ; st: [width - radius][radius]
    fstp [widthMinusRadius] ; st: [radius]
    fsubr [heightMinusRadius] ; st: [height - radius]
    fstp [heightMinusRadius] ; st: []
    
    ; random number seed
    rdtsc
    mov [randomNumber], eax
    
    ; get CPU clock period
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
    
    .while 1 ; game cycle
        ; cover image
        push ds
        mov ax, coverImage
        mov ds, ax
        lea si, image
        mov ax, 0a000h
        mov es, ax
        mov di, imageTop * 320 + imageLeft
        mov cx, imageHeight
        @@:
            push cx
            mov cx, imageWidth
            rep movsb
            add di, 320 - imageWidth
            pop cx
            loop @b
        pop ds
        ; enter text width = 20 words = 20 * 8 = 160 pixels, background width = 168 pixels
        ; height = 3 lines = 3 * 8 = 24 pixels, background height = 32 pixels
        invoke drawRectangle, 12, 132, 168, 32, 0
        ; xor eax, eax
        ; mov di, 132 * 320 + 12
        ; mov cx, 32
        ; @@:
        ;     push cx
        ;     mov cx, 168 / 4
        ;     rep stosd
        ;     add di, 320 - 168
        ;     pop cx
        ;     loop @b
        
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
    
    ; generate random wall
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
    
    mov ax, 03h
    int 33h
    mov [mouseButtonStatus], bl
    
    rdtsc
    mov [lastTimerCount], eax
    
    ; reset game status and choose random player to start
    mov [gameStatus], 0
    nextRandomNumber
    .if al & 10000000b
        or [gameStatus], 10b ; blue first
        and [gameStatus], 11111011b ; next is red
    .else
        and [gameStatus], 11111101b ; red first
        or [gameStatus], 100b ; next is blue
    .endif
    
    .while 1
        ; clear backbuffer
        mov ax, backBuffer
        mov es, ax
        xor eax, eax
        xor di, di
        mov cx, (320 * 200) / 4
        rep stosd
        
        moveTextCursor 0, 0
        
        ; get dt
        rdtsc
        mov ebx, eax
        sub eax, [lastTimerCount]
        mov [lastTimerCount], ebx
        mov dword ptr [fpuTemp], eax
        fild dword ptr [fpuTemp] ; st: [dClock]
        fmul [clockPeriod] ; st: [dt]
        fstp [deltaT] ; st: []
        
        mov dword ptr [fpuTemp], 1000000
        fild dword ptr [fpuTemp] ; st: [1000000]
        fmul [deltaT] ; st: [1000000 * dt]
        fistp dword ptr [fpuTemp] ; st: []
        invoke printUint32, dword ptr [fpuTemp]
        printNewline
        
        ; calculate next position and velocity
        fld [deltaT] ; st: [dt]
        invoke step, addr redBall
        invoke step, addr blueBall
        fstp st ; st: []
        
        ; shoot if click
        mov ax, 03h
        int 33h
        shr cx, 1
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
            or [gameStatus], 00000001b ; new round start
            mov word ptr [fpuTemp], cx
            fild word ptr [fpuTemp] ; st: [mouseX]
            fsub [(circle ptr [si]).x] ; st: [mouseX - x1]
            fadd st, st ; st: [(mouseX - x1) * 2]
            fstp [(circle ptr [si]).vx] ; st: []
            mov word ptr [fpuTemp], dx
            fild word ptr [fpuTemp] ; st: [mouseY]
            fsub [(circle ptr [si]).y] ; st: [mouseY - y1]
            fadd st, st ; st: [(mouseY - y1) * 2]
            fstp [(circle ptr [si]).vy] ; st: []
        @@:
        mov mouseButtonStatus, bl
        
        ; wall collision
        fld [heightMinusRadius] ; st: [h - r]
        fld [widthMinusRadius] ; st: [w - r][h - r]
        fld [circleRadius] ; st: [r][w - r][h - r]
        invoke wallCollision, addr [redBall]
        invoke wallCollision, addr [blueBall]
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
        
        ; round not over yet && all velocity == 0: end of round
        mov eax, 7fffffffh ; 01111...111b, float has +0 and -0
        .if [gameStatus] & 1b && !([redBall.vx] & eax) && !([redBall.vy] & eax) && !([blueBall.vx] & eax) && !([redBall.vy] & eax)
            xor [gameStatus], 1b ; end round
            .if [gameStatus] & 100b ; who's next
                or [gameStatus], 10b ; blue's turn
                and [gameStatus], 11111011b ; next is red
            .else
                and [gameStatus], 11111101b ; red's turn
                or [gameStatus], 100b ; next is blue
            .endif
        .endif
        
        ; invoke printBinary, [gameStatus]
        ; printNewline
        
        invoke printInt16, [redBall.score]
        printNewline
        invoke printInt16, [blueBall.score]
        printNewline
        
        ; invoke printUint16, [debugVariable]
        ; printNewline
        
        
        ; draw to backbuffer
        mov ax, backBuffer
        mov es, ax
        ; draw line
        mov ax, 03h
        int 33h
        shr cx, 1
        mov si, dashedlineFlow
        dec dashedlineFlow
        and dashedlineFlow, 0111b
        .if !([gameStatus] & 00000001b) ; if round not start yet
            .if [gameStatus] & 00000010b ; if player2's turn
                invoke drawLine, blueBall.integerX, blueBall.integerY, cx, dx
            .else
                invoke drawLine, redBall.integerX, redBall.integerY, cx, dx
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
        mov ax, backBuffer
        mov ds, ax
        mov ax, 0a000h
        mov es, ax
        xor si, si
        xor di, di
        mov cx, (320 * 200) / 4
        rep movsd
        pop ds
        
        .if sword ptr [redBall.score] >= 7
            lea si, [redBall]
            .break
        .elseif sword ptr [blueBall.score] >= 7
            lea si, [blueBall]
            .break
        .endif
        
        mov ah, 06h
        mov dl, 0ffh
        int 21h
        .continue .if zero?
        .if al == 1bh ; esc
            jmp return
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
        
    return:
        ret
gameStart endp
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
exitGame proc
    mov ax, 03h
    int 10h
    .exit
exitGame endp

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
    ;   f = m * a = k_f * n * (-v / ||v||), m = mass, a = accelleration, k_f = friction coefficient, n = normal force
    ;   a = -k_f * n / m * (-v / ||v||) = -k * v / ||v||
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
    ; if v < 0: v = 0
    ftst
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
    
    ; left edge
    fld [si].x ; st: [x][r][w - r][h - r][...]
    fcomi_ 1 ; cmp x, r
    ja @f ; x > r
    bt [si].vx, 31 ; sign bit of float
    jnc @f ; vx > 0
        ; x <= r && vx < 0
        btr [si].vx, 31 ; neg -> pos
        
        ; wall effect index = 2 * (wall section)
        ;   = 2 * (25 - y / 40)
        mov ax, [si].integerY
        mov bl, 40
        div bl ; y / 40 = al ... ah
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
    
    ; right edge
    fcomi_ 2 ; cmp x, w - r
    jb @f ; x < w - r
    bt [si].vx, 31 ; sign bit of float
    jc @f ; vx < 0
        ; x >= w - r && vx > 0
        bts [si].vx, 31 ; pos -> neg
        
        ; wall effect index = 2 * (wall section)
        ;   = 2 * (8 + y / 40)
        mov ax, [si].integerY
        mov bl, 40
        div bl ; y / 40 = al ... ah
        xor ah, ah
        add ax, 8
        shl ax, 1
        mov bx, ax
        cmp [wallEffect + bx], noEffect
        je topBottomCollision
        invoke [wallEffect + bx]
    @@:
    
    topBottomCollision:
    ; top edge
    fld [si].y ; st: [y][x][r][w - r][h - r]
    fcomi_ 2 ; cmp y, r
    ja @f ; y > r
    bt [si].vy, 31 ; sign bit of float
    jnc @f ; vy > 0
        ; y <= r && vy < 0
        btr [si].vy, 31 ; neg -> pos
        
        ; wall effect index = 2 * (wall section)
        ;   = 2 * (x / 40)
        mov ax, [si].integerX
        mov bl, 40
        div bl ; y / 40 = al ... ah
        xor ah, ah
        shl ax, 1
        mov bx, ax
        cmp [wallEffect + bx], noEffect
        je return
        invoke [wallEffect + bx]
        jmp return
    @@:
    
    ; bottom edge
    fcomi_ 4 ; cmp y, h - r
    jb @f ; y < h - r
    bt [si].vy, 31 ; sign bit of float
    jc @f ; vy < 0
        ; y >= h - r && vy > 0
        bts [si].vy, 31 ; pos -> neg
        
        ; wall effect index = 2 * (wall section)
        ;   = 2 * (20 - x / 40)
        mov ax, [si].integerX
        mov bl, 40
        div bl ; y / 40 = al ... ah
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
        fstp st ; st: [x][r][w - r][h - r][...]
        fstp st ; st: [r][w - r][h - r][...]
        assume si: nothing
        ret
wallCollision endp
drawRectangle proc, left: word, top: word, squareWidth: word, height: word, color: byte
    mov ax, 320
    mul top
    add ax, left
    mov di, ax
    mov bx, 320
    sub bx, squareWidth
    mov al, color
    mov cx, height
    @@:
        push cx
        mov cx, squareWidth
        rep stosb
        add di, bx
        pop cx
        loop @b
    ret
drawRectangle endp
drawCircle proc, centerX: word, centerY: word, color: byte
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
    
    mov al, color ; color white
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
        or [gameStatus], 100b ; next is blue
    .endif
    ret
extraTurn endp

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
printUint32 proc uses ax cx dx ds si, number: dword
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
printUint32 endp
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
printBinary proc uses ax cx dx, number: byte
    mov ah, 02h
    mov cx, 8
    @@:
        mov dl, 30h
        shl number, 1
        adc dl, 0
        int 21h
    loop @b
    
    ret
printBinary endp
printHex32 proc uses ax cx dx, number: dword
    mov ah, 02h
    mov dl, '0'
    int 21h
    mov dl, 'x'
    int 21h
    mov cx, 8
    @@:
        rol number, 4
        mov dl, 0fh
        and dl, byte ptr [number]
        .if dl < 10
            add dl, '0'
        .else
            add dl, 'A' - 10
        .endif
        int 21h
        loop @b
    
    ret
printHex32 endp

.fardata coverImage
imageWidth equ 320
imageHeight equ 200
imageLeft equ (320 - imageWidth) / 2 ; center
imageTop equ (200 - imageHeight) / 2 ; center
image \
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,21,24,24,21,28,28
db 28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,26,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,21,22,22
db 21,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,24,25,24,21,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,25,26,26,30,31,31,31,31,31,31,31,31,31,31,31,31
db 31,31,31,31,31,31,31,27,20,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,25,29,30,31,31,31,31,24,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,20,26,31,31,31,31,24,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,26,26,26,29,30,30,30,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,27,21,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,22,30,31,31,31,31,31,31,31,31,24,19,19,19,19,19,19,19,19,19,19,19,19,19,19,24,31,31
db 31,31,31,20,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,21,20,21,22,22,22
db 22,26,29,28,28,30,31,31,31,31,31,31,31,31,31,31,31,27,26,27,19,19,19,19,19,19,19,20,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,25,31,31,31,31,31,31,31,31
db 31,31,26,19,19,19,19,19,19,19,19,19,19,19,19,19,19,23,31,31,31,31,29,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,19,19,19,19,19,19,25,28,27,27,31,31,31,31,29
db 28,28,28,30,31,31,31,29,29,29,29,29,29,29,29,29,29,29,29,21,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,25,31,31,31,31,31,23,27,31,31,31,31,23,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,22,31,31,31,31,29,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,20,19,20,20,20,20,20,25,27,24,24,25,26,26,26,26,28,27,27,31,31,31,31,31,31,31
db 31,31,31,31,31,31,31,31,31,28,23,20,20,20,20,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,25
db 31,31,31,31,31,21,19,25,31,31,31,31,31,21,19,19,19,19,19,19,19,19,19,19,19,19,19,22,31,31
db 31,31,30,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,19,20,20,20,20
db 20,25,26,20,19,19,19,19,19,24,26,25,25,27,27,19,19,28,29,30,30,30,31,31,31,31,31,31,31,29
db 27,28,28,28,28,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,31,31,31,31,31,28,19,19,21,31,31
db 31,31,31,22,19,19,19,19,19,19,19,19,19,19,19,19,19,20,28,31,31,31,31,20,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,20,23,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,19,20,20,20,20,20,25,26,21,19,20,20,20,19,25
db 21,19,19,19,19,19,19,25,26,27,28,29,31,31,31,31,31,31,31,31,31,31,31,31,31,28,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,27,31,31,31,31,27,19,19,19,30,31,31,31,30,20,19,19,19,19,19,22
db 24,24,19,19,19,19,19,19,28,31,31,31,31,20,19,19,19,19,19,21,24,24,20,19,19,19,19,19,23,31
db 31,31,23,19,20,24,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,20,19,20,20,20,20,20,25,26,21,19,20,20,20,19,25,21,19,19,19,19,19,19,25,26,27
db 27,29,30,30,30,30,30,30,31,31,31,31,31,31,31,29,24,21,20,27,27,27,26,26,26,26,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,28
db 31,31,31,31,24,19,19,21,31,31,31,31,21,19,19,19,19,23,30,31,31,31,30,22,19,19,19,20,31,31
db 31,31,22,19,19,19,19,22,30,31,31,31,31,23,19,19,19,26,31,31,31,31,21,19,29,31,31,24,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,19,20,20,20,20
db 20,25,26,21,19,20,20,20,19,25,21,19,20,20,20,20,20,25,27,23,21,22,22,22,22,22,22,23,26,28
db 29,28,30,31,31,31,30,30,30,30,30,30,30,30,30,30,27,20,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,21,31,31,31,31,31,25,19,19,23,29,29
db 27,21,19,19,19,19,24,31,31,31,31,31,31,30,19,19,19,20,31,31,31,31,26,19,19,19,22,31,31,31
db 31,31,31,31,19,19,19,31,31,31,31,31,19,25,31,31,31,30,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,19,20,20,20,20,20,26,26,21,20,20,20,20,19,25
db 21,19,20,20,20,20,19,25,27,21,19,19,19,19,19,19,19,19,24,27,27,27,30,31,31,31,31,31,31,31
db 31,31,31,31,31,31,27,23,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,24,31,31,31,31,31,31,20,19,19,19,19,19,19,19,19,19,23,31,31,31,31
db 31,26,21,19,19,19,19,20,31,31,31,31,31,19,19,22,31,31,31,31,31,27,22,20,19,19,19,26,31,31
db 31,31,28,31,31,31,31,31,20,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,20,19,20,20,20,20,20,26,26,21,20,20,20,20,19,25,21,19,20,20,20,20,20,25,27,21
db 19,20,20,20,20,20,20,20,25,25,23,24,26,26,26,26,28,29,29,30,31,31,31,31,31,31,29,26,29,28
db 29,29,29,29,29,29,29,28,20,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,25,31
db 31,31,31,31,26,19,19,19,19,19,19,19,19,19,19,27,31,31,31,31,23,19,21,25,25,22,19,20,31,31
db 31,31,29,19,19,26,31,31,31,31,25,19,19,24,26,23,19,22,31,31,31,31,31,31,31,31,31,31,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,19,20,20,20,20
db 20,26,26,21,20,20,20,20,19,25,21,19,20,20,20,20,20,25,27,21,19,20,20,20,20,20,20,20,25,22
db 19,20,20,20,20,20,24,27,27,29,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,26,21
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,24,31,31,31,31,31,24,19,19,19,19,19
db 19,19,19,19,19,31,31,31,31,25,19,22,31,31,31,28,19,20,31,31,31,31,28,19,19,30,31,31,31,28
db 19,21,31,31,31,31,19,21,31,31,31,31,31,31,30,28,31,30,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,19,20,20,20,20,20,26,26,21,20,20,20,20,19,25
db 21,19,20,20,20,20,20,25,26,21,19,20,20,20,20,20,20,20,25,22,19,20,20,20,20,20,25,26,25,27
db 28,29,29,30,30,30,30,31,31,31,31,31,31,31,31,31,31,31,28,19,21,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,23,31,31,31,31,31,29,19,19,19,19,19,19,19,19,19,25,31,31,31,31,19
db 20,31,31,31,31,30,19,19,31,31,31,31,28,19,24,31,31,31,31,19,19,31,31,31,31,31,19,23,31,31
db 31,31,31,26,19,19,20,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,20,19,19,19,19,19,19,25,26,21,19,20,20,20,19,25,21,19,20,20,20,20,20,25,26,21
db 19,20,20,20,20,20,20,20,25,22,19,20,20,20,20,20,25,21,18,19,19,19,19,24,26,26,27,29,31,31
db 31,31,31,31,31,31,31,31,31,31,31,26,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,22,31
db 31,31,31,31,28,19,19,19,19,19,19,19,19,19,30,31,31,31,29,19,20,27,31,31,31,31,24,20,31,31
db 31,31,28,19,29,31,31,31,30,19,19,26,31,31,31,31,25,20,31,31,31,31,26,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,18,19,19,19,19
db 19,25,26,21,20,20,20,20,19,25,21,19,20,20,20,20,20,25,27,21,19,20,20,20,20,20,20,20,25,22
db 19,20,20,20,20,20,25,21,18,19,19,19,18,24,26,26,26,30,31,31,31,31,31,31,31,31,31,31,31,31
db 31,29,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,25,31,31,31,31,28,19,19,19,19,20
db 20,19,19,21,31,31,31,31,28,19,19,23,31,31,31,31,25,21,31,31,31,31,28,19,31,31,31,31,30,19
db 19,22,31,31,31,31,27,19,31,31,31,31,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,24,23,24,24,24,24,24,26,25,21,19,20,20,20,19,25
db 21,19,20,20,20,20,20,24,27,22,19,20,20,20,20,20,20,20,25,22,19,20,20,20,20,20,25,21,19,20
db 20,20,20,25,26,23,22,24,24,24,24,24,29,29,28,28,28,28,28,28,28,29,29,29,21,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,21,28,31,31,31,31,27,19,19,19,19,24,31,26,19,20,31,31,31,31,30,20
db 19,27,31,31,31,31,19,22,31,31,31,31,29,19,30,31,31,31,31,22,19,26,31,31,31,31,21,20,31,31
db 31,31,31,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,20,29,29,29,29,29,29,27,25,20,19,19,19,19,19,25,21,19,20,20,20,20,20,24,27,21
db 19,19,20,20,20,20,20,20,25,22,19,20,20,20,20,20,25,21,19,20,20,20,19,25,25,20,19,19,19,19
db 19,19,27,28,25,27,26,26,26,26,26,29,31,31,28,20,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,24,31
db 31,31,31,31,29,19,19,19,19,29,31,31,27,19,31,31,31,31,31,24,19,22,31,31,31,31,23,20,28,31
db 31,31,30,19,29,31,31,31,31,29,19,21,31,31,31,31,23,20,31,31,31,31,31,21,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,25,26,26,26,26,26
db 26,26,26,22,21,21,21,21,19,25,21,19,20,20,20,20,19,24,27,21,18,19,19,19,19,19,19,19,25,22
db 19,20,20,20,20,20,25,21,19,20,20,20,20,25,25,20,19,19,19,19,19,19,27,28,26,27,27,27,27,27
db 26,29,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,30,31,31,31,31,31,24,19,19,25,31
db 31,31,31,23,28,31,31,31,31,21,19,21,31,31,31,31,23,19,28,31,31,31,31,19,28,31,31,31,31,25
db 19,20,31,31,31,31,23,21,31,31,31,31,31,20,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,19,20,20,20,20,20,25,27,27,27,27,27,27,23,25
db 21,19,19,19,19,19,19,24,27,22,20,20,20,20,20,21,21,21,25,22,19,20,20,20,20,20,25,21,19,20
db 20,20,20,25,25,21,19,19,19,19,19,19,27,28,26,27,27,27,27,27,26,29,31,31,28,22,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,24,31,31,31,31,31,31,26,26,31,31,31,31,31,21,26,31,31,31,31,23
db 19,24,31,31,31,31,19,19,31,31,31,31,29,19,26,31,31,31,31,27,19,22,31,31,31,31,20,21,31,31
db 31,31,31,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,20,19,20,20,19,19,20,25,27,28,28,28,28,28,27,27,21,19,19,19,19,19,19,24,26,27
db 28,28,28,28,28,28,28,27,27,21,19,20,20,20,20,20,25,21,19,20,20,20,19,25,25,21,19,19,20,20
db 19,19,27,28,26,27,27,27,27,27,26,29,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 30,31,31,31,31,31,31,31,31,31,31,31,28,19,21,25,31,31,31,31,26,30,31,31,31,28,19,19,30,31
db 31,31,28,19,21,24,29,31,31,31,26,30,31,31,31,31,19,19,28,31,31,31,31,20,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,19,20,20,20,20
db 20,25,26,22,21,21,21,21,21,25,25,25,25,25,25,26,25,26,26,28,29,29,29,29,29,29,29,28,27,21
db 19,19,19,19,19,20,25,21,19,20,20,20,19,25,26,21,19,20,20,20,19,19,27,28,26,27,27,27,27,27
db 26,29,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,22,31,31,31,31,31,31,31,31,31
db 31,30,20,19,19,19,31,31,31,31,31,31,31,31,31,20,19,21,31,31,31,31,31,22,19,19,31,31,31,31
db 31,31,31,31,31,23,19,22,31,31,31,31,31,20,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,19,20,20,20,20,20,25,26,20,19,19,19,19,18,24
db 27,27,27,28,28,29,29,27,26,24,23,23,23,23,23,23,23,23,26,21,18,19,19,19,19,19,25,22,19,20
db 20,20,19,25,26,21,19,20,20,20,19,19,27,28,26,27,27,27,27,27,26,29,31,31,28,22,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,23,29,31,31,31,31,31,28,29,28,20,19,19,19,21,29,31,31,31
db 31,31,31,29,22,19,19,25,31,31,31,31,31,29,19,20,29,31,31,31,31,31,31,29,23,19,19,25,31,31
db 31,31,31,24,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,20,19,20,20,20,20,20,25,26,21,19,20,20,20,19,25,24,22,23,23,24,24,24,26,27,21
db 19,19,19,19,19,19,19,19,24,24,23,24,24,24,24,23,26,22,19,20,20,20,19,25,26,21,19,20,20,20
db 19,19,27,28,26,27,27,27,27,27,26,29,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,23,27,29,28,25,22,24,22,19,19,19,19,19,22,31,31,31,31,26,20,19,19,19,19,22,29,31
db 31,31,27,21,19,19,19,25,30,31,31,27,20,19,19,19,19,26,31,31,31,31,28,20,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,19,20,20,20,20
db 20,25,26,21,19,20,20,20,19,25,21,19,20,20,20,20,20,25,27,21,19,20,20,20,20,20,20,20,24,28
db 30,31,31,31,31,29,27,21,19,19,19,19,19,24,26,21,19,20,20,20,20,19,27,28,26,27,27,27,27,27
db 26,29,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,25,29,29,30,31,31
db 31,25,19,19,20,26,31,31,31,31,22,19,19,19,19,19,19,19,19,21,23,22,19,19,19,19,19,19,19,20
db 20,19,19,19,19,19,19,28,31,31,31,31,25,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,22,20,19,20,20,20,20,20,25,27,21,19,20,20,20,19,25
db 21,19,20,20,20,20,19,25,27,22,19,20,20,20,20,20,20,20,24,27,29,29,29,29,29,27,27,22,20,20
db 20,20,21,25,26,21,19,20,20,20,20,19,27,28,25,26,26,26,26,27,26,29,31,31,28,22,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,23,31,31,31,31,31,31,31,31,31,25,19,26,31,31,31,31,29
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,30,31,31
db 31,31,25,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,22,20,19,20,20,20,20,20,25,26,21,19,20,20,20,19,25,21,19,20,20,20,20,20,24,27,22
db 19,20,20,20,20,20,20,20,25,23,21,21,21,21,21,21,25,27,28,28,28,28,29,28,25,21,19,19,19,19
db 19,19,27,26,22,23,24,24,24,24,23,28,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,27,31,31,31,31,31,31,31,31,31,31,25,19,25,31,31,31,31,27,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,26,31,31,31,30,21,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,28,20,19,20,20,20,20
db 20,25,26,21,19,20,20,20,19,25,21,19,20,20,20,20,20,24,27,22,19,20,20,20,20,20,20,20,25,21
db 19,19,19,19,19,20,24,28,29,29,29,29,29,28,25,20,19,19,19,19,19,19,27,25,21,22,22,22,22,23
db 22,27,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,26,31,31,31,31,30,22,27,31,31
db 31,31,23,19,23,31,31,31,31,28,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,24,31,31,31,29,20,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,21,27,27,27,27,27,27,27,27,27,27
db 27,27,27,27,27,27,27,27,27,26,26,27,27,27,21,19,20,19,19,20,19,25,27,21,19,20,20,20,19,25
db 21,19,20,20,20,20,20,24,27,21,19,20,20,20,20,20,20,20,25,22,19,20,20,20,20,20,25,24,23,23
db 23,23,23,25,26,24,24,24,24,24,23,23,28,28,26,26,26,26,26,26,26,29,31,31,28,22,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,26,31,31,31,31,31,20,19,24,31,31,31,31,31,20,21,31,31,31,31,28
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,24,31,31
db 31,31,24,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,27,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26
db 26,26,26,26,20,18,19,19,19,19,19,25,27,21,20,20,20,20,19,25,21,19,20,20,20,20,20,24,27,21
db 19,20,20,20,20,20,20,20,25,22,19,20,20,20,20,20,25,21,19,19,19,20,19,24,27,28,29,29,29,29
db 29,28,29,31,31,31,31,31,31,31,31,31,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,31
db 31,31,31,31,28,19,19,20,31,31,31,31,31,21,19,26,31,31,31,30,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,24,31,31,31,31,21,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,26
db 26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,25,25,21,21,21,21,21,21
db 21,26,26,21,19,20,20,20,19,25,21,19,20,20,20,20,20,24,27,21,19,20,20,20,20,20,20,20,25,22
db 19,20,20,20,20,20,25,22,19,20,20,20,20,25,26,25,26,26,26,26,26,25,28,30,30,29,30,30,30,29
db 29,30,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,27,31,31,31,31,27,19,19,19,30,31
db 31,31,29,20,20,28,31,31,31,29,19,19,19,19,19,21,24,27,28,22,19,19,19,19,19,19,19,19,19,25
db 29,30,31,29,21,19,19,27,31,31,31,31,19,19,19,21,20,19,19,19,19,19,19,22,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,26,26,26,26,26,26,26,26,26,26,26
db 26,26,26,26,26,26,26,26,26,26,26,26,25,25,24,27,28,27,27,27,27,27,26,20,19,19,19,19,19,25
db 21,19,20,20,20,20,20,24,27,21,19,20,20,20,20,20,20,20,25,22,19,20,20,20,20,20,25,22,19,20
db 20,20,20,25,25,21,19,20,20,20,20,19,27,29,26,26,26,26,26,25,25,29,31,31,28,21,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,29,31,31,31,31,24,19,19,22,31,31,31,30,20,19,22,31,31,31,31,21
db 19,19,19,20,28,31,31,31,31,31,29,24,19,19,19,19,19,23,28,31,31,31,31,31,29,19,19,27,31,31
db 31,30,19,21,29,31,31,28,20,19,19,21,28,31,30,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,27,26,26,26,26,27,26,26,26,27,27,27,27,27,27,26,26,26,26,27,27
db 27,27,27,27,23,26,29,28,28,28,29,27,26,20,18,18,19,19,19,25,21,19,20,20,20,20,20,24,27,21
db 19,20,20,20,20,20,20,20,25,22,19,20,20,20,20,20,25,22,19,20,20,20,20,25,25,20,19,19,19,19
db 19,19,27,29,26,26,26,26,26,26,25,29,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,22,31
db 31,31,31,31,25,19,19,22,27,26,24,20,19,19,21,31,31,31,31,26,19,19,19,30,31,31,31,31,31,31
db 31,31,29,22,19,22,29,31,31,31,31,31,31,31,31,22,19,25,31,31,31,29,19,29,31,31,31,31,30,20
db 19,31,31,31,31,27,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 20,21,27,27,27,27,22,22,22,27,22,23,23,27,27,27,27,27,27,27,27,27,27,27,20,21,22,22,22,22
db 22,26,27,26,25,25,25,26,26,27,21,19,20,20,20,20,19,25,27,21,19,20,20,20,20,20,20,20,25,22
db 19,20,20,20,20,20,25,22,19,20,20,20,20,25,25,20,19,19,20,20,20,19,27,28,26,27,27,27,27,27
db 26,29,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,24,31,31,31,31,31,31,20,19,19,19,19
db 19,19,19,19,21,31,31,31,31,30,19,19,24,31,31,29,29,31,31,31,31,31,31,28,19,29,31,31,31,31
db 31,28,25,29,31,22,19,23,31,31,31,30,29,31,31,31,31,31,31,25,21,31,31,31,31,31,20,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,20,20,19,19,19,19,19,19,25,27,26,26,26,26,26,26,27
db 20,18,19,19,19,19,19,24,27,21,19,20,20,20,20,20,20,20,25,22,19,20,20,20,20,20,25,22,19,20
db 20,20,20,25,25,20,19,19,20,19,19,19,27,29,26,27,27,27,27,27,26,29,31,31,28,21,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,25,31,31,31,31,31,25,19,19,19,19,19,19,19,19,19,21,31,31,31,31,28
db 19,19,22,26,22,19,19,19,26,31,31,31,31,21,19,31,31,31,31,30,21,19,19,19,20,19,19,24,31,31
db 31,31,31,31,31,31,31,31,31,27,21,31,31,31,31,27,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,20,19,20,20,20,20,20,25,26,21,20,21,21,21,20,25,24,23,24,23,24,24,24,26,27,21
db 19,20,20,20,20,20,20,20,25,22,19,20,20,20,20,20,25,22,19,20,20,20,20,25,25,20,19,20,20,19
db 19,19,27,29,26,27,27,27,27,27,26,29,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,24,31
db 31,31,31,31,24,19,19,19,19,19,19,19,19,19,20,31,31,31,31,27,19,19,19,19,19,19,22,21,23,31
db 31,31,31,20,20,31,31,31,31,26,20,24,23,19,19,19,19,25,31,31,31,31,31,29,22,30,31,31,31,29
db 19,21,26,28,24,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,19,20,20,20,20
db 20,25,25,20,19,20,20,20,19,25,28,30,31,31,31,31,31,27,26,21,19,20,20,20,20,20,20,20,25,22
db 19,20,20,20,20,20,25,22,19,20,20,20,20,25,25,20,19,20,19,19,19,19,27,29,26,27,27,27,27,27
db 26,29,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,23,31,31,31,31,31,30,19,19,19,19,19
db 19,19,19,19,20,31,31,31,31,26,19,19,19,19,24,30,31,31,31,31,31,31,31,23,22,31,31,31,31,31
db 31,31,31,30,22,19,19,26,31,31,31,31,31,19,22,31,31,31,31,26,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,20,20,19,20,20,20,20,20,25,25,20,19,20,20,20,19,25
db 27,28,29,29,29,29,29,27,26,21,19,19,19,19,19,20,19,21,25,22,19,20,20,20,20,20,25,22,19,20
db 20,20,20,25,25,20,19,20,20,19,20,19,27,29,26,27,27,27,27,27,26,29,31,31,28,21,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,22,31,31,31,31,31,28,19,19,19,19,19,19,19,19,19,24,31,31,31,31,27
db 19,19,22,29,31,31,31,31,31,31,31,31,31,20,22,31,31,31,31,31,31,31,31,31,31,21,19,20,28,31
db 31,31,25,19,27,31,31,31,27,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,20,19,20,20,20,20,20,25,25,21,19,20,20,20,19,25,22,21,21,21,21,21,21,25,26,23
db 22,22,22,22,23,22,23,23,26,21,19,20,20,20,20,20,25,22,19,20,20,20,20,25,25,20,19,19,19,19
db 20,19,27,29,26,27,27,27,27,27,26,29,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,25
db 31,31,31,31,28,19,19,19,19,20,22,19,19,19,25,31,31,31,31,27,19,19,31,31,31,31,31,28,28,31
db 31,31,31,20,20,28,29,31,31,31,31,31,31,31,31,30,19,20,29,31,31,31,22,19,25,31,31,31,30,23
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,20,19,20,20,20,20
db 20,25,25,21,19,20,20,20,19,25,21,19,19,19,19,19,19,25,26,27,28,27,27,28,28,28,28,27,27,21
db 19,20,20,20,20,20,25,21,19,20,20,20,20,25,25,21,19,20,20,20,20,19,27,27,24,25,25,25,25,25
db 24,28,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,22,29,31,31,31,31,28,19,19,19,19,25
db 31,28,20,19,25,31,31,31,31,28,19,23,31,31,31,31,26,19,25,31,31,31,31,24,19,19,19,22,23,20
db 30,31,31,31,31,31,21,25,31,31,31,31,22,19,25,31,31,31,31,26,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20
db 20,19,20,20,19,19,19,20,20,20,20,20,20,20,20,19,20,20,20,20,20,25,25,21,19,20,20,20,19,25
db 21,19,20,20,20,20,19,25,27,24,23,23,23,23,23,23,23,23,26,21,18,19,19,19,19,19,25,22,19,20
db 20,20,20,25,25,20,19,19,19,19,19,19,21,25,22,23,23,23,23,23,21,27,31,31,28,21,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,23,31,31,31,31,31,29,19,19,19,19,29,31,31,29,19,21,27,31,31,31,29
db 19,26,31,31,31,31,22,19,25,31,31,31,31,25,19,19,19,19,19,19,19,27,31,31,31,31,20,24,31,31
db 31,31,23,19,28,31,31,31,31,24,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,27,27,27,25,23,23,19,19,19,19,20,27,27,27,27,27,27,27,27,27,27,27,27
db 27,27,26,26,20,19,20,20,20,20,20,25,25,21,19,20,20,20,19,25,21,19,20,20,20,20,19,25,27,20
db 18,18,18,18,18,18,18,19,25,24,23,23,23,23,23,22,25,22,19,20,20,20,19,24,25,22,22,22,22,21
db 21,20,19,25,21,22,22,22,22,22,20,26,31,31,28,20,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,30
db 31,31,31,31,31,25,19,19,26,31,31,31,31,22,19,27,31,31,31,30,19,28,31,31,31,31,22,20,28,31
db 31,31,29,22,19,20,23,19,19,19,19,27,31,31,31,31,19,25,31,31,31,31,25,19,24,31,31,31,31,24
db 19,20,25,28,27,20,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,25,26,26,26
db 26,27,27,20,19,19,19,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,25,25,20,19,20,20,20,20
db 20,25,25,21,20,20,20,20,19,25,21,19,20,20,20,20,20,25,27,21,19,20,20,20,20,20,20,20,24,30
db 31,31,31,31,31,28,27,21,18,19,19,19,19,24,26,27,29,28,28,27,26,24,27,21,24,24,24,24,24,24
db 23,27,31,31,28,20,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,23,31,31,31,31,31,31,28,28,31,31
db 31,31,31,21,20,31,31,31,31,27,19,29,31,31,31,31,31,31,31,31,31,31,25,19,19,27,31,28,24,26
db 29,31,31,31,31,29,19,25,31,31,31,31,22,19,24,31,31,31,31,22,19,30,31,31,31,25,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,26,26,26,26,26,27,27,20,19,19,19,27,26,26
db 26,26,26,26,26,26,26,26,26,26,26,26,25,25,20,19,20,20,20,20,20,25,25,21,20,20,20,20,19,25
db 21,19,20,20,20,20,20,25,27,21,19,20,20,20,20,20,20,20,24,29,29,29,29,29,29,28,27,22,20,21
db 21,21,20,25,26,27,28,28,28,28,28,27,29,31,30,30,30,30,30,30,30,30,31,31,28,21,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,30,31,31,31,31,31,31,31,31,31,31,31,28,19,19,29,31,31,31,26
db 19,21,29,31,31,31,31,31,31,31,31,31,31,21,20,31,31,31,31,31,31,31,31,31,26,20,19,25,31,31
db 31,30,19,19,29,31,31,31,31,21,19,31,31,31,31,31,21,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,27,26,26,26,26,27,27,22,19,19,19,27,26,26,26,26,26,26,26,26,26,26,26,26
db 26,26,25,25,20,19,20,20,20,20,20,25,26,21,19,20,20,20,19,25,21,19,20,20,20,20,20,25,26,21
db 19,20,20,20,20,20,20,20,25,23,20,20,20,20,20,23,25,28,29,29,29,29,29,27,25,21,20,20,20,20
db 20,20,27,30,28,29,29,29,29,29,29,30,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 21,31,31,31,31,31,31,31,31,31,31,30,19,19,23,31,31,31,31,31,21,19,29,31,31,31,31,31,30,31
db 31,31,31,24,24,31,31,31,31,31,31,31,31,31,25,19,19,27,31,31,31,31,22,19,31,31,31,31,31,25
db 19,30,31,31,31,31,20,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,27,27,27
db 24,23,23,19,19,19,19,20,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,20,19,20,20,20,20
db 20,25,26,21,19,20,20,20,19,25,21,19,20,20,20,20,20,25,26,21,19,20,20,20,20,20,20,21,25,21
db 18,19,19,19,18,22,25,30,31,31,31,31,31,28,24,20,19,19,19,19,19,18,27,29,26,27,27,27,27,27
db 26,29,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,26,31,31,31,31,31,26,27
db 27,19,19,19,27,31,31,31,31,31,28,19,22,29,31,31,29,21,27,31,31,31,31,26,19,29,31,31,31,31
db 31,31,31,27,20,19,20,31,31,31,31,28,20,19,28,31,31,31,31,31,20,22,30,31,27,21,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,19,19,19,19,19,19,19,19,19,19,19
db 19,20,19,20,20,20,20,20,20,20,20,20,20,28,20,19,20,20,20,20,20,25,26,21,19,20,20,20,19,25
db 21,19,20,20,20,20,20,25,26,21,19,20,20,20,20,20,20,21,25,22,19,20,20,20,20,22,26,25,25,25
db 25,25,25,26,25,21,20,20,20,20,20,19,27,28,26,27,27,27,27,27,26,29,31,31,28,21,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,24,31,29,27,24,23,23,25,21,19,19,19,23,28,31,31,31,26
db 20,19,19,19,25,26,19,19,23,27,31,31,31,28,20,19,23,30,31,31,31,30,23,19,20,27,31,31,28,27
db 23,19,19,19,19,27,31,31,31,29,21,19,19,20,19,19,19,19,19,19,19,19,20,26,30,30,23,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,27,20,19,20,20,20,20,20,25,26,21,19,20,20,20,19,25,21,19,20,20,20,20,20,25,26,21
db 19,20,19,18,18,19,20,21,25,21,19,19,19,19,19,22,26,21,19,19,19,19,19,24,25,21,20,20,20,20
db 20,19,27,28,26,27,27,27,27,27,26,29,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,21
db 27,31,31,31,24,19,23,30,31,31,31,26,19,19,19,19,21,23,22,19,19,19,19,19,19,19,19,19,20,26
db 31,31,31,31,21,19,24,31,31,31,31,24,19,20,31,31,31,31,19,19,19,19,19,19,19,19,22,23,22,19
db 19,19,19,19,19,19,19,19,19,19,19,19,28,31,31,31,23,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,27,19,18,19,19,19,19
db 19,25,26,21,19,20,20,20,19,25,21,19,20,20,20,20,20,25,26,21,19,20,20,20,20,20,20,21,25,23
db 21,22,22,22,21,24,26,21,19,20,20,20,19,24,25,21,19,20,20,20,20,19,27,28,26,27,27,27,27,27
db 26,29,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,22,31,31,31,31,31,31,30,31,31,31,31
db 31,31,29,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,26,31,31,31,31,29,19,22,31,31,31,31
db 31,20,19,23,31,31,31,31,25,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,21,25,27,27,22,19
db 19,19,29,31,31,31,29,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 20,20,20,20,27,27,27,27,27,19,19,19,19,27,20,19,20,20,20,20,20,25,26,21,19,20,20,20,19,25
db 21,19,20,20,20,20,20,25,27,21,19,19,26,28,29,23,19,21,25,28,29,29,30,29,29,28,27,21,19,20
db 20,20,19,24,25,21,19,20,20,20,20,19,27,28,26,27,27,27,27,27,26,29,31,31,28,22,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,20,30,31,31,31,31,31,31,31,31,31,31,31,31,24,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,25,31,31,31,31,28,19,21,31,31,31,31,30,19,19,24,31,31,31,31,29,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,24,31,31,31,31,25,19,19,19,31,31,31,31,31,21,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,27,27,27,22,19,19,19,20,27,27,27,19,19,19,19,19
db 19,27,27,27,27,27,27,27,27,27,27,27,27,27,26,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27
db 27,27,27,27,25,26,26,26,26,27,27,26,26,21,19,19,19,19,19,25,21,19,20,20,20,20,20,25,27,21
db 19,19,26,29,31,23,19,21,24,29,30,30,30,30,30,28,26,21,19,20,20,20,20,25,25,20,20,20,20,20
db 20,19,27,28,26,27,27,27,27,27,26,29,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20
db 31,31,31,31,31,30,27,31,31,31,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19,24,31
db 31,31,31,29,19,20,31,31,31,31,31,20,19,20,27,31,31,31,22,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,30,31,31,31,31,22,19,19,19,25,31,31,31,26,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,26,26,26,27,19,19,19,27,26,26,27,19,19,19,19,19,24,26,26,26,26,26,26,26,26,26
db 26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,27,30,30,30,30,30
db 31,27,26,20,19,19,19,19,18,25,21,19,20,20,20,20,20,25,27,21,19,19,25,28,30,23,19,21,25,28
db 29,28,29,29,29,28,26,21,19,20,20,20,20,25,25,20,20,20,20,20,20,19,27,28,26,27,27,27,27,27
db 26,29,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,30,31,31,31,31,23,19,26,31,31
db 31,31,31,24,19,19,19,19,19,19,19,19,19,19,19,19,19,19,23,31,31,31,31,29,19,19,31,31,31,31
db 31,20,19,19,19,22,26,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,28,31,31,31,31,20,19
db 19,19,19,21,25,24,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,26,26,26,27,19,19
db 19,27,26,26,26,19,19,19,19,19,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26
db 26,26,26,26,26,26,26,26,26,26,26,26,26,26,23,24,25,24,24,25,25,26,27,25,25,25,26,25,25,26
db 21,19,20,20,20,20,20,25,27,21,19,19,22,23,24,21,19,21,25,25,24,23,24,24,24,24,25,21,19,20
db 20,20,20,25,25,20,20,20,20,20,20,19,27,28,26,27,27,27,27,27,26,29,31,31,28,26,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,27,31,31,31,31,31,23,19,26,31,31,31,31,31,21,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,26,31,31,31,30,19,19,23,31,31,31,31,21,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,20,19,19,19,19,19,26,31,31,31,30,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,26,26,26,27,19,19,19,27,26,26,27,19,19,19,19,19
db 24,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26
db 26,26,26,26,19,18,19,19,19,19,19,25,28,30,31,31,31,31,31,28,20,19,20,20,20,20,19,25,27,21
db 19,20,20,19,19,19,20,21,25,21,19,19,19,19,19,20,25,21,19,20,20,20,20,25,25,20,20,20,20,20
db 20,19,27,29,26,27,27,27,27,27,26,29,31,31,28,26,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,24
db 31,31,31,31,31,24,19,26,31,31,31,31,30,19,19,19,19,19,19,23,26,28,28,21,19,19,19,19,20,29
db 31,31,31,29,19,19,26,31,31,31,30,20,19,19,19,21,24,24,22,19,19,19,19,19,24,29,30,31,30,24
db 19,19,19,25,31,31,31,30,19,21,22,19,19,20,24,24,23,20,19,19,19,19,19,20,26,27,26,22,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,26,27,26,19,19,19,19,20,27,27,25,19,19,19,19,19,19,27,27,27,27,27,27,27,27,27
db 27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,20,19,20,20,20,20
db 20,25,27,26,27,27,26,26,26,27,20,18,18,18,18,18,18,24,27,21,19,20,19,20,20,20,20,21,25,21
db 19,20,20,20,20,20,25,21,19,20,20,20,20,25,25,20,19,20,20,20,20,19,27,28,25,26,26,26,26,26
db 25,29,31,31,28,26,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,21,31,31,31,31,31,19,19,28,31,31
db 31,31,22,19,19,19,19,24,30,31,31,31,31,31,28,23,19,19,22,31,31,31,31,24,19,19,31,31,31,31
db 24,19,19,20,26,31,31,31,30,19,19,19,22,27,31,31,31,31,31,31,21,19,24,30,31,31,31,31,29,31
db 29,19,24,30,31,31,31,24,19,19,19,24,27,31,31,31,31,31,30,20,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,27,27,27,19
db 19,19,19,19,19,19,19,19,19,19,19,19,27,27,20,19,20,20,20,20,20,25,27,22,20,21,21,21,20,25
db 21,18,18,18,18,18,18,24,27,21,19,19,19,19,20,20,20,21,25,21,19,20,20,20,20,20,25,21,19,20
db 20,20,20,25,25,20,19,19,19,19,19,18,27,26,22,23,23,23,23,23,22,27,31,31,28,22,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,22,31,31,31,31,30,24,30,31,31,31,27,19,19,19,19,19,22,31,31,31
db 31,31,31,31,31,31,29,20,20,31,31,31,31,29,19,19,30,31,31,31,30,20,19,29,31,31,31,31,27,19
db 20,30,31,31,31,31,31,31,31,31,25,19,31,31,31,31,31,31,31,31,22,21,31,31,31,31,31,21,19,19
db 27,31,31,31,31,31,31,31,31,20,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,27,27,27,21,19,19,19,20,20,19,19,19,20,19
db 19,19,19,27,20,19,20,20,20,20,20,25,27,21,19,19,19,19,19,25,26,26,26,26,26,26,26,26,26,24
db 22,21,20,20,22,21,20,21,25,21,19,20,20,20,20,20,25,22,19,20,20,20,20,25,25,20,19,19,18,19
db 18,18,27,25,21,22,22,22,22,22,21,27,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,21
db 31,31,31,31,31,31,31,31,31,29,19,20,19,19,19,19,29,31,31,28,29,31,31,31,31,31,31,22,19,31
db 31,31,31,30,19,19,29,31,31,31,31,20,19,26,31,31,31,31,26,19,25,31,31,31,31,31,28,25,26,30
db 21,23,31,31,31,31,31,31,31,29,19,20,31,31,31,31,30,19,19,26,31,31,31,31,24,25,31,31,30,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,27,27,27,23,19,19,19,19,19,19,19,19,19,19,19,19,19,27,20,19,20,20,20,20
db 20,25,27,21,19,20,20,20,19,25,23,22,22,22,22,22,22,25,26,27,27,27,26,26,27,22,19,21,25,22
db 19,20,20,20,20,20,24,21,19,20,20,20,20,25,25,25,25,25,25,25,25,23,28,29,27,28,28,28,28,28
db 28,30,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,21,31,31,31,31,31,31,31,31,31,28
db 27,31,25,19,19,19,24,25,20,19,19,19,28,31,31,31,30,19,20,31,31,31,31,29,19,19,29,31,31,31
db 30,19,19,22,31,31,31,31,25,19,28,31,31,31,31,23,19,19,19,19,19,26,31,31,31,31,31,26,26,21
db 19,19,28,31,31,31,28,19,19,29,31,31,31,25,19,27,31,31,31,24,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,26,26,26,23
db 19,19,19,19,19,19,19,19,19,19,19,19,19,27,20,19,20,20,20,20,20,26,28,21,19,20,20,20,20,26
db 21,18,18,18,18,18,18,25,26,28,29,28,29,30,30,22,19,20,25,21,19,20,20,20,20,20,24,21,19,20
db 20,20,20,24,26,29,31,30,30,31,30,29,30,31,31,31,31,31,31,31,31,31,31,31,28,21,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,21,31,31,31,31,31,31,31,31,31,31,31,31,31,21,19,19,19,19,19,21
db 24,22,27,31,31,31,30,19,19,31,31,31,31,28,19,19,28,31,31,31,29,19,19,25,31,31,31,31,23,19
db 31,31,31,31,27,19,26,25,21,19,19,20,20,27,31,31,31,25,19,19,19,20,30,31,31,31,28,19,21,31
db 31,31,31,21,19,31,31,31,31,26,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,27,26,26,21,19,19,19,19,19,19,19,19,19,19
db 19,19,19,27,20,19,20,19,19,19,19,23,24,20,19,19,19,19,19,23,20,19,19,19,20,20,19,25,26,25
db 25,23,27,29,29,25,23,23,26,21,19,20,20,20,20,20,24,21,19,20,20,20,20,25,26,25,26,26,26,26
db 26,25,29,30,29,29,29,29,29,29,29,30,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,23
db 31,31,31,31,31,31,26,30,31,31,31,31,31,26,19,19,19,21,27,31,31,31,31,31,31,31,31,20,19,31
db 31,31,31,28,19,19,29,31,31,31,29,19,19,27,31,31,31,29,19,20,31,31,31,31,31,31,31,31,31,26
db 19,19,19,27,31,31,31,31,23,19,19,21,31,31,31,31,20,19,25,31,31,31,31,22,19,26,31,30,23,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,24,25,26,19,19,19,19,19,19,19,19,19,19,19,19,19,20,27,20,19,19,18,18,19
db 18,18,18,18,18,18,18,18,18,17,18,18,18,18,19,20,20,25,27,22,20,19,26,29,29,30,30,28,27,21
db 19,20,20,20,20,20,24,21,19,20,20,20,20,25,25,20,19,20,20,20,20,19,27,28,26,27,27,27,27,27
db 26,29,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,21,29,31,31,31,31,25,19,25,31,31
db 31,31,31,31,19,19,25,31,31,31,31,31,31,31,31,31,30,19,23,31,31,31,31,28,19,21,31,31,31,31
db 30,19,19,26,31,31,31,30,22,20,31,31,31,31,31,31,31,31,31,31,24,19,19,28,31,31,31,31,22,19
db 19,20,31,31,31,31,26,19,28,31,31,31,31,26,19,19,20,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,27,27,20,19,19,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,19,20,19,25,27,21,19,19,26,29,29,29,30,28,27,21,19,20,20,20,20,20,24,21,19,20
db 20,20,20,25,25,20,19,19,19,19,19,19,27,28,26,27,27,27,27,27,26,29,31,31,28,21,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,24,31,31,31,31,22,19,25,31,31,31,31,31,31,20,21,31,31,31,31
db 30,25,29,31,31,31,29,19,24,31,31,31,31,29,19,21,31,31,31,31,30,19,19,25,31,31,31,31,26,19
db 24,26,30,31,30,31,31,31,31,31,31,22,19,27,31,31,31,31,22,19,19,19,30,31,31,31,30,19,30,31
db 31,31,31,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,27,27,28,20,19,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,19,20,19,25,27,22
db 20,19,26,29,29,24,22,23,26,21,19,19,19,19,18,19,24,21,19,20,20,20,20,25,25,20,19,20,20,20
db 20,19,27,28,26,27,27,27,27,27,26,29,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,21
db 31,31,31,31,31,24,19,19,29,31,31,31,31,31,19,26,31,31,31,31,20,19,29,31,31,31,31,19,24,31
db 31,31,31,29,19,21,31,31,31,31,31,20,19,24,31,31,31,31,25,19,19,19,19,23,19,26,31,31,31,31
db 31,27,19,24,31,31,31,31,22,19,19,19,30,31,31,31,28,19,29,31,31,31,31,25,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,28,28,28,28,28,20,19,18,17,17,17
db 17,17,17,17,17,17,17,17,17,17,17,17,17,17,19,20,20,25,27,22,20,19,26,29,28,22,19,21,25,27
db 26,26,26,26,26,26,26,21,19,20,20,20,20,25,25,20,19,20,20,20,20,19,27,28,26,27,27,27,27,27
db 26,29,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,22,31,31,31,31,31,22,19,19,28,31
db 31,31,31,31,19,29,31,31,31,30,19,19,29,31,31,31,31,21,19,27,31,31,31,29,19,19,24,31,31,31
db 31,21,19,26,31,31,31,31,25,19,19,19,19,19,19,19,23,31,31,31,31,25,19,28,31,31,31,30,20,19
db 19,20,31,31,31,31,28,19,28,31,31,31,31,31,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,21,24,23,19
db 19,19,19,19,19,19,19,22,23,22,22,22,22,22,18,18,23,24,24,24,24,24,24,24,24,24,24,24,24,24
db 24,24,24,24,20,18,18,21,22,19,19,18,25,29,28,22,19,21,24,30,31,31,31,31,31,31,28,21,19,20
db 20,20,20,25,25,21,19,20,20,20,20,19,27,28,26,27,27,27,27,27,26,29,31,31,28,21,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,22,31,31,31,31,31,22,19,23,31,31,31,31,31,26,19,31,31,31,31,30
db 22,22,30,31,31,31,26,20,21,28,31,31,31,30,19,19,26,31,31,31,31,22,19,21,30,31,31,31,24,19
db 19,25,20,19,19,19,25,31,31,31,31,22,19,29,31,31,31,26,20,19,19,19,28,31,31,31,28,19,26,31
db 31,31,31,31,23,24,24,24,27,28,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,21,27,27,27,24,19,19,19,19,19,19,19,29,19,16
db 16,16,17,17,17,17,29,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,21,17,17,17,17,17
db 18,17,25,29,28,24,22,24,26,29,29,29,28,27,28,26,26,20,18,18,18,18,18,24,25,21,19,20,20,20
db 20,19,27,28,26,27,27,27,27,27,26,29,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,23
db 31,31,31,31,31,29,28,31,31,31,31,31,28,19,20,31,31,31,31,31,31,31,31,31,31,31,21,19,21,31
db 31,31,31,29,19,19,30,31,31,31,30,20,19,23,31,31,31,31,25,19,23,31,31,25,26,29,31,31,31,31
db 30,19,19,28,31,31,31,31,26,19,19,19,29,31,31,31,27,19,24,31,31,31,31,31,31,31,31,31,31,30
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,28,24,23,24,27,19,19,19,19,19,19,19,29,20,17,18,18,18,18,17,17,28,31,31,31
db 31,31,31,31,31,31,31,31,31,31,31,31,31,30,21,17,18,18,18,18,18,17,25,29,29,29,29,29,29,28
db 27,27,22,19,20,20,25,22,19,20,20,20,19,25,25,21,19,20,20,20,20,19,27,29,27,27,27,27,27,27
db 27,29,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,26,31,31,31,31,31,31,31,31,31,31
db 31,31,30,19,19,21,31,31,31,31,31,31,31,31,31,31,31,19,19,30,31,31,31,28,19,19,26,31,31,31
db 29,19,19,28,31,31,31,31,23,20,30,31,31,31,31,31,31,31,31,29,20,19,19,29,31,31,31,31,28,19
db 19,22,31,31,31,31,28,19,19,21,31,31,31,31,31,31,31,31,31,24,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,28,24,22,23,27
db 20,19,19,19,19,19,19,30,21,17,18,18,18,18,17,17,28,31,31,31,31,31,31,31,31,31,31,31,31,31
db 31,31,31,30,21,17,18,18,18,18,18,17,25,30,29,29,29,29,29,29,27,27,22,19,19,20,24,29,30,30
db 30,30,30,28,25,21,19,20,20,20,20,19,27,29,27,28,28,28,28,28,28,30,31,31,28,21,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,20,31,31,31,31,31,31,31,31,31,31,31,31,31,24,19,19,19,31,31,31,31
db 31,29,30,31,31,31,31,21,21,31,31,31,31,31,22,19,31,31,31,31,31,26,19,29,31,31,31,31,24,20
db 31,31,31,31,31,31,31,31,31,29,19,19,19,30,31,31,31,31,31,23,19,23,31,31,31,31,30,20,19,21
db 31,31,31,31,31,31,31,31,29,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,21,28,23,21,22,27,20,19,19,19,19,19,19,30,21,17
db 18,18,18,18,18,19,29,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,30,21,18,18,18,18,18
db 18,17,25,30,29,29,29,29,29,29,27,27,22,19,19,20,24,30,31,31,31,31,31,28,25,21,19,19,19,19
db 19,19,27,27,23,24,24,24,24,24,23,28,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,23
db 30,31,31,31,30,31,31,31,31,31,25,19,19,19,19,19,23,31,31,31,25,19,31,31,31,31,31,24,25,31
db 31,31,31,31,26,21,31,31,31,31,31,30,20,31,31,31,31,31,31,19,26,31,31,31,31,31,31,31,28,22
db 19,19,19,30,31,31,31,31,31,28,19,23,31,31,31,31,31,25,19,19,20,30,31,31,31,31,31,29,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,27,19,19,25,25,25,27,19,19,19,19,19,19,19,30,20,17,18,18,24,26,26,27,30,31,31,31
db 31,31,31,31,31,31,31,31,31,31,31,31,31,31,25,23,23,23,23,23,23,22,22,22,22,22,23,24,23,24
db 26,27,22,19,19,20,24,24,23,24,24,24,24,26,25,19,17,18,18,18,18,18,27,24,19,21,21,20,20,20
db 20,27,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,25,29,27,20,23,29,29,24,27
db 24,23,19,19,19,23,29,31,31,28,19,19,25,29,31,26,23,19,22,27,30,31,31,26,19,20,27,30,31,31
db 27,20,19,25,30,31,30,26,22,19,19,23,28,30,26,26,29,24,19,19,19,19,19,21,28,31,31,30,27,19
db 21,29,31,31,31,28,23,20,19,19,19,19,25,29,25,23,22,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,27,19,19,20,27,27,27
db 19,19,19,19,19,19,20,30,20,17,18,18,29,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31
db 31,31,31,31,27,26,26,26,26,26,26,26,20,17,18,17,19,20,19,20,25,27,22,19,19,20,25,21,18,19
db 19,19,19,24,26,24,23,23,23,23,23,22,28,27,25,25,25,25,25,25,25,29,31,31,28,21,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,24,27,29,30,31,28,23,31,31,31,21,19,25,31,31,31,31,28
db 19,19,19,19,20,19,19,19,19,19,19,22,21,19,19,19,19,19,22,21,19,19,19,19,19,21,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,21,22,19,19,23,31,31,31,31,31,22,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,27,27,27,21,19,19,19,21,27,27,25,19,19,22,27,27
db 27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,20,19,27,30,20,17
db 18,18,28,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,27,25,26,26,26,26
db 26,25,20,18,18,18,19,20,19,20,25,28,23,19,19,21,25,21,19,20,19,19,19,24,26,27,28,28,28,27
db 27,26,29,31,31,31,31,31,31,31,31,31,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,26,31,31,31,31,31,31,31,31,31,31,21,19,28,31,31,31,31,27,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,23,31,31,31,31,31,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,26,26,26,27,19,19,19,27,26,26,26,20,19,27,26,26,26,26,26,26,26,26,26,26,26,26
db 26,26,26,26,26,26,26,26,26,26,26,26,26,26,23,20,21,30,20,17,17,17,28,31,31,31,31,31,31,31
db 31,31,31,31,31,31,31,31,31,31,31,31,31,31,27,25,26,26,26,26,26,25,19,17,18,17,19,20,19,21
db 27,29,23,19,19,20,25,21,19,20,20,20,19,24,26,23,23,22,22,22,22,22,28,30,29,29,29,29,29,29
db 29,30,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,31,31,31,31,31,31,31,31,31
db 31,31,21,19,23,31,31,31,31,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 29,31,31,31,25,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,26,26,26,27,19,19
db 19,27,26,26,26,19,19,27,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26
db 26,26,26,26,24,19,19,30,21,18,18,18,28,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31
db 31,31,31,31,27,25,26,26,26,26,26,26,20,18,18,18,19,20,19,21,27,30,23,20,20,21,25,21,19,20
db 20,20,19,24,25,21,19,20,20,20,20,19,27,28,26,27,27,27,27,27,26,29,31,31,28,21,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,28,31,31,31,31,31,26,24,29,31,31,31,24,19,22,31,31,31,31,23
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,30,31,31,31,25,20,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,26,26,26,26,19,19,19,27,26,26,26,19,19,27,26,26
db 26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,25,24,24,26,27,28
db 28,28,29,29,29,29,30,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,27,26,26,26,26,26
db 26,26,25,25,25,25,21,19,20,18,25,29,27,27,27,27,26,21,19,20,20,20,19,24,26,21,19,20,19,19
db 20,19,27,28,26,27,27,27,27,27,26,29,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,22
db 31,31,31,31,31,23,19,19,19,24,31,31,22,19,22,31,31,31,31,26,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,20,31,31,31,31,31,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,27,27,27,19,19,19,19,20,27,27,25,19,19,21,27,27,27,27,27,27,27,27,27,27,27,27
db 26,27,27,27,27,27,27,27,27,27,27,27,27,26,20,19,19,19,28,31,31,31,30,29,29,29,30,31,31,31
db 31,31,31,31,31,31,31,31,31,31,31,31,31,31,27,26,26,26,26,26,26,26,27,27,27,27,21,18,18,16
db 25,29,29,30,30,28,27,20,19,19,19,20,19,24,26,21,19,20,20,20,20,19,27,28,26,27,27,27,27,27
db 26,29,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,28,31,31,31,31,27,19,19,19,19,19
db 24,20,19,19,22,31,31,31,31,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,25,26,22,19,19,19,19,19,19,19
db 30,31,31,31,30,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,21,25,25,21,19,19,19,19,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,19,19,19,19,19
db 19,19,19,20,19,19,19,19,19,20,20,20,19,19,19,19,20,20,20,20,20,20,20,20,20,20,20,20,20,20
db 21,21,21,26,19,17,18,17,26,30,29,29,29,29,29,29,30,31,31,31,31,31,31,31,31,31,31,31,31,31
db 31,31,31,31,27,26,26,26,26,26,26,26,26,26,26,26,20,18,18,16,25,29,29,29,29,28,26,24,24,24
db 24,21,19,25,26,21,19,20,20,20,20,19,27,28,26,27,27,27,27,27,26,29,31,31,28,21,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,22,31,31,31,31,31,30,27,26,19,19,19,19,19,19,19,25,31,31,31,31,21
db 19,19,22,22,20,19,19,19,19,19,19,19,22,24,27,26,21,19,19,19,19,19,19,25,26,23,19,19,19,19
db 19,19,19,19,19,19,19,19,25,31,31,31,31,25,19,19,19,20,21,23,31,31,31,31,28,19,19,19,19,19
db 22,25,27,25,19,19,19,19,19,19,21,26,26,21,19,19,19,19,19,19,19,19,19,19,19,20,26,31,31,31
db 31,22,19,25,30,31,28,20,19,20,23,25,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,26,19,18,18,17,27,30
db 29,29,29,29,29,29,30,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,27,26,26,26,26,26
db 26,26,26,26,26,26,20,18,18,16,25,30,29,29,29,28,26,28,30,29,29,23,19,25,26,21,19,20,20,20
db 20,19,27,28,26,27,27,27,27,27,26,29,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,24,31
db 31,31,31,31,31,31,31,29,23,19,19,19,19,19,24,31,31,31,31,19,20,30,31,31,31,24,19,19,19,19
db 19,27,31,31,31,31,31,24,19,19,19,21,29,31,31,31,20,19,19,19,20,25,26,25,23,19,19,19,31,31
db 31,31,31,23,19,19,26,31,31,31,31,31,31,31,28,19,19,19,21,28,31,31,31,31,31,23,19,19,19,22
db 31,31,31,31,19,19,19,19,21,25,26,25,22,19,19,20,31,31,31,31,31,20,27,31,31,31,31,19,22,31
db 31,31,31,24,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,20,20,26,19,17,18,17,26,29,29,29,29,29,29,29,30,31,31,31
db 31,31,31,31,31,31,31,31,31,31,31,31,31,31,27,25,26,26,26,26,26,26,26,26,26,26,20,18,18,17
db 24,28,27,27,27,27,26,28,29,29,28,21,18,24,26,21,19,20,20,20,20,19,27,28,26,27,27,27,27,27
db 26,29,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,28,31,31,31,31,31,31,31,31,31,28
db 23,19,19,19,22,31,31,31,31,20,27,31,31,31,31,31,24,19,19,19,28,31,31,31,31,31,31,28,19,19
db 22,31,31,31,31,31,20,19,19,25,31,31,31,31,31,20,19,19,27,31,31,31,31,19,21,29,31,31,31,31
db 31,31,31,31,31,20,19,20,31,31,31,31,31,31,31,26,19,19,24,31,31,31,31,30,19,19,20,26,31,31
db 31,31,31,19,19,19,29,31,31,31,26,19,28,31,31,31,27,19,31,31,31,31,31,31,24,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,24
db 24,26,26,24,19,18,18,18,26,28,28,27,29,29,29,29,30,31,31,31,31,31,31,31,31,31,31,31,31,31
db 31,31,31,31,27,26,26,26,29,30,29,29,29,28,28,28,21,18,18,17,19,20,20,20,20,22,25,29,29,29
db 27,20,17,24,26,21,19,20,20,20,20,19,27,28,26,26,26,26,26,27,26,29,31,31,28,22,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,30,31,31,31,31,31,31,31,31,31,31,22,19,19,21,31,31,31,31,30
db 31,31,31,31,31,31,30,19,19,26,31,31,31,31,30,23,19,19,19,19,24,31,31,31,31,30,19,19,21,31
db 31,31,31,31,31,21,19,19,28,31,31,31,27,19,31,31,31,31,31,28,30,31,31,31,30,19,19,28,31,31
db 31,31,29,22,19,19,19,19,26,31,31,31,31,29,19,19,23,31,31,31,31,31,31,19,19,19,29,31,31,31
db 25,19,24,31,31,31,29,28,31,31,31,31,31,31,26,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,21,19,20,20,18,23,25,25,25,27,27
db 27,27,29,29,29,29,30,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,27,26,26,26,30,31
db 31,31,31,31,30,30,22,17,18,18,18,18,18,18,18,21,25,29,29,29,28,26,26,26,25,21,19,19,19,19
db 19,19,20,24,22,23,23,23,23,23,21,27,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 21,27,29,31,31,31,31,31,31,31,31,31,21,19,22,31,31,31,31,31,31,30,30,31,31,31,30,19,19,30
db 31,31,31,31,20,19,24,28,28,24,19,29,31,31,31,31,23,19,22,31,31,31,31,31,31,21,19,20,31,31
db 31,29,20,24,31,31,31,31,27,19,30,31,31,31,29,19,19,31,31,31,31,28,19,19,26,28,28,21,19,30
db 31,31,31,31,22,19,24,31,31,31,31,31,31,20,19,21,31,31,31,29,20,19,23,31,31,31,31,31,31,29
db 31,31,31,31,24,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,17,17,17,16,25,28,28,27,27,27,27,27,29,29,29,29,30,31,31,31
db 31,31,31,31,31,31,31,31,31,31,31,31,31,31,27,25,26,26,30,31,31,31,31,31,31,31,22,17,18,18
db 18,18,18,19,19,21,25,28,29,29,30,31,31,28,25,20,19,19,19,19,19,19,19,25,21,22,22,22,22,22
db 20,27,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,24,31,31,31,31,31,31,31
db 31,31,28,19,22,31,31,31,31,31,29,19,27,31,31,31,31,19,22,31,31,31,31,22,19,28,31,31,31,28
db 19,30,31,31,31,31,23,19,20,30,31,31,31,31,31,27,19,21,31,31,31,30,19,27,31,31,31,31,24,19
db 30,31,31,31,29,19,24,31,31,31,31,19,19,30,31,31,31,23,19,31,31,31,31,31,21,19,21,31,31,31
db 31,31,31,25,19,23,31,31,31,29,19,19,22,31,31,31,31,31,25,19,31,31,31,31,25,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20
db 18,18,18,17,25,27,27,27,27,27,27,27,29,29,29,29,30,31,31,31,31,31,31,31,31,31,31,31,31,31
db 31,31,31,31,27,25,26,26,29,31,31,31,31,31,31,31,22,17,18,18,18,18,18,23,25,25,26,27,28,28
db 26,26,26,27,26,24,24,24,24,24,24,23,23,26,26,27,27,27,27,27,26,29,31,31,28,22,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,21,26,29,31,31,31,31,31,31,31,31,19,22,31,31,31,31,31
db 20,21,31,31,31,31,27,19,28,31,31,31,29,19,22,31,31,31,31,30,19,29,31,31,31,31,21,19,24,31
db 31,31,31,31,31,24,19,23,31,31,31,29,19,28,31,31,31,31,22,19,30,31,31,31,29,19,30,31,31,31
db 27,19,24,31,31,31,31,29,20,30,31,31,31,31,19,19,27,31,31,31,31,31,31,23,19,25,31,31,31,28
db 19,19,23,31,31,31,31,29,19,23,31,31,31,28,20,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,18,18,18,17,25,28,27,27,27,27
db 27,27,29,29,29,29,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,27,25,26,26,30,31
db 31,31,31,31,31,31,22,17,18,18,18,18,18,26,30,29,29,26,25,26,22,20,20,25,27,29,31,30,30,30
db 30,28,29,31,31,31,31,31,31,31,31,31,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,24,30,31,31,31,31,31,31,21,19,26,31,31,31,26,19,24,31,31,31,29,19,19,31,31
db 31,31,27,19,19,26,31,31,31,31,23,22,31,31,31,31,22,19,28,31,31,31,31,31,31,24,19,24,31,31
db 31,23,19,25,31,31,31,29,19,22,31,31,31,31,28,22,31,31,31,31,26,19,20,28,31,31,31,31,21,24
db 31,31,31,31,20,19,31,31,31,31,31,31,31,22,19,26,31,31,31,21,19,19,23,31,31,31,31,23,19,27
db 31,31,31,27,20,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,20,20,20,18,18,18,17,25,28,27,27,27,27,27,27,29,29,29,29,30,30,30,30
db 30,30,30,30,30,30,30,30,30,30,30,30,30,30,27,26,26,26,29,30,30,30,30,30,30,30,22,18,18,18
db 18,18,18,26,30,29,29,27,26,27,23,21,22,26,26,26,27,27,27,27,27,26,29,30,30,30,30,30,30,30
db 30,31,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,21,20,19,19,19,20,30,31,31
db 31,31,31,21,20,29,31,31,31,24,19,21,31,31,31,31,26,19,31,31,31,31,27,19,19,28,31,31,31,31
db 22,20,31,31,31,31,23,19,31,31,31,31,31,31,31,28,19,23,31,31,31,19,19,29,31,31,31,31,20,22
db 31,31,31,31,28,24,31,31,31,31,25,19,19,29,31,31,31,30,21,22,31,31,31,31,20,20,31,31,31,31
db 31,31,31,26,19,25,31,31,30,19,19,19,19,29,31,31,31,23,19,24,31,31,31,31,25,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,20,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,20
db 18,18,18,17,22,24,23,22,26,27,27,27,28,29,29,29,27,26,26,26,26,26,26,26,26,26,26,26,26,26
db 26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,27,21,18,18,18,18,18,18,26,30,29,29,28,28,28
db 27,26,27,27,25,21,20,20,20,20,20,20,27,27,25,27,27,26,26,27,26,29,31,31,28,21,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,20,28,31,19,19,19,19,19,26,31,31,31,31,31,20,22,31,31,31,31,25
db 19,22,31,31,31,31,27,19,31,31,31,31,30,22,19,29,31,31,31,30,19,19,31,31,31,31,24,21,31,31
db 31,30,31,31,31,27,19,28,31,31,29,19,19,31,31,31,31,31,19,19,31,31,31,31,20,22,31,31,31,31
db 29,20,20,30,31,31,31,28,19,21,31,31,31,31,22,23,31,31,31,31,31,31,31,24,19,31,31,31,27,19
db 19,19,24,31,31,31,31,27,19,21,31,31,31,31,23,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 20,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,20,18,18,18,17,21,23,23,21,25,27
db 27,27,28,29,29,29,26,25,25,25,26,25,25,25,25,25,25,25,25,25,25,25,25,26,26,26,26,26,26,25
db 25,25,25,25,25,27,21,18,18,18,18,18,18,26,30,29,29,29,29,29,29,29,29,27,24,20,19,19,19,19
db 19,19,27,28,25,26,26,26,26,26,25,29,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,31
db 31,31,22,19,19,19,19,27,31,31,31,31,31,20,22,31,31,31,31,26,19,25,31,31,31,31,26,19,30,31
db 31,31,31,26,19,23,31,31,31,31,23,19,30,31,31,31,31,30,31,31,24,23,31,31,31,30,22,30,31,31
db 27,19,19,29,31,31,31,31,22,24,31,31,31,30,19,22,31,31,31,31,31,21,19,24,31,31,31,31,20,19
db 31,31,31,31,31,31,31,31,23,26,31,31,31,28,23,31,31,31,25,19,19,19,25,31,31,31,31,28,19,24
db 31,31,31,31,24,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,23,26,25,25,25,26,26,26,26,26
db 26,26,26,26,26,26,26,26,26,19,18,18,18,17,22,23,23,21,25,27,27,27,28,29,29,29,26,26,26,26
db 26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,27,21,18,18,18
db 18,18,18,26,30,29,29,29,29,29,29,29,29,27,24,21,19,20,20,20,19,19,27,28,26,27,27,27,27,27
db 26,29,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,21,31,31,31,30,23,19,19,25,31,31,31
db 31,31,28,19,23,31,31,31,31,27,19,20,30,31,31,31,25,19,30,31,31,31,31,22,19,23,31,31,31,31
db 21,19,22,31,31,31,31,31,31,31,19,23,31,31,31,31,31,31,31,31,25,19,19,25,31,31,31,31,31,31
db 31,31,31,31,27,19,31,31,31,31,31,19,19,25,31,31,31,31,19,19,23,31,31,31,31,31,31,31,19,26
db 31,31,31,31,31,31,31,31,22,19,19,19,24,31,31,31,31,21,19,25,31,31,31,31,26,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,22,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,19
db 18,18,18,17,22,23,23,21,25,27,27,27,29,30,29,30,26,26,26,26,26,26,26,26,26,26,26,26,26,26
db 26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,27,21,18,18,18,18,18,18,26,30,29,29,29,29,29
db 29,29,29,27,25,21,19,20,20,19,19,19,27,28,26,27,27,27,27,27,26,29,31,31,28,21,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,20,30,31,31,31,31,29,29,31,31,31,31,31,31,19,19,23,31,31,31,31,23
db 19,22,31,31,31,31,25,19,29,31,31,31,31,26,19,27,31,31,31,31,19,19,20,31,31,31,31,31,31,29
db 19,22,31,31,31,31,31,31,31,31,24,19,19,29,31,31,31,31,31,31,31,31,31,31,28,19,30,31,31,31
db 31,22,19,29,31,31,31,29,19,19,23,31,31,31,31,31,31,28,19,24,31,31,31,31,31,31,31,31,20,19
db 19,19,22,31,31,31,31,22,19,26,31,31,31,31,23,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 20,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,19,18,18,18,17,22,23,23,21,26,27
db 27,27,29,29,29,29,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26
db 26,26,26,26,26,27,21,18,18,18,18,18,18,26,30,29,29,27,26,27,27,28,29,27,25,21,19,20,20,20
db 19,19,27,28,26,27,27,27,27,27,26,29,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,29
db 31,31,31,31,31,31,31,31,31,31,31,21,19,19,22,31,31,31,31,21,19,28,31,31,31,31,26,19,21,23
db 31,31,31,31,29,31,31,31,31,27,19,19,22,31,31,31,31,31,31,26,19,19,26,31,31,31,31,31,31,31
db 23,19,19,28,31,31,31,31,31,27,29,31,31,31,30,19,21,24,31,31,31,31,29,31,31,31,31,23,19,19
db 25,31,31,31,31,31,31,24,19,19,28,31,31,31,31,31,31,31,19,19,19,19,21,31,31,31,31,25,19,20
db 29,31,31,31,24,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,20,18,18,18,17,22,23,23,21,26,27,27,27,27,27,27,27,26,26,26,26
db 26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,27,21,18,18,18
db 18,18,18,26,30,29,29,23,21,21,25,28,29,27,25,21,19,20,20,20,19,19,27,28,26,27,27,27,27,27
db 26,29,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,27,31,31,31,31,31,31,31,31,31,31
db 22,19,19,19,26,31,31,31,31,25,19,29,31,31,31,31,29,19,19,21,31,31,31,31,31,31,31,31,27,19
db 19,19,19,31,31,31,31,31,31,23,19,19,30,31,31,31,31,31,31,24,19,19,19,21,31,31,31,31,29,20
db 30,31,31,31,31,22,19,23,31,31,31,31,31,31,31,31,25,19,19,19,20,31,31,31,31,31,31,21,19,20
db 31,31,31,31,31,31,31,23,19,19,19,19,24,31,31,31,31,25,19,21,30,31,31,31,29,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,20,20,20,20,20,20,20,20,20,19,19
db 18,18,18,17,22,23,23,22,25,27,27,27,27,26,27,27,26,26,26,26,26,26,26,26,26,26,26,26,26,26
db 26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,27,21,18,18,18,18,18,18,26,30,29,29,23,20,21
db 25,29,29,27,25,21,19,20,20,20,20,19,27,28,26,27,27,27,27,27,26,29,31,31,28,21,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,20,26,30,31,31,31,31,29,29,31,22,19,19,19,19,31,31,31,31,28,21
db 19,24,31,31,31,31,31,24,19,20,27,31,31,31,31,31,28,24,19,19,19,19,19,24,31,31,31,31,27,19
db 19,19,27,31,31,31,31,30,23,19,19,19,19,19,21,30,31,31,24,20,31,31,31,31,31,27,19,21,29,31
db 31,31,31,31,28,23,19,19,19,19,19,26,31,31,31,31,26,19,19,19,29,31,31,31,31,29,22,19,19,19
db 19,19,29,31,31,31,31,26,19,26,31,31,31,31,31,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,27,19,19,19,19,19,19,18,18,18,17,22,23,23,22,25,27
db 27,27,27,27,27,27,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26
db 26,26,26,26,26,27,21,18,18,18,18,18,18,26,30,29,29,23,21,21,26,29,29,27,25,21,19,20,20,20
db 20,19,27,28,26,27,27,27,27,27,26,29,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,22,23,25,25,19,19,20,19,19,19,19,19,24,25,25,22,19,19,19,19,23,31,31,31,28,21,19,19
db 19,24,28,29,29,23,19,19,19,19,19,19,19,19,21,23,26,22,19,19,19,19,19,21,24,26,24,19,19,19
db 19,19,19,19,19,19,22,23,19,19,26,31,31,31,30,23,19,19,19,25,28,29,29,21,19,19,19,19,19,19
db 19,19,21,24,26,21,19,19,19,19,19,21,25,26,23,19,19,19,19,19,19,19,24,30,31,30,25,20,19,24
db 31,31,31,31,30,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,19,19,20,19,19,20,21,21
db 21,21,21,27,21,21,21,21,20,19,18,18,18,17,22,23,23,21,26,27,27,27,27,27,27,27,26,26,26,26
db 26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,27,21,18,18,18
db 18,18,18,26,30,29,29,23,21,21,25,29,29,27,24,21,19,20,20,20,20,19,27,29,27,28,27,27,27,27
db 26,29,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,21,22,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,22,25,24,20,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,21,20,19,19,19,20,24,26,26,24,20,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,27,27,27,26,19,19,19,19,27,26,26,26,26,27,27,27,27,26
db 26,26,26,27,27,27,27,27,27,27,27,26,26,26,27,27,26,27,27,27,27,27,27,27,27,27,27,26,26,19
db 18,18,18,17,22,23,23,22,24,25,25,25,26,27,27,27,26,25,25,25,25,25,25,25,25,25,25,25,25,25
db 25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,26,20,18,18,18,18,18,18,26,30,29,29,26,25,25
db 27,29,29,27,24,21,19,19,20,20,19,19,27,28,25,26,26,26,25,26,25,29,31,31,28,22,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,22,26,26,26,26,19,19,19,20,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26
db 26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,25,24,19,18,18,18,17,22,23,23,23,23,22
db 22,22,26,27,27,27,25,25,25,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24
db 25,24,24,24,24,25,20,18,18,18,18,18,18,26,30,29,29,29,29,29,29,29,29,27,24,20,19,19,19,19
db 19,18,27,26,21,22,22,22,22,22,21,27,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,24,26,26,26,26,19,19,19,22
db 26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26
db 26,26,26,26,26,26,26,25,24,19,18,18,18,17,22,23,23,23,23,23,23,23,26,27,27,27,25,25,25,25
db 25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,26,20,18,18,18
db 18,18,18,26,30,29,29,29,29,29,29,29,29,27,25,22,21,21,21,21,21,20,28,26,22,23,23,23,23,23
db 22,28,31,31,28,23,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,23,26,26,26,27,19,19,20,22,26,26,26,26,26,26,26,26,26,26
db 26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,25,25,19
db 18,18,18,17,22,23,23,23,23,23,23,23,27,27,27,27,25,25,25,25,25,25,25,25,25,25,25,25,25,25
db 25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,26,20,18,18,18,18,18,18,26,30,29,29,29,29,29
db 29,29,29,27,25,27,28,27,27,27,27,26,29,30,29,30,30,30,30,30,29,30,31,31,28,22,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,27,27,27,24,19,19,19,19,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27
db 27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,19,18,18,18,17,22,23,23,23,23,23
db 23,23,27,27,27,27,25,24,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25
db 25,25,25,25,25,26,20,18,18,18,18,18,17,27,31,30,29,29,30,30,30,30,31,27,24,23,22,22,22,22
db 22,21,27,30,29,29,29,29,29,28,28,30,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,20,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,20,20,20,19,20,21,20,18,18,18,17,22,23,23,23,23,23,23,23,24,24,24,24,25,26,26,26
db 26,26,26,26,26,26,26,26,26,26,26,26,26,26,25,25,25,25,25,25,25,25,25,25,25,26,20,18,18,18
db 18,18,18,22,24,24,27,25,24,24,24,24,25,26,24,20,19,19,19,19,19,19,27,29,26,27,27,27,27,26
db 26,29,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20
db 18,18,18,17,22,23,23,23,23,23,23,23,22,22,22,22,26,27,27,27,27,27,27,27,27,27,27,27,27,27
db 27,27,27,28,25,25,25,25,25,25,25,25,25,25,25,26,20,18,18,18,18,18,18,18,17,19,25,20,18,19
db 19,19,18,24,25,21,19,20,20,20,20,19,27,29,26,27,27,27,27,27,26,29,31,31,28,21,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,18,18,18,17,22,23,23,23,23,23
db 23,23,23,23,23,23,26,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,26,27,25,25,25,25,25,25
db 25,25,25,25,25,26,20,18,18,18,18,18,18,18,18,19,25,23,21,22,22,22,21,25,25,21,19,20,20,20
db 20,19,27,29,26,27,27,27,27,27,26,29,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,20,19,17,18,18,17,22,23,23,23,23,23,23,23,23,23,23,23,27,27,27,27
db 27,27,27,27,27,27,27,27,27,27,27,27,27,27,25,25,25,25,25,25,25,25,25,25,25,26,20,18,18,18
db 18,18,18,20,21,21,25,29,30,29,29,29,29,27,24,21,19,20,20,20,20,19,27,28,26,27,27,27,27,27
db 26,29,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 18,18,18,17,21,23,23,23,23,23,23,23,23,23,23,23,27,27,27,27,27,27,27,27,27,27,27,27,27,27
db 27,27,26,27,25,25,25,25,25,25,25,25,25,25,25,26,20,18,18,18,18,18,18,25,28,27,26,30,31,30
db 31,30,31,27,24,21,19,20,20,20,20,19,27,28,26,27,27,27,27,27,26,29,31,31,28,21,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,25,25,25,25,23,22,22,21,21,23,23
db 23,23,23,23,23,23,24,24,24,23,24,24,24,24,24,24,24,24,24,24,24,24,24,24,25,25,25,25,25,25
db 25,25,25,25,25,26,20,18,18,18,18,18,18,26,30,28,27,23,21,21,21,21,21,25,25,21,19,20,20,20
db 20,19,27,28,26,27,27,27,27,27,26,29,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,22,24,24,23,27,20,19,19,19,22,23,23,23,23,23,23,23,22,22,22,22
db 22,22,22,22,22,22,22,22,22,23,23,23,22,23,24,25,25,25,25,25,25,25,25,25,25,25,20,18,18,18
db 18,18,18,26,30,28,27,20,18,18,18,18,18,24,25,21,19,20,20,20,20,19,27,28,26,27,27,27,27,27
db 26,29,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,27,19,17,18,17,22,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23
db 23,23,23,22,24,25,25,25,25,25,25,25,25,25,25,25,20,18,18,17,22,24,23,27,29,28,27,21,19,20
db 20,20,20,25,25,21,19,20,20,20,20,19,27,29,26,27,27,27,27,27,26,29,31,31,28,21,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,27,19,17,18,18,22,23
db 23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,22,24,25,25,25,25,25
db 25,25,25,25,25,26,20,18,18,17,25,30,29,29,29,28,27,21,19,20,20,20,20,24,25,21,19,20,20,20
db 20,19,27,27,25,26,26,25,25,26,24,28,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,20,27,19,17,18,18,21,23,23,23,23,23,23,23,23,23,23,23
db 23,23,23,23,23,23,23,23,23,23,23,23,23,23,24,25,25,25,25,25,25,25,24,24,24,25,20,17,18,17
db 25,30,29,29,29,28,27,21,19,20,20,20,20,24,25,20,19,19,19,19,19,19,27,25,21,22,22,22,22,22
db 21,27,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,20,21,19
db 19,19,20,20,19,17,18,18,20,21,20,21,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23
db 23,23,23,22,24,25,25,25,25,25,25,24,22,21,21,22,19,18,18,17,25,29,29,29,29,28,27,21,19,20
db 20,20,19,24,25,20,19,19,19,19,19,19,27,25,21,23,23,23,23,23,22,27,31,31,28,21,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,25,25,24,24,23,20,19,19,19,19,18,18,18,18,18
db 18,18,22,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,22,24,25,25,25,25,25
db 25,23,19,18,18,19,18,18,18,18,21,22,22,22,22,22,25,21,19,20,20,20,20,24,26,25,26,26,26,26
db 26,25,29,29,28,29,29,29,29,29,28,30,31,31,28,20,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,28,23,22,23,27,27,19,19,19,20,18,18,18,18,18,18,18,22,23,23,23,23,23,23,23
db 23,23,23,23,23,23,23,23,23,23,23,23,23,22,24,25,25,25,25,25,25,23,19,18,18,18,18,18,18,18
db 18,18,18,18,18,18,24,21,19,20,20,20,20,24,27,29,30,30,30,30,30,28,30,31,31,31,31,31,31,31
db 31,31,31,31,28,20,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,28,23,22,23,28
db 27,19,19,20,19,17,17,17,18,18,18,18,23,24,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23
db 23,23,23,22,25,25,25,25,25,25,25,23,19,18,18,18,18,18,18,18,18,18,18,18,18,19,24,21,19,20
db 20,20,20,25,26,25,25,25,25,25,25,24,28,30,29,30,30,30,30,30,29,30,31,31,28,20,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,26,25,24,24,24,19,19,19,20,22,20,20,20,18,18
db 18,18,21,22,21,22,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,22,23,23,23,23,23,23
db 23,22,19,18,18,18,18,18,18,18,18,18,18,18,18,19,24,21,19,20,20,20,20,25,24,20,19,20,19,19
db 20,19,27,28,26,27,27,27,27,27,26,29,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,22,27,20,19,19,19,19,20,28,28,28,29,20,17,18,18,18,18,18,18,23,23,23,23
db 23,23,23,23,23,23,23,23,23,23,23,23,23,23,18,17,17,17,17,17,17,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,19,25,22,19,20,20,20,20,25,24,20,19,20,20,20,20,19,27,28,26,27,27,27,27,27
db 26,29,31,31,28,20,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,19,19
db 19,19,19,19,22,23,23,30,21,18,18,18,18,18,18,18,22,23,23,23,23,23,23,23,23,23,23,23,23,23
db 23,23,23,23,18,17,17,17,17,17,17,18,18,18,18,18,18,18,18,18,18,18,18,18,18,19,25,21,19,20
db 20,20,20,25,25,20,20,20,20,20,20,19,27,28,26,27,27,27,27,27,26,29,31,31,28,20,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,29,21,18
db 18,18,18,18,18,18,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,19,18,18,18,18,18
db 18,18,18,18,18,18,18,18,18,18,18,18,18,19,19,19,25,21,19,20,20,20,20,25,25,20,20,20,20,20
db 20,19,27,28,26,27,27,27,27,27,26,29,31,31,28,20,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,29,20,17,17,17,17,17,18,18,23,24,23,23
db 23,23,23,23,23,23,23,23,23,23,23,23,23,23,19,17,18,17,17,17,17,17,18,18,18,18,18,18,18,18
db 17,17,18,18,18,19,25,21,19,20,20,20,20,25,25,20,20,20,20,20,20,19,27,28,26,27,27,27,27,27
db 26,29,31,31,28,20,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,26,23,22,22,22,22,22,19,18,21,21,21,21,21,21,21,21,21,21,21,21,21,21
db 21,21,21,21,19,18,18,18,18,18,18,18,18,18,18,18,18,18,18,19,22,23,23,23,24,23,26,21,19,20
db 20,20,19,25,25,20,20,20,20,20,20,19,27,28,26,27,27,27,27,27,26,29,31,31,28,21,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,25,29
db 29,29,29,29,21,19,18,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,19,19,19,20,20,20
db 20,20,19,18,18,18,18,18,18,21,28,30,29,29,29,28,27,20,18,19,19,19,19,24,25,20,20,20,20,20
db 20,19,27,28,26,27,27,27,27,27,26,29,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,27,27,27,27,27,20,19,18,18,18,18
db 18,18,18,18,18,18,18,18,18,18,18,17,17,18,19,19,19,20,20,20,20,20,19,18,18,18,18,18,18,20
db 24,24,24,24,24,24,26,21,19,20,20,20,20,25,25,20,19,20,20,20,20,19,27,28,26,27,26,26,26,27
db 26,29,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,20,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,27,27,27,27,27,20,19,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,19,19,19,20,20,20,20,20,19,18,18,18,18,18,18,20,20,20,20,20,20,21,25,28,29,28
db 28,28,28,27,24,20,19,19,19,19,19,19,27,26,22,24,23,23,24,24,23,28,31,31,28,22,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,26,26,26,24,19,19,19,25,26,26,27,19,19,25,26,26
db 26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26
db 26,26,26,26,19,18,18,17,17,17,17,18,18,18,18,18,18,18,18,17,17,17,17,18,19,19,19,20,20,19
db 19,20,19,18,18,18,18,18,18,19,20,20,20,20,20,20,24,31,31,31,31,31,31,29,24,20,19,19,19,19
db 19,19,27,25,21,22,22,22,22,22,21,27,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,26,26,26,27,19,19,19,27,26,25,26,27,27,27,26,26,26,26,26,26,26,26,26,26,26,26
db 26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,25,25,23,23,23,23,23,23
db 23,22,22,18,18,18,18,18,18,21,19,18,18,18,19,19,19,20,20,19,18,19,18,18,18,18,18,18,18,19
db 20,20,20,20,20,20,25,26,25,25,25,25,25,26,25,24,24,24,24,24,23,23,28,28,26,27,27,27,27,27
db 26,29,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,26,26,26,27,19,19
db 19,27,26,25,26,19,19,27,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26
db 26,26,26,26,26,26,26,26,26,26,26,26,25,25,27,29,30,30,30,30,30,27,26,24,23,23,23,23,23,26
db 21,19,19,19,19,19,19,20,20,19,18,18,18,18,18,18,18,18,18,19,20,20,20,20,20,20,25,21,19,19
db 19,19,19,24,27,28,30,30,29,29,29,28,29,31,31,31,31,31,31,31,31,31,31,31,28,22,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,26,26,26,25,19,19,19,27,26,26,27,19,19,27,26,26
db 26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26
db 26,26,26,25,23,26,28,27,27,27,27,26,26,30,31,31,31,31,31,28,20,18,19,19,19,19,19,19,19,19
db 18,18,18,18,18,18,19,18,19,20,20,20,20,20,20,20,25,21,19,20,20,20,20,24,27,26,27,27,27,27
db 27,26,29,30,30,30,30,30,30,30,30,31,31,31,28,26,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,27,20,19,19,19,19,19,21,27,19,19,19,19,27,27,27,27,27,27,27,27,27,27,27,27
db 27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,20,19,21,20,21,21
db 21,25,26,28,29,29,29,29,29,27,21,20,20,20,20,20,20,20,20,19,18,18,18,18,18,19,19,20,23,21
db 20,20,20,20,20,20,25,21,19,20,20,20,20,25,25,21,20,20,20,20,20,20,27,28,26,27,27,27,27,27
db 26,29,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,20
db 20,20,20,20,19,19,19,19,19,19,19,19,19,20,19,19,20,20,20,20,20,25,26,22,21,21,21,21,21,25
db 27,28,28,28,28,28,28,26,25,20,18,18,18,18,18,19,20,20,24,21,19,20,20,20,20,21,25,21,19,20
db 20,20,20,25,24,20,19,19,19,19,19,19,27,28,26,27,27,27,27,27,26,29,31,31,28,22,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,20,20,20,20,20,25,26,20,19,19,19,19,19,24,30,31,31,31,31,31,31,28,27,21
db 18,18,19,19,19,19,20,20,24,22,19,20,20,20,20,21,25,21,19,20,20,20,20,25,24,20,19,20,20,20
db 20,19,27,28,26,27,27,27,27,27,26,29,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,19,20,20,20,20
db 20,25,26,21,20,20,20,20,19,25,25,25,26,26,26,26,25,26,27,21,18,18,19,20,20,20,20,20,25,22
db 19,20,20,20,20,21,25,21,19,20,20,20,20,25,24,20,19,20,20,20,20,19,27,28,26,27,27,27,27,27
db 26,29,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,19,20,20,20,20,20,25,26,21,20,20,20,20,19,25
db 21,19,20,19,19,19,19,25,27,20,18,18,19,20,20,20,20,20,25,22,19,20,20,20,20,21,25,21,19,20
db 20,20,20,25,24,20,19,20,20,20,20,19,27,28,26,27,27,27,27,27,26,29,31,31,28,22,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,20,19,20,20,20,20,20,25,26,21,20,20,20,20,19,25,21,19,20,20,20,20,20,25,27,21
db 18,19,19,20,20,20,20,20,25,22,19,20,20,20,20,20,25,21,19,20,20,20,20,25,24,20,20,20,20,20
db 20,19,27,28,26,27,27,27,27,27,26,29,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,19,20,20,20,20
db 20,25,26,21,20,20,20,20,19,25,21,19,20,20,20,20,20,25,27,21,19,19,20,20,19,20,20,20,25,22
db 19,20,20,20,20,20,25,21,19,20,20,20,20,25,24,20,19,20,20,20,20,19,27,28,26,27,27,27,27,27
db 26,29,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,28,21,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,19,20,20,20,20,20,25,26,21,20,20,20,20,19,25
db 21,19,20,20,20,20,20,25,27,21,20,20,20,19,19,20,20,20,25,22,19,20,20,20,20,20,25,21,19,20
db 20,20,20,25,25,20,19,20,20,20,20,19,27,28,26,27,27,27,27,27,26,29,31,31,28,22,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,25,25,25,26,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,20,19,19,19,19,20,19,19,19,20,20
db 19,19,19,21,20,19,20,20,20,20,20,25,26,21,20,20,20,20,19,25,21,19,20,20,20,20,20,25,27,21
db 19,20,20,19,19,20,20,20,25,22,19,20,20,20,20,20,25,21,19,20,20,20,20,25,25,20,19,20,20,20
db 20,19,27,28,26,27,27,27,27,27,26,29,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,23,23,23,28,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,27,26,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,20,19,20,20,20,20
db 20,25,26,21,20,20,20,20,19,25,21,19,20,20,20,20,20,25,27,21,19,20,18,18,18,18,18,18,25,22
db 19,20,20,20,20,20,25,21,19,20,20,20,20,25,25,20,20,20,20,20,20,19,27,28,26,27,27,27,27,27
db 26,29,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,23,22,22,28,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,26,26,26
db 26,26,26,26,26,26,26,26,26,26,26,26,26,26,21,19,20,20,20,20,20,25,26,21,20,20,20,20,19,25
db 21,19,20,20,20,20,20,25,27,21,19,19,21,22,22,22,23,23,26,21,19,19,19,19,19,19,25,21,19,20
db 20,20,20,24,25,20,20,20,20,20,20,19,27,28,26,27,27,27,27,27,26,29,31,31,28,22,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,22,22,22,26,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,26,26,26,26,26,26,26,26,26,26,26,26,26
db 26,26,26,26,20,19,20,20,20,20,20,25,26,21,20,20,20,20,19,25,21,19,20,20,20,20,20,25,27,21
db 19,18,26,30,30,30,30,29,27,22,20,21,21,21,21,21,25,21,19,20,20,20,20,24,25,20,19,20,20,20
db 20,19,27,28,26,27,27,27,27,27,26,29,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,21,26,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,20,18,19,19,19,19
db 19,25,26,21,20,20,20,20,19,25,21,19,20,20,20,20,20,25,27,21,19,19,23,26,25,24,24,23,25,27
db 27,29,29,30,30,28,27,21,19,19,19,19,19,24,25,20,20,20,20,20,20,19,27,28,26,27,27,27,27,27
db 26,29,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,21,27,27
db 27,27,27,27,27,27,27,27,27,27,27,27,27,27,20,18,19,19,19,19,19,25,26,21,20,20,20,20,19,25
db 21,19,20,20,20,20,20,25,27,21,19,19,20,20,20,20,20,20,24,29,30,30,31,31,31,29,27,20,18,18
db 18,18,18,24,25,20,19,20,20,20,20,19,27,28,26,27,27,27,27,27,26,29,31,31,28,22,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,21,27,27
db 27,27,27,27,19,19,21,27,27,27,27,27,27,27,27,27,27,26,27,27,27,27,27,27,27,27,27,27,27,27
db 27,27,27,27,24,24,25,25,25,25,25,26,26,21,20,20,20,20,19,25,21,19,20,20,20,20,20,25,27,21
db 19,20,20,19,20,20,20,19,25,25,23,23,23,23,23,23,25,26,26,26,26,26,26,26,24,20,19,20,20,20
db 20,19,27,28,26,27,27,27,27,27,26,29,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,27,26,26,26,26,26,26,23,20,27,26,26,26
db 26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,27,29,29,29,29,29
db 29,27,26,21,19,20,20,20,19,25,21,19,20,20,20,20,20,25,27,21,19,20,20,20,20,20,20,20,25,21
db 18,19,19,19,19,20,24,31,31,31,31,31,31,28,24,21,19,20,20,20,20,19,27,26,23,25,24,24,24,25
db 24,28,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,26,26,26,26,26,26,26,27,27,27,26,26,26,26,26,26,26,26,26,26,26,26,26
db 26,26,26,26,26,26,26,26,26,25,25,25,25,25,23,25,26,26,26,26,26,26,26,20,18,19,19,19,18,24
db 21,19,20,20,20,20,20,25,27,21,19,20,20,20,20,20,20,20,25,22,19,20,20,20,20,20,24,27,28,28
db 28,27,27,27,25,21,19,19,19,19,19,19,27,23,20,22,22,22,22,22,21,27,31,31,28,22,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,26,26,26
db 26,26,26,26,24,20,27,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26
db 26,25,25,26,20,19,20,20,20,20,20,25,26,22,20,21,21,20,20,25,21,19,20,20,20,20,20,25,27,21
db 19,20,20,20,20,20,20,20,25,22,19,20,20,20,20,20,25,22,19,20,20,20,20,25,26,22,20,20,20,20
db 20,20,27,25,23,25,24,24,25,24,23,28,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,27,27,27,27,27,27,27,21,19,24,27,27,27
db 27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,26,27,27,27,20,19,20,20,20,20
db 20,25,27,27,28,28,28,27,27,27,20,19,19,19,19,19,19,24,27,21,19,20,20,20,20,20,20,20,25,22
db 19,20,20,20,20,20,25,21,19,20,19,19,19,25,26,27,28,27,27,27,27,25,28,30,30,30,30,30,30,30
db 30,31,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,20,20,21,21,27,21,19,19,19,20,27,27,27,27,27,27,27,27,27,27,27,27
db 27,27,27,27,27,27,27,27,27,27,27,27,27,27,20,19,20,20,20,20,20,25,27,28,28,28,28,27,27,27
db 20,17,17,17,17,17,17,24,27,21,19,20,20,20,20,20,20,20,25,22,19,20,20,20,20,21,25,21,19,20
db 20,20,19,25,26,28,30,29,29,29,29,27,29,31,31,31,31,31,31,31,31,31,31,31,28,22,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,20,20,20,20,20,25,26,23,22,22,22,22,22,25,25,25,25,25,25,25,25,26,26,21
db 19,19,19,19,19,19,19,20,25,22,19,20,20,20,20,21,25,21,19,20,20,20,20,25,25,23,23,23,23,23
db 23,22,28,29,28,29,28,29,28,28,28,30,31,31,28,23,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,20,20,20
db 20,25,25,20,19,20,20,20,19,24,25,25,25,25,25,25,25,27,26,22,21,21,21,21,21,21,21,21,25,22
db 19,20,20,20,20,21,25,21,19,20,20,20,20,25,24,20,19,19,19,19,19,19,27,28,26,27,27,27,27,26
db 26,29,31,31,27,23,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,19,20,20,20,20,20,25,25,21,20,20,20,20,20,25
db 21,19,19,19,19,19,19,25,27,26,26,26,26,26,26,26,26,25,26,22,19,20,20,20,20,20,25,21,19,20
db 20,20,20,25,25,20,20,20,20,20,20,19,27,28,26,27,27,27,27,27,26,29,31,31,27,23,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,20,19,20,20,20,20,20,25,25,21,20,20,20,20,19,25,21,19,20,20,20,20,19,25,27,26
db 26,26,26,26,26,26,26,26,27,21,19,19,19,19,19,20,25,21,19,20,20,20,20,25,25,20,19,20,20,20
db 20,19,27,29,26,27,27,27,27,27,26,29,31,31,28,23,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,19,20,20,20,20
db 20,25,25,21,20,20,20,20,19,25,21,19,20,20,20,20,20,25,27,21,19,20,20,20,20,20,20,20,25,22
db 19,20,20,20,20,20,25,21,19,20,20,20,20,25,25,20,19,20,20,20,20,19,27,29,26,27,27,27,27,27
db 26,29,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,19,20,20,20,20,20,25,26,21,20,20,20,20,19,25
db 21,19,20,20,20,20,20,25,27,21,19,19,19,19,19,19,19,19,24,27,27,27,27,27,27,26,26,21,19,20
db 20,20,19,25,25,20,19,20,20,20,20,19,27,29,26,27,27,27,27,27,26,29,31,31,28,21,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,20,19,20,20,20,20,20,25,26,21,20,20,20,20,19,25,21,19,20,20,20,20,20,25,27,21
db 20,20,20,20,20,20,20,20,24,30,31,31,31,31,31,30,27,20,18,19,19,19,19,24,25,20,19,20,20,20
db 20,19,27,29,26,27,27,27,27,27,26,29,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,19,20,20,20,20
db 20,25,26,21,20,20,20,20,20,25,21,19,20,20,20,20,20,25,27,21,19,20,20,20,20,20,20,20,24,26
db 25,25,26,26,26,26,26,24,22,22,22,22,22,25,25,21,19,20,20,20,20,19,27,29,27,27,27,27,27,27
db 27,29,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,20,20,19,20,20,20,20,20,25,26,21,20,20,20,20,19,25
db 21,19,20,20,20,20,20,25,27,21,20,20,20,20,20,20,20,20,25,22,19,19,19,19,19,20,24,28,28,28
db 28,28,28,27,25,21,19,20,20,20,20,19,27,28,26,27,26,26,26,27,26,29,31,31,28,22,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,20
db 19,19,20,21,20,19,19,19,19,19,19,25,26,21,20,20,20,20,20,25,21,19,20,20,20,20,20,25,27,21
db 20,20,20,20,20,20,20,20,25,22,19,20,20,20,20,20,25,27,26,27,27,26,26,27,25,20,18,19,19,19
db 19,18,27,25,21,22,22,22,22,22,21,27,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,27,27,27,27,27,27,27,27,27,27,19,19,19,27
db 27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,19,19,20,18,19,19,19,19
db 19,25,26,21,20,20,20,20,20,25,21,19,20,20,20,20,20,25,27,22,20,20,20,20,20,20,20,20,25,22
db 19,20,20,20,20,20,25,22,20,21,21,21,20,25,25,20,18,18,18,17,17,18,27,24,19,20,20,20,20,20
db 19,26,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,23,21,20,20,20,20,20,20,20,21,21,21,23,23,20,20,20,20,20,20,20,20,20,20,20,20
db 20,20,20,20,20,20,20,20,20,20,20,20,20,20,18,18,18,18,18,18,18,20,20,19,19,19,19,19,19,25
db 21,19,20,20,20,20,20,25,27,22,20,20,20,20,20,20,20,21,25,22,19,20,20,20,20,21,25,21,19,19
db 19,19,19,24,26,25,25,25,26,26,26,25,29,29,27,28,28,28,28,28,28,30,31,31,28,21,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,27,27,21,21,21,21,21,22,21,22,21,21,20,24,30,19,17,17,17,17
db 17,17,17,19,19,19,22,22,18,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17
db 17,17,17,17,18,18,18,18,18,18,18,18,18,19,19,19,19,19,19,26,22,19,20,20,20,20,20,25,27,22
db 20,20,20,20,20,20,20,21,25,22,19,20,20,20,20,20,25,22,19,20,20,20,19,25,25,23,23,23,23,23
db 23,23,28,31,31,31,31,31,31,31,30,31,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,27,20,20,20,20,20,20,20,27,27,19,19,19
db 20,27,23,23,23,23,23,23,23,23,23,23,23,23,24,19,18,18,18,18,18,18,18,20,20,20,23,23,18,18
db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,19,19,19,19,19,19,22,20,19,20,20,20,20,19,25,27,22,20,20,20,20,20,20,20,21,25,22
db 19,20,20,20,20,20,25,22,19,20,20,20,20,25,24,19,18,19,19,18,18,18,26,29,27,28,28,28,28,28
db 27,29,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,27,19,19,19,19,19,19,19,27,27,19,19,19,19,27,18,17,17,17,17,17,17,17
db 17,17,17,17,17,18,18,18,18,18,18,18,18,19,19,20,21,21,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,17
db 19,19,19,19,19,19,19,25,28,22,20,20,20,20,20,20,20,21,25,22,19,20,20,20,20,20,25,22,19,20
db 20,20,20,25,24,20,20,20,20,20,20,19,27,28,26,27,26,26,26,27,26,29,31,31,28,21,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,19,23,24,24,24,24,24,24
db 24,24,24,24,24,24,24,24,24,24,24,24,19,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,19,19,19,19,19,19,19,24,26,21
db 20,20,20,20,20,20,20,20,25,22,19,20,20,20,20,21,25,22,19,20,20,20,19,25,24,20,20,20,20,20
db 20,19,27,28,26,27,27,27,27,27,26,29,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,27,27,27,27,19,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,19,19,19,20,20,20,20,20,20,20,21,26,22
db 19,20,20,20,20,21,25,22,19,20,20,20,20,25,25,20,20,20,20,20,20,19,27,28,26,27,27,27,27,27
db 26,29,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,21,20,19,19,19
db 19,20,19,17,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,18,18,18,17,19,19,19,19,19,19,19,19,21,26,22,19,20,20,20,20,21,25,22,19,20
db 20,20,20,25,25,20,20,20,20,20,20,19,27,28,26,27,27,27,27,27,26,29,31,31,28,21,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,26,17,17,16,16,17,17,17,18,18,18,18,18,18,18
db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,19
db 19,19,19,19,19,19,19,20,26,22,19,20,20,20,20,21,25,22,19,20,20,20,20,25,25,20,20,20,20,20
db 20,19,27,28,26,27,27,27,27,27,26,29,31,31,28,21,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,24,23,24,23,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,19,22,20
db 19,20,20,20,20,21,25,22,19,20,20,20,20,25,25,21,20,20,20,20,20,19,27,28,26,27,27,27,27,27
db 26,29,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,30,30,30,29
db 18,17,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,17,19,19,19,19,20,19,21,26,22,19,20
db 20,20,20,25,26,21,20,20,20,20,20,19,27,28,26,27,27,27,27,27,26,29,31,31,28,22,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,22,19,19,19,19,18,18,18,18,18,18,18
db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,18,18,18,18,19,19,19,19,19,19,20,25,21,19,20,20,20,19,25,26,21,20,20,20,20
db 20,19,27,28,26,27,27,27,27,27,26,29,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,25,25,24,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,19,21,20,19,20,20,20,19,25,26,21,19,20,20,20,20,19,27,28,26,27,27,27,27,27
db 26,29,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,20,26,17,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,19,19,19
db 19,19,19,24,26,21,19,20,20,20,20,19,27,28,26,27,27,27,27,27,26,29,31,31,28,22,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,23,24,24,24,25,25,25
db 25,25,25,25,25,25,25,25,25,19,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,19,19,19,19,19,19,25,28,22,19,20,20,20
db 20,19,27,28,26,27,27,27,27,27,26,29,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,24,28,29,29,29,29,29,29,29,29,29,29,29,29,30,27,19
db 17,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,18,18,19,19,19,19,19,18,22,24,20,19,19,20,20,19,19,27,28,26,27,27,27,27,27
db 26,29,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,21,21,20,20,20,20,20,20,20,20,20
db 20,20,20,20,20,20,20,20,20,20,19,20,20,19,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,19,19,19,19,19,19,19,27,28,26,27,27,27,27,27,26,29,31,31,28,22,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,27,27,27,27,27,27,27,27,27,27,27,26,25
db 22,26,26,20,18,18,18,18,18,18,19,19,19,19,19,19,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,19,19,19,19,19
db 19,19,27,28,25,26,26,26,27,26,24,28,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,20,19,19,19,19,20,19,19,19,19,20,23,23,23,19,21,24,19,19,20,18,18,18,18,18,18
db 18,19,19,19,19,19,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18
db 18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,27,26,22,23,23,23,23,23
db 21,27,31,31,28,22,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17
db 17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17
db 17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17
db 17,17,17,17,17,17,17,17,17,17,17,17,26,24,19,21,21,21,21,21,19,26,31,31,28,21,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,23,22,22,22,22,22,23,23,23,23,23,23,23,23,22,22,22,23,22,22,22,22,22,22,22,22,22
db 22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22
db 22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22
db 22,22,20,23,23,24,24,24,24,24,23,20,29,29,20,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,21,20,21,25,25,22
db 21,21,20,21,21,22,21,21,20,21,21,20,21,20,21,22,22,21,21,20,20,20,21,21,21,21,21,21,21,21
db 21,21,21,21,22,21,21,21,21,21,21,21,25,25,26,25,25,21,21,21,21,21,21,21,21,21,21,21,21,21
db 21,21,20,20,21,20,20,21,20,20,21,20,21,21,21,21,21,21,21,22,21,20,19,19,20,20,20,20,20,20
db 20,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19
db 19,19,19,19,19,19,19,19,19,19
end