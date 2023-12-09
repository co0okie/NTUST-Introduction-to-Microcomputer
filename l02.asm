;file name :ch2_ex1.asm
.model small
.data
value1 dw 0ffffh
value2 dw 0003h
value3 dw 7fffh
value4 dw 8123h
.stack
.code
main proc
	mov ax,@data
	mov ds,ax
	mov ax,0
	add ax,value1
	add ax,value1
	;CF=? OF=? ZF=?
	add ax,value2
	;CF=? OF=? ZF=?
	add ax,value3
	;CF=? OF=? ZF=?
	add ax,value4
	;CF=? OF=? ZF=?
	
	mov bx, ax
	mov ah, 02h
	
	mov dl, bh
	mov cl, 4
	shr dl, cl
	add dl, 48
	int 21h

	mov dl, bh
	mov cl, 4
	shl dl, cl
	shr dl, cl
	add dl, 48
	int 21h

	mov dl, bl
	mov cl, 4
	shr dl, cl
	add dl, 48
	int 21h
	
	mov dl, bl
	mov cl, 4
	shl dl, cl
	shr dl, cl
	add dl, 48
	int 21h

	mov ax,4c00h
	int 21h
main endp
end main