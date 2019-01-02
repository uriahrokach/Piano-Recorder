;-------------------------
;Author - Uriah Rokach
;This program recives notes from the keyboard, 
;and can play and delete using the mouse.
;-------------------------

IDEAL
MODEL small
STACK 100h
DATASEG

; --------------------------
; [variables]
; --------------------------

; ------------[basic variables]-------------

	tav	db 8,5,5,6,3,3,1,3,5,6,8,8,8,0,0,0,'$' ; the notes of the recorder

	xclr dw 44		; The x of the left up corner of the rubber
	yclr dw 2		; The Y of the left up corner of the rubber
	height dw 60	; The height of the rubber
	widthe dw 190	; The width of the rubber

; ------------[button location variables]-------------

	num equ 4						; number of buttons
	current_index dw 39h,'$'		;the index of current button pressed

	xleft dw 235, 235, 274, 274		; The X of the left up corner of the buttons - by order
	yleft dw 10, 46, 10 , 46		; The Y of the left up corner of the buttons - by order
	rec_width dw 27, 27, 27, 27		; The width of the buttons - by order
	rec_height dw 27, 27, 27, 27	; The height of the buttons - by order

; ------------[Graphic letters]-------------

	Aletter db 255,255,0,255,255,255,0,255,0,255,255,0,0,0,255,255,0,255,0,255,255,0,255,0,255 		; A data in graphic mode - FIXED
	Bletter db 0,0,255,255,255,0,255,0,255,255,0,0,255,255,255,0,255,0,255,255,0,0,255,255,255		; B data in graphic mode - FIXED
	Cletter db 255,0,0,0,255,0,255,255,255,255,0,255,255,255,255,0,255,255,255,255,255,0,0,0,255 	; C data in graphic mode - FIXED
	Dletter db 0,0,0,255,255,0,255,255,0,255,0,255,255,0,255,0,255,255,0,255,0,0,0,255,255			; D data in graphic mode - FIXED
	Eletter db 0,0,0,0,255,0,255,255,255,255,0,0,0,0,255,0,255,255,255,255,0,0,0,0,255				; E data in graphic mode - FIXED
	Fletter db 0,0,0,0,255,0,255,255,255,255,0,0,0,0,255,0,255,255,255,255,0,255,255,255,255		; F data in graphic mode - FIXED
	Gletter db 0,0,0,0,255,0,255,255,255,255,0,255,0,0,255,0,255,255,0,255,0,0,0,0,255				; G data in graphic mode - FIXED
	Sletter db 255,0,255,0,255,0,0,0,0,0,255,0,255,0,255,0,0,0,0,0,255,0,255,0,255					; # data in graphic mode - FIXED

	Pletter db 15,255,255,255,255,255,255,255,255,15,15,255,255,255,255,255,255,255,15,15,15,255,255,255,255,255,255,15,15,15,15,255,255,255,255,255,15,15,15,15,15,255,255,255,255,15,15,15,15,255,255,255,255,255,15,15,15,255,255,255,255,255,255,15,15,255,255,255,255,255,255,255,15,255,255,255,255,255,255,255,255	;Play mark's color
	Nletter	db 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255	;Play mark's color

	PlayX	dw	245	;The X of the play mark
	PlayY	dw	55	;The Y of the play mark

	PlayTemp dw 0	;The temporary variable of the play mark

	x dw 44			; The x of the start writing place
	y dw 44			; The y of the start writing place 

	temp dw 0		; temporary variable

; ------------[image variables]-------------

	filename db 'start.bmp',0			; the name of the image
	filehandle dw ?						; file handler
	Header db 54 dup (0)				; header of the image
	Palette db 256*4 dup (0)			; Palette of colors that can be projected to the screen
	ScrLine db 320 dup (0)				; on line of the screen
	ErrorMsg db 'Error', 13, 10,'$'		; error messege to display

CODESEG

; --------------------------
; [Code]
; --------------------------

proc OpenFile ; open the first file 
; Open file
	mov ah, 3Dh
	xor al, al
	mov dx, offset filename
	int 21h
	jc openerror
	mov [filehandle], ax

	ret

openerror:
	mov dx, offset ErrorMsg
	mov ah, 9h
	int 21h
	ret
endp OpenFile

proc ReadHeader
; Read BMP file header, 54 bytes
	mov ah,3fh
	mov bx, [filehandle]
	mov cx,54
	mov dx,offset Header
	int 21h
	ret
endp ReadHeader

proc ReadPalette
; Read BMP file color palette, 256 colors * 4 bytes (400h)
	mov ah,3fh
	mov cx,400h
	mov dx,offset Palette
	int 21h
	ret
endp ReadPalette

proc CopyPal
; Copy the colors palette to the video memory
; The number of the first color should be sent to port 3C8h
; The palette is sent to port 3C9h
	mov si,offset Palette
	mov cx,256
	mov dx,3C8h
	mov al,0			; Copy starting color to port 3C8h
		
	out dx,al			; Copy palette itself to port 3C9h

	inc dx

PalLoop:
; Note: Colors in a BMP file are saved as BGR values rather than RGB.
	
	mov al,[si+2] 		; Get red value.
	shr al,2 			; Max. is 255, but video palette maximal
						; value is 63. Therefore dividing by 4.
	out dx,al 			; Send it.
	mov al,[si+1] 		; Get green value.
	shr al,2
	out dx,al 			; Send it.
	mov al,[si] 		; Get blue value.
	shr al,2
	out dx,al 			; Send it.
	add si,4 			; Point to next color.
						; (There is a null chr. after every color.)

	loop PalLoop
	ret
endp CopyPal

proc CopyBitmap
; BMP graphics are saved upside-down.
; Read the graphic line by line (200 lines in VGA format),
; displaying the lines from bottom to top.
	mov ax, 0A000h
	mov es, ax
	mov cx,200

PrintBMPLoop:
	push cx
						; di = cx*320, point to the correct screen line
	mov di,cx
	shl cx,6    		;*64
	shl di,8    		;*256         256+64 = 320
	add di,cx
						; Read one line to variable ScrLine (buffer)
	mov ah,3fh  
	mov cx,320
	mov dx,offset ScrLine
	int 21h
						; Copy one line into video memory
	cld 				; Clear direction flag, for movsb for inc si, inc di
	mov cx,320
	mov si,offset ScrLine

rep movsb ; Copy line to the screen
 ;rep movsb is same as the following code:
 ;mov es:di, ds:si
 ;inc si
 ;inc di
 ;dec cx
 ;loop until cx=0
pop cx
loop PrintBMPLoop
ret
endp CopyBitmap

proc CloseFile  
	mov ah,3Eh 
	mov bx, [filehandle] 
	int 21h                     
	ret 
endp CloseFile 

proc screen
;prints the BMP image to the screen
	
	mov ax, 13h		; Graphic mode	
	int 10h
	call OpenFile	; Process BMP file
	call ReadHeader
	call ReadPalette
	call CopyPal
	call CopyBitmap
	call CloseFile
	ret
endp screen

halfsec:
;waits half second
	push si
	push cx
	push dx
	push bx
	push ax
					; 	si - timer signals counter
					;   CPU get timer signal ~18 times in second
	mov si, 9
					; read clock values, we need dl value
	mov ah, 2Ch
	int 21h 
	mov bl, dl      ; bl save the last value of dl
	
od:
	int 21h      	; read clock values
	cmp bl, dl
	je od        	; jump to od if dl not updated 
					; because CPU has not got new timer signal
	mov bl, dl   	; dl updated and bl save new dl value
	dec si
	jnz od
	
	pop ax
	pop bx
	pop dx
	pop cx
	pop si
	ret
	
sixtesec:
	push si
	push cx
	push dx
	push bx
	push ax
					; 	si - timer signals counter
					;   CPU get timer signal ~18 times in second
	mov si, 3
					; read clock values, we need dl value
	mov ah, 2Ch
	int 21h 
	mov bl, dl      ; bl save the last value of dl
	
oder:
	int 21h     	; read clock values
	cmp bl, dl
	je oder        	; jump to od if dl not updated 
					; because CPU has not got new timer signal
	mov bl, dl   	; dl updated and bl save new dl value
	dec si
	jnz oder
	
	pop ax
	pop bx
	pop dx
	pop cx
	pop si
	ret

proc startsound
;Plays a sound  
 
	pop bp                   ;ip ---> bp
	in al, 61h               ; port 61h - speaker port
	or al, 00000011b         ; put two lowest bits as 1 for 
	out 61h, al              ; turn speaker on
	mov al, 0b6h             ; send 0b6h to port 43h 
							 ; for open speaker for change frequency ----------
	out 43h, al
							;for put frequency f we need calculate num = 1193180/f
							; and out num to 8 bit port 42h
							; we will do it in two steps: out low byte of num and than out high byte of num
							; for example note LA frequency is 440. num = 1193180/440 = 2712 = 0A98h
	pop ax           
	out 42h, al
	mov al,  ah
	out 42h, al
	
	push bp
	ret
endp startsound	

proc endsound
;turn of sound	
	                    
	in   al,61h                   
	and  al,0FCh                 
	out  61h,al                  
		
	ret
endp endsound

proc search_point
; procedure get coordinate Y, X of any point in the stack,
; search the point in all the rectangles and
; update the [current_index] variable with index of the rectangle 
; that consists the point <X,Y> 
; otherway current_index = 9

    push bp
	mov  bp, sp											;  [bp+6] - Y point coordinate;  [bp+4] - X point coordinate	
	push si
	push bx
	push ax
	push dx
	push cx
	
	mov [word ptr current_index], 39h                  ; 39h = ascii code of 9
    mov si, 0                                          ; si = index * 2 ( for valid address of current item in the arrays)
search_loop:
	mov bx, offset yleft
	add bx, si                   
	mov ax,[bx]                                        ; ax = Y coordinate of current rectangle left_up corner
	mov bx, offset xleft
	add bx, si
	mov dx,[bx]                                         ; dx = X coordinate of current rectangle left_up corner
	
	cmp [bp+6], ax                                      ; if the point is left to current rectangle 
	jb loop_bottom                                      ; yes - the rect not consists the point 
	
	cmp [bp+4], dx                                      ; if the point is up to current rectangle
	jb loop_bottom                                      ; yes - the rect not consists the point 
	
	mov bx, offset rec_width
	add bx, si
	add dx, [bx]
	cmp [bp+4], dx                                       ; if the point is right to current rectangle
	ja loop_bottom                                       ; yes - the rect not consists the point 
	
	mov bx, offset rec_height
	add bx, si
    add ax, [bx]
	cmp [bp+6], ax                                         ; if the point is down to current rectangle
	ja loop_bottom                                         ; yes - the rect not consists the point 
	                                                       ; the current rect consists the point
    sar si,1                                               ;si = index * 2. we need index
    mov [current_index], si
	add [word ptr current_index], 1h
	jmp to_ret
loop_bottom:	
	add si,2                                               ; move to next index in the arrays
	cmp si,num*2
	jne search_loop
to_ret:	
	pop cx
	pop dx
	pop ax
	pop bx
	pop si
	pop bp
	ret 4
endp search_point 

printNote:
; this function prints one letter to the screen
;DI - the offset of the letter that will be printed

push ax
push bx
push cx 
push dx

mov dx, [x]

mov bx, 5
forer:
	mov cx, 5
lop:

		push cx 
		push dx
		push bx 
		
		mov bx, [temp]				; move to the correct byte
		add bx, di					;determins which letter will be printed using DI
		
		mov bh,0h					;prints on pixel to the screen
		mov cx,[x]
		mov dx,[y]
		mov al,[bx]
		mov ah,0ch
		int 10h
		
		inc [word ptr temp]
		inc [x]
		
		pop bx 
		pop dx
		pop cx 
		
		dec cx 
		cmp cx, 0
		jnz lop
	
	mov [x], dx						;update all values
	inc [y]
	
	dec bx
	cmp bx,0
	jnz forer
	mov [temp], 0					; return values to their former value

pop dx
pop cx
pop bx 
pop ax
ret 

printPlay:
; this function prints one letter to the screen
;DI - the offset of the letter that will be printed

push ax
push bx
push cx 
push dx

mov dx, [PlayX]

mov bx, 9
forering1:
	mov cx, 9
loping:

		push cx 
		push dx
		push bx 
		
		mov bx, [PlayTemp]					; move to the correct byte
		add bx, offset Pletter				;add the place of the play button
		
								;prints on pixel to the screen
		mov cx,[PlayX]
		mov dx,[PlayY]
		mov al,[bx]
		mov ah,0ch
		int 10h
		
		inc [word ptr PlayTemp]
		inc [word ptr PlayX]
		
		pop bx 
		pop dx
		pop cx 
		
		dec cx 
		cmp cx, 0
		jnz loping
	
	mov [PlayX], dx							;update all values
	inc [word ptr PlayY]
	
	dec bx
	cmp bx,0		
	jnz forering1
	mov [word ptr PlayX], 245					; return values to their former value
	mov [word ptr playY],55
	mov [word ptr PlayTemp], 0

pop dx
pop cx
pop bx 
pop ax
ret 

printNull:
; this function prints one letter to the screen
;DI - the offset of the letter that will be printed

push ax
push bx
push cx 
push dx

mov dx, [PlayX]

mov bx, 9
forering2:
	mov cx, 9
loping2:

	push cx 
		push dx
		push bx 
		
		mov bx, [PlayTemp]	; move to the correct byte
		add bx, offset Nletter	;add the place of the play button
		
;prints on pixel to the screen
		mov cx,[PlayX]
		mov dx,[PlayY]
		mov al,[bx]
		mov ah,0ch
		int 10h
		
		inc [word ptr PlayTemp]
		inc [PlayX]
		
		pop bx 
		pop dx
		pop cx 
		
		dec cx 
		cmp cx, 0
		jnz loping2
	
	mov [PlayX], dx			;update all values
	inc [PlayY]
	
	dec bx
	cmp bx,0
	jnz forering2
	mov [PlayX], 245		; return values to their former value
	mov [playY],55
	mov [PlayTemp], 0

pop dx
pop cx
pop bx 
pop ax
ret 

proc paint_rect
; get left_up corner coordinates, width and height of rectangle in stack and paint the rectangle

	push bp
    mov bp, sp
;===============> [bp+10] -  yleft ;  [bp+8] - xleft ;	[bp+6] - rec_width ; [bp+4] - rec_height
    push ax
	push dx
	push di
	push si
	push bx

	mov ax, [bp+10]                                           ; yleft --->ax
	mov dx, 320		
	mul dx
    mov di, ax                                                ; ax <---(yleft coordinate)*320
    add di, [bp+8]                                            ; di - address of left_up corner
    mov dx, [bp+4]

loo1:                                                         ; loop for paint all [height] lines
    mov cx, [bp+6]                                            ; cx = ax = rec_width
	mov ax, [bp+6]

loo2:                                                         ; loop for paint current line
    mov [byte ptr es:di], 255                                  ; Set color 3h to current address
	inc di
	loop loo2
	sub di, ax                                                ; go to left side of rectangle
	add di, 320                                               ; move to next line
	dec dx
	jnz loo1

    pop bx	
	pop si
	pop di
	pop dx
	pop ax
	pop bp

	ret 8

endp paint_rect

proc clr_all
; this function clear all notes data from the screen and the memory
	
	mov bx, [yclr]					; initialize veriables for clearscreen
	push bx
	mov bx, [xclr]
	push bx
	mov bx, [widthe]
	push bx
	mov bx, [height]
	push bx

	call paint_rect					; clear the screen

	push di
	push cx
	mov di, offset tav
	mov cx, 16
	
clr_for:							; loop through all the cells of tav
	mov [byte ptr di], 0			; delete the current note
	inc di
	loop clr_for
	mov si, offset tav				; put SI in the start point of the notes
	mov [x], 44
	mov [y], 44
	mov [xclr], 44
	mov [yclr], 2
	pop cx
	pop di
	ret
endp clr_all

play:
call printPlay
mov bx, offset tav

playlop:
	
; play the c note for half second
cnote:
		cmp [byte ptr bx], 1
		jne csnote
		push 9121       				;9121 the frecuency of c           
		call startsound  				;plays the sound
		call halfsec
		call endsound
		jmp restart
	
; play the c# note for half second	
csnote:
		cmp [byte ptr bx], 2
		jne dnote
		push 8609
		call startsound 
		call halfsec
		call endsound
		jmp restart
	
; play the d note for half second
dnote:
		cmp [byte ptr bx], 3
		jne dsnote
		push 8126
		call startsound 
		call halfsec
		call endsound
		jmp restart
	
; play the d# note for half second
dsnote:
		cmp [byte ptr bx], 4
		jne enote
		push 7670
		call startsound 
		call halfsec
		call endsound
		jmp restart
	
; play the e note for half second
enote:
		cmp [byte ptr bx], 5
		jne fnote
		push 7239
		call startsound 
		call halfsec
		call endsound
		jmp restart
	
; play the f note for half second
fnote:
		cmp [byte ptr bx], 6
		jne fsnote
		push 6833
		call startsound 
		call halfsec
		call endsound
		
		jmp restart
	
; play the f# note for half second
fsnote:
		cmp [byte ptr bx], 7
		jne gnote 
		push 6449
		call startsound 
		call halfsec
		call endsound
		jmp restart
	
; play the g note for half second
gnote:
		cmp [byte ptr bx], 8
		jne gsnote
		push 6087
		call startsound 
		call halfsec
		call endsound
		jmp restart
	
; play the g# note for half second
gsnote:
		cmp [byte ptr bx], 9
		jne anote 
		push 5746
		call startsound 
		call halfsec
		call endsound
		jmp restart
	
	checkpoint:
	jmp playlop
	
; play the a note for half second
anote:
		cmp [byte ptr bx], 10
		jne asnote
		push 5423
		call startsound 
		call halfsec
		call endsound
		jmp restart
	
; play the a# note for half second
asnote:
		cmp [byte ptr bx], 11
		jne bnote
		push 5119
		call startsound 
		call halfsec
		call endsound
		jmp restart
	
; play the b note for half second
bnote:
		cmp [byte ptr bx], 12
		jne nnote
		push 4831
		call startsound 
		call halfsec
		call endsound
		jmp restart
	
; if there is no note, play that	
nnote:
		cmp [byte ptr bx], 0
		jne restart
		call halfsec
		jmp restart
	
; restart the loop
restart:
		call endsound
		call sixtesec
		inc bx
		cmp [byte ptr bx],'$'
		jnz checkpoint
		call printNull
		
	ret
	
jmp_clr:
call clr_all
ret

proc print_index
; checks for what the affect of the mouse
	push ax
	push dx
	push bx
	                                                  
	mov dl, 2                                                     ; dl <-- column
	mov dh, 1                                                     ; dh <--  row
	mov bx, 0                                                     ; Page number, 0 for graphics modes
	mov ah, 2h                                                    ; ah=2: set cursor position
	int 10h

                                                                 
	mov bx, offset current_index	
	mov cl, [byte ptr bx]
	
	cmp cl, 4													; checks if 4th button and close the program if so
	je jmp_jmper_exit	
	
	;cmp cl, 2
	;jnz next1
	;call call_help_caller
	
	cmp cl, 3													; checks if 2nd button and clr the note data if so
	jne next1
	call jmp_clr
	
next1:
	cmp cl, 1													; checks if 1st button and play if so
	jnz next2
	call play
	
	
next2:
	
	pop bx
	pop dx
	pop ax
	ret
endp print_index

start:
	mov ax, @data
	mov ds, ax
; --------------------------
; main procedure
; --------------------------
	 
	mov di, offset filename
	
	mov ax, 0A000h                                              ; Set es to graphics screen segment
    mov es, ax
	
	mov ax, 13h                                                 ; put video 13h graphics mode
	int 10h
	
	call screen
	
	push ax
	mov ah, 01h
	int 21h
	pop ax
	
	jmp skip_this
	
jmp_jmper_exit:
	jmp jmperExit
	
skip_this:
	
	mov [byte ptr filename], 'h'
	mov [byte ptr filename  + 1], 'e'
	mov [byte ptr filename  + 2], 'l'
	mov [byte ptr filename  + 3], 'p'
	mov [byte ptr filename  + 4], 'r'
	
	call screen 
	
	push ax
	mov ah, 01h
	int 21h
	pop ax
	
	mov [byte ptr filename], 'p'
	mov [byte ptr filename  + 1], 'i'
	mov [byte ptr filename  + 2], 'a'
	mov [byte ptr filename  + 3], 'n'
	mov [byte ptr filename  + 4], 'o'
	
	call screen
	
	mov ax,0                                                    ; init mouse
	int 33h
	
	mov ax,1                                                    ; show mouse
	int 33h
	
	mov si, offset tav
	
	call clr_all
	
infor:
	call print_index
	
	push bx
	mov bx, offset current_index
	mov [byte ptr bx],0
	pop bx

	in al, 60h
	
	jmp abutton
jmperExit:
	jmp exit
	
	; Check if the 'a' button was pressed
abutton:
		cmp al, 1eh				; is it the 'a' button?
		jne wbutton
		cmp [byte ptr si], '$'
		je jmper25
		push di
		mov di, offset Cletter 	; prints the 'c' note to the screen
		call printNote
		add [x], 6
		sub [y], 5
		pop di
		mov [byte ptr si], 1	; put the number 
		inc si
		call halfsec
		jmp endlop
	
jmper3:
	jmp jmper2
	
wbutton:
		cmp al, 11h				; is it the 'w' button?
		jne sbutton
		cmp [byte ptr si], '$'
		je jmper25
		push di
		mov di, offset Cletter 	; prints the 'c' note to the screen
		call printNote
		add [x], 5
		sub [y], 5
		mov di, offset Sletter 	; prints the '#' note to the screen
		call printNote
		add [x], 6
		sub [y], 5
		pop di
		mov [byte ptr si], 2	; put the number 
		inc si
		call halfsec
		jmp endlop
	
jmper25:
	jmp jmper2

sbutton:
		cmp al, 1fh				; is it the 's' button?
		jne ebutton
		cmp [byte ptr si], '$'
		je jmper2
		push di
		mov di, offset Dletter 	; prints the 'd' note to the screen
		call printNote
		add [x], 6
		sub [y], 5
		pop di
		mov [byte ptr si], 3	; put the number 
		inc si
		call halfsec
		jmp endlop
	
ebutton:
		cmp al, 12h				; is it the 'e' button?
		jne dbutton
		cmp [byte ptr si], '$'
		je jmper2
		push di
		mov di, offset Dletter 	; prints the 'd' note to the screen
		call printNote
		add [x], 5
		sub [y], 5
		mov di, offset Sletter 	; prints the '#' note to the screen
		call printNote
		add [x], 6
		sub [y], 5
		pop di
		mov [byte ptr si], 4	; put the number 
		inc si
		call halfsec
		jmp endlop
	
dbutton:
		cmp al, 20h				; is it the 'd' button?
		jne fbutton
		cmp [byte ptr si], '$'
		je jmper2
		push di
		mov di, offset Eletter 	; prints the 'e' note to the screen
		call printNote
		add [x], 6
		sub [y], 5
		pop di
		mov [byte ptr si], 5	; put the number 
		inc si
		call halfsec
		jmp endlop
		
jmper2:
	jmp jmper1
	
fbutton:
		cmp al, 21h				; is it the 'f' button?
		jne tbutton
		cmp [byte ptr si], '$'
		je jmper15
		push di
		mov di, offset Fletter 	; prints the 'f' note to the screen
		call printNote
		add [x], 6
		sub [y], 5
		pop di
		mov [byte ptr si], 6	; put the number 
		inc si
		call halfsec
		jmp endlop
		
tbutton:
		cmp al, 14h				; is it the 't' button?
		jne gbutton
		cmp [byte ptr si], '$'
		je jmper15
		push di
		mov di, offset Fletter 	; prints the 'f' note to the screen
		call printNote
		add [x], 5
		sub [y], 5
		mov di, offset Sletter 	; prints the '#' note to the screen
		call printNote
		add [x], 6
		sub [y], 5
		pop di
		mov [byte ptr si], 7	; put the number 
		inc si
		call halfsec
		jmp endlop

jmper15:
	jmp jmper1

gbutton:
		cmp al, 22h				; is it the 'g' button?
		jne ybutton
		cmp [byte ptr si], '$'
		je jmper1
		push di
		mov di, offset Gletter 	; prints the 'g' note to the screen
		call printNote
		add [x], 6
		sub [y], 5
		pop di
		mov [byte ptr si], 8	; put the number 
		inc si
		call halfsec
		jmp endlop
	
ybutton:
		cmp al, 15h				; is it the 'y' button?
		jne hbutton
		cmp [byte ptr si], '$'
		je jmper1
		push di
		mov di, offset Gletter 	; prints the 'g' note to the screen
		call printNote
		add [x], 5
		sub [y], 5
		mov di, offset Sletter 	; prints the '#' note to the screen
		call printNote
		add [x], 6
		sub [y], 5
		pop di
		mov [byte ptr si], 9	; put the number 
		inc si
		call halfsec
		jmp endlop
	
hbutton:
		cmp al, 23h				; is it the 'h' button?
		jne ubutton
		cmp [byte ptr si], '$'
		je jmper1
		push di
		mov di, offset Aletter 	; prints the 'a' note to the screen
		call printNote
		add [x], 6
		sub [y], 5
		pop di
		mov [byte ptr si], 10	; put the number 
		inc si
		call halfsec
		jmp endlop   
		
jmper1:
	jmp mcontrol

ubutton:
		cmp al, 16h				; is it the 'u' button?
		jne jbutton
		cmp [byte ptr si], '$'
		je mcontrol
		push di
		mov di, offset Aletter 	; prints the 'a' note to the screen
		call printNote
		add [x], 5
		sub [y], 5
		mov di, offset Sletter 	; prints the '#' note to the screen
		call printNote
		add [x], 6
		sub [y], 5
		pop di
		mov [byte ptr si], 11	; put the number 
		inc si
		call halfsec
		jmp endlop
	
jbutton:
		cmp al, 24h				; is it the 'j' button?
		jne mcontrol
		cmp [byte ptr si], '$'
		je mcontrol
		push di
		mov di, offset Bletter 	; prints the 'b' note to the screen
		call printNote
		add [x], 6
		sub [y], 5
		pop di
		mov [byte ptr si], 12	; put the number 
		inc si
		call halfsec
		jmp endlop
		
; mouse control
mcontrol:
	mov ax, 3
	int 33h
	;CX = horizontal (X) position  (0..639)
	;DX = vertical (Y) position  (0..199)
	;BX = button status:

	;	|F-8|7|6|5|4|3|2|1|0|  Button Status
	;	  |  | | | | | | | `---- left button (1 = pressed)
	;	  |  | | | | | | `----- right button (1 = pressed)
	;	  `------------------- unused
	cmp di, bx                                                  ; if player clicked mouse button? 
	je	endlop
	mov di, bx
	and bx, 11b
	cmp bx, 0
	je 	endlop
	; player clicked mouse button

	sar cx,1
	push dx
	push cx
	call search_point                                            ;yes! we need know if player clicked in any rectangle
	
	jmp endlop
            
endlop:
		jmp infor

exit:
	mov ah, 0
	mov al, 2
	int 10h
	mov ax, 4c00h
	int 21h
END start