[org 0x100]
jmp start
strSUDOKU: 
db 0xdb,0xdb,0xdb,0xdb,0xdb,0xdb,0xdb,0xbb,0xdb,0xdb,0xbb,0x20,0x20,0x20,0xdb,0xdb,0xbb,0xdb,0xdb,0xdb,0xdb,0xdb,0xdb,0xbb,0x20,0x20,0xdb,0xdb,0xdb,0xdb,0xdb,0xdb,0xdb,0xbb,0xdb,0xdb,0xbb,0x20,0xdb,0xdb,0xbb,0xdb,0xdb,0xbb,0x20,0x20,0x20,0xdb,0xdb,0xbb ; row 1
db 0xdb,0xdb,0xc9,0xcd,0xcd,0xcd,0xcd,0xbc,0xdb,0xdb,0xba,0x20,0x20,0x20,0xdb,0xdb,0xba,0xdb,0xdb,0xc9,0xcd,0xcd,0xcd,0xdb,0xdb,0xbb,0xdb,0xdb,0xc9,0xcd,0xcd,0xdb,0xdb,0xba,0xdb,0xdb,0xba,0xdb,0xdb,0xc9,0xbc,0xdb,0xdb,0xba,0x20,0x20,0x20,0xdb,0xdb,0xba ; row 2
db 0xdb,0xdb,0xdb,0xdb,0xdb,0xdb,0xdb,0xbb,0xdb,0xdb,0xba,0x20,0x20,0x20,0xdb,0xdb,0xba,0xdb,0xdb,0xba,0x20,0x20,0x20,0xdb,0xdb,0xba,0xdb,0xdb,0xba,0x20,0x20,0xdb,0xdb,0xba,0xdb,0xdb,0xdb,0xdb,0xc9,0xbc,0x20,0xdb,0xdb,0xba,0x20,0x20,0x20,0xdb,0xdb,0xba ; row 3
db 0xc8,0xcd,0xcd,0xcd,0xcd,0xdb,0xdb,0xba,0xdb,0xdb,0xba,0x20,0x20,0x20,0xdb,0xdb,0xba,0xdb,0xdb,0xba,0x20,0x20,0x20,0xdb,0xdb,0xba,0xdb,0xdb,0xba,0x20,0x20,0xdb,0xdb,0xba,0xdb,0xdb,0xba,0xdb,0xdb,0xbb,0x20,0xdb,0xdb,0xba,0x20,0x20,0x20,0xdb,0xdb,0xba ; row 4
db 0xdb,0xdb,0xdb,0xdb,0xdb,0xdb,0xdb,0xba,0xc8,0xdb,0xdb,0xdb,0xdb,0xdb,0xdb,0xc9,0xbc,0xdb,0xdb,0xdb,0xdb,0xdb,0xdb,0xc9,0xbc,0x20,0xdb,0xdb,0xdb,0xdb,0xdb,0xdb,0xdb,0xba,0xdb,0xdb,0xba,0xc8,0xdb,0xdb,0xbb,0xc8,0xdb,0xdb,0xdb,0xdb,0xdb,0xdb,0xc9,0xbc ; row 5
db 0xc8,0xcd,0xcd,0xcd,0xcd,0xcd,0xcd,0xbc,0x20,0xc8,0xcd,0xcd,0xcd,0xcd,0xcd,0xbc,0x20,0xc8,0xcd,0xcd,0xcd,0xcd,0xcd,0xbc,0x20,0x20,0xc8,0xcd,0xcd,0xcd,0xcd,0xcd,0xcd,0xbc,0xc8,0xcd,0xbc,0x20,0xc8,0xcd,0xbc,0x20,0xc8,0xcd,0xcd,0xcd,0xcd,0xcd,0xbc,0x20 ; row 6
;strlen of each row = 50

;Sample of Output:
;███████╗██╗   ██╗██████╗  ███████╗██╗ ██╗██╗   ██╗
;██╔════╝██║   ██║██╔═══██╗██╔══██║██║██╔╝██║   ██║
;███████╗██║   ██║██║   ██║██║  ██║████╔╝ ██║   ██║
;╚════██║██║   ██║██║   ██║██║  ██║██║██╗ ██║   ██║
;███████║╚██████╔╝██████╔╝ ███████║██║╚██╗╚██████╔╝
;╚══════╝ ╚═════╝ ╚═════╝  ╚══════╝╚═╝ ╚═╝ ╚═════╝ 

strGRID1:
db 201,205,205,205,205,205,205,205,209,205,205,205,205,205,205,205,209,205,205,205,205,205,205,205,203,205,205,205,205,205,205,205,209,205,205,205,205,205,205,205,209,205,205,205,205,205,205,205,203,205,205,205,205,205,205,205,209,205,205,205,205,205,205,205,209,205,205,205,205,205,205,205,187 ; row 1
times 3 db 186,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,186,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,186,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,186 ; row 2-4
db 199,196,196,196,196,196,196,196,197,196,196,196,196,196,196,196,197,196,196,196,196,196,196,196,215,196,196,196,196,196,196,196,197,196,196,196,196,196,196,196,197,196,196,196,196,196,196,196,215,196,196,196,196,196,196,196,197,196,196,196,196,196,196,196,197,196,196,196,196,196,196,196,182 ; row 5
times 3 db 186,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,186,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,186,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,186 ; row 6-8
db 199,196,196,196,196,196,196,196,197,196,196,196,196,196,196,196,197,196,196,196,196,196,196,196,215,196,196,196,196,196,196,196,197,196,196,196,196,196,196,196,197,196,196,196,196,196,196,196,215,196,196,196,196,196,196,196,197,196,196,196,196,196,196,196,197,196,196,196,196,196,196,196,182 ; row 9
times 3 db 186,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,186,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,186,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,186 ; row 10-12
db 204,205,205,205,205,205,205,205,216,205,205,205,205,205,205,205,216,205,205,205,205,205,205,205,206,205,205,205,205,205,205,205,216,205,205,205,205,205,205,205,216,205,205,205,205,205,205,205,206,205,205,205,205,205,205,205,216,205,205,205,205,205,205,205,216,205,205,205,205,205,205,205,185 ; row 13
times 3 db 186,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,186,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,186,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,186 ; row 14-16
db 199,196,196,196,196,196,196,196,197,196,196,196,196,196,196,196,197,196,196,196,196,196,196,196,215,196,196,196,196,196,196,196,197,196,196,196,196,196,196,196,197,196,196,196,196,196,196,196,215,196,196,196,196,196,196,196,197,196,196,196,196,196,196,196,197,196,196,196,196,196,196,196,182 ; row 17
times 3 db 186,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,186,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,186,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,186 ; row 18-20
db 199,196,196,196,196,196,196,196,197,196,196,196,196,196,196,196,197,196,196,196,196,196,196,196,215,196,196,196,196,196,196,196,197,196,196,196,196,196,196,196,197,196,196,196,196,196,196,196,215,196,196,196,196,196,196,196,197,196,196,196,196,196,196,196,197,196,196,196,196,196,196,196,182 ; row 21
times 3 db 186,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,186,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,186,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,186 ; row 22-24
db 204,205,205,205,205,205,205,205,216,205,205,205,205,205,205,205,216,205,205,205,205,205,205,205,206,205,205,205,205,205,205,205,216,205,205,205,205,205,205,205,216,205,205,205,205,205,205,205,206,205,205,205,205,205,205,205,216,205,205,205,205,205,205,205,216,205,205,205,205,205,205,205,185 ; row 25
strGRID2:
times 3 db 186,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,186,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,186,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,186 ; row 26-28
db 199,196,196,196,196,196,196,196,197,196,196,196,196,196,196,196,197,196,196,196,196,196,196,196,215,196,196,196,196,196,196,196,197,196,196,196,196,196,196,196,197,196,196,196,196,196,196,196,215,196,196,196,196,196,196,196,197,196,196,196,196,196,196,196,197,196,196,196,196,196,196,196,182 ; row 29
times 3 db 186,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,186,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,186,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,186 ; row 30-32
db 199,196,196,196,196,196,196,196,197,196,196,196,196,196,196,196,197,196,196,196,196,196,196,196,215,196,196,196,196,196,196,196,197,196,196,196,196,196,196,196,197,196,196,196,196,196,196,196,215,196,196,196,196,196,196,196,197,196,196,196,196,196,196,196,197,196,196,196,196,196,196,196,182 ; row 33
times 3 db 186,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,186,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,186,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,179,32,32,32,32,32,32,32,186 ; row 34-36
db 200,205,205,205,205,205,205,205,207,205,205,205,205,205,205,205,207,205,205,205,205,205,205,205,202,205,205,205,205,205,205,205,207,205,205,205,205,205,205,205,207,205,205,205,205,205,205,205,202,205,205,205,205,205,205,205,207,205,205,205,205,205,205,205,207,205,205,205,205,205,205,205,188 ; row 37
;strlen of each row = 73

Sudoku1: 
db '516983427'
db '793214685'
db '428576931'
db '157362849'
db '239845176'
db '864791352'
db '942137568'
db '371658294'
db '685429713'
;strlen of each row = 9

Sudoku2: 
db '379245861'
db '285361974'
db '164978235'
db '958634127'
db '437192658'
db '621857493'
db '792413586'
db '843526719'
db '516789342'
;strlen of each row = 9

Sudoku3: 
db '963174258'
db '178325649'
db '254689731'
db '821437596'
db '496852317'
db '735961824'
db '589713462'
db '317246985'
db '642598173'
;strlen of each row = 9

cardsRemaining: db '000000000' 
;strlen = 9

Score: dw 0
Mistakes: dw 0
tickcount: dw 0
seconds:   dw 0
minutes:   dw 0
rand: db 0
randnum: dw 0
oldISR: dd 0
currentSudoku: dw 0
currentPage: dw 0xb900
currSelection: dw 4000
Notes: db 0

undoStack: times 6*150 dw 0  	; stores state of score, mistakes, number moved, currentPage, di of num, prevnum  
undoPtr: dw 0

strStart: db '1-Start' ;strlen = 7
strHelp: db '2-Help' ;strlen = 6
strExit: db '3-Exit' ;strlen = 6
strThanks: db 'Thanks for Playing!' ;strlen = 19
strLose: db 'You Lose!!! Better Luck Next Time.' ;strlen = 34
strPressKey: db 'Press any key to exit' ;strlen = 21
strWin: db 'Congratulations!!! You Won.' ;strlen = 27
strTimer: db 'Timer: 00:00' ; strlen = 12
strScore: db 'Score: 0' ; strlen = 8
strMistakes: db 'Mistakes Count: 0' ; strlen = 17
strLevels: db 'Enter 1 for Easy Mode, 2 for Medium, 3 for Hard.' ;strlen = 48
strBack: db 'Enter 4 return back to Start Screen.' ;strlen = 36
strInfo1: db '- Use WASD to move selection cursor in the grid.' ; strlen = 48
strInfo2: db '- Press number keys to fill the selected cell.' ; strlen = 46
strInfo3: db '- Press Up Key for 1st page of grid, and Down Key for 2nd page of grid.'  ; strlen = 71
strInfo4: db '- For entering notes, toggle Notes Mode via N key.' ;strlen = 50
strInfo5: db '- To undo move, press Z key.' ;strlen = 28
strInfo6: db '- To exit game, press ESC key.' ;strlen = 30
strInfo7: db 'Press any key to return to Start Screen' ;strlen = 39
strNotes: db 'Notes' ; strlen = 5
strSpace: db '     ' ; strlen = 5

music_length: dw 700 
music_data: incbin "nazirap.imf"

music:
		push si
		push dx
		push ax
		push bx
		push cx
		mov si, 200
	.next_note:
		mov dx, 388h
		mov al, [si + music_data + 0]
		out dx, al
		mov dx, 389h
		mov al, [si + music_data + 1]
		out dx, al
		mov bx, [si + music_data + 2]
		add si, 4
	.repeat_delay:
		mov cx, 610
	.delay:
		mov ah, 1
		int 16h
		jnz st
		loop .delay
		dec bx
		jg .repeat_delay
		cmp si, [music_length]
		jb .next_note
	st:
		mov dx, 388h
		mov al, 0xff
		out dx, al
		mov dx, 389h
		mov al, 0xff
		out dx, al
		mov bx, 0
		pop cx
		pop bx
		pop ax
		pop dx
		pop si
ret

; Generate random number from 1 to n and return it in the stack
randG:
    push bp
    mov bp, sp
    pusha

    cmp byte [rand], 0   ; Check if the random seed is initialized
    jne next

    mov ah, 00h          ; BIOS interrupt to get system timer in cx:dx
    int 1Ah
    inc byte [rand]      ; Mark random seed as initialized
    mov [randnum], dx    ; Initialize the seed with dx
    jmp next1

next:
    mov ax, 25173        ; LCG Multiplier
    mul word [randnum]   ; dx:ax = LCG multiplier * seed
    add ax, 13849        ; Add LCG increment value
    mov [randnum], ax    ; Update the seed (ax contains the random value)

next1:
    xor dx, dx
    mov ax, [randnum]    ; Load the generated random value
    mov cx, [bp+4]       ; Load the parameter n
    ;inc cx (made range from 1 to n+1) ; Increment n to make the range inclusive (1 to n)
    div cx               ; ax / cx -> Quotient in ax, Remainder in dx
    inc dx               ; Adjust range from 0-(n-1) to 1-n
    mov [bp+4], dx       ; Store the result (random number) in the stack

    popa
    pop bp
ret		; returns value in the space taken by parameter.

printtime:
    push bp
    mov bp, sp
    push es
    push ax
    push bx
    push cx
    push dx
    push di

    mov ax, 0xba00
    mov es, ax

    ; Print minutes (2 digits)
    mov ax, [cs:minutes]
    mov bx, 10
    mov cx, 2
    mov di, (80 * 18 + 17) * 2

	minuteloop:
		mov dx, 0
		div bx
		add dl, 0x30
		push dx
		dec cx
		cmp cx, 0
		jnz minuteloop

    mov cx, 2
	minutepos:
		pop dx
		mov dh, 7ah
		mov [es:di], dx
		add di, 2
		loop minutepos

    ; Leave space of colon
    add di, 2
    ; Print seconds (2 digits)
    mov ax, [cs:seconds]
    mov bx, 10
    mov cx, 2

	secondloop:
		mov dx, 0
		div bx
		add dl, 0x30
		push dx
		dec cx
		cmp cx, 0
		jnz secondloop

    mov cx, 2
	secondpos:
		pop dx
		mov dh, 7ah
		mov [es:di], dx
		add di, 2
		loop secondpos

    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    pop es
    pop bp
ret

printtimeEnd:
    push bp
    mov bp, sp
    push es
    push ax
    push bx
    push cx
    push dx
    push di


    ; Print minutes (2 digits)
    mov ax, [cs:minutes]
    mov bx, 10
    mov cx, 2
    mov di, (80 * 18 + 17) * 2

	minuteloop1:
		mov dx, 0
		div bx
		add dl, 0x30
		push dx
		dec cx
		cmp cx, 0
		jnz minuteloop1

    mov cx, 2
	minutepos1:
		pop dx
		mov dh, 7ah
		mov [es:di], dx
		add di, 2
		loop minutepos1

    ; Leave space of colon
    add di, 2
    ; Print seconds (2 digits)
    mov ax, [cs:seconds]
    mov bx, 10
    mov cx, 2

	secondloop1:
		mov dx, 0
		div bx
		add dl, 0x30
		push dx
		dec cx
		cmp cx, 0
		jnz secondloop1

    mov cx, 2
	secondpos1:
		pop dx
		mov dh, 7ah
		mov [es:di], dx
		add di, 2
		loop secondpos1

    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    pop es
    pop bp
ret

; timer interrupt service routine
timer:
    push ax

    inc word [cs:tickcount]
    mov ax, [cs:tickcount]
    cmp ax, 18
    jb skipSecond

    mov word [cs:tickcount], 0
    inc word [cs:seconds]

    ; Check if seconds reached 60
    mov ax, [cs:seconds]
    cmp ax, 60
    jb updateTime

    ; Reset seconds and increment minutes
    mov word [cs:seconds], 0
    inc word [cs:minutes]

updateTime:
    call printtime

skipSecond:
    mov al, 0x20
    out 0x20, al

    pop ax
iret

startTimer:
	push ax
	push es
	
	xor ax, ax
    mov es, ax
	mov ax, [es:8*4] 
	mov [oldISR], ax ; save offset of old routine 
	mov ax, [es:8*4+2] 
	mov [oldISR+2], ax ; save segment of old routine 
    cli
    mov word [es:8*4], timer
    mov [es:8*4+2], cs
    sti
	
	pop es
	pop ax
ret

pauseTimer:
	cmp word [oldISR], 0
	je exitFunc  ; if timer routine not hooked, exit function
	
	push ax
	push bx
	push es
	
	xor ax, ax
	mov es, ax
	mov ax, [oldISR] ; read old offset in ax 
	mov bx, [oldISR+2] ; read old segment in bx 
	cli ; disable interrupts 
	mov [es:8*4], ax ; restore old offset from ax 
	mov [es:8*4+2], bx ; restore old segment from bx 
	sti ; enable interrupts
	
	pop es
	pop bx
	pop ax
	exitFunc:
ret

keystroke: 
	; keystroke wait
	mov ah, 00h
	int 16h
ret

fillWhite: ; No params
	push ax
	push di

	mov di, 0
	nextloc:
	mov word [es:di], 0x7020
	add di, 2
	cmp di, 4000
	jnz nextloc

	pop di
	pop ax
ret

; subroutine to print a number at current [es:di] of screen 
; takes the number to be printed as its parameter 
printnum: 
	push bp 
	mov bp, sp 
	push es 
	push ax 
	push bx 
	push cx 
	push dx 
	push di 
	
	mov ax, [bp+4] ; load number in ax 
	mov bx, 10 ; use base 10 for division 
	mov cx, 0 ; initialize count of digits 
	nextdigit: 
	mov dx, 0 ; zero upper half of dividend 
	div bx ; divide by 10 
	add dl, 0x30 ; convert digit into ascii value 
	push dx ; save ascii value on stack 
	inc cx ; increment count of values 
	cmp ax, 0 ; is the quotient zero 
	jnz nextdigit ; if no divide it again 
	nextpos: 
	pop dx ; remove a digit from the stack 
	mov dh, 0x7a ; use normal attribute 
	mov [es:di], dx ; print char on screen 
	add di, 2 ; move to next screen location 
	loop nextpos ; repeat for all digits on stack
	
	pop di 
	pop dx 
	pop cx 
	pop bx 
	pop ax 
	pop es 
	pop bp 
ret 2 

printSingleRect:
; Assuming left, right is in bytes, not exact coords
; [bp+4] = attribute, 
; [bp+6] = right, [bp+8] = bottom, [bp+10] = left, [bp+12] = top
	push bp
	mov bp, sp
	push ax
	push cx
	push di
	push dx

	; Print top line
	mov ax, 80
	mul byte [bp+12]
	add ax, [bp+10]
	shl ax, 1
	mov di, ax  ; Location Acquired ((top*80 + left) * 2)
	mov cx, [bp+6]
	sub cx, [bp+10]  ; right - left = length of horizontal line
	sub cx, 2 ; top left and right characters not in loop
	mov ah, [bp+4]  ; Attribute
	mov al, 0xda ; top left char
	stosw
	mov al, 0xc4 ; horizontal char
	rep stosw
	mov al, 0xbf ; top right char
	stosw
	; Print vertical lines
	mov cx, [bp+8]
	sub cx, [bp+12]  ; bottom - top = length of vertical lines
	dec cx ; prevent overriding corner chars
	 
	mov dx, 80
	nextrow:
		; print left line
		mov ax, [bp+8]
		sub ax, cx  ; bottom - cx = current row
		mul dl
		add ax, [bp+10]
		shl ax, 1
		mov di, ax  ; Location Acquired (((bottom-cx)*80 + left) * 2)
		mov ah, [bp+4]  ; Attribute
		mov al, 0xb3
		mov [es:di], ax  
		; print right line
		mov ax, [bp+8]
		sub ax, cx  ; bottom - cx = current row
		mul dl
		add ax, [bp+6]
		dec ax  ; offset correction for right line
		shl ax, 1
		mov di, ax  ; Location Acquired (((bottom-cx)*80 + right) * 2)
		mov ah, [bp+4]  ; Attribute
		mov al, 0xb3
		mov [es:di], ax 
		loop nextrow
	; Print bottom line
	mov ax, 80
	mul byte [bp+8]
	add ax, [bp+10]
	shl ax, 1
	mov di, ax  ; Location Acquired ((bottom*80 + left) * 2)
	mov cx, [bp+6]
	sub cx, [bp+10]  ; right - left = length of horizontal line
	sub cx, 2 ; bottom left and right char not in loop
	mov ah, [bp+4]  ; Attribute
	mov al, 0xc0 ; bottom left char
	stosw
	mov al, 0xc4 ; horizontal char
	rep stosw
	mov al, 0xd9 ; bottom right char
	stosw
	

	pop dx
	pop di
	pop cx
	pop ax
	mov sp, bp
	pop bp
ret 10 ; 5 params = 10 bytes
	
printDoubleRect:
; Assuming left, right is in bytes, not exact coords
; [bp+4] = attribute, 
; [bp+6] = right, [bp+8] = bottom, [bp+10] = left, [bp+12] = top
	push bp
	mov bp, sp
	push ax
	push cx
	push di
	push dx

	; Print top line
	mov ax, 80
	mul byte [bp+12]
	add ax, [bp+10]
	shl ax, 1
	mov di, ax  ; Location Acquired ((top*80 + left) * 2)
	mov cx, [bp+6]
	sub cx, [bp+10]  ; right - left = length of horizontal line
	sub cx, 2 ; top left and right characters not in loop
	mov ah, [bp+4]  ; Attribute
	mov al, 0xc9 ; top left char
	stosw
	mov al, 0xcd ; horizontal char
	rep stosw
	mov al, 0xbb ; top right char
	stosw
	; Print vertical lines
	mov cx, [bp+8]
	sub cx, [bp+12]  ; bottom - top = length of vertical lines
	dec cx ; prevent overriding corner chars
	 
	mov dx, 80
	nextrow2:
		; print left line
		mov ax, [bp+8]
		sub ax, cx  ; bottom - cx = current row
		mul dl
		add ax, [bp+10]
		shl ax, 1
		mov di, ax  ; Location Acquired (((bottom-cx)*80 + left) * 2)
		mov ah, [bp+4]  ; Attribute
		mov al, 0xba
		mov [es:di], ax  
		; print right line
		mov ax, [bp+8]
		sub ax, cx  ; bottom - cx = current row
		mul dl
		add ax, [bp+6]
		dec ax  ; offset correction for right line
		shl ax, 1
		mov di, ax  ; Location Acquired (((bottom-cx)*80 + right) * 2)
		mov ah, [bp+4]  ; Attribute
		mov al, 0xba
		mov [es:di], ax 
		loop nextrow2
	; Print bottom line
	mov ax, 80
	mul byte [bp+8]
	add ax, [bp+10]
	shl ax, 1
	mov di, ax  ; Location Acquired ((bottom*80 + left) * 2)
	mov cx, [bp+6]
	sub cx, [bp+10]  ; right - left = length of horizontal line
	sub cx, 2 ; bottom left and right char not in loop
	mov ah, [bp+4]  ; Attribute
	mov al, 0xc8 ; bottom left char
	stosw
	mov al, 0xcd ; horizontal char
	rep stosw
	mov al, 0xbc ; bottom right char
	stosw
	

	pop dx
	pop di
	pop cx
	pop ax
	mov sp, bp
	pop bp
ret 10 ; 5 params = 10 bytes

printStr:
; requires that all req of lodsb, stosw and strlen are met
	nextChar:
		lodsb
		stosw
		loop nextChar
ret
	
startScreen:
	call fillWhite
	; border
	push word 0 ;top
	push word 0 ;left
	push word 24 ;bottom
	push word 80 ;right
	push word 30h ;attribute
	call printDoubleRect
	; buttons / boxes
	push word 18 ;top
	push word 15 ;left
	push word 20 ;bottom
	push word 24 ;right
	push word 79h ;attribute
	call printSingleRect
	
	push word 18 ;top
	push word 37 ;left
	push word 20 ;bottom
	push word 45 ;right
	push word 79h ;attribute
	call printSingleRect
	
	push word 18 ;top
	push word 59 ;left
	push word 20 ;bottom
	push word 67 ;right
	push word 79h ;attribute
	call printSingleRect
	
    ; Write "SUDOKU" (row 4, center)
	mov bx, 6
	mov si, strSUDOKU
	nextLine:
		mov ax, 80
		mov cx, 11
		sub cx, bx
		mul cl
		add ax, 15
		shl ax, 1
		mov di, ax
		mov cx, 50
		mov ah, 76h
		call printStr
		dec bx
		jne nextLine
		
    ; Write "1-Start" on row 19
    mov di, (80 * 19 + 16) * 2
	mov si, strStart
	mov cx, 7
	mov ah, 7ah
	call printStr
	
    ; Write "2-Help" on row 19
    mov di, (80 * 19 + 38) * 2
	mov si, strHelp
	mov cx, 6
	call printStr
	
    ; Write "3-Exit" on row 19
    mov di, (80 * 19 + 60) * 2
	mov si, strExit
	mov cx, 6
	call printStr
	
ret
	
gridScreen1:
; shows 73x25 of the grid
	call fillWhite
	mov bx, 25
	mov si, strGRID1
	nextLine2:
		mov ax, 80
		mov cx, 25
		sub cx, bx
		mul cl
		shl ax, 1
		mov di, ax
		mov cx, 73
		mov ah, 76h
		call printStr
		dec bx
		jne nextLine2
	
ret

gridScreen2:
; shows the rest of the grid as 73x12
	call fillWhite
	mov bx, 12
	mov si, strGRID2
	nextLine3:
		mov ax, 80
		mov cx, 12
		sub cx, bx
		mul cl
		shl ax, 1
		mov di, ax
		mov cx, 73
		mov ah, 76h
		call printStr
		dec bx
		jne nextLine3
	; call printCards
	mov di, (80 * 18 + 10) * 2
    mov ah, 0x7a
    mov si, strTimer
	mov cx, 12
    call printStr

	mov di, (80 * 18 + 33) * 2
    mov si, strScore
	mov cx, 8
    call printStr

	mov di, (80 * 18 + 51) * 2
    mov si, strMistakes
	mov cx, 17
    call printStr

ret

endScreen:
	call fillWhite
	; border
	push word 0 ;top
	push word 0 ;left
	push word 24 ;bottom
	push word 80 ;right
	push word 30h ;attribute
	call printDoubleRect
	
	; Write "Thanks for Playing!" on row 10
    mov di, (80 * 10 + 30) * 2
	mov si, strThanks
	mov cx, 19
	mov ah, 7ah
	call printStr
	
	; Write "Press any key to exit" on row 14
    mov di, (80 * 14 + 29) * 2
	mov si, strPressKey
	mov cx, 21
	call printStr

ret

helpScreen:
	call fillWhite
	; border
	push word 0 ;top
	push word 0 ;left
	push word 24 ;bottom
	push word 80 ;right
	push word 30h ;attribute
	call printDoubleRect
	
	; Write 1st line on row 8
    mov di, (80 * 8 + 6) * 2
	mov si, strInfo1
	mov cx, 48
	mov ah, 7ah
	call printStr
	
	; Write 2nd line on row 9
    mov di, (80 * 9 + 6) * 2
	mov si, strInfo2
	mov cx, 46
	call printStr
	
	; Write 3rd line on row 10
    mov di, (80 * 10 + 6) * 2
	mov si, strInfo3
	mov cx, 71
	call printStr
	
	; Write 4th line on row 11
    mov di, (80 * 11 + 6) * 2
	mov si, strInfo4
	mov cx, 50
	call printStr
	
	; Write 5th line on row 12
    mov di, (80 * 12 + 6) * 2
	mov si, strInfo5
	mov cx, 28
	call printStr
	
	
	; Write 6th line on row 13
    mov di, (80 * 13 + 20) * 2
	mov si, strInfo6
	mov cx, 30
	call printStr
	
	; Write 7th line on row 16
    mov di, (80 * 16 + 20) * 2
	mov si, strInfo7
	mov cx, 39
	call printStr
	
ret

difficultyScreen:
	call fillWhite
	; border
	push word 0 ;top
	push word 0 ;left
	push word 24 ;bottom
	push word 80 ;right
	push word 30h ;attribute
	call printDoubleRect
	
	; Write 1st line on row 8
    mov di, (80 * 8 + 15) * 2
	mov si, strLevels
	mov cx, 48
	mov ah, 7ah
	call printStr
	
	; Write 2nd line on row 9
    mov di, (80 * 9 + 20) * 2
	mov si, strBack
	mov cx, 36
	call printStr
	
ret

hideCursor:
	mov ah, 01h         ; Function to set cursor shape
	mov ch, 32           ; Start scan line (invisible cursor)
	mov cl, 0           ; End scan line (invisible cursor)
	int 10h             ; BIOS video interrupt
ret

showCursor:
	mov ah, 01h         ; Function to set cursor shape
	mov ch, 6           ; Start scan line
	mov cl, 7           ; End scan line
	int 10h             ; Call BIOS video interrupt
ret

setRandomBoard:
	push ax
	push cx
	push si
	push di
	push es
	
	push 3
	call randG
	pop bx
	dec bx  ; rand num from 0-2
	mov ax, 81
	mul bl
	mov bx, ax  ; (0-2) * 81
	add bx, Sudoku1    ; selects from board 1,2,3 via offset of 1st puzzle
	mov [currentSudoku], bx
	
	; Code for filling empty spaces of grid1 with numbers
	push 0xb900
	pop es
	
	mov cx, 6
	mov si, bx
	mov di, (80 * 2 + 4) * 2
	mov ah, 70h
	l1:
		push cx
		mov cx, 9
		l2:
			lodsb
			stosw
			add di, 14
			loop l2
		add di, (80 * 3 + 8) * 2
		pop cx
		loop l1	
	; code for filling empty spaces of grid2 with numbers
	push 0xba00
	pop es
	
	mov cx, 3
	mov si, bx
	add si, 9 * 6
	mov di, (80 * 1 + 4) * 2
	mov ah, 70h
	l3:
		push cx
		mov cx, 9
		l4:
			lodsb
			stosw
			add di, 14
			loop l4
		add di, (80 * 3 + 8) * 2
		pop cx
		loop l3
	
	pop es
	pop di
	pop si
	pop cx
	pop ax
ret

; Takes cx as input to remove that many numbers from grid
removeRandomNumbers:
	push bx
	push di
	push es
	
	lop1:
	push cx
	
	push 81
	call randG
	pop cx
	dec cx  ; random num from 0-80
	call compute_screen_position  ; set es:di
	xor bx, bx
	mov bl, [es:di] 
	cmp bl, 0x20  ; check if space is there (already deleted)
	je again
	sub bl, 0x30  ; convert to integer
	cmp byte [cardsRemaining + bx - 1], '6'  ; cannot remove more than 6 numbers of the same value, for game winning purposes
	je again
	inc byte [cardsRemaining + bx - 1] ; set cards remaining according to how they are removed
	mov byte [es:di], 0x20 ; replace num with space
	
	pop cx
	loop lop1
	jmp exit1
	again:
	pop cx
	inc cx
	loop lop1

	exit1:
	pop es
	pop di
	pop bx
ret

; Input:
;   cx = Index 'n' (0-based) of the number to display
; Output:
;   di = Screen position of the nth number
;   es = Screen of grid where it exists
compute_screen_position:
    push ax
	push dx
	push bx
	
	; Step 1: Compute row_array and col_array, and set es accordingly
	mov ax, cx       ; ax = n
    mov dx, 0        ; Clear dx
    mov bx, 9        ; Divisor = 9
    div bx           ; ax = row_array, dx = col_array
	cmp ax, 6
	jae pg2
	push 0xb900
	pop es
	; Step 2a: Compute row_offset = (80 * (2 + 4 * row_array) + 4) * 2
    mov bx, ax       ; bx = row_array
    shl bx, 2        ; bx = row_array * 4
    add bx, 2        ; bx = 2 + 4 * row_array
    imul bx, 80      ; bx = 80 * (2 + 4 * row_array)
    add bx, 4        ; bx = 80 * (2 + 4 * row_array) + 4
    shl bx, 1        ; bx = row_offset = (80 * (2 + 4 * row_array) + 4) * 2
	jmp step3
	pg2:
	push 0xba00
	pop es
	sub ax, 6
    ; Step 2b: Compute row_offset = (80 * (1 + 4 * row_array) + 4) * 2
    mov bx, ax       ; bx = row_array
    shl bx, 2        ; bx = row_array * 4
    add bx, 1        ; bx = 1 + 4 * row_array
    imul bx, 80      ; bx = 80 * (1 + 4 * row_array)
    add bx, 4        ; bx = 80 * (1 + 4 * row_array) + 4
    shl bx, 1        ; bx = row_offset = (80 * (1 + 4 * row_array) + 4) * 2

	step3:
    ; Step 3: Compute col_offset = col_array * 16
    mov ax, dx       ; ax = col_array
    shl ax, 4           ; ax = col_offset = col_array * 16

    ; Step 4: Compute total screen offset
    add bx, ax       ; bx = screen_offset = row_offset + col_offset
    mov di, bx       ; di = screen_offset
	
	pop bx
	pop dx
	pop ax
ret

; Input:
;   di = Screen position
;   es = Screen page
; Output:
;   cx = Index 'n' (0-based)
compute_index_from_screen:
    push ax
    push bx
	push di
	push es

	mov ax, es
	cmp ax, 0xb900
	jne checks2
	mov bx, 0
	mov cx, 9
	mov ax, 328
	c1:
		cmp di, ax
		je done
		inc bx
		add ax, 16
		loop c1
	mov cx, 9
	mov ax, 968
	c2:
		cmp di, ax
		je done
		inc bx
		add ax, 16
		loop c2
	mov cx, 9
	mov ax, 1608
	c3:
		cmp di, ax
		je done
		inc bx
		add ax, 16
		loop c3
	mov cx, 9
	mov ax, 2248
	c4:
		cmp di, ax
		je done
		inc bx
		add ax, 16
		loop c4
		mov cx, 9
	mov ax, 2888
	c5:
		cmp di, ax
		je done
		inc bx
		add ax, 16
		loop c5
	mov cx, 9
	mov ax, 3528
	c6:
		cmp di, ax
		je done
		inc bx
		add ax, 16
		loop c6
	checks2:
	mov bx, 54
	mov cx, 9
	mov ax, 168
	c7:
		cmp di, ax
		je done
		inc bx
		add ax, 16
		loop c7
	mov cx, 9
	mov ax, 808
	c8:
		cmp di, ax
		je done
		inc bx
		add ax, 16
		loop c8
	mov cx, 9
	mov ax, 1448
	c9:
		cmp di, ax
		je done
		inc bx
		add ax, 16
		loop c9
	
	done:
	mov cx, bx
	
    pop es
    pop di
    pop bx
    pop ax
ret

initScreens:
	mov ax, 03h ; Clear screen, set text mode
	int 10h
	call hideCursor

	mov ax, 0xb800     ; set video segment to page 0
	mov es, ax
	call startScreen ; Display Start Screen on page 0

	mov ax, 0xb900     ; set video segment to page 1
	mov es, ax
	call gridScreen1

	mov ax, 0xba00     ; set video segment to page 2
	mov es, ax
	call gridScreen2

	mov ax, 0xbb00     ; set video segment to page 3
	mov es, ax
	call endScreen

	mov ax, 0xbc00		; set video segment to page 4
	mov es, ax
	call helpScreen

	mov ax, 0xbd00		; set video segment to page 5
	mov es, ax
	call difficultyScreen
	
	call setRandomBoard ; sets both bx and currentSudoku to a random board

ret

clearCurrSelection:
	push ax
	push di
	push cx
	push es
	
	mov di, [currSelection]  ; di is top left of selection box
	push word [currentPage]
	pop es
	; reset 7x3 box's bg
	mov cx, 3
	lp:
	push cx
    mov cx, 7
	lp1:
		mov ax , [es:di]
		and ah, 0x0f  ; disable blink
		or ah, 0x70  ; white bg
		stosw
		loop lp1
	add di, 160 - (7*2) ; next row 
	pop cx
	loop lp
    
	pop es
	pop cx
	pop di
	pop ax
ret 

setCurrSelection:
    push ax
	push di
	push cx
	push es
	
	mov di, [currSelection]  ; di is top left of selection box
	push word [currentPage]
	pop es
	; reset 7x3 box's bg
	mov cx, 3
	lp2:
	push cx
    mov cx, 7
	lp3:
		mov ax , [es:di]
		and ah, 0x0f  ; reset bg
		or ah, 0xb0  ; blink on + cyan bg
		stosw
		loop lp3
	add di, 160 - (7*2)  ; next row 
	pop cx
	loop lp2
    
	pop es
	pop cx
	pop di
	pop ax
ret 

checkWinCondition:
	pusha
	
	; see if all cards are used or not
	mov cx, 9
	mov bx, 0
	cwc:
		cmp byte [cardsRemaining + bx], '0'
		jne notYet
		inc bx
		loop cwc
		
	; if here, then condition met, update exit screen with vars and msg
	call updateExitSc
	popa
	pop ax ; pop ip from call to prevent stack shiz
	jmp exitGame
	
	notYet:
	popa
ret

updateExitSc:
	pusha
	push 0xbb00
	pop es
	
	mov di, (80 * 5 + 26) * 2
    mov ah, 0x7a
    mov si, strWin
	mov cx, 27
    call printStr
	
	mov di, (80 * 18 + 10) * 2
    mov ah, 0x7a
    mov si, strTimer
	mov cx, 12
    call printStr

	mov di, (80 * 18 + 33) * 2
    mov si, strScore
	mov cx, 8
    call printStr

	mov di, (80 * 18 + 51) * 2
    mov si, strMistakes
	mov cx, 17
    call printStr
	
	call printtimeEnd
	
	mov di, (80 * 18 + 33 + 7) * 2
	push word [Score]
	call printnum
	
	mov di, (80 * 18 + 51 + 16) * 2
	push word [Mistakes]
	call printnum
	popa
ret

moveSelect:
	push cx
	push ax

	cmp al, 'w'
    jz moveUp
    cmp al, 'a'
    jz moveLeft
    cmp al, 's'
    jz moveDown
    ; cmp al, 'd'
    ; jz moveRight  
	; move right:
		mov cx, 25  ; check all rows at col 65 (last col)
		mov ax, (80 * 0 + 65) * 2
		chkRightLine:
			cmp word [currSelection], ax 
			je exitMove
			add ax, 160
			loop chkRightLine
		call clearCurrSelection
		add word [currSelection], 8*2 ; move 8 cols right
		call setCurrSelection
		jmp exitMove
	moveUp:
		cmp word [currentPage], 0xb900
		jne Up2
		cmp word [currSelection], (80 * 3 + 0) * 2  ; if currSelection is on 1st row of cells, then dont move
		jle exitMove
		
		call clearCurrSelection
		sub word [currSelection], (80 * 4 + 0) * 2 ; move 4 rows above
		call setCurrSelection
		jmp exitMove
		Up2:
		cmp word [currSelection], (80 * 1 + 0) * 2  ; if currSelection is on 1st row of cells, then change screen to pg1
		jle Up3
		
		call clearCurrSelection
		sub word [currSelection], (80 * 4 + 0) * 2 ; move 4 rows above
		call setCurrSelection
		jmp exitMove
		Up3: ; switch to pg1
		call clearCurrSelection
		add word [currSelection], (80 * 21 + 0) * 2 ; move 21 rows down
		mov word [currentPage], 0xb900
		call setCurrSelection
		mov ax, 0501h  ; show page 1
		int 10h
		jmp exitMove
	moveLeft:
		mov cx, 25  ; check all rows at col 65 (last col)
		mov ax, (80 * 0 + 1) * 2
		chkLeftLine:
			cmp word [currSelection], ax 
			je exitMove
			add ax, 160
			loop chkLeftLine
		call clearCurrSelection
		sub word [currSelection], 8*2 ; move 8 cols left
		call setCurrSelection	
		jmp exitMove
	moveDown:
		cmp word [currentPage], 0xb900
		je Down2
		cmp word [currSelection], (80 * 8 + 0) * 2  ; if currSelection is on 3rd row of cells, then dont move
		jge exitMove
		
		call clearCurrSelection
		add word [currSelection], (80 * 4 + 0) * 2 ; move 4 rows down
		call setCurrSelection
		jmp exitMove
		Down2:
		cmp word [currSelection], (80 * 21 + 0) * 2  ; if currSelection is on 6th row of cells, change screen to pg2
		jge Down3
		
		call clearCurrSelection
		add word [currSelection], (80 * 4 + 0) * 2 ; move 4 rows down
		call setCurrSelection
		jmp exitMove
		Down3: ; switch to pg2
		call clearCurrSelection
		sub word [currSelection], (80 * 21 + 0) * 2 ; move 23 rows above
		mov word [currentPage], 0xba00
		call setCurrSelection
		mov ax, 0502h  ; show page 1
		int 10h
	
	exitMove:
	pop ax
	pop cx
ret

toggleNotes:
	pusha
	cmp byte [Notes], 0
	je setN
	;resetN
	dec byte [Notes]
	; Remove Notes on pg 1
	push 0xb900
	pop es
    mov di, (80 * 5 + 74) * 2
	mov si, strSpace
	mov cx, 5
	mov ah, 70h
	call printStr
	; Remove Notes on pg 2
	push 0xba00
	pop es
    mov di, (80 * 5 + 74) * 2
	mov si, strSpace
	mov cx, 5
	mov ah, 70h
	call printStr
	
	jmp ret1
	
	setN:
	inc byte [Notes]
	; Write Notes on pg 1
	push 0xb900
	pop es
    mov di, (80 * 5 + 74) * 2
	mov si, strNotes
	mov cx, 5
	mov ah, 75h
	call printStr
	; Write Notes on pg 2
	push 0xba00
	pop es
    mov di, (80 * 5 + 74) * 2
	mov si, strNotes
	mov cx, 5
	mov ah, 75h
	call printStr
	
	ret1:
	popa
ret

removeNotes:
	push ax
	push di
	push cx
	push es
	
	mov di, [currSelection]  ; di is top left of selection box
	push word [currentPage]
	pop es
	; reset 7x3 box's bg
	mov cx, 3
	lp4:
	push cx
    mov cx, 7
	lp5:
		mov ax , [es:di]
		mov al, ' '
		stosw
		loop lp5
	add di, 160 - (7*2)  ; next row 
	pop cx
	loop lp4
    
	pop es
	pop cx
	pop di
	pop ax
ret

printScore:	push es
	push di
	
	push 0xba00
	pop es
	mov di, (80 * 18 + 33 + 7) * 2
	mov word [es:di+2], 0x7a20
	push word [Score]
	call printnum
	
	pop di
	pop es
ret

printMistakes:
	push es
	push di
	
	push 0xba00
	pop es	
	mov di, (80 * 18 + 51 + 16) * 2
	push word [Mistakes]
	call printnum

	pop di
	pop es
ret

input: ; al contains ascii of entered number 1-9
	pusha
	; Pre-Filled = Black(00), User-Filled = Green(7a), Notes = Blue(09), Wrong = Red(04)
	push word [currentPage]
	pop es
	mov di, [currSelection]  
	mov bx, [es:di+166]
	
	cmp bh, 0xb0  ; 166 offset is center of cell, if its black num or green num, then exit
	je blackting
	cmp bh, 0x70
	je blackting
	cmp bh, 0xba
	je ret2
	cmp bh, 0x7a
	je ret2
	jmp valCol

	blackting:
	cmp bl, '1'
	jae ret2
	
	valCol:
	cmp byte [Notes], 1
	je addNote
	
	; non Notes input, check if valid, then decide green or red
	add di, 166 ; point to mid num
	call compute_index_from_screen
	mov bx, [currentSudoku]
	add bx, cx
	cmp [bx], al
	jne InvalidMove	
	
	; validMove:
	call removeNotes
	call music
	mov ah, 7ah
	call pushUndo
	stosw
	
	add word [Score], 5
	; print score
	call printScore
	; update cardsRemaining
	xor bx, bx
	mov bl, al
	sub bl, 0x30  ; convert to integer
	dec byte [cardsRemaining + bx - 1] ; set cards remaining according to how they are removed
	call printCards
	call checkWinCondition
	
	jmp ret2
	InvalidMove:
	mov ah, 74h
	call pushUndo
	stosw
	
	inc word [Mistakes]
	call printMistakes
	cmp word [Mistakes], 4
	jne notLost
	; lose msg update in 0xbb00
	call updateExitSc
	push 0xbb00
	pop es
	mov di, (80 * 5 + 22) * 2
    mov ah, 0x7a
    mov si, strLose
	mov cx, 34
    call printStr
	
	popa
	pop ax
	jmp exitGame
	
	notLost:
	jmp ret2
	addNote:
	cmp al, '1'
	je addN1
	cmp al, '2'
	je addN2
	cmp al, '3'
	je addN3
	cmp al, '4'
	je addN4
	cmp al, '5'
	je addN5
	cmp al, '6'
	je addN6
	cmp al, '7'
	je addN7
	cmp al, '8'
	je addN8
	cmp al, '9'
	je addN9
	
	addN1:
	mov word [es:di+2], 0x7131
	jmp ret2
	addN2:
	mov word [es:di+6], 0x7132
	jmp ret2
	addN3:
	mov word [es:di+10], 0x7133
	jmp ret2
	addN4:
	mov word [es:di+162], 0x7134
	jmp ret2
	addN5:
	mov word [es:di+166], 0x7135
	jmp ret2
	addN6:
	mov word [es:di+170], 0x7136
	jmp ret2
	addN7:
	mov word [es:di+322], 0x7137
	jmp ret2
	addN8:
	mov word [es:di+326], 0x7138
	jmp ret2
	addN9:
	mov word [es:di+330], 0x7139
	jmp ret2
	
	ret2:
	popa
ret

; Assuming es, di, ax is set, and contain currpage, screen pos, number to mov
pushUndo: 
	push bx
	push ax
	push dx
		
	mov bx, [undoPtr]
	cmp bx, 6*150*2  ; check if undo stack is full
	je ret3
	; stores state of score, mistakes, number moved, currentPage, di of num, prevnum 
	mov dx, [Score]
	mov [undoStack + bx], dx
	add bx, 2
	mov dx, [Mistakes]
	mov [undoStack + bx], dx
	add bx, 2
	mov [undoStack + bx], ax  ; number moved
	add bx, 2
	mov ax, es
	mov [undoStack + bx], ax  ; currentpage
	add bx, 2
	mov [undoStack + bx], di  ; di 
	add bx, 2
	mov ax, [es:di]
	mov [undoStack + bx], ax  ; prevnum
	add bx, 2
	
	mov [undoPtr], bx  ; update mem as well

	ret3:
	pop dx
	pop ax
	pop bx
ret

undoOp:
	push bx
	push ax
	push dx
	
	mov bx, [undoPtr]
	cmp bx, 0  ; check if undo stack is empty
	je ret4
	; restores state of score, mistakes, number moved, currentPage, di of num, prevnum 
	sub bx, 2
	mov dx, [undoStack + bx]  ; restore prevnum
	sub bx, 2
	mov di, [undoStack + bx]  ; restore di
	sub bx, 2
	mov ax, [undoStack + bx]  ; restore es (currentPage)
	mov es, ax
	; restore prevnum
	mov ah, [es:di+3]
	and ah, 0xf0  ; remove fg of screen 
	and dh, 0x0f  ; remove bg of prevnum
	or dh, ah  ; combine bg of screen with fg of prevnum
	mov [es:di], dx
	; restore score, mistakes, cardsRemaining
	sub bx, 2
	mov ax, [undoStack + bx]  ; restore numbermoved
	sub bx, 2
	mov dx, [undoStack + bx] 
	sub bx, 2
	mov [Mistakes], dx
	mov dx, [undoStack + bx]
	mov [Score], dx
	call printScore
	call printMistakes
	
	mov [undoPtr], bx  ; update mem as well
	
	cmp ah, 0x7a
	jne ret4  ; if not valid move, then it means cardsRemaining werent incremented
	
	xor bx, bx
	mov bl, al
	sub bl, 0x30  ; convert to integer
	inc byte [cardsRemaining + bx - 1] 
	call printCards
	
	ret4:
	
	pop dx
	pop ax
	pop bx  
	; pop ax ; pop ip    ;; this part is too disrupting

	; mov ax, es
	; cmp ax, 0xb900
	; jne jpPg2
	; jmp showPg1
	; jpPg2:
	; jmp showPg2
ret

printCards:
	pusha
	
	push 0xba00
	pop es
	; Print cards on pg2
	mov di, (80 * 15 + 11) * 2
	mov cx, 1
	mov bx, 9
	mov dx, 14
	loop1:
		push word 14 ;top
		push bx ;left
		push word 17 ;bottom
		push dx ;right
		push word 79h ;attribute
		call printSingleRect
		mov al, cl
		add al, 30h
		mov ah, 70h
		mov word [es:di], ax
		mov si, cardsRemaining
		add si, cx
		mov al, [si-1]
		mov ah, 7bh
		mov word [es:di + 160], ax
		
		; next card pos
		add bx, 7
		add dx, 7
		add di, 14
		inc cx
		cmp cx, 10
		jne loop1
	popa
ret

start:
	call initScreens
		
	menu:
		mov ax, 0500h  ; show page 0 (StartScreen)
		int 10h
		call keystroke ; wait for key press
		cmp al, '3'
		je exitGame
		cmp al, '2'
		je helpPg
		cmp al, '1'
		je startGame
		jmp menu

	helpPg:
		mov ax, 0504h
		int 10h
		call keystroke

		mov ax, 0500h
		int 10h
		jmp menu
		
	startGame:
		mov ax, 0505h  ; show page 5 (difficulty)
		int 10h

		call keystroke
		cmp al, '1'
		je setEasy
		cmp al, '2'
		je setMedium
		cmp al, '3'
		je setHard
		cmp al, '4'
		je menu
		jmp startGame
	setEasy:
	mov cx, 1 ; removes 15 numbers from the grid
	jmp startGameForReal
	setMedium:
	mov cx, 25 ; removes 25 numbers from the grid
	jmp startGameForReal
	setHard:
	mov cx, 40 ; removes 40 numbers from the grid
	
	startGameForReal:
	call removeRandomNumbers
	call printCards
	call startTimer
	jmp showPg1
	
	Moves:  
    mov ah, 00h
    int 16h
	
    cmp ah, 0x48        ; up key    
    jz showPg1           
    cmp ah, 0x50        ; down key
    jz showPg2           
    cmp ah, 0x01        ; esc key 
    jz exitGame          
    cmp al, 'w'
    jz moveSelection  
    cmp al, 'a'
    jz moveSelection
    cmp al, 's'
    jz moveSelection
    cmp al, 'd'
    jz moveSelection
    cmp al, 'n'
    jz switchNotes
    cmp al, 'z'
    je doUndo
	cmp al, '9'
    jbe checkNum
    
    jmp Moves         	; Otherwise, keep waiting for a key
	
	; IF CONDITIONS FOR MOVES
	showPg1:
	call clearCurrSelection
	mov word [currentPage], 0xb900
	mov word [currSelection], (80 * 1 + 1) * 2  ; first cell topleft corner
	call setCurrSelection
	mov ax, 0501h  ; show page 1
	int 10h
	jmp Moves
	
	showPg2:
	call clearCurrSelection
	mov word [currentPage], 0xba00
	mov word [currSelection], (80 * 0 + 1) * 2  ; first cell topleft corner
	call setCurrSelection
	mov ax, 0502h  ; show page 2
	int 10h
	jmp Moves
	
	moveSelection:
	call moveSelect
	jmp Moves
	
	switchNotes:
	call toggleNotes
	jmp Moves
	
	checkNum:
	cmp al, '0'
	jbe Moves
	call input
	jmp Moves
	
	doUndo:
	call undoOp
	jmp Moves
	

	exitGame:
	call pauseTimer
	mov ax, 0503h  ; show page 3
	int 10h
	call keystroke ; wait for key press

; end program
call showCursor
mov ax, 0506h  ; show page 6, an empty page
int 10h
mov ax, 4c00h
int 21h
