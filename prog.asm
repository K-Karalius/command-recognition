.model small
.stack 100h
.data

    regAX dw ?
	regBX dw ?
	regCX dw ?
	regDX dw ?
	regSP dw ?
	regBP dw ?
	regSI dw ?
	regDI dw ?
	sregES dw ?
	sregCS dw ?
	sregSS dw ?
	sregDS dw ?
    byte1 db ?
	reg_ax db "AX$"
	reg_bx db "BX$"
	reg_cx db "CX$"
	reg_dx db "DX$"
	reg_sp db "SP$"
	reg_bp db "BP$"
	reg_si db "SI$"
	reg_di db "DI$"
	sreg_es db "ES$"
	sreg_cs db "CS$"
	sreg_ss db "SS$"
	sreg_ds db "DS$"
	stack_word dw ?
	
	not_pop db "Not a pop command!", 13, 10, "$"
    new_line db 13,10,"$"
	adr_msg db "Address: $"
	pop_msg db "POP $"
	stack_msg db "Stack word: $"
		
.code
    start:
    mov ax, @data
    mov ds, ax
    mov ax, 0       ;i es isirasome 0, nes pertraukimu vektoriu lentele yra segmente
                    ;kurios pradzios adresas yra 0000
    mov es, ax
	                ;Issisaugome tikra PAP adresa, kad veliau galetume ja atsatyti
    push es:[4]
    push es:[6]    
    
	                ; Pertraukimu vektoriu lenteleje suformuojame PAP adrease
    mov ax, cs
    mov bx, offset int_1
    mov es:[4], bx
    mov es:[6], ax
	
    pushf           ;Issisaugome SF reiksme testavimo pradzioje   
    pushf           ;Issisaugome SF kad galetume ja isimti ir nustatyti TF
    pop ax          ;Isimame SF reiksme i TF 
    or ax, 0100h    ;Nustatome TF=1
    push ax         ;Idedame pakoreguota reiksme
    popf            ;Isimame pakoreguota reiksme i SF, nuo cia TF=1
    nop             ;Pirmas pertraukimas kyla ne pries sia komando, o po jos
                    ;todel tesiog viena komana nieko nedaro
	
	mov ax, 5
    push ax
    pop ax
	push bx
	pop bx
	push bp
	pop bp
	push ss
	pop ss
	 	
    popf
    
	pop es:[6]
	pop es:[4]
	
    mov ah, 4ch
    mov al, 0
    int 21h

proc int_1
    mov regAX, ax				
	mov regBX, bx
	mov regCX, cx
	mov regDX, dx
	mov regSP, sp
	mov regBP, bp
	mov regSI, si
	mov regDI, di
	mov sregES, es
	mov sregCS, cs
	mov sregSS, ss
	mov sregDS, ds

	pop si ;ip      
	pop di ;cs
		
    pop ax
    mov stack_word, ax
    push ax
	
	push di ;cs
	push si ;ip
	mov ax, cs:[si]  ;ax- operacijos kodas
	
	mov byte1, al   
    and al, 0F8h    ;and 1111 1000
	cmp al, 058h    ;check if 0101 1reg 
	je check_ax
	
	mov al, byte1   ;pop sreg
	and al, 0E7h    ;and 1110 0111
    cmp al, 007h    ; 000 sreg 111
	je check_es1
	
	netinka:
	mov dx, offset not_pop
	mov ah, 9
	int 21h
	jmp return_end
	
	check_ax:
	call print_address
	mov al, byte1
	and al, 7h; and 0000 0111 
	
	cmp al, 0h
	jne check_bx
	mov al, byte1
	call print_chr
	call print_space
	call print_pop
	mov dx, offset reg_ax
    mov cx, regAX
    call print_reg
	mov ax, stack_word
	call print_stack
	jmp return_end
	
	check_bx:
	cmp al, 3h
	jne check_cx
	mov al, byte1
	call print_chr
	call print_space
	call print_pop
	mov dx, offset reg_bx
    mov cx, regBX
    call print_reg
	mov ax, stack_word
	call print_stack
	jmp return_end
	
	check_cx:
	cmp al, 1h
	jne check_dx
	mov al, byte1
	call print_chr
	call print_space
	call print_pop
	mov dx, offset reg_cx
    mov cx, regCX
    call print_reg
	mov ax, stack_word
	call print_stack
	jmp return_end
	
	check_es1:
	jmp check_es2
	
	check_dx:
	cmp al, 2h
	jne check_sp
	mov al, byte1
	call print_chr
	call print_space
	call print_pop
	mov dx, offset reg_dx
    mov cx, regDX
    call print_reg
	mov ax, stack_word
	call print_stack
	jmp return_end
	
	
	check_sp:
	cmp al, 4h
	jne check_bp
	mov al, byte1
	call print_chr
	call print_space
	call print_pop
	mov dx, offset reg_sp
    mov cx, regSP
    call print_reg
	mov ax, stack_word
	call print_stack
	jmp return_end
	
	check_bp:
	cmp al, 5h
	jne check_si
	mov al, byte1
	call print_chr
	call print_space
	call print_pop
	mov dx, offset reg_bp
    mov cx, regBP
    call print_reg
	mov ax, stack_word
	call print_stack
	jmp return_end
	
	check_es2:
	jmp check_es
	
	check_si:
	cmp al, 6h
	jne check_di
	mov al, byte1
	call print_chr
	call print_space
	call print_pop
	mov dx, offset reg_si
    mov cx, regSI
    call print_reg
	mov ax, stack_word
	call print_stack
	jmp return_end
	
	check_di:
	cmp al, 7h
	jne return_end1
	mov al, byte1
	call print_chr
	call print_space
	call print_pop
	mov dx, offset reg_di
    mov cx, regDI
    call print_reg
	mov ax, stack_word
	call print_stack
	jmp return_end
	
	return_end1:
	jmp return_end
	
	check_es:
	call print_address
	mov al, byte1
	and al, 18h; and 000 11 000
	cmp al, 0h
	jne check_cs
	mov al, byte1
	call print_chr
	call print_space
	call print_pop
	mov dx, offset sreg_es
    mov cx, sregES
    call print_reg
	mov ax, stack_word
	call print_stack
	jmp return_end
	
	check_cs:
	cmp al, 8h
	jne check_ss
	mov al, byte1
	call print_chr
	call print_space
	call print_pop
	mov dx, offset sreg_cs
    mov cx, sregCS
    call print_reg
	mov ax, stack_word
	call print_stack
	jmp return_end
	
	check_ss:
	cmp al, 10h
	jne check_ds
	mov al, byte1
	call print_chr
	call print_space
	call print_pop
	mov dx, offset sreg_ss
    mov cx, sregSS
    call print_reg
	mov ax, stack_word
	call print_stack
	jmp return_end
	
	check_ds:
	cmp al, 18h 
	jne return_end
	mov al, byte1
	call print_chr
	call print_space
	call print_pop
	mov dx, offset sreg_ds
    mov cx, sregDS
    call print_reg
	mov ax, stack_word
	call print_stack
	jmp return_end
	
	return_end:
	mov ax, regAX
	mov bx, regBX
	mov cx, regCX
	mov dx, regDX
	mov sp, regSP
	mov bp, regBP
	mov si, regSI
	mov di, regDI
	mov es, sregES
	mov ss, sregSS
	mov ds, sregDS 
	
    IRET

int_1 endp

PROC print_address
	mov ah, 9
	mov dx, offset adr_msg
	int 21h
	mov ax, di ;cs
	call print_byte
	mov ah, 2
	mov dl, ":"
	int 21h
	mov ax, si ;ip
	call print_byte
	call print_space
	ret
ENDP print_address

PROC print_pop
    mov ah, 9
	mov dx, offset pop_msg
    int 21h
    ret
ENDP print_pop

PROC print_reg
    call printline
    call print_space
    call printline
    mov ah, 2
    mov dl, '='
    int 21h
    mov ax, cx
    call print_byte
    ret
ENDP print_reg   
 
PROC printline
    mov ah, 9
    int 21h
    ret
ENDP printline 
      
PROC print_byte
	push ax
	mov al, ah
	call print_chr
    pop ax
	call print_chr
    RET
ENDP print_byte

PROC print_chr
	push ax
	push ax
    mov cl, 4
	shr al, cl       ;move 4 bits to the end -> 1111 xxxx = 0000 1111
	call print_to_Hexa
	pop ax
	call print_to_Hexa
	pop ax
    RET
ENDP print_chr

PROC print_stack
	call print_space
    mov ah, 9
    mov dx, offset stack_msg
    int 21h
	call print_byte
	mov ah, 9
    mov dx, offset new_line
    int 21h
    ret
ENDP print_stack

PROC print_space
	push ax
	push dx
	mov ah, 2
	mov dl, " "
	int 21h
	pop dx
	pop ax
    ret  
ENDP print_space

PROC print_to_Hexa
	push ax
	;push dx
	and al, 0Fh             ; 0000 xxxx
	cmp al, 9
	jbe print_number        ;<= 9
	jmp print_letter        ;A-F
	print_letter: 
	sub al, 10              ;al = 0 - 5
	add al, 41h             ;41-46 ASCII = A-F
	mov dl, al
	mov ah, 2           
	int 21h
	jmp print_hex_end
	print_number:           ;0-9
	add al, 30h             ;30-39 ASCII = 0-9
	mov dl, al
	mov ah, 2
	int 21h
	jmp print_hex_end
	print_hex_end:
	;pop dx
	pop ax
    RET 
ENDP print_to_Hexa    


   
end start  