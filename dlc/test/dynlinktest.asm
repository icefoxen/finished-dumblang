; So, we want a module to know the location of all functions in it, 
; and be able to link a new function dynamically.

; The problem is that upon loading, the actual addresses of the functions
; might change, and the addresses in the link table might not.
; In that case, we will probably need to find some base address, 
; and the link table will hopefully be an offset to that.

; Another problem is that if we write a function that overrides an existing
; function (which shouldn't happen probably, but still), everything points
; (statically) to the old function.  Since I really don't know if we want 
; run-time scoping of functions, it gets a bit weird.  If we do not want this,
; then we can dynamically update things by replacing the old function code
; with a jump to the new function, or a trampoline to change the address to
; the new function in the calling code.
; This could get funky.  It might be best to just not allow overriding
; functions, at least until I have a consistant model worked out...

extern printInt
global main

; Linker table
f1entry dd f1str, f1
f2entry dd f2str, f2
newfentry dd newfstr, newf

; DL-style arrays/strings
f1str dd 2
db 'f1'

f2str dd 2
db 'f2'

newfstr dd 4
db 'newf'


inputtedstr dd 4
db 'newf'

main:
	jmp f1

f1:
	call f2
	push eax
	call printInt
	pop eax

	; Compare given string, we find newf's address
	; Do I want to write a string-search in asm?  No.
	mov eax, [newfentry+4]
	; Check the address is the one we want
	cmp eax, newf
	jne .die

	call eax
	push eax
	call printInt
	pop eax

	ret
	
	.die
	mov eax, -1
	ret


f2:
	mov eax, 20
	ret

newf:
	mov eax, 10
	ret
