
; C FFI routines.
; Someday we should either make a Real FFI or an inline assembler.
; Until then, this works.
; Except we don't actually use it, since we have no module system, etc...

extern printIntC, printCharC
global printInt, printChar
printInt:
	sub esp, 4
	mov [esp], ebp
	mov ebp, esp

	mov eax, [ebp+4]
	push eax
	call printIntC
	pop eax

	mov esp, ebp
	mov ebp, [esp]
	add esp, 4
	ret


printChar:
	sub esp, 4
	mov [esp], ebp
	mov ebp, esp

	mov eax, [ebp+4]
	push eax
	call printCharC
	pop eax

	mov esp, ebp
	mov ebp, [esp]
	add esp, 4
	ret
