
extern printIntC
extern printCharC
extern printNLC
global returnStuff
global isGreaterThan5
global isGreaterThan10
global addNums
global max
global recFact
global iterFact
global fib
global silliness
global main
   ; Start functionreturnStuff
returnStuff:
   sub esp, 0x4
   mov [esp], ebp
   mov ebp, esp
   sub esp, 0x0
   ; Func prolog done
   mov eax, [ebp+8]
   ; Func epilog
   mov esp, ebp
   mov ebp, [esp]
   add esp, 0x4
   ret
   ; End function

   ; Start functionisGreaterThan5
isGreaterThan5:
   sub esp, 0x4
   mov [esp], ebp
   mov ebp, esp
   sub esp, 0x0
   ; Func prolog done
   sub esp, 0x8
   mov eax, [ebp+8]
   mov [esp], eax
   mov eax, 0x5
   mov [esp+4], eax
   mov eax, [esp]
   cmp eax, [esp+4]
   jg NEAR .cmptrueSYM1
   mov eax, 0x0
   jmp NEAR .cmpendSYM2
.cmptrueSYM1:
   mov eax, 0x1
.cmpendSYM2:
   add esp, 0x8
   ; Func epilog
   mov esp, ebp
   mov ebp, [esp]
   add esp, 0x4
   ret
   ; End function

   ; Start functionisGreaterThan10
isGreaterThan10:
   sub esp, 0x4
   mov [esp], ebp
   mov ebp, esp
   sub esp, 0x0
   ; Func prolog done
   ; Starting if block
   sub esp, 0x8
   mov eax, [ebp+8]
   mov [esp], eax
   mov eax, 0xA
   mov [esp+4], eax
   mov eax, [esp]
   cmp eax, [esp+4]
   jg NEAR .cmptrueSYM6
   mov eax, 0x0
   jmp NEAR .cmpendSYM7
.cmptrueSYM6:
   mov eax, 0x1
.cmpendSYM7:
   add esp, 0x8
   cmp eax, 0x0
   jne NEAR .ifblockSYM3
.elseblockSYM4:
   mov eax, 0x1
   jmp NEAR ifendSYM5
.ifblockSYM3:
   mov eax, 0x1
ifendSYM5:
   ; Func epilog
   mov esp, ebp
   mov ebp, [esp]
   add esp, 0x4
   ret
   ; End function

   ; Start functionaddNums
addNums:
   sub esp, 0x4
   mov [esp], ebp
   mov ebp, esp
   sub esp, 0x0
   ; Func prolog done
   sub esp, 0x8
   mov eax, [ebp+8] ;arg 1
   mov [esp], eax
   sub esp, 0x8
   mov eax, [ebp+12] ;arg 2
   mov [esp-8], eax
   mov eax, [ebp+16] ;arg 3
   mov [esp-8], eax  ; XXX: Der er ikke regtig!
   call max
   add esp, 0x8
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   mov [esp+4], eax
   mov eax, [esp]
   add eax, [esp+4]
   add esp, 0x8
   ; Func epilog
   mov esp, ebp
   mov ebp, [esp]
   add esp, 0x4
   ret
   ; End function

   ; Start functionmax
max:
   sub esp, 0x4
   mov [esp], ebp
   mov ebp, esp
   sub esp, 0x0
   ; Func prolog done
   ; Starting if block
   sub esp, 0x8
   mov eax, [ebp+8]
   mov [esp], eax
   mov eax, [ebp+12]
   mov [esp+4], eax
   mov eax, [esp]
   cmp eax, [esp+4]
   jl NEAR .cmptrueSYM11
   mov eax, 0x0
   jmp NEAR .cmpendSYM12
.cmptrueSYM11:
   mov eax, 0x1
.cmpendSYM12:
   add esp, 0x8
   cmp eax, 0x0
   jne NEAR .ifblockSYM8
.elseblockSYM9:
   mov eax, [ebp+12]
   jmp NEAR ifendSYM10
.ifblockSYM8:
   mov eax, [ebp+12]
ifendSYM10:
   ; Func epilog
   mov esp, ebp
   mov ebp, [esp]
   add esp, 0x4
   ret
   ; End function

   ; Start functionrecFact
recFact:
   sub esp, 0x4
   mov [esp], ebp
   mov ebp, esp
   sub esp, 0x0
   ; Func prolog done
   ; Starting if block
   sub esp, 0x8
   mov eax, [ebp+8]
   mov [esp], eax
   mov eax, 0x2
   mov [esp+4], eax
   mov eax, [esp]
   cmp eax, [esp+4]
   jl NEAR .cmptrueSYM16
   mov eax, 0x0
   jmp NEAR .cmpendSYM17
.cmptrueSYM16:
   mov eax, 0x1
.cmpendSYM17:
   add esp, 0x8
   cmp eax, 0x0
   jne NEAR .ifblockSYM13
.elseblockSYM14:
   mov eax, 0x1
   jmp NEAR ifendSYM15
.ifblockSYM13:
   mov eax, 0x1
ifendSYM15:
   ; Func epilog
   mov esp, ebp
   mov ebp, [esp]
   add esp, 0x4
   ret
   ; End function

   ; Start functioniterFact
iterFact:
   sub esp, 0x4
   mov [esp], ebp
   mov ebp, esp
   sub esp, 0x8
   ; Func prolog done
   mov eax, 0x1
   mov [ebp-8], eax
   mov eax, [ebp+8]
   mov [ebp-4], eax
.whilestartSYM18:
   sub esp, 0x8
   mov eax, [ebp-4]
   mov [esp], eax
   mov eax, 0x1
   mov [esp+4], eax
   mov eax, [esp]
   cmp eax, [esp+4]
   jg NEAR .cmptrueSYM20
   mov eax, 0x0
   jmp NEAR .cmpendSYM21
.cmptrueSYM20:
   mov eax, 0x1
.cmpendSYM21:
   add esp, 0x8
   cmp eax, 0x0
   je NEAR .whileendSYM19
   sub esp, 0x8
   mov eax, [ebp-8]
   mov [esp], eax
   mov eax, [ebp-4]
   mov [esp+4], eax
   mov eax, [esp]
   imul eax, [esp+4]
   add esp, 0x8
   mov [ebp-8], eax
   sub esp, 0x8
   mov eax, [ebp-4]
   mov [esp], eax
   mov eax, 0x1
   mov [esp+4], eax
   mov eax, [esp]
   sub eax, [esp+4]
   add esp, 0x8
   mov [ebp-4], eax
   jmp NEAR .whilestartSYM18
.whileendSYM19:
   mov eax, [ebp-8]
   ; Func epilog
   mov esp, ebp
   mov ebp, [esp]
   add esp, 0x4
   ret
   ; End function

   ; Start functionfib
fib:
   sub esp, 0x4
   mov [esp], ebp
   mov ebp, esp
   sub esp, 0x0
   ; Func prolog done
   ; Starting if block
   sub esp, 0x8
   mov eax, [ebp+8]
   mov [esp], eax
   mov eax, 0x2
   mov [esp+4], eax
   mov eax, [esp]
   cmp eax, [esp+4]
   jl NEAR .cmptrueSYM25
   mov eax, 0x0
   jmp NEAR .cmpendSYM26
.cmptrueSYM25:
   mov eax, 0x1
.cmpendSYM26:
   add esp, 0x8
   cmp eax, 0x0
   jne NEAR .ifblockSYM22
.elseblockSYM23:
   mov eax, 0x1
   jmp NEAR ifendSYM24
.ifblockSYM22:
   mov eax, 0x1
ifendSYM24:
   ; Func epilog
   mov esp, ebp
   mov ebp, [esp]
   add esp, 0x4
   ret
   ; End function

   ; Start functionsilliness
silliness:
   sub esp, 0x4
   mov [esp], ebp
   mov ebp, esp
   sub esp, 0x0
   ; Func prolog done
   sub esp, 0x8
   mov eax, 0xA
   mov [esp], eax
   mov eax, 0xA
   mov [esp+4], eax
   mov eax, [esp]
   cmp eax, [esp+4]
   je NEAR .cmptrueSYM27
   mov eax, 0x0
   jmp NEAR .cmpendSYM28
.cmptrueSYM27:
   mov eax, 0x1
.cmpendSYM28:
   add esp, 0x8
   ; Func epilog
   mov esp, ebp
   mov ebp, [esp]
   add esp, 0x4
   ret
   ; End function

   ; Start functionmain
main:
   sub esp, 0x4
   mov [esp], ebp
   mov ebp, esp
   sub esp, 0x0
   ; Func prolog done
   sub esp, 0xC
   mov eax, 0x5
   mov [esp], eax
   mov eax, 0xA
   mov [esp+4], eax
   mov eax, 0xF
   mov [esp+8], eax
   call addNums
   add esp, 0xC
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x8
   mov eax, 0x12C
   mov [esp], eax
   mov eax, 0x190
   mov [esp+4], eax
   call max
   add esp, 0x8
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0x5B
   mov [esp], eax
   call returnStuff
   add esp, 0x4
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0x0
   mov [esp], eax
   call isGreaterThan5
   add esp, 0x4
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0x14
   mov [esp], eax
   call isGreaterThan5
   add esp, 0x4
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0x0
   mov [esp], eax
   call isGreaterThan10
   add esp, 0x4
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0x14
   mov [esp], eax
   call isGreaterThan10
   add esp, 0x4
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0xA
   mov [esp], eax
   call iterFact
   add esp, 0x4
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0xA
   mov [esp], eax
   call recFact
   add esp, 0x4
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x0
   call silliness
   add esp, 0x0
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   ; Func epilog
   mov esp, ebp
   mov ebp, [esp]
   add esp, 0x4
   ret
   ; End function

