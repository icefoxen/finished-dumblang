
segment .text
extern printIntC
extern printCharC
extern printNLC
extern printFloatC
global notTest
global main
   ; Start function notTest
notTest:
   sub esp, 0x4
   mov [esp], ebp
   mov ebp, esp
   sub esp, 0x0
   ; Func prolog done
   mov eax, 0x1
   sub esp, 0x4
   mov eax, 0x1
   mov [esp], eax
   mov eax, [esp]
   add esp, 0x4
   ; Func epilog
   mov esp, ebp
   mov ebp, [esp]
   add esp, 0x4
   ret
   ; End function

   ; Start function main
main:
   sub esp, 0x4
   mov [esp], ebp
   mov ebp, esp
   sub esp, 0x0
   ; Func prolog done
   sub esp, 0x0
   call _
   add esp, 0x0
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   mov eax, 0x0
   ; Func epilog
   mov esp, ebp
   mov ebp, [esp]
   add esp, 0x4
   ret
   ; End function
