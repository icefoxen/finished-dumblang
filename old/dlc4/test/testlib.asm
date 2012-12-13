
segment .text
extern printIntC
extern printCharC
extern printNLC
extern printFloatC
extern test3_addStuff
extern test3_idf
extern test3_id
global testlib_add
global testlib_sub
global testlib_test
global testlib_sum4
   ; Start function testlib_add
testlib_add:
   sub esp, 0x4
   mov [esp], ebp
   mov ebp, esp
   sub esp, 0x0
   ; Func prolog done
   sub esp, 0x8
   mov eax, [ebp+8]
   mov [esp], eax
   mov eax, [ebp+12]
   mov [esp+4], eax
   call test3_addStuff
   add esp, 0x8
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

   ; Start function testlib_sub
testlib_sub:
   sub esp, 0x4
   mov [esp], ebp
   mov ebp, esp
   sub esp, 0x0
   ; Func prolog done
   sub esp, 0x8
   mov eax, [ebp+8]
   mov [esp], eax
   mov eax, [ebp+12]
   mov [esp+4], eax
   mov eax, [esp]
   sub eax, [esp+4]
   add esp, 0x8
   ; Func epilog
   mov esp, ebp
   mov ebp, [esp]
   add esp, 0x4
   ret
   ; End function

   ; Start function testlib_test
testlib_test:
   sub esp, 0x4
   mov [esp], ebp
   mov ebp, esp
   sub esp, 0x0
   ; Func prolog done
   sub esp, 0x4
   sub esp, 0x8
   mov eax, [ebp+8]
   mov [esp], eax
   mov eax, 0x0
   mov [esp+4], eax
   mov eax, [esp]
   cmp eax, [esp+4]
   je NEAR .cmptrueSYM1
   mov eax, 0x0
   jmp NEAR .cmpendSYM2
.cmptrueSYM1:
   mov eax, 0x1
.cmpendSYM2:
   add esp, 0x8
   mov [esp], eax
   mov eax, [esp]
   add esp, 0x4
   ; Func epilog
   mov esp, ebp
   mov ebp, [esp]
   add esp, 0x4
   ret
   ; End function

   ; Start function testlib_sum4
testlib_sum4:
   sub esp, 0x4
   mov [esp], ebp
   mov ebp, esp
   sub esp, 0x0
   ; Func prolog done
   sub esp, 0x10
   mov eax, [ebp+8]
   mov [esp], eax
   mov eax, [ebp+16]
   mov [esp+4], eax
   mov eax, [ebp+20]
   mov [esp+8], eax
   mov eax, [ebp+12]
   mov [esp+12], eax
   mov eax, [esp]
   add eax, [esp+4]
   add eax, [esp+8]
   add eax, [esp+12]
   add esp, 0x10
   ; Func epilog
   mov esp, ebp
   mov ebp, [esp]
   add esp, 0x4
   ret
   ; End function
