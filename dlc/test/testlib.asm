
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
   mov dword [esp], ebp
   mov dword ebp, esp
   sub esp, 0x0
   ; Func prolog done
   sub esp, 0x8
   mov dword eax, [ebp+8]
   mov dword [esp], eax
   mov dword eax, [ebp+12]
   mov dword [esp+4], eax
   call test3_addStuff
   add esp, 0x8
   sub esp, 0x4
   mov dword [esp], eax
   call printIntC
   mov dword eax, [esp]
   add esp, 0x4
   ; Func epilog
   mov dword esp, ebp
   mov dword ebp, [esp]
   add esp, 0x4
   ret
   ; End function

   ; Start function testlib_sub
testlib_sub:
   sub esp, 0x4
   mov dword [esp], ebp
   mov dword ebp, esp
   sub esp, 0x0
   ; Func prolog done
   sub esp, 0x8
   mov dword eax, [ebp+8]
   mov dword [esp], eax
   mov dword eax, [ebp+12]
   mov dword [esp+4], eax
   mov dword eax, [esp]
   sub eax, [esp+4]
   add esp, 0x8
   ; Func epilog
   mov dword esp, ebp
   mov dword ebp, [esp]
   add esp, 0x4
   ret
   ; End function

   ; Start function testlib_test
testlib_test:
   sub esp, 0x4
   mov dword [esp], ebp
   mov dword ebp, esp
   sub esp, 0x0
   ; Func prolog done
   sub esp, 0x4
   sub esp, 0x8
   mov dword eax, [ebp+8]
   mov dword [esp], eax
   mov dword eax, 0x0
   mov dword [esp+4], eax
   mov dword eax, [esp]
   cmp eax, [esp+4]
   je NEAR .cmptrueSYM1
   mov dword eax, 0x0
   jmp NEAR .cmpendSYM2
.cmptrueSYM1:
   mov dword eax, 0x1
.cmpendSYM2:
   add esp, 0x8
   mov dword [esp], eax
   mov dword eax, [esp]
   add esp, 0x4
   ; Func epilog
   mov dword esp, ebp
   mov dword ebp, [esp]
   add esp, 0x4
   ret
   ; End function

   ; Start function testlib_sum4
testlib_sum4:
   sub esp, 0x4
   mov dword [esp], ebp
   mov dword ebp, esp
   sub esp, 0x0
   ; Func prolog done
   sub esp, 0x10
   mov dword eax, [ebp+8]
   mov dword [esp], eax
   mov dword eax, [ebp+16]
   mov dword [esp+4], eax
   mov dword eax, [ebp+20]
   mov dword [esp+8], eax
   mov dword eax, [ebp+12]
   mov dword [esp+12], eax
   mov dword eax, [esp]
   add eax, [esp+4]
   add eax, [esp+8]
   add eax, [esp+12]
   add esp, 0x10
   ; Func epilog
   mov dword esp, ebp
   mov dword ebp, [esp]
   add esp, 0x4
   ret
   ; End function
