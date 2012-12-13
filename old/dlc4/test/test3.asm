
segment .text
extern printIntC
extern printCharC
extern printNLC
extern printFloatC
extern testlib_test
extern testlib_add
extern testlib_sum4
extern testlib_sub
global test3_id
global test3_idf
global test3_addStuff
global main
   ; Start function test3_id
test3_id:
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

   ; Start function test3_idf
test3_idf:
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

   ; Start function test3_addStuff
test3_addStuff:
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
   add eax, [esp+4]
   add esp, 0x8
   ; Func epilog
   mov esp, ebp
   mov ebp, [esp]
   add esp, 0x4
   ret
   ; End function

   ; Start function test3_testTrivial
test3_testTrivial:
   sub esp, 0x4
   mov [esp], ebp
   mov ebp, esp
   sub esp, 0x0
   ; Func prolog done
   sub esp, 0x8
   mov eax, 0xA
   mov [esp], eax
   mov eax, 0x14
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

   ; Start function test3_testVars
test3_testVars:
   sub esp, 0x4
   mov [esp], ebp
   mov ebp, esp
   sub esp, 0x8
   ; Func prolog done
   mov eax, 0xF
   mov [ebp-4], eax
   mov eax, 0x19
   mov [ebp-8], eax
   sub esp, 0x8
   mov eax, [ebp-4]
   mov [esp], eax
   mov eax, [ebp-8]
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

   ; Start function test3_testArgs
test3_testArgs:
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
   imul eax, [esp+4]
   add esp, 0x8
   ; Func epilog
   mov esp, ebp
   mov ebp, [esp]
   add esp, 0x4
   ret
   ; End function

   ; Start function test3_testArgsAndVars
test3_testArgsAndVars:
   sub esp, 0x4
   mov [esp], ebp
   mov ebp, esp
   sub esp, 0x4
   ; Func prolog done
   mov eax, 0x2
   mov [ebp-4], eax
   sub esp, 0x8
   mov eax, [ebp+8]
   mov [esp], eax
   mov eax, [ebp-4]
   mov [esp+4], eax
   mov eax, [esp]
   mov edx, 0x0
   idiv dword [esp+4]
   add esp, 0x8
   ; Func epilog
   mov esp, ebp
   mov ebp, [esp]
   add esp, 0x4
   ret
   ; End function

   ; Start function test3_testLogic
test3_testLogic:
   sub esp, 0x4
   mov [esp], ebp
   mov ebp, esp
   sub esp, 0x0
   ; Func prolog done
   sub esp, 0x8
   sub esp, 0x8
   mov eax, 0xA
   mov [esp], eax
   mov eax, 0xA
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
   sub esp, 0x8
   mov eax, 0xA
   mov [esp], eax
   mov eax, 0xA
   mov [esp+4], eax
   mov eax, [esp]
   cmp eax, [esp+4]
   je NEAR .cmptrueSYM3
   mov eax, 0x0
   jmp NEAR .cmpendSYM4
.cmptrueSYM3:
   mov eax, 0x1
.cmpendSYM4:
   add esp, 0x8
   mov [esp+4], eax
   mov eax, [esp]
   and eax, [esp+4]
   add esp, 0x8
   ; Func epilog
   mov esp, ebp
   mov ebp, [esp]
   add esp, 0x4
   ret
   ; End function

   ; Start function test3_float2int
test3_float2int:
   sub esp, 0x4
   mov [esp], ebp
   mov ebp, esp
   sub esp, 0x0
   ; Func prolog done
   mov eax, [ebp+8]
   sub esp, 0x4
   mov [esp], eax
   fld dword [esp]
   fistp dword [esp]
   mov eax, [esp]
   add esp, 0x4
   ; Func epilog
   mov esp, ebp
   mov ebp, [esp]
   add esp, 0x4
   ret
   ; End function

   ; Start function test3_floatMath
test3_floatMath:
   sub esp, 0x4
   mov [esp], ebp
   mov ebp, esp
   sub esp, 0x0
   ; Func prolog done
   sub esp, 0x8
   mov eax, 0x40400000
   mov [esp], eax
   sub esp, 0x8
   mov eax, [ebp+8]
   mov [esp], eax
   sub esp, 0x8
   mov eax, [ebp+12]
   mov [esp], eax
   sub esp, 0x8
   mov eax, 0x3F800000
   mov [esp], eax
   mov eax, 0x40000000
   mov [esp+4], eax
   mov eax, [esp]
   mov [esp-4], eax
   fld dword [esp-4]
   fdiv dword [esp+4]
   fstp dword [esp-4]
   mov eax, [esp-4]
   add esp, 0x8
   mov [esp+4], eax
   mov eax, [esp]
   mov [esp-4], eax
   fld dword [esp-4]
   fmul dword [esp+4]
   fstp dword [esp-4]
   mov eax, [esp-4]
   add esp, 0x8
   mov [esp+4], eax
   mov eax, [esp]
   mov [esp-4], eax
   fld dword [esp-4]
   fadd dword [esp+4]
   fstp dword [esp-4]
   mov eax, [esp-4]
   add esp, 0x8
   mov [esp+4], eax
   mov eax, [esp]
   mov [esp-4], eax
   fld dword [esp-4]
   fsub dword [esp+4]
   fstp dword [esp-4]
   mov eax, [esp-4]
   add esp, 0x8
   ; Func epilog
   mov esp, ebp
   mov ebp, [esp]
   add esp, 0x4
   ret
   ; End function

   ; Start function test3_int2float
test3_int2float:
   sub esp, 0x4
   mov [esp], ebp
   mov ebp, esp
   sub esp, 0x0
   ; Func prolog done
   mov eax, [ebp+8]
   sub esp, 0x4
   mov [esp], eax
   fild dword [esp]
   fstp dword [esp]
   mov eax, [esp]
   add esp, 0x4
   ; Func epilog
   mov esp, ebp
   mov ebp, [esp]
   add esp, 0x4
   ret
   ; End function

   ; Start function test3_loopTest
test3_loopTest:
   sub esp, 0x4
   mov [esp], ebp
   mov ebp, esp
   sub esp, 0x8
   ; Func prolog done
   mov eax, [ebp+8]
   mov [ebp-4], eax
   mov eax, 0x0
   sub esp, 0x4
   mov [esp], eax
   fild dword [esp]
   fstp dword [esp]
   mov eax, [esp]
   add esp, 0x4
   mov [ebp-8], eax
.whilestartSYM5:
   sub esp, 0x8
   mov eax, [ebp-4]
   mov [esp], eax
   sub esp, 0x8
   mov eax, [ebp+8]
   mov [esp], eax
   mov eax, 0x2
   mov [esp+4], eax
   mov eax, [esp]
   imul eax, [esp+4]
   add esp, 0x8
   mov [esp+4], eax
   mov eax, [esp]
   cmp eax, [esp+4]
   jl NEAR .cmptrueSYM7
   mov eax, 0x0
   jmp NEAR .cmpendSYM8
.cmptrueSYM7:
   mov eax, 0x1
.cmpendSYM8:
   add esp, 0x8
   cmp eax, 0x0
   je NEAR .whileendSYM6
   sub esp, 0x8
   mov eax, [ebp-4]
   mov [esp], eax
   mov eax, 0x1
   mov [esp+4], eax
   mov eax, [esp]
   add eax, [esp+4]
   add esp, 0x8
   mov [ebp-4], eax
   sub esp, 0x8
   mov eax, [ebp-8]
   mov [esp], eax
   mov eax, 0x1
   sub esp, 0x4
   mov [esp], eax
   fild dword [esp]
   fstp dword [esp]
   mov eax, [esp]
   add esp, 0x4
   mov [esp+4], eax
   mov eax, [esp]
   mov [esp-4], eax
   fld dword [esp-4]
   fadd dword [esp+4]
   fstp dword [esp-4]
   mov eax, [esp-4]
   add esp, 0x8
   mov [ebp-8], eax
   jmp NEAR .whilestartSYM5
.whileendSYM6:
   mov eax, [ebp-8]
   ; Func epilog
   mov esp, ebp
   mov ebp, [esp]
   add esp, 0x4
   ret
   ; End function

   ; Start function test3_floatComp
test3_floatComp:
   sub esp, 0x4
   mov [esp], ebp
   mov ebp, esp
   sub esp, 0x8
   ; Func prolog done
   sub esp, 0x8
   mov eax, 0x41200000
   mov [esp], eax
   mov eax, 0x3FC00000
   mov [esp+4], eax
   mov eax, [esp]
   mov [esp-4], eax
   fld dword [esp-4]
   fmul dword [esp+4]
   fstp dword [esp-4]
   mov eax, [esp-4]
   add esp, 0x8
   mov [ebp-8], eax
   mov eax, 0x0
   mov [ebp-4], eax
.whilestartSYM9:
   sub esp, 0x8
   mov eax, [ebp-8]
   mov [esp], eax
   mov eax, 0x42C80000
   mov [esp+4], eax
   mov eax, [esp]
   fld dword [esp+4]
   fld dword [esp]
   fcomip st0, st1
   jb .cmptrueSYM11
   mov eax, 0x0
   jmp NEAR .cmpendSYM12
.cmptrueSYM11:
   mov eax, 0x1
.cmpendSYM12:
   add esp, 0x8
   cmp eax, 0x0
   je NEAR .whileendSYM10
   sub esp, 0x8
   mov eax, [ebp-8]
   mov [esp], eax
   mov eax, 0x3FC00000
   mov [esp+4], eax
   mov eax, [esp]
   mov [esp-4], eax
   fld dword [esp-4]
   fmul dword [esp+4]
   fstp dword [esp-4]
   mov eax, [esp-4]
   add esp, 0x8
   mov [ebp-8], eax
   sub esp, 0x8
   mov eax, [ebp-4]
   mov [esp], eax
   mov eax, 0x1
   mov [esp+4], eax
   mov eax, [esp]
   add eax, [esp+4]
   add esp, 0x8
   mov [ebp-4], eax
   jmp NEAR .whilestartSYM9
.whileendSYM10:
   mov eax, [ebp-8]
   ; Func epilog
   mov esp, ebp
   mov ebp, [esp]
   add esp, 0x4
   ret
   ; End function

   ; Start function test3_notTest
test3_notTest:
   sub esp, 0x4
   mov [esp], ebp
   mov ebp, esp
   sub esp, 0x0
   ; Func prolog done
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

   ; Start function test3_ifTest
test3_ifTest:
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
   jg NEAR .cmptrueSYM16
   mov eax, 0x0
   jmp NEAR .cmpendSYM17
.cmptrueSYM16:
   mov eax, 0x1
.cmpendSYM17:
   add esp, 0x8
   cmp eax, 0x0
   jne NEAR .ifblockSYM13
.elseblockSYM14:
   ; Starting if block
   sub esp, 0x8
   mov eax, [ebp+8]
   mov [esp], eax
   mov eax, 0x5
   mov [esp+4], eax
   mov eax, [esp]
   cmp eax, [esp+4]
   jg NEAR .cmptrueSYM21
   mov eax, 0x0
   jmp NEAR .cmpendSYM22
.cmptrueSYM21:
   mov eax, 0x1
.cmpendSYM22:
   add esp, 0x8
   cmp eax, 0x0
   jne NEAR .ifblockSYM18
.elseblockSYM19:
   ; Starting if block
   sub esp, 0x8
   mov eax, [ebp+8]
   mov [esp], eax
   mov eax, 0x0
   mov [esp+4], eax
   mov eax, [esp]
   cmp eax, [esp+4]
   jg NEAR .cmptrueSYM26
   mov eax, 0x0
   jmp NEAR .cmpendSYM27
.cmptrueSYM26:
   mov eax, 0x1
.cmpendSYM27:
   add esp, 0x8
   cmp eax, 0x0
   jne NEAR .ifblockSYM23
.elseblockSYM24:
   sub esp, 0x8
   mov eax, 0x0
   mov [esp], eax
   mov eax, [ebp+8]
   mov [esp+4], eax
   mov eax, [esp]
   sub eax, [esp+4]
   add esp, 0x8
   jmp NEAR .ifendSYM25
.ifblockSYM23:
   mov eax, 0x1
.ifendSYM25:
   jmp NEAR .ifendSYM20
.ifblockSYM18:
   mov eax, 0x5
.ifendSYM20:
   jmp NEAR .ifendSYM15
.ifblockSYM13:
   mov eax, 0xA
.ifendSYM15:
   ; Func epilog
   mov esp, ebp
   mov ebp, [esp]
   add esp, 0x4
   ret
   ; End function

   ; Start function test3_do__stuff
test3_do__stuff:
   sub esp, 0x4
   mov [esp], ebp
   mov ebp, esp
   sub esp, 0x0
   ; Func prolog done
   mov eax, 0xA
   ; Func epilog
   mov esp, ebp
   mov ebp, [esp]
   add esp, 0x4
   ret
   ; End function

   ; Start function test3_main
test3_main:
   sub esp, 0x4
   mov [esp], ebp
   mov ebp, esp
   sub esp, 0x0
   ; Func prolog done
   sub esp, 0x0
   call test3_testTrivial
   add esp, 0x0
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0x1E
   mov [esp], eax
   call test3_id
   add esp, 0x4
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x0
   call test3_testVars
   add esp, 0x0
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0xFFFFFFF6
   mov [esp], eax
   call test3_id
   add esp, 0x4
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x8
   mov eax, 0x5
   mov [esp], eax
   mov eax, 0xA
   mov [esp+4], eax
   call test3_testArgs
   add esp, 0x8
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0x32
   mov [esp], eax
   call test3_id
   add esp, 0x4
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0xA
   mov [esp], eax
   call test3_testArgsAndVars
   add esp, 0x4
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0x5
   mov [esp], eax
   call test3_id
   add esp, 0x4
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x0
   call test3_testLogic
   add esp, 0x0
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0x1
   mov [esp], eax
   call test3_id
   add esp, 0x4
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0x41280000
   mov [esp], eax
   call test3_float2int
   add esp, 0x4
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0xA
   mov [esp], eax
   call test3_id
   add esp, 0x4
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0x14
   mov [esp], eax
   call test3_int2float
   add esp, 0x4
   sub esp, 0x4
   mov [esp], eax
   call printFloatC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0x14
   sub esp, 0x4
   mov [esp], eax
   fild dword [esp]
   fstp dword [esp]
   mov eax, [esp]
   add esp, 0x4
   mov [esp], eax
   call test3_idf
   add esp, 0x4
   sub esp, 0x4
   mov [esp], eax
   call printFloatC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x8
   mov eax, 0x5
   sub esp, 0x4
   mov [esp], eax
   fild dword [esp]
   fstp dword [esp]
   mov eax, [esp]
   add esp, 0x4
   mov [esp], eax
   mov eax, 0x32
   sub esp, 0x4
   mov [esp], eax
   fild dword [esp]
   fstp dword [esp]
   mov eax, [esp]
   add esp, 0x4
   mov [esp+4], eax
   call test3_floatMath
   add esp, 0x8
   sub esp, 0x4
   mov [esp], eax
   call printFloatC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0xC1D80000
   mov [esp], eax
   call test3_idf
   add esp, 0x4
   sub esp, 0x4
   mov [esp], eax
   call printFloatC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0xA
   mov [esp], eax
   call test3_loopTest
   add esp, 0x4
   sub esp, 0x4
   mov [esp], eax
   call printFloatC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0xA
   sub esp, 0x4
   mov [esp], eax
   fild dword [esp]
   fstp dword [esp]
   mov eax, [esp]
   add esp, 0x4
   mov [esp], eax
   call test3_idf
   add esp, 0x4
   sub esp, 0x4
   mov [esp], eax
   call printFloatC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x0
   call test3_floatComp
   add esp, 0x0
   sub esp, 0x4
   mov [esp], eax
   call printFloatC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0x42E3D000
   mov [esp], eax
   call test3_idf
   add esp, 0x4
   sub esp, 0x4
   mov [esp], eax
   call printFloatC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x0
   call test3_notTest
   add esp, 0x0
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0x0
   mov [esp], eax
   call test3_id
   add esp, 0x4
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0x1B
   mov [esp], eax
   call test3_ifTest
   add esp, 0x4
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0xA
   mov [esp], eax
   call test3_id
   add esp, 0x4
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x8
   mov eax, 0xA
   mov [esp], eax
   mov eax, 0x5
   mov [esp+4], eax
   call testlib_add
   add esp, 0x8
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0xF
   mov [esp], eax
   call test3_id
   add esp, 0x4
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x8
   mov eax, 0xA
   mov [esp], eax
   mov eax, 0x5
   mov [esp+4], eax
   call testlib_sub
   add esp, 0x8
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0x5
   mov [esp], eax
   call test3_id
   add esp, 0x4
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0x1
   mov [esp], eax
   call testlib_test
   add esp, 0x4
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0x0
   mov [esp], eax
   call test3_id
   add esp, 0x4
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x10
   mov eax, 0x1
   mov [esp], eax
   mov eax, 0x2
   mov [esp+4], eax
   mov eax, 0x3
   mov [esp+8], eax
   mov eax, 0x4
   mov [esp+12], eax
   call testlib_sum4
   add esp, 0x10
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0xA
   mov [esp], eax
   call test3_id
   add esp, 0x4
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

   ; Start function main
main:
   sub esp, 0x4
   mov [esp], ebp
   mov ebp, esp
   sub esp, 0x0
   ; Func prolog done
   sub esp, 0x0
   call test3_main
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
