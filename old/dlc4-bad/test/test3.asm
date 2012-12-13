
segment .text
extern printIntC
extern printCharC
extern printNLC
extern printFloatC
global foo_id
global foo_idf
global foo_testTrivial
global foo_testVars
global foo_testArgs
global foo_testArgsAndVars
global foo_testLogic
global foo_float2int
global foo_floatMath
global foo_int2float
global foo_loopTest
global foo_floatComp
global foo_notTest
global foo_ifTest
global foo_do__stuff
global foo_main
   ; Start function foo_id
foo_id:
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

   ; Start function foo_idf
foo_idf:
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

   ; Start function foo_testTrivial
foo_testTrivial:
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

   ; Start function foo_testVars
foo_testVars:
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

   ; Start function foo_testArgs
foo_testArgs:
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

   ; Start function foo_testArgsAndVars
foo_testArgsAndVars:
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

   ; Start function foo_testLogic
foo_testLogic:
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

   ; Start function foo_float2int
foo_float2int:
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

   ; Start function foo_floatMath
foo_floatMath:
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

   ; Start function foo_int2float
foo_int2float:
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

   ; Start function foo_loopTest
foo_loopTest:
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

   ; Start function foo_floatComp
foo_floatComp:
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

   ; Start function foo_notTest
foo_notTest:
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

   ; Start function foo_ifTest
foo_ifTest:
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

   ; Start function foo_do__stuff
foo_do__stuff:
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

   ; Start function foo_main
foo_main:
   sub esp, 0x4
   mov [esp], ebp
   mov ebp, esp
   sub esp, 0x0
   ; Func prolog done
   sub esp, 0x0
   call foo_testTrivial
   add esp, 0x0
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0x1E
   mov [esp], eax
   call foo_id
   add esp, 0x4
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x0
   call foo_testVars
   add esp, 0x0
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0xFFFFFFF6
   mov [esp], eax
   call foo_id
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
   call foo_testArgs
   add esp, 0x8
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0x32
   mov [esp], eax
   call foo_id
   add esp, 0x4
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0xA
   mov [esp], eax
   call foo_testArgsAndVars
   add esp, 0x4
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0x5
   mov [esp], eax
   call foo_id
   add esp, 0x4
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x0
   call foo_testLogic
   add esp, 0x0
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0x1
   mov [esp], eax
   call foo_id
   add esp, 0x4
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0x41280000
   mov [esp], eax
   call foo_float2int
   add esp, 0x4
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0xA
   mov [esp], eax
   call foo_id
   add esp, 0x4
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0x14
   mov [esp], eax
   call foo_int2float
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
   call foo_idf
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
   call foo_floatMath
   add esp, 0x8
   sub esp, 0x4
   mov [esp], eax
   call printFloatC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0xC1D80000
   mov [esp], eax
   call foo_idf
   add esp, 0x4
   sub esp, 0x4
   mov [esp], eax
   call printFloatC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0xA
   mov [esp], eax
   call foo_loopTest
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
   call foo_idf
   add esp, 0x4
   sub esp, 0x4
   mov [esp], eax
   call printFloatC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x0
   call foo_floatComp
   add esp, 0x0
   sub esp, 0x4
   mov [esp], eax
   call printFloatC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0x42E3D000
   mov [esp], eax
   call foo_idf
   add esp, 0x4
   sub esp, 0x4
   mov [esp], eax
   call printFloatC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x0
   call foo_notTest
   add esp, 0x0
   sub esp, 0x4
   mov [esp], eax
   call printIntC
   mov eax, [esp]
   add esp, 0x4
   sub esp, 0x4
   mov eax, 0xFFFFFFE5
   mov [esp], eax
   call foo_ifTest
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
