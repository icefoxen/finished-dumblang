(* Outputs assembly for x86
   Duh.
*)

open Codegen
open Printf

let sp = sprintf;;

let rec location2str l =
  match l with 
      Mem( i ) -> sp "[%d]" i
    | Reg( i ) ->
	(match i with
	     0 -> "eax"
	   | 1 -> "ebx"
	   | 2 -> "ecx"
	   | 3 -> "edx"
	   | 4 -> "esi"
	   | 5 -> "edi"
	   | _ -> ErrorReport.errorAndDie "Five is right out!")
    | Literal( i ) -> sp "0x%lX" i
    | RegOffset( loc, i ) -> 
	(sp "[%s" (location2str loc)) ^
	(if i > 0 then (sp "+%d]" i) 
	 else if i = 0 then (sp "]")
	 else (sp "%d]" i))
    | RegOffsetLoc( loc1, loc2 ) -> 
	(sp "[%s+%s]" (location2str loc1) (location2str loc2))
    | StackPointer -> "esp"
    | BasePointer -> "ebp"


and instr2str i =
  match i with
      MOVE( l1, l2 ) -> 
	(sp "   mov %s, %s" (location2str l1) (location2str l2))

    | ADD( l1, l2 ) -> (sp "   add %s, %s" (location2str l1) (location2str l2))
    | SUB( l1, l2 ) -> (sp "   sub %s, %s" (location2str l1) (location2str l2))
    | MUL( l1, l2 ) -> 
	(sp "   imul %s, %s" (location2str l1) (location2str l2))

    | DIV( l1 ) -> 
	(sp "   idiv dword %s" (location2str l1))

    (* Not quite right...  Register weirdness. 
       It's taken care of by the code generator though.
    *)
    | MOD( l1, l2 ) -> (sp "   div %s, %s" (location2str l1) (location2str l2))
    | INC( l1 ) -> (sp "   inc %s" (location2str l1))
    | DEC( l1 ) -> (sp "   dec %s" (location2str l1))

    | SHL( l1, l2 ) -> (sp "   shl %s, %s" (location2str l1) (location2str l2))
    | SHR( l1, l2 ) -> (sp "   shr %s, %s" (location2str l1) (location2str l2))
    | AND( l1, l2 ) -> (sp "   and %s, %s" (location2str l1) (location2str l2))
    | OR( l1, l2 ) -> (sp "   or %s, %s" (location2str l1) (location2str l2))
    | NOT( l1 ) -> (sp "   not %s" (location2str l1))
    | XOR( l1, l2 ) -> (sp "   xor %s, %s" (location2str l1) (location2str l2))
    | COMP( l1, l2 ) -> 
	(sp "   cmp %s, %s" (location2str l1) (location2str l2))
    | NEG( l1 ) -> (sp "   neg %s" (location2str l1))

    (* All jumps should be near, mainly so they aren't short... *)
    | JMP( s ) -> (sp "   jmp NEAR %s" s)
    | JL( s ) -> (sp "   jl NEAR %s" s)
    | JG( s ) -> (sp "   jg NEAR %s" s)
    | JLE( s ) -> (sp "   jle NEAR %s" s)
    | JGE( s ) -> (sp "   jge NEAR %s" s)
    | JE( s ) -> (sp "   je NEAR %s" s)
    | JNE( s ) -> (sp "   jne NEAR %s" s)
    | LABEL( s ) -> (sp "%s:" s)

    (* Function *)
    | CALL( s ) -> (sp "   call %s" s)
    | RET -> "   ret"

    (* Floating point *)
    | FLOAD( loc ) -> ""
    | FSTORE( loc ) -> ""
    | FLOADINT( loc ) -> ""
    | FSTOREINT( loc ) -> ""
    | FADD( loc ) -> ""
    | FSUB( loc ) -> ""
    | FMUL( loc ) -> ""
    | FDIV( loc ) -> ""
    | FMOD( loc ) -> ""

    | FCOMP ( loc ) -> ""



    (* Hey, could be handy *)
    | COMMENT( s ) -> (sp "   ; %s" s)



;;


let module2str m =
  let retstr = ref "" in
  let append x = retstr := !retstr ^ "\n" ^ x
  in
    Queue.iter (fun x -> append ("extern " ^ x)) m.imports;
    Queue.iter (fun x -> append ("global " ^ x)) m.exports;
    Queue.iter (fun x -> append (instr2str x)) m.codeSeg;
    !retstr
;;

let outputModule m stream =
  let str = module2str m in
    output_string stream str
;;

let writeModule m file =
  let f = open_out file in
    outputModule m f;
    close_out f;
    print_endline ("Written to " ^ file);
;;
