
open Symtbl
open Syntree
open Ir

type location =
    Mem of int
  | Reg of int
  | Literal of int32
  | RegOffset of location * int
  | RegOffsetLoc of location * location
  | StackPointer
  | BasePointer
;;

(* x86 specific silliness, since we don't do real register allocation *)
let eax = Reg( 0 )
and ebx = Reg( 1 )
and ecx = Reg( 2 )
and edx = Reg( 3 )
and esi = Reg( 4 )
and edi = Reg( 5 )
;;


(* These go dest, src just like nasm.  So sue me. *)
type opcode =
  (* Load/store *)
    MOVE of location * location

  (* Arithmatic *)
  | ADD of location * location
  | SUB of location * location
  | MUL of location * location
  | DIV of location
  | MOD of location * location
  | INC of location
  | DEC of location

  (* Logic *)
  | SHL of location * location
  | SHR of location * location
  | AND of location * location
  | OR of location * location
  | NOT of location
  | XOR of location * location
  | COMP of location * location
  | NEG of location

  (* Branch *)
  (* Oh fine, we're not gonna be jumping to anything but a label anyway *)
  | JMP of string
  | JL of string
  | JG of string
  | JLE of string
  | JGE of string
  | JE of string
  | JNE of string
  | LABEL of string

  (* Oh, my aching head. *)
  (* jb, jbe *)
  | FJL of string
  | FJLE of string
  (* ja, jae *)
  | FJG of string
  | FJGE of string
  (* normal je and jne... yeesh. *)
  | FJE of string
  | FJNE of string

  (* Function *)
  | CALL of string
  | RET

  (* Floating point --all floating point ops only take a 
     memory location, because they want to be weird.  At least div isn't
     a special case.
  *)
  | FLOAD of location
  | FSTORE of location
  | FLOADINT of location
  | FSTOREINT of location
  | FADD of location
  | FSUB of location
  | FMUL of location
  | FDIV of location
  | FMOD of location

  (* Need to figure out how the hell FP comparison works... oh, it's ugly 
     ...hmm, not so much, actually.  Thank gods for fcomi.
  *)
  | FCOMP (*of location*)


  (* Hey, could be handy *)
  | COMMENT of string
;;

type datadecl =
    DeclByte of string
  | DeclWord of string
  | DeclDword of string
  | DeclDD of string
;;

(* Used for naming if/loop labels. *)
let gensymidx = ref 0;;
let gensym () =
  incr gensymidx;
  Printf.sprintf "SYM%d" !gensymidx;
;;

let makeLabelName name =
  name ^ (gensym ())
;;

let makeLocalLabelName name =
  "." ^ name ^ (gensym ())
;;

(* We need to be able to print the binary representation of a float... *)
let float2binary f =
  ()
;;

(* Value for "No return type": 0xFEEDAAE0 
   For debugging.  It works if you kinda squint at it.
*)


(* So we need a structure to actually hold the metadata as well as the
   raw instructions.
   Metadata we need: File name, imported/exported symbols, data declerations.

   So:
   1) How do we know where the variables are (local/arg)?  The symtbl has to 
   tell us.
   2) How do we return values?  We stick them on the top of the stack; the
   caller has to allocate space for it, just below the args.
*)

type asmModule = {
  fileName: string;
  codeSeg: opcode Queue.t;
  dataSeg: datadecl Queue.t;
  imports: string Queue.t;
  exports: string Queue.t;
};;

let makeModule filename = {
  fileName = filename;
  codeSeg = Queue.create ();
  dataSeg = Queue.create ();
  imports = Queue.create ();
  exports = Queue.create ();
}
;;


let addInstr q x =
  Queue.add x q.codeSeg
;;


(* For global data and such... *)
let addData q x =
  Queue.add x q.dataSeg
;;

let addImport q x =
  Queue.add x q.imports
;;

let addExport q x =
  Queue.add x q.exports
;;
