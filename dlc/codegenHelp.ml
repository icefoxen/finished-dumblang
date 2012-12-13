(* This is a helper file for the code generator.
   Massively x86-specific, but who cares?
*)


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
    DeclByte of string * location list
  | DeclWord of string * location list
  | DeclDword of string * location list
  | DeclDD of string * location list
;;

(* We need to be able to print the binary representation of a float... *)
let float2binary f =
  ()
;;

let declByte name i =
  DeclByte( name, [Literal( i )] )
;;

let declWord name i =
  DeclWord( name, [Literal( i )] )
;;

let declDword name i =
  DeclDword( name, [Literal( i )] )
;;

let declDD name i =
  DeclDD( name, [Literal( i )] )
;;

let declArray name lst =
  [DeclDD( (name ^ "_len"), [Literal( (Int32.of_int (List.length lst)) )] );
   DeclDD( name, lst )]
;;

(* Right now, we only do word-size arrays *)
let declArrayWithDefault name i length =
  let itm = Literal( i ) in
  let rec buildLiteralList len accm =
    if len > 0 then
      buildLiteralList (len - 1) (itm :: accm)
    else
      accm
  in
  let litlist = buildLiteralList length [] in
    declArray name litlist
;;

let declArrayFromList name lst =
  let newlst = List.map (fun x -> Literal( x )) lst in
    declArray name newlst
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

   Later.  ATM we just put 'em in eax.
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

let addImport q str =
  Queue.add str q.imports
;;


let addExport q str =
  Queue.add str q.exports
;;

(* This mangles a module name to a real assembly-level name 
   Modules are seperated by _, underscores are escaped as __.
   Foo:Bar:Bop_module:Foo_function becomes
   Foo_Bar_Bop__module_Foo__function

   It doesn't work.  No clue why.
   We also don't do the module stuff properly yet; the actual
   semantic importing and such.
   And we need to generate interface files, too.

   This is an ugly hack, but not as ugly as the alternatives.
   IDEALLY, we would be able to tell the parser "we're in this
   module!" and it'd realize that and build all the module names from
   the start.  HOWEVER, ocamlyacc doesn't seem to do that.

   That'd be the SANE thing to do...
   ...ironically, it's actually harder, because you have to distinguish
   between local variables and such then.  This is utterly rediculous,
   but it works.
   Mostly.  ..."main" function is now a bit wiggy.  We can hack around
   that, though.
*)
let id2name id pkg =
  let modul = Symtbl.getIdModule id in
  let newid = 
    if modul = Symtbl.emptyModule then
(*      ErrorReport.errorAndDie ("This should never happen!  Have you initialized the module right?") *)
      Symtbl.makeId (Symtbl.getIdName id) pkg.modulename
    else
      id
  in
  let name = Symtbl.id2str newid in
  let re = Str.regexp "_" in
  let name = Str.global_replace re "__" name in
  let re = Str.regexp ":" in
  let n = Str.global_replace re "_" name in
(*    print_endline n; *)
    n
;;



let addImports q stbl =
  let addIfExternal sym =
(*    let id = sym.vname in *)
      if not (Symtbl.isIdLocal sym) then
	addImport q (id2name sym stbl)
  in
    (* Tad iffy, here... *)
    Hashtbl.iter (fun x y -> addIfExternal x) (List.hd stbl.symtbl)
;;
