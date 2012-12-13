(* Intermediate representation.

Tree-form, somewhat simpler than the AST, and unambiguous.
At the moment, the only real difference is that all type conversions
are explicit.

...do vars get specific locations now?  No.

Simpler too; for loops turn into while loops, elif's turn into
chains of if's.

We should never touch the IR directly from the semantic analyzer, but 
through the interface functions given below.  These functions let us
dink with the IR all we want without having to change the semantic
analyzer.  Yay abstraction.  ^_^

At some point in here, we should replace localModule with a real
module...  hmmm.
Do we set localModule with a real module name before starting compilation?
That could work, if we set it up right...
Nah.  We defer all that until the actual code-generation.

Should we bring it a notch lower-level, replacing loops and ifs and such
with jumps?  Make it flatter, with explicit temporaries?  Hmm...
That might, indeed, make optimization easier.  But we don't care about
optimization right now.
*)

open Syntree

type decl =
    Fundecl of id * symtype list * typetype * stm list *
      (id, symtype) Hashtbl.t ref
    | NopDecl

and stm = 
    OpStm of op * stm list
      (*  | CallStm of id * stm list *)
    | AssignStm of stm * stm
    | LoopStm of stm * stm list
    | IfStm of stm * stm list * stm list
    | FuncallStm of id * stm list
    | LiteralStm of litstm
    | CastStm of typetype * typetype * stm
    | VarStm of id
    | VarDeclStm of id * typetype * stm
    | ArefStm of stm * stm
    | AddrStm of stm
    | DerefStm of stm
    | NopStm
    | LabelStm of string
    | GotoStm of string

and litstm =
    Intlit of int
    | Floatlit of float
    | Arraylit of litstm list

and op =
    IntAdd
    | IntSub
    | IntMul
    | IntDiv
    | IntMod
    | FloatAdd
    | FloatSub
    | FloatMul
    | FloatDiv
    | FloatMod
    | IntAnd
    | IntOr
    | IntNot
    | IntXor

    | IntEq
    | IntNeq
    | IntGt
    | IntLt
    | IntGte
    | IntLte  
    | FloatEq
    | FloatNeq
    | FloatGt
    | FloatLt
    | FloatGte
    | FloatLte

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



(************************ GENERATION FUNCTIONS ************************)


let syntreeop2irop typ op =
  match op with
      Add -> if typ = inttype then IntAdd else FloatAdd
    | Sub -> if typ = inttype then IntSub else FloatSub
    | Mul -> if typ = inttype then IntMul else FloatMul
    | Div -> if typ = inttype then IntDiv else FloatDiv
    | Mod -> if typ = inttype then IntMod else FloatMod

    (* Logic always takes and returns integers *)
    | And -> IntAnd
    | Or  -> IntOr
    | Xor -> IntXor
    | Not -> IntNot

    (* Equality can take ints or floats, and always returns integers *)
    | Eq  -> if typ = inttype then IntEq else FloatEq
    | Neq -> if typ = inttype then IntNeq else FloatNeq
    | Gt  -> if typ = inttype then IntGt else FloatGt
    | Gte -> if typ = inttype then IntGte else FloatGte
    | Lt  -> if typ = inttype then IntLt else FloatLt
    | Lte -> if typ = inttype then IntLte else FloatLte
;;

(* This inserts cast statements into the given IR, if necessary.
*)
let ir2type ir fromtype totype =
  if fromtype = totype then
    ir
  else
    CastStm( totype, fromtype, ir )
;;


(* This is like above, except it changes the last ir in a list.
*)
let irList2type irLst fromtype totype =
  let newlst = List.rev irLst in
  let newir = ir2type (List.hd newlst) fromtype totype in
  let retlst = List.rev (newir :: (List.tl newlst)) in
    retlst

;;

(* This is supposed to turn (= a b c) into (and (= a b) (= b c))
Where = can be any logical operator (well, really = or /=)
*)
let splitLogic op ir =
  let rec loop lst accm =
    match lst with
	hd :: tl :: rest ->
	  loop (List.tl lst) ((OpStm( op, [hd; tl] )) :: accm)
      | [] -> List.rev accm
      | _ :: [] -> List.rev accm
  in
  let ret = loop ir [] in
    OpStm( IntAnd, ret )
;;


let convertVar name vartype exprIr =
VarDeclStm( name, vartype, exprIr )
;;

let convertIf ifcond ifbody elsebody =
   IfStm( ifcond, ifbody, elsebody )
;;

let convertWhile cond body =
   LoopStm( cond, body ) 
;;

let convertOp op args =
   OpStm( op, args )
;;


let convertFuncall name args =
   FuncallStm( name, args )
;;


let convertVarRef name =
   VarStm( name )
;;


let convertAssign lhs rhs =
   AssignStm( lhs, rhs )
;;


let convertLiteral lit =
  let rec checkLit x = 
    match x with
	Syntree.Intlit( i ) -> Intlit( i )
      | Syntree.Floatlit( f ) -> Floatlit( f )
      | Syntree.Arraylit( l ) -> 
	  let litlst = List.map checkLit l in
	    Arraylit( litlst )
  in
    LiteralStm( checkLit lit )
;;

let convertCast fromtype totype stm =
   CastStm( totype, fromtype, stm )
;; 

let convertNegate stm typ =
   (* Special negate statement?  Just say 0 - whatever, or -1 * whatever?
      Let's just say 0 - whatever *)
      let op = if typ = inttype then IntSub else FloatSub
      and zero = if typ = inttype then Intlit( 0 ) else Floatlit( 0. ) in
   OpStm( op, [LiteralStm( zero ); stm] )
;;

let convertAref arraystm indexstm =
  ArefStm( arraystm, indexstm )
;;

let convertAddr stm =
  AddrStm( stm )
;;

let convertRef stm =
  DerefStm( stm )
;;



(**************** PRINTING FUNCTIONS ********************)


let op2str = function
    IntAdd -> "IntAdd"
  | IntSub -> "IntSub"
  | IntMul -> "IntMul" 
  | IntDiv -> "IntDiv" 
  | IntMod -> "IntMod" 
  | FloatAdd -> "FloatAdd"
  | FloatSub -> "FloatSub"
  | FloatMul -> "FloatMul"
  | FloatDiv -> "FloatDiv"
  | FloatMod -> "FloatMod"
  | IntAnd -> "IntAnd"
  | IntOr -> "IntOr"
  | IntNot -> "IntNot"
  | IntXor -> "IntXor"

  | IntEq -> "IntEq"
  | IntNeq -> "IntNeq"
  | IntGt -> "IntGt"
  | IntLt -> "IntLt"
  | IntGte -> "IntGte"
  | IntLte   -> "IntLte"
  | FloatEq -> "FloatEq"
  | FloatNeq -> "FloatNeq"
  | FloatGt -> "FloatGt"
  | FloatLt -> "FloatLt"
  | FloatGte -> "FloatGte"
  | FloatLte -> "FloatLte"
;;


let rec literal2str = function
    Intlit( i ) -> Printf.sprintf "INTLIT:{%d}" i
  | Floatlit( f ) -> Printf.sprintf "FLOATLIT:{%f}" f
  | Arraylit( a ) -> Printf.sprintf "ARRAYLIT:{%s}"
      (List.fold_left (fun str itm -> str ^ " " ^ (literal2str itm)) "" a)


let rec irexpr2str = function
      OpStm( op, args ) -> 
	Printf.sprintf "(%s %s)" (op2str op) (irexprlist2str args)
    | AssignStm( tostm, fromstm ) ->
	Printf.sprintf "(ASSIGN %s %s)" (irexpr2str tostm) (irexpr2str fromstm)
	
    | LoopStm( test, body ) -> "Loops shouldn't exist!!"
    | IfStm( cond, ifbody, elsebody ) -> 
	Printf.sprintf "IF %s THEN\n   %s\nELSE   %s\nENDIF"
	  (irexpr2str cond) (irexprlist2str ifbody) (irexprlist2str elsebody)
    | FuncallStm( name, args ) ->
	let name, _ = name in
	Printf.sprintf "(FUNCALL %s %s)" name (irexprlist2str args)
    | LiteralStm( litstm ) -> 
	literal2str litstm
    | CastStm( totype, fromtype, fromexpr ) ->  
	Printf.sprintf "(CAST from %s to %s: %s)"
	  (Symtbl.type2str fromtype) (Symtbl.type2str totype)
	  (irexpr2str fromexpr)
    | VarStm( name ) -> 
	let name, _ = name in
	Printf.sprintf "(VAR %s)" name
    | VarDeclStm( name, typ, value ) -> 
	let name, _ = name in
	Printf.sprintf "VARDECL %s: %s = %s" name (Symtbl.type2str typ)
	  (irexpr2str value)
    | ArefStm( arr, idx ) -> 
	Printf.sprintf "(AREF %s %s)" (irexpr2str arr) (irexpr2str idx)
    | AddrStm( stm ) ->
	Printf.sprintf "(ADDRESS %s)" (irexpr2str stm)
    | DerefStm( stm ) -> 
	Printf.sprintf "(DEREF %s)" (irexpr2str stm)
    | NopStm -> "NOP"
    | LabelStm( label ) ->
	Printf.sprintf "LABEL %s:" label
    | GotoStm( label ) -> 
	Printf.sprintf "(GOTO %s)" label

and irexprlist2str irlist =
  List.fold_left (fun str itm -> (irexpr2str itm) ^ "\n" ^ str) "" irlist
;;


let irdecl2str = function
    Fundecl( id, args, rettype, body, vars ) ->
      let name, _ = id
      and args = List.fold_left 
	(fun str itm -> (Symtbl.id2str itm.Syntree.vname) ^ " " ^ str) "" args
      and rettype = Symtbl.type2str rettype
      and body = irexprlist2str body in
      Printf.sprintf "FUNDECL %s %s (%s):\n %s\nENDFUNDECL %s\n"
	name args rettype body name
  | NopDecl -> "NOPDECL\n"
;;

let printIR ir =
  List.iter (fun x -> print_endline (irdecl2str x)) ir
;;
