(* Intermediate representation.
   
   Tree-form, somewhat simpler than the AST, and unambiguous.
   At the moment, the only real difference is that all type conversions
   are explicit.

   ...do vars get specific locations now?  No.

   Simpler too; for loops turn into while loops, elif's turn into
   chains of if's.
   Or will, once we get those things.
   Though loops in general may end up turning into jumps; same with
   if's in general, really.

   We should never touch the IR directly from the semantic analyzer, but 
   through the interface functions given below.  These functions let us
   dink with the IR all we want without having to change the semantic
   analyzer.  Yay abstraction.  ^_^
*)

open Syntree

type decl =
    Fundecl of string * arg list * typetype * stm list *
      (string, vartype) Hashtbl.t ref
  | NopDecl

and stm = 
    OpStm of op * stm list
  | CallStm of string * stm list
  | AssignStm of stm * stm
  | LoopStm of stm * stm list
  | IfStm of stm * stm list * stm list
  | FuncallStm of string * stm list
  | LiteralStm of litstm
  | CastStm of typetype * typetype * stm
  | VarStm of string 
  | VarDeclStm of string * typetype * stm
  | NopStm

and litstm =
    Intlit of int
  | Floatlit of float

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


let convertVar name vartype exprtype exprIr =
   let convertedExprIr = ir2type exprIr exprtype vartype in
      VarDeclStm( name, vartype, convertedExprIr )
;;

let convertIf ifcond ifbody eliflst elsebody =
   (* First, we turn "if x then y elif z then a else b end" into
      "if x then y else (if z then a else b end) end"
      I love recursion.
   *)
   let rec canonicalize eliflst elsebit =  (* Is that even a word? *)
      match eliflst with
         [] -> elsebit
       | elif :: rest ->
          let cond, body = elif in
             IfStm( cond, body, [(canonicalize rest elsebit)])
   in
   let elsebody = canonicalize eliflst elsebody in
      IfStm( ifcond, ifbody, [elsebody] )
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
   match lit with
      Syntree.Intlit( i ) -> LiteralStm( Intlit( i ) )
    | Syntree.Floatlit( f ) ->  LiteralStm( Floatlit( f ) )
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
