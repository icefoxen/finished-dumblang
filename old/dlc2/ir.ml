(* Intermediate representation.
   
   Tree-form, somewhat simpler than the AST, and unambiguous.
   At the moment, the only real difference is that all type conversions
   are explicit.

   ...do vars get specific locations now?  No.

   Simpler too; for loops turn into while loops, elif's turn into
   chains of if's.
   Or will, once we get those things.

   XXX: Do comparison operators get turned into binary ops?
   Does everything?  It might well make sense...
   'Cause in a programming sense, it's not (((a = b) = c) = d), it's
   ((a = b) and (b = c) and (c = d))
   But if we do that, we might as well do the same for all binary operations.

   Another issue...  In the IR, all casts from float to int and 
   vice-versa should be explicit.  Which means we have to do that in the 
   syntree conversion...
   Okay, done.  Have a nice day.

*)

open Syntree

type decl =
    Fundecl of string * arg list * typetype * stm list *
      (string, vartype) Hashtbl.t ref

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


