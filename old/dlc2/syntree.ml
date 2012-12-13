(* So... are the built-in types treated differently from any other types?
   Hmmm.  I guess not, really...  Then types are just strings, at the
   moment.

   And, we just put all the types in this file so they're all handy.
   Bloody OCaml modules...
*)

type typetype = string;;

(* Nice type constants *)
let inttype = "int"
and floattype = "float"
;;



type alloctype =
    ARG
  | LOCAL
(*  | GLOBAL  Not used *) 
;;

type vartype = {
  vname : string;
  valloc : alloctype;
  vtype : typetype;
  mutable vloc : int;
};;
type functype = {
  funcname : string;
  funcret : typetype;
  funcargs : vartype list;
};;

type symTbl = {
  mutable symtbl : (string, vartype) Hashtbl.t list;
  mutable functbl : (string, functype) Hashtbl.t;
  mutable typetbl : (string, typetype) Hashtbl.t;
};;


type arg = string * typetype;;



type decl =
    (* linenum, name, args, body, rettype *)
    Fundecl of int * string * arg list * typetype * expr list 

and expr =
  (* Name, value *)
    Var of int * string * typetype * expr

  (* Condition, ifpart, elsepart *)
  | If of int * expr * expr list * expr list

  (* Condition, body *)
  | While of int * expr * expr list
  
  (* Addition, etc *)
  | Op of int * op * expr list

  (* Function, params *)
  | Funcall of int * string * expr list

  | VarRef of int * string

  | Assign of int * expr * expr

  | Literal of int * litval

  | Cast of int * typetype * expr

and litval =
    Intlit of int
  | Floatlit of float

and op =
    Add
  | Sub
  | Mul
  | Div
  | Mod
  | And
  | Or
  | Not
  | Xor
  | Eq
  | Neq
  | Gt
  | Gte
  | Lt
  | Lte
;;




