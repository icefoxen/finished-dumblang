(* We're actually gonna put LINE NUMBERS into these for a change.
   Ooo, I'm getting too advanced for my own good!

   And, we just put all the types in this file so they're all handy.
   Bloody OCaml modules...
*)


type alloctype =
    ARG
  | LOCAL
(*  | GLOBAL  Not used *) 
;;

type vartype = {
  vname : string;
  valloc : alloctype;
  mutable vloc : int;
};;
type functype = {
  funcname : string;
  funcargs : vartype list;
};;

type symTbl = {
  mutable symtbl : (string, vartype) Hashtbl.t list;
  mutable functbl : (string, functype) Hashtbl.t;
};;



type arg = string;;

type decl =
    (* linenum, name, args, body, locals (used to pass to codegen) *)
    Fundecl of int * string * arg list * expr list * 
      (string, vartype) Hashtbl.t ref

and expr =
  (* Name, value *)
    Var of int * string * expr

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

  | Literal of int * int

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

