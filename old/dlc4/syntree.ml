(* So... are the built-in types treated differently from any other types?
   Hmmm.  I guess not, really...  Then types are just strings, at the
   moment.

   And, we just put all the types in this file so they're all handy.
   Bloody OCaml modules...


   Hmm.  Functions in the code generator need to have actual symbol
   structures for args, because those args are gonna end up with
   addresses someday.
*)

type typetype = 
    AtomType of string * int
  | FuncType of typetype * symtype list
(*
  | FuncType of typetype * arg list (* Symtype list instead? No reason not to... *)
*)
  | NoneType


and alloctype =
    ARG
  | LOCAL
  | GLOBAL

(* .....hm.  Okay.  So, now, with modules involved, types can have
   names that aren't the same as their actual identity.  So we need to
   make a type resolution layer too...
   So do we have a typename id and another slot for a typetype?  Or
   can we get away with just the id?
   Let's try.
*)
and symtype = {
  vname : id;
(*  vlitname : string *)
  valloc : alloctype;
  vtype : typetype;
  mutable vloc : int;
}

(* ...I'm an idiot.  We have an ID type; treat it like one.  Hash everything
   by fully-qualified ID instead of name.  That gives us all module stuff
   utterly freakin' transparently, and gives us at least three ways of
   handling imports.

   We are simply going to add them all into the top-level module --the
   current one.  We can tell we don't export any of them, because we will
   check the exports before we add the imports.
*)
and symTbl = {
  mutable symtbl : (id, symtype) Hashtbl.t list;
  mutable typetbl : (id, typetype) Hashtbl.t;

  mutable importedmodules : modulepath list;
  mutable usedmodules : modulepath list;
  mutable exportlist : id list;

  mutable modulename : modulepath;
}


and arg = id * typetype

(* name * module path *)
and modulepath = string list
and id = string * modulepath



and decl =
    (* linenum, name, args, body, rettype *)
    Fundecl of int * id * symtype list * typetype * expr list 
  | ImportDecl of modulepath list
  | UseDecl of modulepath list
  | ExportDecl of string list

and expr =
  (* Name, value *)
    Var of int * id * typetype * expr

  (* Condition, ifpart, elsepart *)
  | If of int * expr * expr list * (expr * expr list) list * expr list

  (* Condition, body *)
  | While of int * expr * expr list
  
  (* Addition, etc *)
  | Op of int * op * expr list

  (* Function, params *)
  | Funcall of int * id * expr list

  | VarRef of int * id

  | Assign of int * expr * expr

  | Literal of int * litval

  | Cast of int * typetype * expr

  | Negate of int * expr

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




(* Nice type constants *)
let inttype = AtomType( "int", 4 )
and floattype = AtomType( "float", 4 )
;;

