(* Intermediate representation.
   
   Tree-form, rather simpler than the AST.
   ...except I'm not sure that's actually possible...
*)

type stm = 
    BinopStm of binop * stm * stm
  | FuncStm of string * string list * stm list
  | CallStm of string * stm list
  | AssignStm of 
