(* Okay, all we have to actually do here is make sure vars
   actually exist when they're used, and that the right number of them are
   there...

   Wow, semantic checking is REALLY EASY when you have no types...

   Maybe some optimizations like const folding and loop hoisting?
   That should probably be done right before it gets turned into IR.
   Except we don't bother with IR right now, so.  We can have a
   transformation pass if we really want to, except I don't right now.
*)


open Printf
open ErrorReport
open Syntree

(* This makes a first pass through the symbol table, adding all the decls
   to the symbol table, which allows backreferencing.
   ie If func b is declared after func a, func a can still refer to b.
   I really really really wish OCaml did this sometimes.
*)
let populateSymtbl stbl decllst =
  let addDecl decl =
    match decl with
	Fundecl( _ ) -> Symtbl.addFunc stbl decl
  in
    List.iter addDecl decllst
;;

let getLocalVars exprlst =
  let getVar expr lst =
    match expr with 
	Var( _, name, _ ) ->
	  name :: lst
      | _ -> lst
  in
    List.fold_right getVar exprlst []
;;


let rec analyzeDecl stbl decl =
  match decl with
      Fundecl( lineno, name, args, body, locals ) ->
   (*Symtbl.addFunc stbl decl; *)
	Symtbl.pushScope stbl;
	List.iter (fun x -> Symtbl.addVar stbl x ARG) args;
	List.iter (analyzeExpr stbl) body;
	(* We save all local values so we have 'em handy for code gen *)
	locals := Symtbl.getTopScope stbl;
	Symtbl.popScope stbl;
	

and analyzeExpr stbl = function
    Var( lineno, name, value ) ->
      Symtbl.addVar stbl name LOCAL

  | If( lineno, cond, ifpart, elsepart ) ->
      analyzeExpr stbl cond;
      List.iter (analyzeExpr stbl) ifpart;
      List.iter (analyzeExpr stbl) elsepart;

  | While( lineno, cond, body ) ->
      analyzeExpr stbl cond;
      List.iter (analyzeExpr stbl) body;
      
  | Op( lineno, op, args ) ->
      analyzeOpExpr stbl op args lineno

  | Funcall( lineno, name, args ) ->
      List.iter (analyzeExpr stbl) args;

      let fnc = Symtbl.getFunc stbl name in
      let givenArgs = (List.length args)
      and takenArgs = (List.length fnc.funcargs) in
	if givenArgs <> takenArgs then
	  errorAndDieAtLine lineno 
	    (sprintf "Function %s given %d args when it only takes %d"
	       name givenArgs takenArgs)
	    

  | VarRef( lineno, name ) ->
      if not (Symtbl.varExists stbl name) then
	errorAndDieAtLine lineno ("Variable " ^ name ^ " does not exist!")

  | Assign( lineno, lval, rval ) ->
      (match lval with
	   VarRef( _ ) -> 
	     analyzeExpr stbl lval; 
	     analyzeExpr stbl rval;
	 | _ -> errorAndDieAtLine lineno ("Assign not given a valid lvalue!"))

  | Literal( lineno, intval ) ->
      (* Really, what CAN go wrong with this one? *)
      ()


and analyzeOpExpr stbl op args lineno =
  match op with
      Add
    | Sub
    | Mul
    | Div
    | Mod ->
	(* These are simply chained... *)
	List.iter (analyzeExpr stbl) args;
    | And
    | Or
    | Xor ->
	List.iter (analyzeExpr stbl) args;
    | Not ->
	List.iter (analyzeExpr stbl) args;
	if (List.length args) > 1 then
	  errorAndDieAtLine lineno
	    ("Logical operator 'not' only takes one argument!")
    | Eq
    | Neq ->
	List.iter (analyzeExpr stbl) args;
    | Gt
    | Gte
    | Lt
    | Lte ->
	List.iter (analyzeExpr stbl) args;
	if (List.length args) > 2 then
	  errorAndDieAtLine lineno
	    ("Inequality operators only takes two arguments!");
;;


let doSemanticStuff tree =
  let stbl = Symtbl.makeSymTbl () in
    populateSymtbl stbl tree;
    List.iter (analyzeDecl stbl) tree;
    stbl
;;
