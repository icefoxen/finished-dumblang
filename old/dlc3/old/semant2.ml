(* Okay, all we have to actually do here is make sure vars
   actually exist when they're used, and that the right number of them are
   there...

   We have to actually check types now.  Int->float coercion can happen
   transparently, but float->int cannot.
   We might need to transform the syntax tree somehow to make this happen,
   turning implicit casts into explicit ones.


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
	Var( _, name, _, _ ) ->
	  name :: lst
      | _ -> lst
  in
    List.fold_right getVar exprlst []
;;

(* Returns the type of an expression without checking that it's sane. 
   Used by the code generator.
*)
let rec quickExprType stbl expr =
  match expr with
      Var( lineno, name, typ, value ) -> typ
    | If( lineno, cond, ifpart, elsepart ) ->
	let retexpr = List.nth ifpart ((List.length ifpart) - 1) in
	  quickExprType stbl retexpr
    | While( lineno, cond, body ) ->
	let retexpr = List.nth body ((List.length body) - 1) in
	  quickExprType stbl retexpr
    | Op( lineno, op, arglst ) ->
	let rets = List.map (quickExprType stbl) arglst in
	  if List.mem floattype rets then
	    floattype
	  else
	    inttype
    | Funcall( lineno, name, arglst ) ->
	let f = Symtbl.getFunc stbl name in
	  f.funcret
    | VarRef( lineno, name ) ->
	let v = Symtbl.getVar stbl name in
	  v.vtype
    | Assign( lineno, lval, rval ) ->
	quickExprType stbl lval
    | Literal( lineno, litval ) ->
	(match litval with
	     Intlit( _ ) -> inttype
	   | Floatlit( _ ) -> floattype)
    | Cast( lineno, totype, fromtype ) ->
	totype
;;


let rec analyzeDecl stbl decl =
  match decl with
      Fundecl( lineno, name, args, rettype, body, locals ) ->
	let addArg (name, typ) =
	  Symtbl.addVar stbl name typ ARG
	in
	  Symtbl.pushScope stbl;
	  List.iter addArg args;
	  let returnedType = analyzeExprList stbl body in
	    if not (Symtbl.hasCompatibleType stbl rettype returnedType) then
	      errorAndDieAtLine lineno
		("Function " ^ name ^ " returns type " ^ 
		 (Symtbl.typeToString stbl returnedType) ^ 
		 " but should return type " ^ 
		 (Symtbl.typeToString stbl rettype));

	    (* We save all local values so we have 'em handy for code gen *)
	    locals := Symtbl.getTopScope stbl;
	    Symtbl.popScope stbl;


(* Hmmmm.  Does analyzeExpr return the type of an expr, or should that
   be done elsewhere?
   Nah, should be done here...  I think.  Well, the framework functionality
   should remain about the same regardless of type system; all the changes
   would happen in symtbl.ml
*)
and analyzeExpr stbl = function
    Var( lineno, name, typ, value ) ->
      Symtbl.addVar stbl name typ LOCAL;
      let exprtype = analyzeExpr stbl value in
	if Symtbl.hasCompatibleType stbl typ exprtype then
	  typ
	else
	  let errmsg = "Var " ^ name ^ " declared as " ^ 
		       (Symtbl.typeToString stbl typ) ^ " but given a " ^
		       (Symtbl.typeToString stbl exprtype) in
	    errorAndDieAtLine lineno errmsg
	      
  (* Hmmm...  This makes a non-existant else an error the same way
     that OCaml does...  
     We would need a "returns nothing" type like Python to change that...
  *)
  | If( lineno, cond, ifpart, elsepart ) ->
      let condtype = analyzeExpr stbl cond in
	if Symtbl.hasCompatibleType stbl inttype condtype then (
	  let iftype = analyzeExprList stbl ifpart
	  and elsetype = analyzeExprList stbl elsepart in
	    if Symtbl.hasCompatibleType stbl iftype elsetype then
	      iftype
	    else
	      let errmsg = "If expression returns a " ^ 
			   (Symtbl.typeToString stbl iftype) ^
			   " but else expression returns a " ^
			   (Symtbl.typeToString stbl elsetype) in
		errorAndDieAtLine lineno errmsg

	)
	else
	  let errmsg = "If condition expected an int but got a" ^ 
		       (Symtbl.typeToString stbl condtype) in
	    errorAndDieAtLine lineno errmsg
	      

  | While( lineno, cond, body ) ->
      let condtype = analyzeExpr stbl cond in
	if Symtbl.hasCompatibleType stbl inttype condtype then (
	  analyzeExprList stbl body
	)
	else
	  let errmsg = "While condition expected an int but got a" ^ 
		       (Symtbl.typeToString stbl condtype) in
	    errorAndDieAtLine lineno errmsg

	      
  | Op( lineno, op, args ) ->
      analyzeOpExpr stbl op args lineno

  | Funcall( lineno, name, args ) ->

      (* Foist, we make sure the number of args is the same *)
      let fnc = Symtbl.getFunc stbl name in
      let givenArgLen = (List.length args)
      and takenArgLen = (List.length fnc.funcargs) in
	if givenArgLen <> takenArgLen then
	  errorAndDieAtLine lineno 
	    (sprintf "Function %s given %d args when it only takes %d"
	       name givenArgLen takenArgLen);

	(* Secoind, we make sure they're all of the correct type *)
	let argtypes = List.map (analyzeExpr stbl) args in
	let checkArg given expected =
	  if not (Symtbl.hasCompatibleType stbl expected.vtype given) then
	    errorAndDieAtLine lineno 
	      ("Function " ^ name ^ " given an arg of type " ^
	       (Symtbl.typeToString stbl given) ^ 
	       " but expects an arg of type " ^
	       (Symtbl.typeToString stbl expected.vtype))
	in
	  List.iter2 checkArg argtypes fnc.funcargs;
	  
	  (* Thiod, we return the function's return type *)
	  fnc.funcret
	    
  | VarRef( lineno, name ) ->
      if not (Symtbl.varExists stbl name) then
	errorAndDieAtLine lineno ("Variable " ^ name ^ " does not exist!")
      else
	(Symtbl.getVar stbl name).vtype

  | Assign( lineno, lval, rval ) ->
      (match lval with
	   VarRef( _ ) -> 
	     let giventype = analyzeExpr stbl rval
	     and rettype = analyzeExpr stbl lval in
	       if Symtbl.hasCompatibleType stbl rettype giventype then
		 rettype
	       else
		 errorAndDieAtLine lineno
		   ("Assignment got a type " ^
		    (Symtbl.typeToString stbl giventype) ^ 
		    " but expects a type of " ^
		    (Symtbl.typeToString stbl rettype))
	 | _ -> errorAndDieAtLine lineno ("Assign not given a valid lvalue!"))

  | Cast( lineno, totype, fromexpr ) ->
      totype

  | Literal( lineno, litval ) ->
      match litval with
	  Intlit( _ ) -> inttype
	| Floatlit( _ ) -> floattype


(* Hokay, this gets a little more complicated. *)
and analyzeOpExpr stbl op args lineno =
  match op with
      (* Math operators always return the most general type --float,
	 if there is a float in existance in them
      *)
      Add
    | Sub
    | Mul
    | Div 
    | Mod ->
	if exprListContains stbl args floattype then
	  floattype
	else
	  inttype

(*    | Mod ->
	if exprListContains stbl args floattype then
	  errorAndDieAtLine lineno "Mod operator only works on ints!"
	else
	  inttype
	  *)

    (* Logic always takes and returns integers *)
    | And
    | Or
    | Xor ->
	if exprListContains stbl args floattype then
	  errorAndDieAtLine lineno "Logic operators only works on ints!"
	else
	  inttype
    | Not ->
	if (List.length args) > 1 then
	  errorAndDieAtLine lineno
	    ("Logical operator 'not' only takes one argument!")
	else
	  if exprListContains stbl args floattype then
	    errorAndDieAtLine lineno "Logic operators only works on ints!"
	  else
	    inttype

    (* Equality can take ints or floats, and always returns integers *)
    | Eq
    | Neq ->
	ignore (analyzeExprList stbl args);
	inttype;
    | Gt
    | Gte
    | Lt
    | Lte ->
	if (List.length args) > 2 then
	  errorAndDieAtLine lineno
	    ("Inequality operators only takes two arguments!")
	else (
	  ignore (analyzeExprList stbl args);
	  inttype;
	)

(* analyzes a list of expressions and returns the type of the last item in 
   it *)
and analyzeExprList stbl exprlst =
  let retlst = List.map (analyzeExpr stbl) exprlst in
    List.nth retlst ((List.length retlst) - 1)

and exprListContains stbl exprlst typ =
  let retlst = List.map (analyzeExpr stbl) exprlst in
    List.mem typ retlst
;;


let doSemanticStuff tree =
  let stbl = Symtbl.makeSymTbl () in
    populateSymtbl stbl tree;
    Symtbl.printSymtbl stbl;
    List.iter (analyzeDecl stbl) tree;
    stbl
;;
