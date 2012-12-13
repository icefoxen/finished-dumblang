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
   Except it isn't, 'cause we generate the IR here.
*)
(*
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
*)

(* ...hmm.  It might actually be simplest to put these around EVERY
   potential type conversion, and filter out the bogus ones with the same
   from and to type at codegen?
   ...hmm.
   ...Well, we can do the same here, really.
*)
let ir2type ir fromtype totype =
  if fromtype = totype then
    ir
  else
    Ir.CastStm( totype, fromtype, ir )
;;

(* XXX *)
let irList2type irLst fromtype totype =
  irLst
;;

let rec analyzeDecl stbl decl =
  match decl with
      Fundecl( lineno, name, args, rettype, body ) ->
	let addArg (name, typ) =
	  Symtbl.addVar stbl name typ ARG
	in
	  Symtbl.pushScope stbl;
	  List.iter addArg args;
	  let returnedType, returnedIr = analyzeExprList stbl body in
	    if not (Symtbl.hasCompatibleType stbl rettype returnedType) then
	      errorAndDieAtLine lineno
		("Function " ^ name ^ " returns type " ^ 
		 (Symtbl.typeToString stbl returnedType) ^ 
		 " but should return type " ^ 
		 (Symtbl.typeToString stbl rettype));

	    (* We save all local values so we have 'em handy for code gen *)
	    let locals = Symtbl.getTopScope stbl in
	      Symtbl.popScope stbl;
	      Ir.Fundecl( name, args, rettype, returnedIr, ref locals );

	      


(* Hmmmm.  Does analyzeExpr return the type of an expr, or should that
   be done elsewhere?
   Nah, should be done here...  I think.  Well, the framework functionality
   should remain about the same regardless of type system; all the changes
   would happen in symtbl.ml

   This is a big huge long function.  The trick is that it's a bunch of 
   seperate bits that each handle one type of statement.
*)
and analyzeExpr stbl = function
    Var( lineno, name, typ, value ) ->
      Symtbl.addVar stbl name typ LOCAL;
      let exprtype, exprir = analyzeExpr stbl value in
      let convertedExprIr = ir2type exprir exprtype typ in
	if Symtbl.hasCompatibleType stbl typ exprtype then
	  (typ, Ir.VarDeclStm( name, typ, convertedExprIr ))
	else
	  let errmsg = "Var " ^ name ^ " declared as " ^ 
		       (Symtbl.typeToString stbl typ) ^ " but given a " ^
		       (Symtbl.typeToString stbl exprtype) in
	    errorAndDieAtLine lineno errmsg
	      
  (* Hmmm...  This makes a non-existant else a type error the same way
     that OCaml does...  
     We would need a "returns nothing" type like Python to change that...
  *)
  | If( lineno, cond, ifpart, elsepart ) ->
      let condtype, condir = analyzeExpr stbl cond in
	if Symtbl.hasCompatibleType stbl inttype condtype then (
	  let iftype, ifir = analyzeExprList stbl ifpart
	  and elsetype, elseir = analyzeExprList stbl elsepart in
	    if Symtbl.hasCompatibleType stbl iftype elsetype then
	      let convertedElseIr = irList2type ifir elsetype iftype in
		(iftype, Ir.IfStm( condir, ifir, convertedElseIr ))
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
      let condtype, condir = analyzeExpr stbl cond in
	if Symtbl.hasCompatibleType stbl inttype condtype then (
	  let rettype, bodyir = analyzeExprList stbl body in
	    (rettype, Ir.LoopStm( condir, bodyir ))
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
	let arglst = List.map (analyzeExpr stbl) args in
	let checkArg given expected =
	  let giventype, givenir = given in
	    if not 
	      (Symtbl.hasCompatibleType stbl expected.vtype giventype) then
		errorAndDieAtLine lineno 
		  ("Function " ^ name ^ " given an arg of type " ^
		   (Symtbl.typeToString stbl giventype) ^ 
		   " but expects an arg of type " ^
		   (Symtbl.typeToString stbl expected.vtype))
	    else
	      (expected, (ir2type givenir giventype expected.vtype))
	in
	let arglst = List.map2 checkArg arglst fnc.funcargs in
	  
	(* Thiod, we return the function's return type & IR *)
	let argirs = List.map snd arglst in
	  (fnc.funcret, Ir.FuncallStm( name, argirs ))
	    
  | VarRef( lineno, name ) ->
      if not (Symtbl.varExists stbl name) then
	errorAndDieAtLine lineno ("Variable " ^ name ^ " does not exist!")
      else
	((Symtbl.getVar stbl name).vtype, Ir.VarStm( name ))

  | Assign( lineno, lval, rval ) ->
      (match lval with
	   VarRef( _ ) -> 
	     let giventype, rvalir = analyzeExpr stbl rval
	     and rettype, lvalir = analyzeExpr stbl lval in
	       if Symtbl.hasCompatibleType stbl rettype giventype then
		 let rvalir = ir2type rvalir giventype rettype in
		   (rettype, Ir.AssignStm( lvalir, rvalir ))
	       else
		 errorAndDieAtLine lineno
		   ("Assignment got a type " ^
		    (Symtbl.typeToString stbl giventype) ^ 
		    " but expects a type of " ^
		    (Symtbl.typeToString stbl rettype))
	 | _ -> errorAndDieAtLine lineno ("Assign not given a valid lvalue!"))

  | Cast( lineno, totype, fromexpr ) ->
      let fromtype, fromir = analyzeExpr stbl fromexpr in
	(totype, Ir.CastStm( totype, fromtype, fromir ))

  | Literal( lineno, litval ) ->
      (match litval with
	   Intlit( i ) -> (inttype, Ir.LiteralStm( Ir.Intlit( i ) ))
	 | Floatlit( f ) -> (floattype, Ir.LiteralStm( Ir.Floatlit( f ) )))


(* Hokay, this gets a little more complicated. 
   And now we must make it work with the new ir-stuff.
   And we must make type conversions explicit in the ir...
*)
and analyzeOpExpr stbl op args lineno =
  let exprs = List.map (analyzeExpr stbl) args in
  let irlst = List.map snd exprs 
  and typelst = List.map fst exprs in
  let argstype = 
    if List.mem floattype typelst then floattype else inttype in

  let convertedIr = List.map2 
		      (fun ir typ ->
			   ir2type ir typ argstype) irlst typelst in

    match op with
	(* Math operators always return the most general type --float,
	   if there is a float argument, int otherwise.
	   God help me when we make different size numbers...
	*)
	Add
      | Sub
      | Mul
      | Div 
      | Mod ->

	  let newop = Ir.syntreeop2irop argstype op in
	    (argstype, Ir.OpStm( newop, convertedIr ))
	      
      (* Logic always takes and returns integers *)
      | And
      | Or
      | Xor ->
	  if argstype = floattype then
	    errorAndDieAtLine lineno "Logic operators only works on ints!"
	  else
	    let newop = Ir.syntreeop2irop inttype op in
	      (inttype, Ir.OpStm( newop, convertedIr ))
      | Not ->
	  if (List.length args) > 1 then
	    errorAndDieAtLine lineno
	      ("Logical operator 'not' only takes one argument!")
	  else
	    if argstype = floattype then
	      errorAndDieAtLine lineno "Logic operators only works on ints!"
	    else
	      let newop = Ir.syntreeop2irop inttype op in
		(inttype, Ir.OpStm( newop, convertedIr ))

      (* Equality can take ints or floats, and always returns integers *)
      | Eq
      | Neq ->
	  let newop = Ir.syntreeop2irop argstype op in
	    (argstype, Ir.OpStm( newop, convertedIr ))
      | Gt
      | Gte
      | Lt
      | Lte ->
	  if (List.length args) > 2 then
	    errorAndDieAtLine lineno
	      ("Inequality operators only takes two arguments!")
	  else (
	    let newop = Ir.syntreeop2irop argstype op in
	      (argstype, Ir.OpStm( newop, convertedIr ))
	  )

(* analyzes a list of expressions and returns the type of the last item in 
   it, with a list if ir statements. *)
and analyzeExprList stbl exprlst =
  let lst = List.map (analyzeExpr stbl) exprlst in
  let rettype, _ = List.nth lst ((List.length lst) - 1) in
  let irlst = List.map snd lst in
    (rettype, irlst)

and exprListContains stbl exprlst typ =
  let retlst = List.map (fun x -> fst (analyzeExpr stbl x)) exprlst in
    List.mem typ retlst
;;


let doSemanticStuff tree =
  let stbl = Symtbl.makeSymTbl () in
    populateSymtbl stbl tree;
    Symtbl.printSymtbl stbl;
    (* Later, this should return an IR list too.  Once we can handle it. *)
    let irlst = (List.map (analyzeDecl stbl) tree) in
      (stbl, irlst)
;;
