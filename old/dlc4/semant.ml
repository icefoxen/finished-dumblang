(* Okay, all we have to actually do here is make sure vars
   actually exist when they're used, and that the right number of them are
   there...

   We have to actually check types now.  Int->float coercion can happen
   transparently, but float->int cannot.


   Maybe some optimizations like const folding and loop hoisting?
   No, that happens to the IR, when it happens.
*)


open Printf
open ErrorReport
open Syntree
open Symtbl

(* This makes a first pass through the symbol table, adding all the decls
   to the symbol table, which allows backreferencing.
   ie If func b is declared after func a, func a can still refer to b.
   I really really really wish OCaml did this sometimes.
*)

let populateSymtbl stbl decllst =
  let addDecl decl =
    match decl with
	Fundecl( _ ) -> Symtbl.addSymbol stbl (Symtbl.fundecl2symbol decl)
      | ImportDecl( ilst ) -> List.iter (Modules.addImportedModule stbl) ilst
      | UseDecl( ulst ) -> List.iter (Modules.addUsedModule stbl) ulst
      | ExportDecl( _ ) -> ()
  in
    List.iter addDecl decllst
;;



let addExports stbl decllst =
  let addExport decl = 
    match decl with
	ExportDecl( elst ) ->
	  let elst = List.map (fun x -> Symtbl.makeId x emptyModule) elst in
	  List.iter (Modules.exportSymbol stbl) elst
      | _ -> ()
  in
    List.iter addExport decllst
;;

(*
  We need to add exports BEFORE we do imports, that way we can't export
  an import.
  ...but we also need to add fundecls, populating the syntax tree, before
  we do imports also then.  Hokay. doable.  We just have to split up
  populateSymtbl into different bits.
let addExports stbl decllst =
  List.iter (function -> ExportDecl( s ) ->

*)

let getLocalVars exprlst =
  let getVar expr lst =
    match expr with 
	Var( _, name, _, _ ) ->
	  name :: lst
      | _ -> lst
  in
    List.fold_right getVar exprlst []
;;



let rec analyzeDecl stbl decl =
  match decl with
      Fundecl( lineno, name, args, rettype, body ) ->
	Symtbl.pushScope stbl;
	List.iter (Symtbl.addSymbol stbl) args;

	let returnedType, returnedIr = analyzeExprList stbl body in

	  if not (Symtbl.hasCompatibleType stbl rettype returnedType) then
	    errorAndDieAtLine lineno
	      (Printf.sprintf 
		 "Function %s returns type %s but should return type %s" 
		 (id2str name) (Symtbl.type2str stbl returnedType)
		 (Symtbl.type2str stbl rettype));

	  let returnedIr = Ir.irList2type returnedIr returnedType rettype in

	  (* We save all local values so we have 'em handy for code gen *)
	  let locals = Symtbl.getTopScope stbl in
	    Symtbl.popScope stbl;
	    Ir.Fundecl( name, args, rettype, returnedIr, ref locals );

    (* These should check that all the symbols exported exist.  The 
       package importation will check that the symbols imported exist.
       
    *)
    | ImportDecl( ilst ) -> Ir.NopDecl
    | UseDecl( ulst ) -> Ir.NopDecl
    | ExportDecl( elst ) -> Ir.NopDecl

	


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
      Symtbl.addNewSymbol stbl name typ LOCAL;
      let exprtype, exprir = analyzeExpr stbl value in
	if Symtbl.hasCompatibleType stbl typ exprtype then
	  (typ, (Ir.convertVar name typ exprtype exprir))
	else
	  let errmsg = "Var " ^ (Symtbl.id2str name) ^ " declared as " ^ 
	    (Symtbl.type2str stbl typ) ^ " but given a " ^
	    (Symtbl.type2str stbl exprtype) in
	    errorAndDieAtLine lineno errmsg
	      
  (* Hmmm...  This makes a non-existant else a type error the same way
     that OCaml does...  
     We would need a "returns nothing" type like Python to change that...
  *)
  | If( lineno, cond, ifpart, elif, elsepart ) ->
      let condtype, condir = analyzeExpr stbl cond in
      let iftype, ifir = analyzeExprList stbl ifpart in
      let elsetype, elseir = analyzeExprList stbl elsepart in
	if not (Symtbl.hasCompatibleType stbl inttype condtype) then
	  let errmsg = "If condition expected an int but got a" ^ 
            (Symtbl.type2str stbl condtype) in
	    errorAndDieAtLine lineno errmsg
	else
	  let rec analyzeElif elif =
            let cond, body = elif in
	    let condtype, condir = analyzeExpr stbl cond in
	    let bodytype, bodyir = analyzeExprList stbl body in
              if not (Symtbl.hasCompatibleType stbl inttype condtype) then
		let errmsg = "If condition expected an int but got a" ^ 
	          (Symtbl.type2str stbl condtype) in
		  errorAndDieAtLine lineno errmsg
              else if not (Symtbl.hasCompatibleType stbl iftype bodytype) then
		let errmsg = "If expression returns a " ^ 
		  (Symtbl.type2str stbl iftype) ^
		  " but elif expression returns a " ^
		  (Symtbl.type2str stbl bodytype) in
		  errorAndDieAtLine lineno errmsg
	      else
	        let condir = Ir.ir2type condir condtype inttype
		and bodyir = Ir.irList2type bodyir bodytype iftype in
	          (condir, bodyir)
          in
	    if not (Symtbl.hasCompatibleType stbl iftype elsetype) then
	      let errmsg = "If expression returns a " ^ 
		(Symtbl.type2str stbl iftype) ^
		" but else expression returns a " ^
		(Symtbl.type2str stbl elsetype) in
	        errorAndDieAtLine lineno errmsg 
	    else
	      let elifir = List.map analyzeElif elif in
		(inttype, Ir.convertIf condir ifir elifir (List.hd elseir))


  | While( lineno, cond, body ) ->
      let condtype, condir = analyzeExpr stbl cond in
	if Symtbl.hasCompatibleType stbl inttype condtype then (
	  let rettype, bodyir = analyzeExprList stbl body in
	    (rettype, Ir.convertWhile condir bodyir )
	)
	else
	  let errmsg = "While condition expected an int but got a " ^ 
	    (Symtbl.type2str stbl condtype) in
	    errorAndDieAtLine lineno errmsg

	      
  | Op( lineno, op, args ) ->
      analyzeOpExpr stbl op args lineno

  (* XXX??? *)
  | Funcall( lineno, name, args ) ->
      (* Foist, we make sure the number of args is the same *)
      let fnc = Symtbl.getFunc stbl name in
      let givenArgLen = (List.length args)
      and takenArgLen = (List.length (Symtbl.getFuncArgs fnc)) in
	if givenArgLen <> takenArgLen then
	  errorAndDieAtLine lineno 
	    (sprintf "Function %s given %d args when it only takes %d"
	       (Symtbl.id2str name) givenArgLen takenArgLen);

	(* Secoind, we make sure they're all of the correct type *)
	let arglst = List.map (analyzeExpr stbl) args in
	let checkArg given expected =
	  let giventype, givenir = given in
	    if not 
	      (Symtbl.hasCompatibleType stbl expected.vtype giventype) then
		errorAndDieAtLine lineno 
		  ("Function " ^ (id2str name) ^ " given an arg of type " ^
		     (Symtbl.type2str stbl giventype) ^ 
		     " but expects an arg of type " ^
		     (Symtbl.type2str stbl expected.vtype))
	    else
	      (expected, (Ir.ir2type givenir giventype expected.vtype))
	in
	let arglst = List.map2 checkArg arglst (getFuncArgs fnc) in
	  
	(* Thiod, we return the function's return type & IR *)
	let argirs = List.map snd arglst in
	  ((getFuncRettype fnc), Ir.convertFuncall name argirs )
	    
  | VarRef( lineno, name ) ->      
      if not (Symtbl.symbolExists stbl name) then (
	errorAndDieAtLine lineno ("Variable " ^ (id2str name)
				  ^ " does not exist!")
      )
      else
	((Symtbl.getSymbol stbl name).vtype, Ir.VarStm( name ))

  | Assign( lineno, lval, rval ) ->
      (match lval with
	   VarRef( _ ) -> 
	     let giventype, rvalir = analyzeExpr stbl rval
	     and rettype, lvalir = analyzeExpr stbl lval in
	       if Symtbl.hasCompatibleType stbl rettype giventype then
		 let rvalir = Ir.ir2type rvalir giventype rettype in
		   (rettype, Ir.convertAssign lvalir rvalir )
	       else
		 errorAndDieAtLine lineno
		   ("Assignment got a type " ^
		      (Symtbl.type2str stbl giventype) ^ 
		      " but expects a type of " ^
		      (Symtbl.type2str stbl rettype))
	 | _ -> errorAndDieAtLine lineno ("Assign not given a valid lvalue!"))

  | Cast( lineno, totype, fromexpr ) ->
      let fromtype, fromir = analyzeExpr stbl fromexpr in
	(totype, Ir.convertCast fromtype totype fromir )

  | Negate( lineno, expr ) ->
      let exprtype, exprir = analyzeExpr stbl expr in
	(exprtype, Ir.convertNegate exprir exprtype )

  | Literal( lineno, litval ) ->
      match litval with
	  Intlit( i ) -> (inttype, Ir.convertLiteral litval)
	| Floatlit( f ) -> (floattype, Ir.convertLiteral litval)


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
       Ir.ir2type ir typ argstype) irlst typelst in

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
	  if (List.length args) < 2 then
	    errorAndDieAtLine lineno
	      ("Operators take more than one argument!");
	  let newop = Ir.syntreeop2irop argstype op in
	    (argstype, Ir.convertOp newop convertedIr )
	      
      (* Logic always takes and returns integers *)
      | And
      | Or
      | Xor ->
	  if (List.length args) < 2 then
	    errorAndDieAtLine lineno
	      ("Operators take more than one argument!");
	  if argstype = floattype then
	    errorAndDieAtLine lineno "Logic operators only works on ints!"
	  else
	    let newop = Ir.syntreeop2irop inttype op in
	      (inttype, Ir.convertOp newop convertedIr )

      (* XXX: Not worketh not... 
         Dammit, this is weird.  It gets generated, but the code generator
	 never seems to hit it...  no clue what's actually happening.
      *)
      | Not ->
	  if (List.length args) > 1 then
	    errorAndDieAtLine lineno
	      ("Logical operator 'not' only takes one argument!")
	  else
	    if argstype = floattype then
	      errorAndDieAtLine lineno "Logic operators only works on ints!"
	    else
	      (
		let newop = Ir.syntreeop2irop inttype op in
	          print_endline (Ir.op2str newop);
	          print_endline "So full of hate...";
	          (inttype, Ir.convertOp newop convertedIr )
              )

      (* Equality can take ints or floats, and always returns integers *)
      | Eq
      | Neq ->
	  if (List.length args) < 2 then
	    errorAndDieAtLine lineno
	      ("Operators take more than one argument!");
	  let newop = Ir.syntreeop2irop argstype op in
	  let convertedIr = Ir.splitLogic newop convertedIr in
	    (inttype, convertedIr )
      | Gt
      | Gte
      | Lt
      | Lte ->
	  if (List.length args) < 2 then
	    errorAndDieAtLine lineno
	      ("Operators take more than one argument!"); 
	  if (List.length args) > 2 then
	    errorAndDieAtLine lineno
	      ("Inequality operators only takes two arguments!");
	  let newop = Ir.syntreeop2irop argstype op in
	    (inttype, Ir.convertOp newop convertedIr )

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


let doSemanticStuff tree modulename =
  let stbl = Symtbl.makeSymTbl modulename in
    populateSymtbl stbl tree;
    addExports stbl tree;
(*    Symtbl.printSymtbl stbl; *)
    let irlst = (List.map (analyzeDecl stbl) tree) in
      (stbl, irlst)
;;
