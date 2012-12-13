(* Actually compiles the stuff to asm...
*)

open Symtbl
open Syntree

type location =
    Mem of int
  | Reg of int
  | Literal of int
  | RegOffset of location * int
  | RegOffsetLoc of location * location
  | StackPointer
  | BasePointer
;;

(* x86 specific silliness, since we don't do real register allocation *)
let eax = Reg( 0 )
and ebx = Reg( 1 )
and ecx = Reg( 2 )
and edx = Reg( 3 )
and esi = Reg( 4 )
and edi = Reg( 5 )
;;


(* These go dest, src just like nasm.  So sue me. *)
type opcode =
  (* Load/store *)
    MOVE of location * location

  (* Arithmatic *)
  | ADD of location * location
  | SUB of location * location
  | MUL of location * location
  | DIV of location
  | MOD of location * location
  | INC of location
  | DEC of location

  (* Logic *)
  | SHL of location * location
  | SHR of location * location
  | AND of location * location
  | OR of location * location
  | NOT of location
  | XOR of location * location
  | COMP of location * location
  | NEG of location

  (* Branch *)
  (* Oh fine, we're not gonna be jumping to anything but a label anyway *)
  | JMP of string
  | JL of string
  | JG of string
  | JLE of string
  | JGE of string
  | JE of string
  | JNE of string
  | LABEL of string
      (* Not actually different from LABEL, currently... *)
  | LOCALLABEL of string

  (* Function *)
  | CALL of string
  | RET

  (* Floating point *)
(*
  | FLOAD
  | FSTORE
  | FADD
  | FSUB
  | FMUL
  | FDIV
*)

  (* Hey, could be handy *)
  | COMMENT of string
;;

type datadecl =
    DeclByte of string
  | DeclWord of string
  | DeclDword of string
  | DeclDD of string
;;

(* Used for naming if/loop labels. *)
let gensymidx = ref 0;;
let gensym () =
  incr gensymidx;
  Printf.sprintf "SYM%d" !gensymidx;
;;

let makeLabelName name =
  name ^ (gensym ())
;;



(* So we need a structure to actually hold the metadata as well as the
   raw instructions.
   Metadata we need: File name, imported/exported symbols, data declerations.

   So:
   1) How do we know where the variables are (local/arg)?  The symtbl has to 
   tell us.
   2) How do we return values?  We stick them on the top of the stack; the
   caller has to allocate space for it, just below the args.
*)

type asmModule = {
  fileName: string;
  codeSeg: opcode Queue.t;
  dataSeg: datadecl Queue.t;
  imports: string Queue.t;
  exports: string Queue.t;
};;

let makeModule filename = {
  fileName = filename;
  codeSeg = Queue.create ();
  dataSeg = Queue.create ();
  imports = Queue.create ();
  exports = Queue.create ();
}
;;


let addInstr q x =
  Queue.add x q.codeSeg
;;


(* For global data and such... *)
let addData q x =
  Queue.add x q.dataSeg
;;

let addImport q x =
  Queue.add x q.imports
;;

let addExport q x =
  Queue.add x q.exports
;;


(* Computes offsets of locals and arguments, and saves 'em 
   The variables are put on the stack in the order the hasthable runs
   across 'em.  The technical name for this is Any Old Order.

   However, a copy of the args in list-form is kept with each function
   in the symbol table also, so we also update the locations there.
   This is necessary so caller functions know where to put arguments for
   the callee.
*)
let computeStackOffsets stbl fname stackframe =

  let fargs = (getFunc stbl fname).funcargs in
    
  let localloc = ref 4 in
  let argloc = ref 8 in
  let computeOffset name var =
    match var.valloc with
	ARG -> 
	  let currentArg = List.find (fun x -> x.vname = var.vname) fargs in
	    currentArg.vloc <- !argloc;
	    var.vloc <- !argloc; 
	    argloc := !argloc + 4;
      | LOCAL -> 
	  var.vloc <- !localloc;
	  localloc := !localloc + 4;
	  
  in
    Hashtbl.iter computeOffset stackframe;
;;


let getVarOffset name stbl =
  let var = getVar stbl name in 
    match var.valloc with
	ARG -> RegOffset( BasePointer, var.vloc )
      | LOCAL -> RegOffset( BasePointer, -var.vloc )
;;

let numLocalsInFrame stackframe =
  let count = ref 0 in
    Hashtbl.iter (fun x y -> if y.valloc = LOCAL then incr count) stackframe;
    !count
;;


(* Actual codegen function *)
(*
  sub esp, 4
  mov [esp], ebp
  sub esp, (# of args * 4)

  (locals are currently always 4 bytes, y'see)
*)
let addFuncProlog q name frame=
  let localSize = (numLocalsInFrame frame) * 4 in
    addInstr q (COMMENT( "Start function"));
    addInstr q (LABEL( name ));
    addInstr q (SUB( StackPointer, Literal( 4 ) ));
    addInstr q (MOVE( (RegOffset( StackPointer, 0 ) ), BasePointer ));
    addInstr q (MOVE( BasePointer, StackPointer ));
    addInstr q (SUB( StackPointer, Literal( localSize ) ));
    addInstr q (COMMENT( "Func prolog done" ));
;;

(*
  mov esp, ebp
  mov ebp, [esp]
  add esp, 4
  ret
*)
let addFuncEpilog q =
  addInstr q (COMMENT( "Func epilog" ));
  addInstr q (MOVE( StackPointer, BasePointer ));
  addInstr q (MOVE( BasePointer, (RegOffset( StackPointer, 0 )) ));
  addInstr q (ADD( StackPointer, Literal( 4 ) ));
  addInstr q (RET);
  addInstr q (COMMENT( "End function\n" ));
;;



(* ...aarg.  Okay.  So each scope block (function, for now) has a chunk of
  the symbol table attached to it, containing it's scope.  We use it to
   figure locals, and we push it to the symbol table again before we mess
   with the body of the function, so everything should be hunky-dory.
*)

(* TODO: Compute local and arg offsets *)
let rec compileDecl stbl q decl =
  match decl with
      Fundecl( _, name, args, body, locals ) ->
	addExport q name;

	(*	Printf.printf "Function %s has %d vars\n" name 
		(Symtbl.hashtblSize !locals);
		Printf.printf "They are: ";
		Hashtbl.iter (fun x y -> Printf.printf "%s " x) !locals;
		Printf.printf "\n";
	*)

	computeStackOffsets stbl name !locals;
	addFuncProlog q name !locals;

	setTopScope stbl !locals;
	compileExprList stbl q body;
	popScope stbl;

	addFuncEpilog q;
	
(* Let's just say... all exprs exchange leave their return value in eax.
   Since we return things in eax, that makes life easy for now.
*)
and compileExpr stbl q expr =
  let addi = addInstr q in
    match expr with 
	Var( lineno, name, value ) ->
	  let varloc = getVarOffset name stbl in
	    compileExpr stbl q value;
	    addi (MOVE( varloc, eax ));

      | If( lineno, cond, ifpart, elsepart ) ->
	  addIf stbl q cond ifpart elsepart

      | While( lineno, cond, body ) ->
	  addWhile stbl q cond body
	    
      | Op( lineno, op, args ) ->
	  addOp stbl q op args

      | Funcall( lineno, name, args ) ->
	  addFuncall stbl q name args

      | VarRef( lineno, name ) ->
	  addVarRef stbl q name

      | Assign( lineno, lval, rval ) ->
	  addAssign stbl q lval rval

      | Syntree.Literal( lineno, intval ) ->
	  addi (MOVE( eax, Literal( intval ) ))


and compileExprList stbl q exprlst =
  List.iter (compileExpr stbl q) exprlst


and addIf stbl q cond ifpart elsepart =
  let addi = addInstr q in
  let ifblock = makeLabelName "ifblock"
  and elseblock = makeLabelName "elseblock"
  and ifend = makeLabelName "ifend" in
    addi (COMMENT( "Starting if block" ));
    compileExpr stbl q cond;
    addi (COMP( eax, Literal( 0 ) )); (* cond = false? *)
    addi (JNE( ifblock )); (* If cond /= 0, jmp to ifpart *)

    addi (LOCALLABEL( elseblock )); (* Purely for documentation *)
    compileExprList stbl q elsepart;
    addi (JMP( ifend ));

    addi (LOCALLABEL( ifblock ));
    compileExprList stbl q ifpart;
    addi (LOCALLABEL( ifend ));

and addWhile stbl q cond body =
  let addi = addInstr q in
  let whilestart = makeLabelName "whilestart"
  and whileend = makeLabelName "whileend" in
    addi (LOCALLABEL( whilestart ));
    compileExpr stbl q cond;
    addi (COMP( eax, Literal( 0 ) ));
    addi (JE( whileend ));
    compileExprList stbl q body;
    addi (JMP( whilestart ));
    addi (LOCALLABEL( whileend ));

(* ...Okay.  So say we have an operation with 3 args, (+ 1 2 3).  We
   make room for 3 vars, then evaluate each of 'em, which puts their
   result in eax.  We then move the result from eax to the appropriate
   var.  Then we move the first result to eax, and add the rest to it
   left-to-right Amazing how easy it is when you don't bother with
   registers...

   I think it's safe to refer to these temporary values by esp, which
   makes life easy 'cause we don't need to know how many local vars we have.
*)
and addOp stbl q op args =
  let addi = addInstr q in
  let doOp op argloc =
    match op with
	Add -> 
	  addi (ADD( eax, argloc ))
      | Sub -> 
	  addi (SUB( eax, argloc ))

      | Mul ->
	  addi (MUL( eax, argloc ))

      (* Silly intel *)
      | Div ->
	  addi (DIV( argloc ));

      (* More silly intel *)
      | Mod ->
	  addi (DIV( argloc ));
	  addi (MOVE( eax, edx ));

      | And ->
	  addi (AND( eax, argloc ))

      | Or -> 
	  addi (OR( eax, argloc ))

      | Xor ->
	  addi (XOR( eax, argloc ))

      | Not -> 
	  addi (MOVE( eax, argloc ));
	  addi (NOT( eax ));

      (* Logical operators really compile to an itty-bitty "if" statement,
	 'cause I'm unsure how to implement them otherwise.
	 (= a b) -> if (= a b) then 1 else 0 end
      *)
      | Eq -> 
	  let cmptrue = makeLabelName "cmptrue"
	  and cmpend = makeLabelName "cmpend" in
	    addi (COMMENT( "Starting = comparison" ));
	    addi (COMP( eax, argloc ));
	    addi (JE( cmptrue ));
	    addi (MOVE( eax, Literal( 0 )));
	    addi (JMP( cmpend ));
	    addi (LOCALLABEL( cmptrue ));
	    addi (MOVE( eax, Literal( 1 )));
	    addi (LOCALLABEL( cmpend ));
	    
      | Neq ->
	  let cmptrue = makeLabelName "cmptrue"
	  and cmpend = makeLabelName "cmpend" in
	    addi (COMMENT( "Starting /= comparison" ));
	    addi (COMP( eax, argloc ));
	    addi (JNE( cmptrue ));
	    addi (MOVE( eax, Literal( 0 )));
	    addi (JMP( cmpend ));
	    addi (LOCALLABEL( cmptrue ));
	    addi (MOVE( eax, Literal( 1 )));
	    addi (LOCALLABEL( cmpend ));


      | Gt -> 
	  let cmptrue = makeLabelName "cmptrue"
	  and cmpend = makeLabelName "cmpend" in
	    addi (COMMENT( "Starting > comparison" ));
	    addi (COMP( eax, argloc ));
	    addi (JG( cmptrue ));
	    addi (MOVE( eax, Literal( 0 )));
	    addi (JMP( cmpend ));
	    addi (LOCALLABEL( cmptrue ));
	    addi (MOVE( eax, Literal( 1 )));
	    addi (LOCALLABEL( cmpend ));

      | Gte ->
	  let cmptrue = makeLabelName "cmptrue"
	  and cmpend = makeLabelName "cmpend" in
	    addi (COMMENT( "Starting >= comparison" ));
	    addi (COMP( eax, argloc ));
	    addi (JGE( cmptrue ));
	    addi (MOVE( eax, Literal( 0 )));
	    addi (JMP( cmpend ));
	    addi (LOCALLABEL( cmptrue ));
	    addi (MOVE( eax, Literal( 1 )));
	    addi (LOCALLABEL( cmpend ));

      | Lt -> 
	  let cmptrue = makeLabelName "cmptrue"
	  and cmpend = makeLabelName "cmpend" in
	    addi (COMMENT( "Starting < comparison" ));
	    addi (COMP( eax, argloc ));
	    addi (JL( cmptrue ));
	    addi (MOVE( eax, Literal( 0 )));
	    addi (JMP( cmpend ));
	    addi (LOCALLABEL( cmptrue ));
	    addi (MOVE( eax, Literal( 1 )));
	    addi (LOCALLABEL( cmpend ));

      | Lte ->
	  let cmptrue = makeLabelName "cmptrue"
	  and cmpend = makeLabelName "cmpend" in
	    addi (COMMENT( "Starting <= comparison" ));
	    addi (COMP( eax, argloc ));
	    addi (JLE( cmptrue ));
	    addi (MOVE( eax, Literal( 0 )));
	    addi (JMP( cmpend ));
	    addi (LOCALLABEL( cmptrue ));
	    addi (MOVE( eax, Literal( 1 )));
	    addi (LOCALLABEL( cmpend ));

  in

  (* We evaluate all the expressions and store the results on the
     stack.  Then we move the first result to eax, and operate on that...
  *)

  let neededRoom = max ((List.length args) * 4) 0 in
  let nthvar = ref 0 in
    addi (SUB( StackPointer, Literal( neededRoom ) ));
    List.iter (fun x -> compileExpr stbl q x;
		 addi (MOVE( RegOffset( StackPointer, !nthvar ), eax ));
		 nthvar := !nthvar + 4;
	      ) args;
    addi (MOVE( eax, RegOffset( StackPointer, 0 ) ));
    nthvar := 4;
    while !nthvar < neededRoom do
      doOp op (RegOffset( StackPointer, !nthvar ));
      nthvar := !nthvar + 4;
    done;
    addi (ADD( StackPointer, Literal( neededRoom ) ));

and addFuncall stbl q name argexprs =
  let addi = addInstr q in

    (* Okay.  So we first need to find the amount of space we need to
       allocate, then since the location of each arg is handily kept in
       the function record in the symbol table, we grab that and use it
       to find the appropriate offsets for each argument.
       We can't use the vars in the symbol table directly because:
       1) They're unordered, so we'd have to go through interesting
       contortions to put them in the right order
       2) This is outside the scope of the function so they don't 
       actually exist anyway.

       Whew.
    *)
  let argsize = (List.length argexprs) * 4 in
  let argItems = (getFunc stbl name).funcargs in
  let addArg argexpr argitem =
    let varloc = RegOffset( StackPointer, argitem.vloc - 8 ) in
    compileExpr stbl q argexpr;
    addi (MOVE( varloc, eax ));
  in

  (* Okay, this gets a tad trippy.  We move esp to make room for the
     args, then evaluate each arg, then put it into place relative to esp.  
     This is why all local vars and args are relative to ebp, 'cause
     otherwise trying to access them during this would get Interesting.
  *)

    (* Move esp and add args (hopefully sanely) *)
    addi (SUB( StackPointer, Literal( argsize ) ));
    (*List.iter (addArg (getFunc stbl name)) argexprs;*)
    List.iter2 addArg argexprs argItems;

    (* Call it and clean up.  Return value is already in eax.
    *)
    addi (CALL( name ));
    addi (ADD( StackPointer, Literal( argsize ) ));

    (* Print return value for debugging *)
    addi (SUB( StackPointer, Literal( 4 ) ));
    addi (MOVE( RegOffset( StackPointer, 0 ), eax ));
    addi (CALL( "printIntC"));
    addi (MOVE( eax, RegOffset( StackPointer, 0 ) ));
    addi (ADD( StackPointer, Literal( 4 ) ));
    
    

and addVarRef stbl q name =
  let addi = addInstr q in
  let varloc = getVarOffset name stbl in
    addi (MOVE( eax, varloc ));

and addAssign stbl q lval rval =
  let addi = addInstr q in
  let lvalname = 
    (match lval with
	 VarRef( line, name ) -> name
       | _ -> ErrorReport.errorAndDie
	   ("Assign not given a valid lvalue, and semantic checking didn't find it.  Hail the revolution!"))
  in
  let lvalloc = getVarOffset lvalname stbl in
    compileExpr stbl q rval;
    addi (MOVE( lvalloc, eax ))
;;


let compileJunk stbl decls filename =
  let q = makeModule filename in
    (* Add library routines *)
    addImport q "printIntC";
    addImport q "printCharC";
    addImport q "printNLC";
    List.iter (compileDecl stbl q) decls;
    (*printSymtbl stbl; *)
    q      
;;
