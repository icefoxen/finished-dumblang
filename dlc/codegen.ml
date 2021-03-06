(* Actually compiles the stuff to asm...
   We have to modify it to take IR now, instead of the syntax tree.

   Okay, a bit of actual documentation of the binary interface.
   1) We treat the floating-point stack like a stack.  All operations
   happen to st0.
   2) We don't bother with register allocation.  Every expression
   returns it's value in eax, and every expression is composed of sub-
   expressions that leave /their/ value eax, so.  Intermediate values
   are stored on the stack, and we move esp to make room for 'em.
   3) Functions return everything in eax.  This will change; later on they'll
   (maybe) return small things in eax and big things on the stack.  This won't
   take extra space on the stack, since we can overwrite the arguments with
   the return value (if the arguments are bigger; otherwise we need to
   allocate more space.)
   4) Functions take their arguments on the stack, and I think they're
   pushed in left-to-right order.  I believe this is opposite of normal
   C calling conventions.  Boo hoo.
*)

open Symtbl
open Syntree
open Ir
open CodegenHelp




(* Computes offsets of locals and arguments, and saves 'em 
   The variables are put on the stack in the order the hasthable runs
   across 'em.  The technical name for this is Any Old Order.

   However, a copy of the args in list-form is kept with each function
   in the symbol table also, so we also update the locations there.
   This is necessary so caller functions know where to put arguments for
   the callee.

   ...hrm.  Okay, we need to call this in a sane way on all the 
   functions that have been imported, so we know where their function
   args should be.  However, we do this for normal functions during IR 
   analysis, but imported symbols don't actually have any IR,
   soooo...
   See function computeImportedStackOffsets

   This actually doesn't seem too bad.  Needs to be fixed to handle variable-
   size types, but that shouldn't be hard to finagle.
*)
let computeStackOffsets stbl fname stackframe =

  (* This should return a list of symbols, not a list of arg types.
     We want the actual variables! *)
  let fargs = getFuncArgs (getFunc stbl fname) in
    
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
      | GLOBAL -> () (* XXX: globals don't exist yet. *)
	  
  in
    Hashtbl.iter computeOffset stackframe;
;;

(* XXX:
   This is a horrible, twisted, ugly, nasty, terrible kludge.
   But it works.
   Ideally, we should be able to combine it somehow with the above
   func.  But the above func is something of a kludge anyway.
   ...anyway.  Maybe we'll clean it up in some other version...?

   ...wait, how is it at all different from the above function?
   Hmm, in that it goes over fargs instead of the stackframe...
   What exactly does that mean?
*)
let computeImportedStackOffsets stbl fname =
  let fargs = getFuncArgs (getFunc stbl fname) in
  let argloc = ref 8 in
  let computeOffset var =
    match var.valloc with
	ARG ->
	  var.vloc <- !argloc;
	  argloc := !argloc + 4
      | _ -> ErrorReport.errorAndDie "Squark?"
  in
    List.iter computeOffset fargs
;;

let getVarOffset name stbl =
  let var = getSymbol stbl name in 
    match var.valloc with
	ARG -> RegOffset( BasePointer, var.vloc )
      | LOCAL -> RegOffset( BasePointer, -var.vloc )
      | GLOBAL -> RegOffset( BasePointer, var.vloc ) (* XXX *)
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
  mov ebp, esp
  sub esp, (# of args * 4)

  (locals are currently always 4 bytes, y'see)
  They won't always be, though.
*)
let addFuncProlog addi name frame=
  let localSize = (numLocalsInFrame frame) * 4 in
    addi (COMMENT( "Start function " ^ name ));
    addi (LABEL( name ));
    addi (SUB( StackPointer, Literal( 4l ) ));
    addi (MOVE( (RegOffset( StackPointer, 0 ) ), BasePointer ));
    addi (MOVE( BasePointer, StackPointer ));
    addi (SUB( StackPointer, Literal( (Int32.of_int localSize) ) ));
    addi (COMMENT( "Func prolog done" ));
;;

(*
  mov esp, ebp
  mov ebp, [esp]
  add esp, 4
  ret
*)
let addFuncEpilog addi =
  addi (COMMENT( "Func epilog" ));
  addi (MOVE( StackPointer, BasePointer ));
  addi (MOVE( BasePointer, (RegOffset( StackPointer, 0 )) ));
  addi (ADD( StackPointer, Literal( 4l ) ));
  addi (RET);
  addi (COMMENT( "End function\n" ));
;;



(* ...aarg.  Okay.  So each scope block (function, for now) has a chunk of
  the symbol table attached to it, containing it's scope.  We use it to
   figure locals, and we push it to the symbol table again before we mess
   with the body of the function, so everything should be hunky-dory.


*)

let rec compileDeclIr stbl q decl =
  match decl with
      Fundecl( name, args, rettype, ir, locals ) ->
	let addi = addInstr q in
	let namestr = id2name name stbl in 
	  if (Modules.isExported stbl name) then
	    addExport q namestr;

	  computeStackOffsets stbl name !locals; 
	  addFuncProlog addi namestr !locals;

	  setTopScope stbl !locals;
	  compileIrExprListQ stbl q ir;
	  popScope stbl;

	  addFuncEpilog addi;
    | NopDecl -> ()


(* Let's just say... all exprs exchange leave their return value in eax.
   Since we return things in eax, that makes life easy for now.

   The Q versions of this function and the next take an actual instruction
   queue (well, CodegenHelp.asmModule objects), where as the normal ones
   just take a function that add the given instruction to whatever queue
   exists.
*)
and compileIrExprQ stbl q expr =
  (* Huzzah!  Partial application! *)
  let addi = addInstr q in
    compileIrExpr stbl addi expr

and compileIrExpr stbl addi expr =
  match expr with 
      VarDeclStm( name, typ, value ) ->
	let varloc = getVarOffset name stbl in
	  compileIrExpr stbl addi value;
	  addi (MOVE( varloc, eax ));

    | IfStm( cond, ifpart, elsepart ) ->
	addIf stbl addi cond ifpart elsepart

    | LoopStm( cond, body ) ->
	addWhile stbl addi cond body
	  
    | OpStm( op, args ) ->
	addOp stbl addi op args

    | FuncallStm( name, args ) ->
	addFuncall stbl addi name args

    | VarStm( name ) ->
	addVarRef stbl addi name

    | AssignStm( lval, rval ) ->
	addAssign stbl addi lval rval

    (* XXX: Make this work with array lits *)
    | LiteralStm( vl ) ->
	addLiteral stbl addi vl;

    | CastStm( totype, fromtype, fromexpr ) ->
	addCast stbl addi totype fromtype fromexpr
	  
    (* XXX: Make this work with array lits *)
    | ArefStm( array, index ) ->
	addAref stbl addi array index

    | AddrStm( stm ) -> 
	addAddr stbl addi stm

    | DerefStm( stm ) -> 
	addDeref stbl addi stm

    | GotoStm( label ) ->
          addi (JMP( label ))

    | LabelStm( label ) ->
          addi (LABEL( label ))

    | NopStm -> print_endline "Nop!  Nop!  Nop!";  ()


and compileIrExprListQ stbl q exprlst =
  List.iter (compileIrExprQ stbl q) exprlst

and compileIrExprList stbl addi exprlst =
  List.iter (compileIrExpr stbl addi) exprlst


and addIf stbl addi cond ifpart elsepart =
  let ifblock = makeLocalLabelName "ifblock"
  and elseblock = makeLocalLabelName "elseblock"
  and ifend = makeLocalLabelName "ifend" in
    addi (COMMENT( "Starting if block" ));
    compileIrExpr stbl addi cond;
    addi (COMP( eax, Literal( 0l ))); (* cond = false? *)
    addi (JNE( ifblock )); (* If cond /= 0, jmp to ifpart *)

    addi (LABEL( elseblock )); (* Purely for documentation *)
    compileIrExprList stbl addi elsepart;
    addi (JMP( ifend ));

    addi (LABEL( ifblock ));
    compileIrExprList stbl addi ifpart;
    addi (LABEL( ifend ));

and addWhile stbl addi cond body =
  let whilestart = makeLocalLabelName "whilestart"
  and whileend = makeLocalLabelName "whileend" in
    addi (LABEL( whilestart ));
    compileIrExpr stbl addi cond;
    addi (COMP( eax, Literal( 0l ) ));
    addi (JE( whileend ));
    compileIrExprList stbl addi body;
    addi (JMP( whilestart ));
    addi (LABEL( whileend ));

(* ...Okay.  So say we have an operation with 3 args, (+ 1 2 3).  We
   make room for 3 vars, then evaluate each of 'em, which puts their
   result in eax.  We then move the result from eax to the appropriate
   var.  Then we move the first result to eax, and add the rest to it
   left-to-right Amazing how easy it is when you don't bother with
   registers...

   I think it's safe to refer to these temporary values by esp, which
   makes life easy 'cause we don't need to know how many local vars we have.

   ...ye gods though, this is a horrid function.  Not so bad if you split
   it up right, but still twisted.
*)
and addOp stbl addi op args =
  let doOp op argloc =

    (* Add a comparison *)
    let addComp comptype =
      let cmptrue = makeLocalLabelName "cmptrue"
      and cmpend = makeLocalLabelName "cmpend" in

      let comp = match comptype with
	  IntEq -> (JE( cmptrue ))
	| IntNeq -> (JNE( cmptrue ))
	| IntGt -> (JG( cmptrue ))
	| IntLt -> (JL( cmptrue ))
	| IntGte -> (JGE( cmptrue ))
	| IntLte -> (JLE( cmptrue ))
	| _ -> ErrorReport.errorAndDie "Empients are attacking!"
      in
	addi (COMP( eax, argloc ));
	addi comp;
	addi (MOVE( eax, Literal( 0l )));
	addi (JMP( cmpend ));
	addi (LABEL( cmptrue ));
	addi (MOVE( eax, Literal( 1l )));
	addi (LABEL( cmpend ));
    in

    (* ...rrgl.  FP expr conventions are a bit wiggy, since we can't
       touch eax easily...    *)
    let addFpComp comptype =
      let cmptrue = makeLocalLabelName "cmptrue"
      and cmpend = makeLocalLabelName "cmpend" in

      let comp = match comptype with
	  FloatEq -> (FJE( cmptrue ))
	| FloatNeq -> (FJNE( cmptrue ))
	| FloatGt -> (FJG( cmptrue ))
	| FloatLt -> (FJL( cmptrue ))
	| FloatGte -> (FJGE( cmptrue ))
	| FloatLte -> (FJLE( cmptrue ))
	| _ -> ErrorReport.errorAndDie "Floating Empients are attacking!"
      in
	(* ...hm.  FCOMI takes two fp-reg locations... *)
      let tmpLoc = RegOffset( StackPointer, 0 ) in
	addi (FLOAD( argloc ));
	addi (FLOAD( tmpLoc ));
	addi (FCOMP);
	addi comp;
	addi (MOVE( eax, Literal( 0l )));
	addi (JMP( cmpend ));
	addi (LABEL( cmptrue ));
	addi (MOVE( eax, Literal( 1l )));
	addi (LABEL( cmpend ));

    in

    (* ...ugh.  We can't move the stack pointer because it'll mess with
       the argloc.  But I should never touch below the stackpointer, and I do.
       I'm not quite sure how to solve this.  But it doesn't actually seem to
       cause any problems with funcalls, so...
       No, it never causes a problem, because while the temploc is below
       the stack pointer, it is only used for this op, and this op never
       calls other ops.  
    *)
    let addFpMath mathtype argloc =
      let tempLoc = RegOffset( StackPointer, -4 ) in
      let op = match mathtype with
	  FloatAdd -> FADD( argloc )
	| FloatSub -> FSUB( argloc )
	| FloatMul -> FMUL( argloc )
	| FloatDiv -> FDIV( argloc )
	| FloatMod -> FMOD( argloc )
	| _ -> ErrorReport.errorAndDie "Help!  H'riak!"
      in
	(*
          addi (SUB( StackPointer, Literal( 4l ) ));
	*)
	addi (MOVE( tempLoc, eax ) );
	addi (FLOAD( tempLoc ));
	addi op;
	addi (FSTORE( tempLoc ));
	addi (MOVE( eax, tempLoc ));
	(*
	  addi (ADD( StackPointer, Literal( 4l ) ));
	*)
    in

      match op with
	  IntAdd -> addi (ADD( eax, argloc ));
	| IntSub -> addi (SUB( eax, argloc ));
	| IntMul -> addi (MUL( eax, argloc ));
	    (* Silly Intel *)
	| IntDiv -> 
	    addi (MOVE( edx, Literal( 0l ) ));
	    addi (DIV( argloc ));
	    (* More silly Intel *)
	| IntMod -> 
	    addi (MOVE( edx, Literal( 0l ) ));
	    addi (DIV( argloc ));
	    addi (MOVE( eax, edx ));

	| IntAnd ->
	    addi (AND( eax, argloc ))

	| IntOr -> 
	    addi (OR( eax, argloc ))

	| IntXor ->
	    addi (XOR( eax, argloc ))

	| IntNot -> 
	    print_endline "Bop!  Help!  Vreet!  Squawk!";
	    addi (MOVE( eax, argloc ));
	    addi (NOT( eax ));


	| IntEq
	| IntNeq
	| IntGt
	| IntLt
	| IntGte
	| IntLte  -> addComp op

	(* ...oy.  Hmmm. 

	   Well, all type conversions in the IR are explicit, so any
	   variables we get should be of the correct type.
	   Hopefully.

	   And I THINK this will work...
	   No, we need a temporary location for FP stuff...
	   Can we do it all in the FP stack?  Theoretically, since
	   everything will coerce to a float, but I don't wanna bother
	   making sure.  That's for the optimizer to take care of.
	*)
	| FloatAdd 

	| FloatSub 
	| FloatMul 

	| FloatDiv 
	| FloatMod -> addFpMath op argloc

	| FloatEq 
	| FloatNeq
	| FloatGt 
	| FloatLt 
	| FloatGte
	| FloatLte -> addFpComp op
  in

  (* We evaluate all the expressions and store the results on the
     stack.  Then we move the first result to eax, and operate on that...
  *)
  let neededRoom = ((List.length args) * 4) in
  let nthvar = ref 0 in
    addi (SUB( StackPointer, Literal( (Int32.of_int neededRoom) ) ));
    List.iter (fun x -> compileIrExpr stbl addi x;
		 addi (MOVE( RegOffset( StackPointer, !nthvar ), eax ));
		 nthvar := !nthvar + 4;
	      ) args;

    (* We move the first arg to eax... *)
    addi (MOVE( eax, RegOffset( StackPointer, 0 ) ));
    nthvar := 4;
    while !nthvar < neededRoom do
      doOp op (RegOffset( StackPointer, !nthvar ));
      nthvar := !nthvar + 4;
    done;
    addi (ADD( StackPointer, Literal( (Int32.of_int neededRoom) ) ));

and addFuncall stbl addi name argexprs =
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
  let argItems = (getFuncArgs (getSymbol stbl name)) in
  let addArg argexpr argitem =
    let varloc = RegOffset( StackPointer, argitem.vloc - 8 ) in
      compileIrExpr stbl addi argexpr;
      addi (MOVE( varloc, eax ));
  in

    (* Okay, this gets a tad trippy.  We move esp to make room for the
       args, then evaluate each arg, then put it into place relative to esp.  
       This is why all local vars and args are relative to ebp, 'cause
       otherwise trying to access them during this would get Interesting.

       ...um, or we could just use the push instruction... bah, that's for
       losers.  And doesn't handle 8-byte values (as if we do now)
       XXX: To be vaguely compliant with C calling conventions, the args
       should be pushed right-to-left!
    *)

    (* Move esp and add args (hopefully sanely) *)
    addi (SUB( StackPointer, Literal( (Int32.of_int argsize) ) ));
    (*List.iter (addArg (getFunc stbl name)) argexprs;*)
    List.iter2 addArg argexprs argItems;

    (* Call it and clean up.  Return value is already in eax.
    *)
    addi (CALL( (id2name name stbl) ));
    addi (ADD( StackPointer, Literal( (Int32.of_int argsize) ) ));

    (* Print return value for debugging *)
    let rettype = (Symtbl.getFuncRettype (Symtbl.getSymbol stbl name)) in
      addi (SUB( StackPointer, Literal( 4l ) ));
      addi (MOVE( RegOffset( StackPointer, 0 ), eax ));
      if rettype = Syntree.floattype then
	addi (CALL( "printFloatC" ))
      else
	addi (CALL( "printIntC"));
      addi (MOVE( eax, RegOffset( StackPointer, 0 ) ));
      addi (ADD( StackPointer, Literal( 4l ) ));
      
      

and addVarRef stbl addi name =
  let varloc = getVarOffset name stbl in
    addi (MOVE( eax, varloc ));

and addAssign stbl addi lval rval =
  match lval with
      VarStm( name ) ->
	let lvalloc = getVarOffset name stbl in
	  compileIrExpr stbl addi rval;
	  addi (MOVE( lvalloc, eax ))

    (* Hmmm...  Cache the result of rval, get the result of lval, and
       move the one to the other.
       Yay, we actually use a register other than eax!
       I THINK this works...  Fuck, what if there's more than one deref?
       No, I think that actually works then, 'cause lval gets evaluated
       like normal.
       Crap, but then the lval ends up being the value of the pointer,
       not the pointer itself.
    *)
    | DerefStm( s ) ->
	compileIrExpr stbl addi rval;
	addi (MOVE( ebx, eax ));
(*
	(match expr with
	     VarStm( name ) ->
	       let var = getSymbol stbl name in 
	       let varaddr = 
		 match var.valloc with
		     ARG -> var.vloc
		   | LOCAL -> -var.vloc
		   | GLOBAL -> var.vloc (* XXX *)
	       in
		 addi (MOVE( eax, BasePointer ));
		 addi (ADD( eax, Literal( Int32.of_int varaddr ) ))
	   | _ -> ErrorReport.errorAndDie "You kung-fu is no damn good!  I keel you!"
	)
*)

	  compileIrExpr stbl addi s;
	  addi (MOVE( RegOffset( eax, 0 ), ebx ))
	  

    | _ -> ErrorReport.errorAndDie
	("Assign not given a valid lvalue, and semantic checking didn't find it.  Hail the revolution!")

and addLiteral stbl addi lit =
  match lit with
      Ir.Intlit( i ) -> 
	addi (MOVE( eax, Literal( (Int32.of_int i) ) ))
	  
    (* ...stupid nasm not doing float literals... 
       ...stupid ocaml not doing 32-bit ints...
    *)
    | Ir.Floatlit( f ) ->
	addi (MOVE( eax, Literal( FpConverter.float32Repr f ) ))
    | Ir.Arraylit( itms ) ->  ()
          (*
	let arraydecl = declArrayFromList "_" itms in
addi (MOVE( eax, XXX address of arraydecl ))
*)

(* int->float = fild, fstp
   float->int = fld, fistp
*)
and addCast stbl addi totype fromtype fromexpr =
  compileIrExpr stbl addi fromexpr;
  let tmpposition = RegOffset( StackPointer, 0 ) in
    addi (SUB( StackPointer, Literal(  4l ) ));
    addi (MOVE( tmpposition, eax ));
    if fromtype = inttype && totype = floattype then (
      addi (FLOADINT( tmpposition ));
      addi (FSTORE( tmpposition ));
    ) else if fromtype = floattype && totype = inttype then (
      addi (FLOAD( tmpposition ));
      addi (FSTOREINT( tmpposition ));
    ) else if (isRefType fromtype) && totype = inttype then (
      (* print_endline "From ref to int"; *)
      ()
    ) else if (isRefType totype) && fromtype = inttype then (
      (* print_endline "From int to ref"; *)
      ()
    ) else (
      let errmsg = "Frinkfrinkfinkr... wheeoooo!  Bad cast!\nFrom " ^
	(Symtbl.type2str fromtype) ^ " to " ^
	(Symtbl.type2str totype) in 
	ErrorReport.errorAndDie errmsg
    );
    addi (MOVE( eax, tmpposition ));
    addi (ADD( StackPointer, Literal( 4l ) ));


and addAref stbl addi array index =
  ()

(*  (! foo) <- (! bar) I THINK works, but addAssign needed a bit of work
    ...this does NOT in fact work.  We need the ADDRESS of the variable,
    compileIrExpr returns the VALUE.
*)
and addDeref stbl addi expr =
  (*  match expr with
      VarStm( name ) ->
      let var = getSymbol stbl name in 
      let varaddr = 
      match var.valloc with
      ARG -> var.vloc
      | LOCAL -> -var.vloc
      | GLOBAL -> var.vloc (* XXX *)
      in
      addi (MOVE( eax, BasePointer ));
      addi (ADD( eax, Literal( Int32.of_int varaddr ) ));
      
      | _ -> ErrorReport.errorAndDie "You bastard!"
  *)

  compileIrExpr stbl addi expr;
  addi (MOVE( ecx, RegOffset( eax, 0 ) ));
  addi (MOVE( eax, ecx ))

and addAddr stbl addi expr =
  match expr with
      VarStm( name ) ->
	let var = getSymbol stbl name in 
	let varaddr = 
	  match var.valloc with
	      ARG -> var.vloc
	    | LOCAL -> -var.vloc
	    | GLOBAL -> var.vloc (* XXX *)
	in
	  addi (MOVE( eax, BasePointer ));
	  addi (ADD( eax, Literal( Int32.of_int varaddr ) ))
    | _ -> ErrorReport.errorAndDie "You bastard!"
	
;;


(* This adds the "main" function, or rather, whatever stub we might need
   to make the OS call the function we want called.
   ...how do we decide which function we want called?  Just "main"?  The
   function with the module name?  Something else?
   ...hang on, C handles it in the library.  And it's really kinda the linker's
   job.  And... um, OCaml lets one have arbitrary statements outside of
   functions, more or less (let _ = ... ;;).  

   Hrm...  Modula-3 uses a Main module, but module != filename for it, recall.
   That might be a good idea for us, too...
   We could add a decleration, but that's just a little oooogly.
   
   Hokay.  If there exists a function that has the same name as the
   module, then that's the main func.

   Ooooor.  If there's a function called "main" in this module, we make
   another func to call it.  You don't have to export it, even.
   
*)
let addMainFunc stbl q =
  let mainid = makeId "main" emptyModule in
    if (symbolExists stbl mainid) then
      if (idIsFunc stbl mainid) then (
	(* Oops.  This still mangles the name with the module!  ^_^; *)
	(*	let mainfunc = 
		Fundecl( (makeId "main" emptyModule) , [], inttype, 
		[FuncallStm( mainid, [] )], ref (Hashtbl.create 0) ) in
		compileDeclIr stbl q mainfunc;
	*)
	
	(* This might be non-ideal since I don't know if we return
	   the return value the module-main gave us.  ...we should, though. *)
	let name = "main" in
	let addi = addInstr q in
	  addExport q name;
	  addFuncProlog addi name (Hashtbl.create 1);
	  compileIrExprList stbl addi [FuncallStm( mainid, [] )];
	  addFuncEpilog addi;
	  
      )
      else
	()
    else
      ();

;;

let compileJunk stbl decls filename =
  let q = makeModule filename in
    Hashtbl.iter (fun x y -> computeImportedStackOffsets stbl x)
      (List.hd stbl.symtbl);
    (* Add library routines *)
    addImport q "printIntC";
    addImport q "printCharC";
    addImport q "printNLC";
    addImport q "printFloatC";
    List.iter (compileDeclIr stbl q) decls;
    addImports q stbl;
    addMainFunc stbl q;
    (*printSymtbl stbl; *)
    q      
;;
