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
   return small things in eax and big things on the stack.  This won't
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


type location =
    Mem of int
  | Reg of int
  | Literal of int32
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

  (* Function *)
  | CALL of string
  | RET

  (* Floating point --all floating point ops only take a 
     memory location, because they want to be weird.  At least div isn't
     a special case.
  *)
  | FLOAD of location
  | FSTORE of location
  | FLOADINT of location
  | FSTOREINT of location
  | FADD of location
  | FSUB of location
  | FMUL of location
  | FDIV of location
  | FMOD of location

  (* Need to figure out how the hell FP comparison works... oh, it's ugly 
     ...hmm, not so much, actually.  Thank gods for fcomi.
  *)
  | FCOMP of location


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

let makeLocalLabelName name =
  "." ^ name ^ (gensym ())
;;

(* We need to be able to print the binary representation of a float... *)
let float2binary f =
  ()
;;

(* Value for "No return type": 0xFEEDAAE0 
   For debugging.  It works if you kinda squint at it.
*)


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
  mov ebp, esp
  sub esp, (# of args * 4)

  (locals are currently always 4 bytes, y'see)
*)
let addFuncProlog q name frame=
  let localSize = (numLocalsInFrame frame) * 4 in
    addInstr q (COMMENT( "Start function " ^ name ));
    addInstr q (LABEL( name ));
    addInstr q (SUB( StackPointer, Literal( 4l ) ));
    addInstr q (MOVE( (RegOffset( StackPointer, 0 ) ), BasePointer ));
    addInstr q (MOVE( BasePointer, StackPointer ));
    addInstr q (SUB( StackPointer, Literal( (Int32.of_int localSize) ) ));
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
  addInstr q (ADD( StackPointer, Literal( 4l ) ));
  addInstr q (RET);
  addInstr q (COMMENT( "End function\n" ));
;;



(* ...aarg.  Okay.  So each scope block (function, for now) has a chunk of
  the symbol table attached to it, containing it's scope.  We use it to
   figure locals, and we push it to the symbol table again before we mess
   with the body of the function, so everything should be hunky-dory.
*)

let rec compileDeclIr stbl q decl =
  match decl with
      Fundecl( name, args, rettype, ir, locals ) ->
	addExport q name;

	computeStackOffsets stbl name !locals;
	addFuncProlog q name !locals;

	setTopScope stbl !locals;
	compileIrExprList stbl q ir;
	popScope stbl;

	addFuncEpilog q;


(* Let's just say... all exprs exchange leave their return value in eax.
   Since we return things in eax, that makes life easy for now.
*)
and compileIrExpr stbl q expr =
  let addi = addInstr q in
    match expr with 
	VarDeclStm( name, typ, value ) ->
	  let varloc = getVarOffset name stbl in
	    compileIrExpr stbl q value;
	    addi (MOVE( varloc, eax ));

      | IfStm( cond, ifpart, elsepart ) ->
	  addIf stbl q cond ifpart elsepart

      | LoopStm( cond, body ) ->
	  addWhile stbl q cond body
	    
      | OpStm( op, args ) ->
	  addOp stbl q op args

      | FuncallStm( name, args ) ->
	  addFuncall stbl q name args

      | VarStm( name ) ->
	  addVarRef stbl q name

      | AssignStm( lval, rval ) ->
	  addAssign stbl q lval rval

      | LiteralStm( vl ) ->
	  addLiteral stbl q vl;

      | CallStm( name, argexprs ) ->
	  addFuncall stbl q name argexprs

      | CastStm( totype, fromtype, fromexpr ) ->
	  addCast stbl q totype fromtype fromexpr

      | NopStm -> ()


and compileIrExprList stbl q exprlst =
  List.iter (compileIrExpr stbl q) exprlst


and addIf stbl q cond ifpart elsepart =
  let addi = addInstr q in
  let ifblock = makeLocalLabelName "ifblock"
  and elseblock = makeLocalLabelName "elseblock"
  and ifend = makeLabelName "ifend" in
    addi (COMMENT( "Starting if block" ));
    compileIrExpr stbl q cond;
    addi (COMP( eax, Literal( 0l ))); (* cond = false? *)
    addi (JNE( ifblock )); (* If cond /= 0, jmp to ifpart *)

    addi (LABEL( elseblock )); (* Purely for documentation *)
    compileIrExprList stbl q elsepart;
    addi (JMP( ifend ));

    addi (LABEL( ifblock ));
    compileIrExprList stbl q ifpart;
    addi (LABEL( ifend ));

and addWhile stbl q cond body =
  let addi = addInstr q in
  let whilestart = makeLocalLabelName "whilestart"
  and whileend = makeLocalLabelName "whileend" in
    addi (LABEL( whilestart ));
    compileIrExpr stbl q cond;
    addi (COMP( eax, Literal( 0l ) ));
    addi (JE( whileend ));
    compileIrExprList stbl q body;
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

   XXX: Equality doesn't work right!
   (= x y z) turns into ((x = y) = z), not (x = y) && (y = z)!
   Very rarely will the result of (x = y) equal z!
   We should solve that in the IR conversion.
*)
and addOp stbl q op args =
  let addi = addInstr q in
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
	  FloatEq -> (JE( cmptrue ))
	| FloatNeq -> (JNE( cmptrue ))
	| FloatGt -> (JG( cmptrue ))
	| FloatLt -> (JL( cmptrue ))
	| FloatGte -> (JGE( cmptrue ))
	| FloatLte -> (JLE( cmptrue ))
	| _ -> ErrorReport.errorAndDie "Floating Empients are attacking!"
      in
      let tmpLoc = RegOffset( StackPointer, -4 ) in
	addi (MOVE( tmpLoc, eax ));
	addi (FLOAD( tmpLoc ));
	addi (FCOMP( argloc ));
	addi comp;
	addi (MOVE( eax, Literal( 0l )));
	addi (JMP( cmpend ));
	addi (LABEL( cmptrue ));
	addi (MOVE( eax, Literal( 1l )));
	addi (LABEL( cmpend ));

    in

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
	addi (MOVE( tempLoc, eax ) );
	addi (FLOAD( tempLoc ));
	addi op;
	addi (FSTORE( tempLoc ));
	addi (MOVE( eax, tempLoc ))
    in

      match op with
	  IntAdd -> addi (ADD( eax, argloc ));
	| IntSub -> addi (SUB( eax, argloc ));
	| IntMul -> addi (MUL( eax, argloc ));
	    (* Silly Intel *)
	| IntDiv -> addi (DIV( argloc ));
	    (* More silly Intel *)
	| IntMod -> 
	    addi (DIV( argloc ));
	    addi (MOVE( eax, edx ));

	| IntAnd ->
	    addi (AND( eax, argloc ))

	| IntOr -> 
	    addi (OR( eax, argloc ))

	| IntXor ->
	    addi (XOR( eax, argloc ))

	| IntNot -> 
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
	   making sure.
	   I think that, in this one case only, we can safely touch
	   esp-4.  Or we can just move esp...  That might be a better idea.
	   Except it'll make argloc invalid.  Let's poke esp-4 and see
	   what happens.

	   XXX: It might not be a bad idea to just break off ALL FP
	   expressions into another function.
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


  (*	    Add -> 
	    addi (ADD( eax, argloc ))
	    | Sub -> 
	    addi (SUB( eax, argloc ))

	    | Mul ->
	    addi (MUL( eax, argloc ))


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
	    | Eq
	    | Neq
	    | Gt
	    | Gte
	    | Lt
	    | Lte -> addComp op
  *)
  in

  (* We evaluate all the expressions and store the results on the
     stack.  Then we move the first result to eax, and operate on that...
  *)
  let neededRoom = ((List.length args) * 4) in
  let nthvar = ref 0 in
    addi (SUB( StackPointer, Literal( (Int32.of_int neededRoom) ) ));
    List.iter (fun x -> compileIrExpr stbl q x;
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
      compileIrExpr stbl q argexpr;
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
    addi (CALL( name ));
    addi (ADD( StackPointer, Literal( (Int32.of_int argsize) ) ));

    (* Print return value for debugging *)
    addi (SUB( StackPointer, Literal( 4l ) ));
    addi (MOVE( RegOffset( StackPointer, 0 ), eax ));
    addi (CALL( "printIntC"));
    addi (MOVE( eax, RegOffset( StackPointer, 0 ) ));
    addi (ADD( StackPointer, Literal( 4l ) ));
    
    

and addVarRef stbl q name =
  let addi = addInstr q in
  let varloc = getVarOffset name stbl in
    addi (MOVE( eax, varloc ));

and addAssign stbl q lval rval =
  let addi = addInstr q in
  let lvalname = 
    (match lval with
	 VarStm( name ) -> name
       | _ -> ErrorReport.errorAndDie
	   ("Assign not given a valid lvalue, and semantic checking didn't find it.  Hail the revolution!"))
  in
  let lvalloc = getVarOffset lvalname stbl in
    compileIrExpr stbl q rval;
    addi (MOVE( lvalloc, eax ))

and addLiteral stbl q lit =
  let addi = addInstr q in
    match lit with
	Ir.Intlit( i ) -> 
	  addi (MOVE( eax, Literal( (Int32.of_int i) ) ))
	  
      (* ...stupid nasm not doing float literals... 
	 ...stupid ocaml not doing 32-bit ints...
      *)
      | Ir.Floatlit( f ) ->
	  addi (MOVE( eax, Literal( FpConverter.float32Repr f ) ))

and addCast stbl q totype fromtype fromexpr =
  if fromtype = "int" && totype = "float" then
    ()
 else if fromtype = "float" && totype = "int" then
   ()
 else
   ErrorReport.errorAndDie "Frinkfrinkfrink... wheeooooo!"
;;


let compileJunk stbl decls filename =
  let q = makeModule filename in
    (* Add library routines *)
    addImport q "printIntC";
    addImport q "printCharC";
    addImport q "printNLC";
    List.iter (compileDeclIr stbl q) decls;
    (*printSymtbl stbl; *)
    q      
;;
