(* irTransform.ml
   We're going to be transforming the IR in a series of passes, each
   bringing it closer to machine language.
   Most of these passes will just be ir-to-ir.

   The number of passes is probably wastefully inefficient, but is also
   probably rather simpler than doing every transformation at once.
*)

open Ir


(* First: Constant folding *)

let isFloatOp op =
  false
;;
(*
let opFunc = function
    IntAdd ->   (+)
  | IntSub ->   (-)
  | IntMul ->   ( * )
  | IntDiv ->   (/)
  | IntMod ->   (mod)
  | FloatAdd -> (+.)
  | FloatSub -> (-.)
  | FloatMul -> ( *. )
  | FloatDiv -> (/.)
  | FloatMod -> (/.)
  | IntAnd ->   (land)
  | IntOr ->    (lor)
  | IntNot ->   (lnot)
  | IntXor ->   (lxor)
      
  | IntEq ->    (=)
  | IntNeq ->   (<>)
  | IntGt ->    (>)
  | IntLt ->    (<)
  | IntGte ->   (>=)
  | IntLte ->   (<=)
  | FloatEq ->  (=)
  | FloatNeq -> (<>)
  | FloatGt ->  (>)
  | FloatLt ->  (<)
  | FloatGte -> (>=)
  | FloatLte -> (<=)
;;
*)

(*
let foldIntAdd args =
  OpStm( IntAdd, args )
;;

let foldFloatAdd args =
  OpStm( IntAdd, args )
;;

let foldIntMul args =
  OpStm( IntAdd, args )
;;

let foldFloatMul args =
  OpStm( IntAdd, args )
;;

let foldIntSub args =
  OpStm( IntAdd, args )
;;

let foldFloatSub args =
  OpStm( IntAdd, args )
;;

let foldIntDiv args =
  OpStm( IntAdd, args )
;;

let foldFloatDiv args =
  OpStm( IntAdd, args )
;;


let foldConstants irlst = 
  let rec foldStm = function
      OpStm( op, args ) ->
	let newargs = foldStmList args in
	  match op with
	      IntAdd -> foldIntAdd newargs
	    | FloatAdd -> foldFloatAdd newargs


	    | IntMul -> foldIntMul newargs
	    | FloatMul -> foldFloatMul newargs

	    | IntSub -> foldIntSub newargs
	    | FloatSub -> foldFloatSub newargs

	    | IntDiv -> foldIntDiv newargs
	    | FloatDiv -> foldFloatDiv newargs

	    | a -> OpStm( op, newargs )

	  (*
        let newargs = foldStmList args in
        let isFloat = isFloatOp op in
        let loop arglst accm literal =
          if arglst = [] then
            OpStm( op, [Intlit( literal ); accm] )
          else
            let arg = List.hd arglst
            and tail = List.tl arglst in
              match arg with
		  LiteralStm( l ) -> doStuffToLiteral op l literal
		| a -> a
	in
	  loop newargs [] 0
	       *)



    | CastStm( totype, fromtype, fromexpr ) ->
	let newfrom = foldStm fromexpr in
	  (match newfrom with
	      LiteralStm( Intlit( i ) ) ->
		if totype = Syntree.inttype then
		  LiteralStm( Intlit( i ) )
		else if totype = Syntree.floattype then
		  LiteralStm( Floatlit( float_of_int i ) )
		else
		  CastStm( totype, fromtype, newfrom )
	    | LiteralStm( Floatlit( i ) ) ->
		if totype = Syntree.inttype then
		    LiteralStm( Intlit( int_of_float i ) )
		  else if totype = Syntree.floattype then
		    LiteralStm( Floatlit( i ) )
		  else
		    CastStm( totype, fromtype, newfrom )
	    | a -> CastStm( totype, fromtype, newfrom ))

    | AssignStm( tostm, fromstm ) -> AssignStm( tostm, foldStm fromstm )
    | LoopStm( test, body ) -> LoopStm( foldStm test, foldStmList body )
    | IfStm( cond, ifbody, elsebody ) -> 
        IfStm( foldStm cond, foldStmList ifbody, foldStmList elsebody )
    | FuncallStm( name, args ) -> FuncallStm( name, foldStmList args )
    | VarDeclStm( name, typ, value ) -> VarDeclStm( name, typ, foldStm value )
    | ArefStm( arr, idx ) -> ArefStm( foldStm arr, foldStm idx )
    | AddrStm( stm ) -> AddrStm( foldStm stm )
    | DerefStm( stm ) -> DerefStm( foldStm stm )
    | other -> other
  and foldStmList lst =
    List.map foldStm lst
  in 
  let foldDecl = function
      Fundecl( id, args, rettype, body, symtbl ) ->
        Fundecl( id, args, rettype, foldStmList body, symtbl ) 
    | NopDecl -> NopDecl
  in
    List.map foldDecl irlst
;;
*)

(* Next, loops to jumps 
   while foo do
      stuff
   end

   becomes

   .loopHeadN:
   if foo then
      stuff
      goto .loopHeadN
   end

*)

let rec transformLoops irlist =
  let rec transformLoop = function
      LoopStm( test, body ) -> 
	let newbody = transformLoops body in
	let headlabel = makeLocalLabelName "loopHead" in
	let ifpart = IfStm( test, GotoStm( headlabel ) :: newbody, [] ) in
	  [ifpart; LabelStm( headlabel )]
    | IfStm( cond, ifbody, elsebody ) ->
	[IfStm( cond, transformLoops ifbody, transformLoops elsebody )]

    | other -> [other]
  in

  (* Erm, couldn't this be... you know, List.map? 
     No, because the new list may be a different size than the old one.
     Okay.
  *)
  let rec loop inlist accm =
    match inlist with
	hd :: tl -> loop tl ((transformLoop hd) @ accm)
      | [] -> accm
  in
    (loop irlist [])
;;

(* Next, explicit temporaries *)
(* XXX: Do this next! *)


(* Next, tail-call optimization 
   def foo x y:
      ...
      (foo a b)
   end

   becomes

   def foo x y:
      .start:
      ...
      x <- a
      y <- b
      goto .start
   end


*)

let tailCall funname args irlist =
  let labelname = makeLocalLabelName "tailcall" in
  let rec doTailCall = function
      FuncallStm( id, stmlist ) ->
	if id = funname then
	  let assigns = 
	    List.map2
	      (fun argsym argstm ->
		AssignStm( VarStm( argsym.Syntree.vname ), argstm ))
	      args stmlist
	  in
	    (GotoStm( labelname )) :: assigns
	else
	  [FuncallStm( id, stmlist )]
    | IfStm( cond, ifbody, elsebody ) -> 
	let rev1 = List.rev ifbody
	and rev2 = List.rev elsebody in
	let newcall1 = doTailCall (List.hd rev1)
	and newcall2 = doTailCall (List.hd rev2) in
	let newifbody = (List.rev (List.tl rev1)) @ newcall1
	and newelsebody = (List.rev (List.tl rev2)) @ newcall2 in
	  [IfStm( cond, newifbody, newelsebody )]
    | other -> [other]
  in
  let rev = List.rev irlist in
  let newcall = doTailCall (List.hd rev) in
  let newlst = (List.rev (List.tl rev)) @ newcall in
    if newlst <> irlist then
      newlst @ [LabelStm( labelname )]
    else
      irlist
;;


(* Next, new representation --more asm-like, with conditional jumps 
   and explicit returns
*)


(*
      OpStm( op, args ) -> 
    | AssignStm( tostm, fromstm ) -> 
    | LoopStm( test, body ) -> 
    | IfStm( cond, ifbody, elsebody ) ->
    | FuncallStm( name, args ) ->
    | LiteralStm( litstm ) ->
    | CastStm( totype, fromtype, fromexpr ) -> 
    | VarStm( name ) -> 
    | VarDeclStm( name, typ, value ) ->
    | ArefStm( arr, idx ) ->
    | AddrStm( stm ) ->
    | DerefStm( stm ) -> 
    | NopStm
    | LabelStm( label ) ->
    | GotoStm( label ) -> 
          *)

let applyToStatements func irlist =
  let doToStatements = function
      Fundecl( id, args, rettype, body, vars ) ->
	Fundecl( id, args, rettype, (func body), vars )
    | other -> other
  in
    List.map doToStatements irlist 
;;

let doTailCall irlist = 
  let doToStatements = function
      Fundecl( id, args, rettype, body, vars ) ->
	Fundecl( id, args, rettype, (tailCall id args body), vars )
    | other -> other
  in
    List.map doToStatements irlist 

let transformIR ir =
  (* let t0 = applyToStatements foldConstants ir in *)
  let t1 = applyToStatements transformLoops ir in
  let t2 = doTailCall t1 in
    t2
;;


let irexpr2string  = function
      OpStm( op, args ) -> ""
    | AssignStm( tostm, fromstm ) -> ""
    | LoopStm( test, body ) -> ""
    | IfStm( cond, ifbody, elsebody ) -> ""
    | FuncallStm( name, args ) -> ""
    | LiteralStm( litstm ) -> ""
    | CastStm( totype, fromtype, fromexpr ) ->  ""
    | VarStm( name ) -> ""
    | VarDeclStm( name, typ, value ) -> ""
    | ArefStm( arr, idx ) -> ""
    | AddrStm( stm ) -> ""
    | DerefStm( stm ) -> ""
    | NopStm -> ""
    | LabelStm( label ) -> ""
    | GotoStm( label ) ->  ""
;;

let irdecl2string = function
    Fundecl( id, args, rettype, body, vars ) ->
      Printf.sprintf "FUNDECL %s %s (%s):\n %s\n"
	"ID" "ARGS" "RETTYPE" "BODY"
  | NopDecl -> "NOPDECL\n"
;;

let printIR ir =
  List.iter (fun x -> print_endline (irdecl2string x)) ir
;;
