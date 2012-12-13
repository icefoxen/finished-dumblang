(* We're not quite so minimal we don't need a symbol table... 

   OKAY.  This is distinctly less than minimal by now.
   In fact, I think a (near) total re-write is in order, to accomidate
   the module system.  We can probably stick in the framework for
   function types while we're at it.

   ....rrrrgl.  It seems like there should be a better way to represent
   this, and I really don't quite know how heirarchical modules are
   gonna work.  But fing is, there really is NO reason to not have 'em,
   in terms of implementation --it's conceptually dead simple, just
   kinda hairy in application.
*)

open Hashtbl
open ErrorReport
open Syntree

let emptyModule = [];;


(*let localModule = ref [];;*)

let isVar v =
  match v.vtype with
      FuncType( _ ) -> false
    | _ -> true
;;

let isFunc v =
    match v.vtype with
      FuncType( _ ) -> true
    | _ -> false
;;

(* Insert isArray, isReference, etc here as necessary. *)



let makeVar name vartype alloctype = {
  vname = name; 
(*  vlitname = ""; *)
  vtype = vartype; 
  valloc = alloctype; 
  vloc = 0
}
;;

let setVarLoc var loc =
  var.vloc <- loc
;;

let module2str modul =
  List.fold_right (fun x y -> x ^ ":" ^ y) modul ""
;;


let makeId nm modul =
  (nm, modul)
;;

let getIdName v =
  fst v
;;

let getIdModule v =
  snd v
;;

let id2str id =
  let nm = getIdName id
  and md = getIdModule id in
    (module2str md) ^ nm
;;


let isIdLocal var =
  (getIdModule var) = []
;;

let moduleIsImported tbl modul =
  List.mem modul tbl.importedmodules
;;

let rec moduleIsUsed tbl modul =
  List.mem modul tbl.usedmodules
;;

let moduleExists tbl modul =
  (moduleIsImported tbl modul) || (moduleIsUsed tbl modul)
;;


let addNewSymbol tbl id typ alloctype =
  if Hashtbl.mem (List.hd tbl.symtbl) id then
    errorAndDie ("Symbol " ^ (id2str id) ^ " declared twice!")
  else
    Hashtbl.add (List.hd tbl.symtbl) id (makeVar id typ alloctype)
;;

let addSymbol tbl sym =
(*  Printf.printf "Adding symbol %s!\n" (id2str sym.vname); *)
  if Hashtbl.mem (List.hd tbl.symtbl) sym.vname then
    errorAndDie ("Symbol " ^ (id2str sym.vname) ^ " declared twice!")
  else
    Hashtbl.add (List.hd tbl.symtbl) sym.vname sym
;;



let symbolExists stbl id =
  let rec loop id = function
      [] -> false
    | tbl :: rst ->
	if Hashtbl.mem tbl id then
	  true
	else
	  loop id rst
  in
    loop id stbl.symtbl
;;

let getSymbol stbl id =
  let rec loop tbllst id =
    match tbllst with
	[] -> errorAndDie ("getSymbol: Symbol " ^ 
			     (id2str id) ^ " does not exist!")
      | a :: b -> 
	  if Hashtbl.mem a id then Hashtbl.find a id
	  else loop b id
  in
    loop stbl.symtbl id
;;



let idIsVar tbl id =
  let v = getSymbol tbl id in
    isVar v
;;


let idIsFunc tbl id =
  let v = getSymbol tbl id in
    isFunc v
;;



let getFunc stbl id =
    if idIsFunc stbl id then
      getSymbol stbl id
    else
      errorAndDie ("getFunc: Got a non-func for " ^ (id2str id))
;;

let getFuncRettype func =
  match func.vtype with
      FuncType( ret, args ) -> ret
    | _ -> errorAndDie ("getFuncRettype: didn't get a function!")
;;

let getFuncArgs func =
  match func.vtype with
      FuncType( ret, args ) -> args
    | _ -> errorAndDie ("getFuncRettype: didn't get a function!")
;;
    


let getIdType stbl id =
  (getSymbol stbl id).vtype
;;

let arg2symbol x =
  let nm, typ = x in
    makeVar nm typ ARG
;;


let fundecl2symbol = function
    Fundecl( _, funname, funargs, funtype, _) ->
(*      let args = List.map arg2symbol funargs in *)
      let funtype = FuncType( funtype, funargs ) in
	makeVar funname funtype GLOBAL
  | ImportDecl( _ ) -> 
      errorAndDie "fundecl2functype: Recieved an importdecl, not a fundecl!"
  | ExportDecl( _ ) -> 
      errorAndDie "fundecl2functype: Recieved an exportdecl, not a fundecl!"
  | UseDecl( _ ) -> 
      errorAndDie "fundecl2functype: Recieved a usedecl, not a fundecl!"

;;



let typeExists tbl typid =
  Hashtbl.mem tbl.typetbl typid
;;

let addType tbl typname typtyp =
  if typeExists tbl typname then
    errorAndDie ("Tried to create type " ^ (id2str typname) ^ " twice!")
  else
    Hashtbl.add tbl.typetbl typname typtyp
;;


let getType tbl typ =
  if typeExists tbl typ then
    Hashtbl.find tbl.typetbl typ
  else
    errorAndDie ("Tried to get type " ^ (id2str typ) ^ " which doesn't exist!")
;;
    

let pushScope tbl =
  tbl.symtbl <- (Hashtbl.create 8) :: tbl.symtbl
;;

let popScope tbl =
  tbl.symtbl <- List.tl tbl.symtbl
;;

let getTopScope tbl =
  List.hd tbl.symtbl
;;

let setTopScope tbl scope =
  tbl.symtbl <- scope :: tbl.symtbl
;;


(* Returns a string representing a type.  This will actually exist
   more fully later.
*)
let type2str tbl name =
  match name with
      AtomType( s, i ) -> s
    | FuncType( rettype, args ) -> "function"
    | NoneType -> "None"
;;


(* Ummm.... yeah.  Sure, we can hard-code it for now... *)
let hasCompatibleType tbl exptyp giventyp =
(*  let exptyp = getType tbl expectedtype
  and giventyp = getType tbl giventype in*)
    (* We can transparently coerce ints to floats. *)
    if (exptyp = floattype) && (giventyp = inttype) then
      true
    else if exptyp = giventyp then
      true
    else
      false
;;

let moreGeneralType stbl typ1 typ2 =
  if (typ1 = floattype) or (typ2 = floattype) then
    floattype
  else
    inttype
;;


let makeSymTbl modulename = 
  let st = {
    symtbl = [Hashtbl.create 16];
    typetbl = Hashtbl.create 16;
    modulename = modulename;

    importedmodules = [];
    usedmodules = [];
    exportlist = [];
  } in
    addType st (makeId "int" modulename) inttype;
    addType st (makeId "float" modulename) floattype;
    st
;;

(*
let printSymTbl stbl =
  let printSymbol sym =
    ()
  and r
      *)

let printSymtbl tbl =
  let printSymbol x =
    Printf.printf "Symbol %s:%s at %d\n" (id2str x.vname)
      (type2str tbl x.vtype) x.vloc
  in
(*    Hashtbl.iter (fun x y -> printFunc y) tbl.functbl; *)
    List.iter (fun frame -> Hashtbl.iter (fun x y -> printSymbol y) frame) tbl.symtbl;
;;


(*
let populateSymtbl stbl decllst =
  let addDecl decl =
    match decl with
	Fundecl( _ ) -> addSymbol stbl (fundecl2symbol decl)
      | ImportDecl( ilst ) -> List.iter (Modules.importModule stbl) ilst
      | UseDecl( ulst ) -> List.iter (Modules.useModule stbl) ulst
      | ExportDecl( _ ) -> ()
  in
    List.iter addDecl decllst
;;
*)
