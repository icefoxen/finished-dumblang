(* We're not quite so minimal we don't need a symbol table... 
*)

open Hashtbl
open ErrorReport
open Syntree




let makeVar name vartype alloctype = {
  vname = name; 
  vtype = vartype; 
  valloc = alloctype; 
  vloc = 0
}
;;

let setVarLoc var loc =
  var.vloc <- loc
;;


let addVar tbl name typ alloctype =
  if Hashtbl.mem (List.hd tbl.symtbl) name then
    errorAndDie ("Var " ^ name ^ " already exists!")
  else (
    (*Printf.printf "Adding var %s\n" name; *)
    Hashtbl.add (List.hd tbl.symtbl) name (makeVar name typ alloctype)
  )
;;

let fundecl2functype = function
    Fundecl( _, funname, funargs, funtype, _) ->
      let arg2var x =
	makeVar (fst x) (snd x) ARG
      in
      let args = List.map arg2var funargs in
      {funcname = funname; funcargs = args; funcret = funtype}
;;

let varExists tbl name =
  let rec loop tbllst =
    if tbllst = [] then
      false
    else if Hashtbl.mem (List.hd tbllst) name then
      true
    else
      loop (List.tl tbllst)
  in
    loop tbl.symtbl
;;

let getVar tbl name =
  let rec loop tbllst =
    if tbllst = [] then
      errorAndDie ("Tried to get var " ^ name ^ " that doesn't exist!")
    else if Hashtbl.mem (List.hd tbllst) name then
      Hashtbl.find (List.hd tbllst) name
    else
      loop (List.tl tbllst)
  in
    loop tbl.symtbl
;;

let typeExists tbl typ =
  Hashtbl.mem tbl.typetbl typ
;;

let addType tbl typ =
  if typeExists tbl typ then
    errorAndDie ("Tried to create type " ^ typ ^ " twice!")
  else
    Hashtbl.add tbl.typetbl typ typ
;;

let getType tbl typ =
  if typeExists tbl typ then
    Hashtbl.find tbl.typetbl typ
  else
    errorAndDie ("Tried to get type " ^ typ ^ " which doesn't exist!")
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


let addFunc tbl fundecl =
  match fundecl with
      Fundecl( _, funname, funargs, funcret, funbody ) ->
	if Hashtbl.mem tbl.functbl funname then
	  errorAndDie ("Func " ^ funname ^ " already exists!")
	else
	  Hashtbl.add tbl.functbl funname (fundecl2functype fundecl)
;;

let funcExists tbl name =
  Hashtbl.mem tbl.functbl name
;;

let getFunc tbl name =
  if funcExists tbl name then
    Hashtbl.find tbl.functbl name
  else
    errorAndDie ("Tried to get function " ^ name ^ " that doesn't exist!")
;;

let printSymtbl tbl =
  let printFunc x =
    Printf.printf "Func %s:%s with args " x.funcname x.funcret;
    List.iter 
      (fun var -> Printf.printf "%s:%s at %d, " var.vname var.vtype var.vloc)
      x.funcargs;
    Printf.printf "\n";
  in
  let printVar x =
    Printf.printf "Var %s:%s at %d\n" x.vname x.vtype x.vloc
  in
    Hashtbl.iter (fun x y -> printFunc y) tbl.functbl;
    List.iter (fun frame -> Hashtbl.iter (fun x y -> printVar y) frame) tbl.symtbl;
;;


let hashtblSize htbl =
  let cnt = ref 0 in
    Hashtbl.iter (fun x y -> incr cnt) htbl;
    !cnt
;;

let hashtblItems htbl =
  Hashtbl.fold (fun x y lst -> (x, y) :: lst) htbl []
;;

let varType var =
  var.vtype
;;

let getVarType tbl name =
  if varExists tbl name then
    let v = getVar tbl name in
      v.vtype
  else
    errorAndDie ("Tried to the type of var " ^ name ^ ", which doesn't exist")
;;

(* Returns a string representing a type.  This will actually exist later,
   but it's nice to have now.
*)
let typeToString tbl name =
  name
;;


(* Ummm.... yeah.  Sure, we can hard-code it for now... *)
let hasCompatibleType tbl expectedtype giventype =
  let exptyp = getType tbl expectedtype
  and giventyp = getType tbl giventype in
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


let makeSymTbl () = 
  let st = {
    symtbl = [Hashtbl.create 16];
    functbl = Hashtbl.create 16;
    typetbl = Hashtbl.create 16;
  } in
    addType st inttype;
    addType st floattype;
    st
;;
