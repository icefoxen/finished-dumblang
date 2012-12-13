(* We're not quite so minimal we don't need a symbol table... 
*)

open Hashtbl
open ErrorReport
open Syntree


let makeSymTbl () = {
  symtbl = [Hashtbl.create 16];
  functbl = Hashtbl.create 16;
};;


let makeVar name alloctype =
  {vname = name; valloc = alloctype; vloc = 0}
;;

let setVarLoc var loc =
  var.vloc <- loc
;;


let addVar tbl name alloctype =
  if Hashtbl.mem (List.hd tbl.symtbl) name then
    errorAndDie ("Var " ^ name ^ " already exists!")
  else (
    (*Printf.printf "Adding var %s\n" name; *)
    Hashtbl.add (List.hd tbl.symtbl) name (makeVar name alloctype)
  )
;;

let fundecl2functype = function
    Fundecl( _, funname, funargs, _, _ ) ->
      let args = List.map (fun x -> makeVar x ARG) funargs in
      {funcname = funname; funcargs = args}
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
      Fundecl( _, funname, funargs, funbody, _ ) ->
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
    Printf.printf "Func %s with args " x.funcname;
    List.iter (fun var -> Printf.printf "%s at %d, " var.vname var.vloc)
      x.funcargs;
    Printf.printf "\n";
  in
  let printVar x =
    Printf.printf "Var %s at %d\n" x.vname x.vloc
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


