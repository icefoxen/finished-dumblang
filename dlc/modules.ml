(* modules.ml
   This has all the stuff that involves the parsing and loading of
   modules.

   Ideally, we should be able to load modules either from files or from
   whatever database interface we want...

   It'd be keen to only do databases, but people would bitch about
   portability and such.

   ...hm.  What we need is some unified thingamadugin input interface
   that can read and resolve things intellegently either from files or
   from a database.

   The lexer can pull from a string instead of a channel, so that rocks.
   Then we just need some unified way of referring to things...  which
   I suppose is the module system itself.  You just need to tell the
   compiler beforehand whether you want to pull from files, from
   a database, or maybe both.

   Hm, that can be done.  It just needs the right interface.

   The compiler needs to know where libraries are, though.  That
   can be part of the database module, but is it part of the 

   Hmm.  I think it would be reasonable to just use the symtbl data
   structure as a module structure.  It IS basically right, so.
   And any changes that need to be made to one should be made to the other
   anyway.

   And we suddenly get heirarchical modules for free.  Wiggy.  I just
   have to write the mechanics for them, but the data structure is in place.
   
   Hokay.  FOR NOW, just write the text-file interface.

   ...so.  The way it works is this:
   After semantically checking everything, it goes through the syntax
   tree and dumps all the decls to a string or file.

   It just uses the exact same syntax as the language itself.
   
   To read 'em back, we parse the file and get a list of decls.  We can
   then go through and feed 'em into the symbol table without too much
   trouble, I think.

   Recall that order of operations is important:
   1) Populate symbol table
   2) Add exports, check they exist
   3) Import symbols
   4) Semantic jazz


   Hokay.  We need to make sure we do:
   Make module/library paths work

   XXX: Importing does not work, because the final assembly symbol is
   put in the local module!  So if we import testlib2 into test3, and
   call the function testlib2:foo as (foo), it compiles to test3_foo.
   To fix this, we will need to make ALL symbols in the symbol table
   have an absolute module path.  However, this is kinda tricksy, since
   it makes lookup more complicated; we will need a list of module paths
   to search to find "local" symbols.  Either that or an absolute way of
   figuring out where an arbitrary identifier came from, which seems more
   complicated.
   ...still.  using modules still works just fine, which is enough for our
   purposes right now.  This is harder than it looks, it seems.
   
   Simon Heath
   28/1/2006
*)

open ErrorReport
open Syntree
open Symtbl

let modulesuffix = ".dli";;
let libpath = "./lib";;


(* Importing modules *)

(* XXX: should try to find and compile the given file... *)
let getModuleFromFile modul =
  let modulename = List.hd modul in
  let fn = modulename ^ modulesuffix in
  let instream = open_in fn in
  let lexbuf = Lexing.from_channel instream in
    Parse.main Lex.token lexbuf
;;

let addImportedSymbol stbl decl modul =
  match decl with
      Fundecl( lineno, id, args, rettype, body ) ->
	Symtbl.addSymbol stbl (Symtbl.fundecl2symbol decl)
    | _ -> ()
;;

let addUsedSymbol stbl decl modul =
    match decl with
      Fundecl( lineno, id, args, rettype, body ) ->
	let newid = makeId (getIdName id) modul in
	let newdecl = Fundecl( lineno, newid, args, rettype, body ) in
	  Symtbl.addSymbol stbl (Symtbl.fundecl2symbol newdecl)
    | _ -> ()
;;

let addDepend stbl depend =
  if not ((List.mem depend stbl.usedmodules) || 
	    (List.mem depend stbl.importedmodules)) then
    stbl.usedmodules <- depend :: stbl.usedmodules
;;

let addAllDepends stbl decls =
  let addDependDecl decl =
    match decl with
	ImportDecl( ilst )
      | UseDecl( ilst ) ->
	  List.iter (fun x -> addDepend stbl x) ilst
      | _ -> ()
  in
    List.iter addDependDecl decls
;;


let addImportedModule stbl modul =
  let moduledecls = getModuleFromFile modul in
    List.iter (fun x -> addImportedSymbol stbl x modul) moduledecls;
    addAllDepends stbl moduledecls
;;

let addUsedModule stbl modul =
  let moduledecls = getModuleFromFile modul in
    List.iter (fun x -> addUsedSymbol stbl x modul) moduledecls;
    addAllDepends stbl moduledecls
;;



(*
let importModule stbl modulename =
(*  let addDecl decl =
    match decl with
	Fundecl( _ ) -> Symtbl.addSymbol stbl (Symtbl.fundecl2symbol decl)
      | ImportDecl( ilst ) -> 
      | UseDecl( ulst ) -> 
      | ExportDecl( _ ) -> ()
  in
  let parsetree = getModuleFromFile modul in
*)
    ()
;;

*)


(* Exporting modules *)

let exportSymbol stbl ident =
  if Symtbl.symbolExists stbl ident then
    stbl.exportlist <- ident :: stbl.exportlist
  else
    errorAndDie 
      ("Symbol " ^ (id2str ident) ^ " is exported but doesn't exist!")
;;

let isExported stbl ident =
(*  print_string ("Is " ^ (id2str ident) ^ " exported?  ");
  if  List.mem ident stbl.exportlist then
    (print_endline "Yes!";
    true)
  else
    (print_endline "No.";
    false)
*)
    List.mem ident stbl.exportlist
;;


(* This basically does a limited un-parsing of the syntax tree. 
   Takes all the exports, makes sure they exist, then dumps 'em to
   a string.

   ...this function ended up about 5x longer than I thought it would.
   This is slightly worrisome.
*)
let makeInterface stree modul =
  let rec getDependsAndExports tree exports dependancies =
    match tree with
	hd :: tl ->
	  (match hd with
	       Fundecl( int, ident, args, rettype, body ) ->
		 getDependsAndExports tl exports dependancies
	     | ImportDecl( import ) ->
		 getDependsAndExports tl exports (dependancies @ import)
	     | UseDecl( use ) ->
		 getDependsAndExports tl exports (dependancies @ use)
	     | ExportDecl( export ) ->
		 getDependsAndExports tl (exports @ export) dependancies
	  )
      | [] -> (dependancies, exports)
  in

  let funcIsExported exportlst decl =
    match decl with
	Fundecl( _, ident, _, _, _ ) -> 
	  let name = getIdName ident in
	    List.mem name exportlst
      | _ -> false
  in

  let getExportedSymbols exportlst tree =
    List.filter (funcIsExported exportlst) tree
  in

  let symbol2str decl =
    match decl with
	Fundecl( _, ident, args, rettype, _ ) ->
	  (* In an interface, the names should come with the module path on
	     'em?  No.
	  *)
	  let name = id2str ident 
	  and arglst = List.map (fun x -> ((getIdName x.vname), x.vtype)) args
	  in

	  let argstr = 
	    List.fold_left 
	      (* Whoo, no trailing commas! *)
	      (fun x y -> Printf.sprintf "%s, %s %s"
		 x (fst y) (Symtbl.type2str (snd y)) )
	      (* No leading commas, either...  Um. That's trickier *)
	      "" arglst
	  in
	    (* If all else fails, use brute force. *)
	  let argstr =
	    if String.length argstr > 0 then 
	      String.sub argstr 1 ((String.length argstr) - 1)
	    else
	      argstr
	  in

	    Printf.sprintf "func %s %s (%s): INTERFACE end" 
	      name argstr (type2str rettype)
      | _ -> ""

  and dependancy2str d =
    let dstr = module2str d in
    "use " ^ (String.sub dstr 0 ((String.length dstr) - 1))
  in

  let dependancies, exports = getDependsAndExports stree [] [] in
  let exportedSymbols = getExportedSymbols exports stree in
    (*    Printf.printf "Dependslength %d, exportslength %d\n" (List.length dependancies) (List.length exports); *)
  let dependstr = 
    List.fold_left (fun x y -> x ^ (dependancy2str y) ^ "\n") "" dependancies
  and symbolstr =
    List.fold_left (fun x y -> x ^ (symbol2str y) ^ "\n") "" exportedSymbols
  in

    dependstr ^ "\n\n" ^ symbolstr

      
;;


let dumpInterfaceToFile stree modul =
  let modname = (List.hd modul) in
  let interfacestr = makeInterface stree modname in
  let interfacestr = ";  Interface file for: " ^ modname ^ "\n;  WARNING!  This file is machine-generated.  DO NOT EDIT BY HAND, please.\n;  The module reader doesn't do semantic checks, so be nice to it.  Thanks.\n\n" ^ interfacestr
  in
  let fout = open_out (modname ^ modulesuffix) in
    output_string fout interfacestr;
    close_out fout
;;

let addExportName tbl name =
  if List.mem name tbl.exportlist then
    ErrorReport.errorAndDie ("Module exported " ^ (id2str name) ^ " twice!")
  else
    tbl.exportlist <- name :: tbl.exportlist;
;;

