(* interface.ml
   This handles the reading, writing and general mangling of interface files.

   Interface files consist of:
   dependancies
   function headers

   Simon Heath
   8/1/2006
*)

open Syntree
open Symtbl

let importModule stbl modname =
   ()
;;

let useModule stbl modname =
   ()
;;

let importModTree stbl modtree =
   ()
;;

let useModTree stbl modtree =
   ()
;;

let parseModule modname =
   let fullpath = findModule modname in
   try
      let instream = open_in fn in
      let lexbuf = Lexing.from_channel instream in
      let parsetree = Parse.main Lex.token lexbuf in
      close_in instream;
      parsetree
   with
      Parsing.Parse_error ->
         ErrorReport.errorAndDie ("Fatal error parsing interface file " ^
	    modname)
;;

(* We could do this either by scanning the symbol table (before all the
   imports have been imported) or by walking the syntax tree.  We do the
   former.
*)
let createModule stbl = 
   ()
;;

(* This turns a module into a string, which can then be written to a file. *)
let dumpModule modtree =
   ()
;;

(* This searches for a module in whatever standard library paths there are
   (of which we have none), and returns the full path to the file.
*)
let findModule modname =
   modname
;;
