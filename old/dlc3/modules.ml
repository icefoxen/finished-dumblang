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

   Simon Heath
   28/1/2006
*)

open Syntree
open Symtbl

let importModule stbl modul =
  ()
;;

let useModule stbl modul =
  ()
;;

let getModule modul =
  ()
;;

let getModuleFromFile modul =
  ()
;;

