(* lex.mll
   A lexer for Dumb.


   Simon Heath
   18/4/2005
*)

{

open Parse
open ErrorReport
exception Eof
exception Lexer_error

let inComment = ref 0;;

(* Abbreviation for the func that returns the string
   being lexed.
*)
let gs = Lexing.lexeme;;

(* Advances the position of the error-checking vars. *)
let adv lb =
  (*
  let c = (gs lb) in
  if c <> " " then
     Printf.printf "Lexed: '%s'\n" (gs lb);
  *)
  chrNum := !chrNum + (String.length (Lexing.lexeme lb));;

let str2float x =
   Scanf.sscanf x "%f" (fun x -> x)
;;

let str2int x =
   Scanf.sscanf x "%i" (fun x -> x)
;;
(*
let str2char x =
   Scanf.sscanf x "%C" (fun x -> x) 
;;

let str2str x =
   Scanf.sscanf x "%S" (fun x -> x) 
;;
*)

}


let id = 
  ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_' ]*

let inum =
   '-'?(['0'-'9']+|"0x"['0'-'9''a'-'f''A'-'F']+|"0o"['0'-'7']+)
let bnum =
   '-'?"0b"['0''1']+
let fnum =
   '-'?['0'-'9']+'.'['0'-'9']*
(*
let chr =
   ("'"_"'") | ("'\\"(inum|bnum)"'") | ("'\\"("n"|"b"|"r"|"t"|"'"|"\\")"'")

let str = '"'([^'"''\\']|'\\'_)*'"'
*)

rule token = parse
   ";"			{ adv lexbuf; lcomment lexbuf }
 | "/-"			{ adv lexbuf; incr inComment; bcomment lexbuf }
 | (inum|bnum)		{ adv lexbuf; INT( str2int (gs lexbuf) ) }
 | fnum			{ adv lexbuf; FLOAT( str2float (gs lexbuf) ) }
 (*| chr                  { adv lexbuf; CHAR( str2char (gs lexbuf) ) } *)
 | "\n"			{ nl (); token lexbuf }
 | [' ''\t']		{ adv lexbuf; token lexbuf }
 | "+"			{ adv lexbuf; ADD }
 | "-"			{ adv lexbuf; SUB }
 | "*"			{ adv lexbuf; MUL }
 | "/"			{ adv lexbuf; DIV }
 | "%"			{ adv lexbuf; MOD }
 | "("			{ adv lexbuf; LPAREN }
 | ")"			{ adv lexbuf; RPAREN }
 | ";"			{ adv lexbuf; SEMICOLON }
 | ":"			{ adv lexbuf; COLON }
 | ","			{ adv lexbuf; COMMA }
 | "|"			{ adv lexbuf; BAR }
 | "<-"			{ adv lexbuf; ASSIGN }
 | "import"		{ adv lexbuf; IMPORT }
 | "use"		{ adv lexbuf; USE }
 | "export"		{ adv lexbuf; EXPORT }
 | "var"		{ adv lexbuf; VAR }
 | "func"		{ adv lexbuf; FUNC }
 | "if"			{ adv lexbuf; IF }
 | "then"		{ adv lexbuf; THEN }
 | "elif"		{ adv lexbuf; ELIF }
 | "else"		{ adv lexbuf; ELSE }
 | "while"		{ adv lexbuf; WHILE }
 | "do"			{ adv lexbuf; DO }
 | "end"		{ adv lexbuf; END }
 | ">"			{ adv lexbuf; GT }
 | "<"			{ adv lexbuf; LT }
 | ">="			{ adv lexbuf; GTE }
 | "<="			{ adv lexbuf; LTE }
 | "="			{ adv lexbuf; EQ }
 | "/="			{ adv lexbuf; NEQ }
 | "and"		{ adv lexbuf; AND }
 | "or"			{ adv lexbuf; OR }
 | "not"		{ adv lexbuf; NOT }
 | "xor"		{ adv lexbuf; XOR }
 (*
 | "<<"			{ adv lexbuf; SHL }
 | ">>"			{ adv lexbuf; SHR }
 | "band"		{ adv lexbuf; BAND }
 | "bor"		{ adv lexbuf; BOR }
 | "bnot"		{ adv lexbuf; BNOT }
 | "bxor"		{ adv lexbuf; BXOR }

 *)
 | id			{ adv lexbuf; SYMBOL( (gs lexbuf) ) }
 | eof			{ EOF }
 | _			{ errorAndDie "Invalid token!" }

and bcomment = parse
   "/-"			{ adv lexbuf; incr inComment; 
   (*Printf.printf "Pushing comment stack, now %d\n" !inComment;*)
                          bcomment lexbuf }
 | "-/"			{ adv lexbuf; decr inComment; 
   (*Printf.printf "Popping comment stack, now %d\n" !inComment;*)
                          if !inComment <= 0 then token lexbuf
			                    else bcomment lexbuf }
 | '\n'			{ nl (); bcomment lexbuf }
 | _			{ adv lexbuf; bcomment lexbuf } 


and lcomment = parse
   '\n'			{ nl (); token lexbuf }
 | _			{ adv lexbuf; lcomment lexbuf }
