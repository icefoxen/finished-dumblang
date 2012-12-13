%{
(* parse.mly
   Parser for Dumblang
   Fairly simple; the syntax isn't exactly complicated.
   Each rule builds up part of an abstract syntax tree, which then has
   various fun things done to it by semant.ml.

   Simon Heath
   20ish/4/2005
*)

open ErrorReport
open Syntree

let print_bop () = print_endline "Bop!";;


%}

%token LPAREN RPAREN SEMICOLON COLON COMMA BAR ASSIGN
%token FUNC VAR EOF IMPORT EXPORT USE
%token IF THEN ELIF ELSE WHILE DO END
%token ADD SUB MUL DIV MOD
%token GT LT GTE LTE EQ NEQ
%token AND OR NOT XOR BAND BOR BNOT BXOR SHL SHR
%token <int> INT
%token <float> FLOAT
%token <string> SYMBOL
/*%token <float> FLOAT
%token <char> CHAR
%token <string> STRING
%token <bool> BOOLEAN
*/

%type <Syntree.decl list> main
%start main

%%

main:  
	  /* EMPTY */ 
	  	{[]}
	| decllst
	  	{$1}
	;

decllst:
	  decl
	  	{[$1]}
	| decl decllst
	  	{$1 :: $2}
	;

decl:
	  fundecl
	  	{$1}
	| importdecl
		{$1}
	| exportdecl
		{$1}
        | usedecl
                {$1}
	;


fundecl:
	  FUNC SYMBOL arglist LPAREN typedecl RPAREN funcbody
	  	{Fundecl( !lineNum, (Symtbl.makeId $2 !Symtbl.localModule), 
                                $3, $5, $7 )}
	| FUNC SYMBOL LPAREN typedecl RPAREN funcbody
	  	{Fundecl( !lineNum, (Symtbl.makeId $2 !Symtbl.localModule), 
                                [], $4, $6 )}
	/*
	| FUNC SYMBOL arglist funcbody
	  	{Fundecl( !lineNum, $2, $3, "NoneType", $4 )}
	| FUNC SYMBOL funcbody
	  	{Fundecl( !lineNum, $2, [], $3 )}
	*/
	;

importdecl:
	  IMPORT stringlist
	  	{ ImportDecl( $2 ) }
	;

exportdecl:
	  EXPORT stringlist
	  	{ ExportDecl( $2 ) }
	;

usedecl:
	  USE stringlist
	  	{ UseDecl( $2 ) }

stringlist:
	  SYMBOL
	  	{[$1]}
	| SYMBOL COMMA stringlist
		{$1 :: $3}
	;

funcbody:
	  COLON exprlst END
	  	{$2}
	;

/*
funstm:
	  exprlst
	  	{$1}
	| fundecl
		{fundecl2funstm $1}
	;

*/
arglist:
	  arg COMMA arglist
	  	{$1 :: $3}
	| arg
	  	{[$1]}
	;

/* Yay, we have easy machinery for default values for function args */
arg:
	  SYMBOL typedecl
		{Symtbl.makeVar (Symtbl.makeId $1 Symtbl.emptyModule) $2 ARG}
	;

typedecl:
	  SYMBOL
	  	{AtomType( $1, 4 )}
	;

modulepath:
          SYMBOL
                {[$1]}
        | modulepath COLON SYMBOL
                {$3 :: $1}
        ;

identifier:
          SYMBOL
                {($1, [])}
        | modulepath COLON SYMBOL
                {($3, (List.rev $1)) }
        ;



expr:
	  identifier
	  	{VarRef( !lineNum, $1 )}
	| LPAREN funcall RPAREN
	  	{$2}
	| ifexpr
	  	{$1}
	| whileexpr
	  	{$1}
	| LPAREN mathexpr RPAREN 
	  	{$2}
	| LPAREN relexpr  RPAREN 
	  	{$2}
	| LPAREN logicexpr RPAREN 
	  	{$2}
	| vardecl
		{$1}
	| assignexpr
		{$1}
	| litexpr
		{$1}
	| negateexpr
		{$1}
	| castexpr
		{$1}
	/*| LPAREN bitexpr RPAREN
	  	{$2}
		*/
	| LPAREN assignexpr RPAREN
		{$2}
	/*| fundecl
		{Literal( 5, 5 )}
	*/
	;

exprlst:
	  expr exprlst
	  	{$1 :: $2}
	| expr
	  	{[$1]}
	;

assignexpr:
	  lvalue ASSIGN expr
	  	{Assign( !lineNum, $1, $3 )}
	;

lvalue:
	  identifier
                 {VarRef( !lineNum, $1 )}
	;



vardecl:
	  VAR SYMBOL typedecl ASSIGN expr
	  	{Var( !lineNum, (Symtbl.makeId $2 Symtbl.emptyModule), $3, $5 )}
	;


funcall:
	  identifier
	  	{Funcall( !lineNum, $1, [] )}
	| identifier exprlst
		{Funcall( !lineNum, $1, $2 )}
	;

/* Could be useful for elif's
ifend:
	  END
	  	{ [] }
	| ELSE exprlst END
		{ $2 }
*/

ifexpr:
	  IF expr THEN exprlst END
	  	{If( !lineNum, $2, $4, [], [] )}
	| IF expr THEN exprlst ELSE exprlst END
	  	{If( !lineNum, $2, $4, [], $6 )}
	| IF expr THEN exprlst elifexprlst END
	  	{If( !lineNum, $2, $4, $5, [] )}
	| IF expr THEN exprlst elifexprlst ELSE exprlst END
	  	{If( !lineNum, $2, $4, $5, $7 )}
	;

elifexpr:
	  ELIF expr THEN exprlst
	     {($2, $4)}
	;

elifexprlst:
	  elifexpr
		{[$1]}
	| elifexpr elifexprlst
		{$1 :: $2}
	;

whileexpr:
	  WHILE expr DO exprlst END
	  	{While( !lineNum, $2, $4 )}
	;

litexpr:
	  INT
	  	{Literal( !lineNum, Intlit( $1 ) )}
	| FLOAT
		{Literal( !lineNum, Floatlit( $1 ) )}
	;

negateexpr:
	  SUB expr
	  	{Negate( !lineNum, $2 )}

/* Holy shit, this is unambiguous? */
castexpr:
	  BAR typedecl expr BAR
	  	{Cast( !lineNum, $2, $3 )}
	;

mathexpr:
	  ADD exprlst
	  	{Op( !lineNum, Add, $2 )}
	| SUB exprlst
	  	{Op( !lineNum, Sub, $2 )}
	| MUL exprlst
	  	{Op( !lineNum, Mul, $2 )}
	| DIV exprlst
	  	{Op( !lineNum, Div, $2 )}
	| MOD exprlst
	  	{Op( !lineNum, Mod, $2 )}
	;

relexpr:
	  EQ exprlst
	  	{Op( !lineNum, Eq, $2 )}
	| NEQ exprlst
	  	{Op( !lineNum, Neq, $2 )}
	| GT exprlst
	  	{Op( !lineNum, Gt, $2 )}
	| GTE exprlst
	  	{Op( !lineNum, Gte, $2 )}
	| LT exprlst
	  	{Op( !lineNum, Lt, $2 )}
	| LTE exprlst
	  	{Op( !lineNum, Lte, $2 )}
	;

logicexpr:
	  AND exprlst
	  	{Op( !lineNum, And, $2 )}
	| OR exprlst
	  	{Op( !lineNum, Or, $2 )}
	| XOR exprlst
	  	{Op( !lineNum, Xor, $2 )}
	| NOT expr
	  	{Op( !lineNum, Not, [$2] )}
	;

/*
bitexpr:
	  BNOT expr
	  	{Opexpr( Bnot, [$2] )}
	| BAND exprlst
	  	{Opexpr( Band, $2 )}
	| BOR exprlst
	  	{Opexpr( Bor, $2 )}
	| BXOR exprlst
	  	{Opexpr( Bxor, $2 )}
	| SHL expr expr
	  	{Opexpr( Shl, [$2; $3] )}
	| SHR expr expr
	  	{Opexpr( Shr, [$2; $3] )}
	;

*/


%% 

let print_bar () = print_endline "Bar!";;
