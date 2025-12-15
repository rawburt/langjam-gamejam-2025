%{
open Syntax

let mkloc (startpos : Lexing.position) = Loc startpos.pos_lnum

%}

%token <string> IDENT
%token <string> COLOR
%token <int> INTEGER
%token TRUE FALSE
%token LPAREN RPAREN LBRACK RBRACK
%token COMMA COLON EQ
%token VAR IF DO ELSE END FOR TO DEF
%token PLUS
%token EOF

%left PLUS

%start <program> program
%%

program: list(toplevel) EOF { Program $1 }

toplevel:
| stmt { TLStmt $1 }
| def { TLDef $1 }

def:
| DEF name=IDENT LPAREN params=param_list RPAREN DO body=block END { {name; params; body; loc=mkloc $startpos} }

param_list: separated_list(COMMA, param) { $1 }

param: IDENT COLON typing { ($1, $3) }

block: list(stmt) { Block $1 }

typing:
| IDENT { TName $1 }
| LBRACK typing RBRACK { TList $2 }

stmt:
| VAR IDENT COLON typing EQ expr { SVar ($2, $4, $6, mkloc $startpos) }
| IDENT EQ expr { SMutate ($1, $3, mkloc $startpos) }
| IF expr DO block ELSE block END { SIfElse ($2, $4, $6, mkloc $startpos) }
| FOR IDENT EQ expr TO expr DO block END { SFor ($2, $4, $6, $8, mkloc $startpos) }
| call { SExpr ($1, mkloc $startpos) }

call:
| var LPAREN separated_list(COMMA, expr) RPAREN { ECall ($1, $3, mkloc $startpos) }

expr:
| constant { $1 }
| call { $1 }
| var { EVar $1 }
| expr bop expr { EBinary ($2, $1, $3, mkloc $startpos) }
| LBRACK separated_list(COMMA, expr) RBRACK { EList ($2, mkloc $startpos) }

%inline bop:
| PLUS { Add }

var:
| IDENT { VName ($1, mkloc $startpos) }
| var LBRACK expr RBRACK { VSub ($1, $3, mkloc $startpos) }

constant:
| TRUE { EBool true }
| FALSE { EBool false }
| COLOR { EColor $1 }
| INTEGER { EInt $1 }
