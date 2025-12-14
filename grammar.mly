%{
open Syntax
%}

%token <string> IDENT
%token <string> COLOR
%token <int> INTEGER
%token TRUE FALSE
%token COMMA LPAREN RPAREN COLON EQ
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
| DEF name=IDENT LPAREN params=param_list RPAREN DO body=block END { {name; params; body} }

param_list: separated_list(COMMA, param) { $1 }

param: IDENT COLON typing { ($1, $3) }

block: list(stmt) { Block $1 }

typing:
| IDENT { TName $1 }

stmt:
| VAR IDENT COLON typing EQ expr { SVar ($2, $4, $6) }
| IDENT EQ expr { SMutate ($1, $3) }
| IF expr DO block ELSE block END { SIfElse ($2, $4, $6) }
| FOR IDENT EQ expr TO expr DO block END { SFor ($2, $4, $6, $8) }
| expr { SExpr $1 }

expr:
| constant { $1 }
| var LPAREN separated_list(COMMA, expr) RPAREN { ECall ($1, $3) }
| var { EVar $1 }
| expr bop expr { EBinary ($2, $1, $3) }

%inline bop:
| PLUS { Add }

var:
| IDENT { VName $1 }

constant:
| TRUE { EBool true }
| FALSE { EBool false }
| COLOR { EColor $1 }
| INTEGER { EInt $1 }
