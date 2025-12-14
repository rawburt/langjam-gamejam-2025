%{
open Syntax
%}

%token <string> IDENT
%token <string> COLOR
%token <int> INTEGER
%token TRUE FALSE
%token COMMA LPAREN RPAREN COLON EQ
%token VAR IF DO ELSE END
%token EOF

%start <program> program
%%

program: block EOF { Program $1 }

block: list(stmt) { Block $1 }

typing:
| IDENT { TName $1 }

stmt:
| VAR IDENT COLON typing EQ expr { SVar ($2, $4, $6) }
| IF expr DO block ELSE block END { SIfElse ($2, $4, $6) }
| expr { SExpr $1 }

expr:
| constant { $1 }
| var LPAREN separated_list(COMMA, expr) RPAREN { ECall ($1, $3) }
| var { EVar $1 }

var:
| IDENT { VName $1 }

constant:
| TRUE { EBool true }
| FALSE { EBool false }
| COLOR { EColor $1 }
| INTEGER { EInt $1 }
