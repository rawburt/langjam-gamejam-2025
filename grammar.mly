%{
open Syntax
%}

%token <string> IDENT
%token <string> COLOR
%token <int> INTEGER
%token COMMA LPAREN RPAREN
%token EOF

%start <program> program
%%

program: list(expr) EOF { Program $1 }

expr:
| constant { $1 }
| IDENT LPAREN separated_list(COMMA, expr) RPAREN { ECall ($1, $3) }

constant:
| COLOR { EColor $1 }
| INTEGER { EInt $1 }
