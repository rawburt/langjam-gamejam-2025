%{
open Syntax

let mkcompound var bop expr loc =
  let expr' = EBinary (bop, (EVar var), expr, loc) in
  SMutate (var, expr', loc)

%}

%token <string> IDENT
%token <string> TIDENT
%token <string> COLOR
%token <int> INTEGER
%token <string> STRING
%token TRUE FALSE
%token LPAREN RPAREN LBRACK RBRACK
%token COMMA DOT COLON EQ
%token VAR IF DO ELSE END FOR TO DEF RET REC USE
%token PLUS MINUS EQEQ TIMES LT GT OR AND
%token EOF

%left OR
%left AND
%nonassoc EQEQ LT GT
%left PLUS MINUS
%left TIMES

%start <library> library
%%

library: imports=list(import) top=list(toplevel) EOF { Library { top; imports; } }

import:
| USE import_name { $2 }

import_name:
| IDENT { ($1, mkloc $startpos) }
| TIDENT { ($1, mkloc $startpos) }

toplevel:
| stmt { TLStmt $1 }
| def { TLDef $1 }
| record { TLRec $1 }

def:
| DEF name=IDENT LPAREN params=param_list RPAREN ret=def_ret DO body=block END { {name; params; body; ret; loc=mkloc $startpos} }

def_ret:
| { None }
| typing { Some $1 }

param_list: separated_list(COMMA, param) { $1 }

param: IDENT COLON typing { ($1, $3) }

record:
| REC name=TIDENT DO fields=list(field_decl) END { {name; fields; loc=mkloc $startpos } }

field_decl:
| IDENT COLON typing { ($1, $3) }

block: list(stmt) { Block $1 }

typing:
| IDENT { TName $1 }
| TIDENT { TName $1 }
| LBRACK typing RBRACK { TList $2 }

stmt:
| VAR IDENT COLON typing EQ expr { SVar ($2, $4, $6, mkloc $startpos) }
| mutate { $1 }
| IF expr DO block ELSE block END { SIfElse ($2, $4, Some $6, mkloc $startpos) }
| IF expr DO block END { SIfElse ($2, $4, None, mkloc $startpos) }
| FOR IDENT EQ expr TO expr DO block END { SFor ($2, $4, $6, $8, mkloc $startpos) }
| call { SExpr ($1, mkloc $startpos) }
| RET expr { SRet ($2, mkloc $startpos) }

mutate:
| var EQ expr { SMutate ($1, $3, mkloc $startpos) }
| var PLUS EQ expr { mkcompound $1 Add $4 (mkloc $startpos) }
| var MINUS EQ expr { mkcompound $1 Sub $4 (mkloc $startpos) }

call:
| var LPAREN separated_list(COMMA, expr) RPAREN { ECall ($1, $3, mkloc $startpos) }

expr:
| constant { $1 }
| call { $1 }
| var { EVar $1 }
| expr bop expr { EBinary ($2, $1, $3, mkloc $startpos) }
| LBRACK separated_list(COMMA, expr) RBRACK { EList ($2, mkloc $startpos) }
| record_expr { $1 }

record_expr:
| TIDENT LPAREN separated_list(COMMA, field_expr) RPAREN { ERec ($1, $3, mkloc $startpos) }

field_expr:
| IDENT COLON expr { ($1, $3) }

%inline bop:
| PLUS { Add }
| MINUS { Sub }
| TIMES { Mul }
| LT { Lt }
| GT { Gt }
| OR { Or }
| AND { And }
| EQEQ { Eq }

var:
| IDENT { VName ($1, mkloc $startpos) }
| var LBRACK expr RBRACK { VSub ($1, $3, mkloc $startpos) }
| var DOT IDENT { VField ($1, $3, mkloc $startpos) }

constant:
| TRUE { EBool true }
| FALSE { EBool false }
| COLOR { EColor $1 }
| INTEGER { EInt $1 }
| STRING { EStr $1 }
