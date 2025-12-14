{
open Grammar
open Lexing

exception SyntaxError
}

let ident = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']*
let integer = ['0'-'9']+
let hex = ['0'-'9' 'a'-'f' 'A'-'F']
let color = '#' hex hex hex hex hex hex
let ws = [' ' '\t' '\r']+

rule token = parse
  | ws { token lexbuf }
  | '\n' { new_line lexbuf; token lexbuf }
  | ',' { COMMA }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | integer as i { INTEGER (int_of_string i) }
  | color as c { COLOR c }
  | ident as i { IDENT i }
  | eof { EOF }
  | _  { raise SyntaxError }
