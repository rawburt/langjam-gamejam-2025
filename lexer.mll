{
open Grammar
open Lexing

exception SyntaxError of string

let error msg = raise (SyntaxError msg)
}

let ident = ['a'-'z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']*
let tident = ['A'-'Z']['a'-'z' 'A'-'Z' '_' '0'-'9']*
let cident = ['A'-'Z'][ 'A'-'Z' '_' '0'-'9']+
let integer = ['0'-'9']+
let hex = ['0'-'9' 'a'-'f' 'A'-'F']
let color = '#' hex hex hex hex hex hex
let ws = [' ' '\t' '\r']+

rule token = parse
  | ws { token lexbuf }
  | "---" { comment lexbuf }
  | '\n' { new_line lexbuf; token lexbuf }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACK }
  | ']' { RBRACK }
  | ',' { COMMA }
  | '.' { DOT }
  | ':' { COLON }
  | '=' { EQ }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '%' { MODULO }
  | '/' { DIV }
  | '<' { LT }
  | '>' { GT }
  | '!' { NEGATE }
  | "==" { EQEQ }
  | "!=" { NEQ }
  | "<=" { LTE }
  | ">=" { GTE }
  | "||" { OR }
  | "&&" { AND }
  | "var" { VAR }
  | "true" { TRUE }
  | "false" { FALSE }
  | "if" { IF }
  | "do" { DO }
  | "else" { ELSE }
  | "end" { END }
  | "for" { FOR }
  | "to" { TO }
  | "def" { DEF }
  | "ret" { RET }
  | "rec" { REC }
  | "enum" { ENUM }
  | "use" { USE }
  | "asset" { ASSET }
  | "const" { CONST }
  | "match" { MATCH }
  | "when" { WHEN }
  | "break" { BREAK }
  | cident as c { CIDENT c }
  | integer as i { INTEGER (int_of_string i) }
  | color as c { COLOR c }
  | ident as i { IDENT i }
  | tident as t { TIDENT t }
  | '"' { read_string (Buffer.create 17) lexbuf }
  | eof { EOF }
  | _  { error "unexpected token" }
and comment = parse
  | '\n' { new_line lexbuf; token lexbuf }
  | _ { comment lexbuf }
  | eof { EOF }
and read_string buf = parse
  | '"'         { STRING (Buffer.contents buf) }
  | '\\' '/'    { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\'   { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'    { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'    { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'    { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'    { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'    { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | eof         { error "string not terminated" }
  | _           { error "unexpected string item" }
