let parse_file file =
  let chan = open_in file in
  let lexbuf = Lexing.from_channel chan in
  Lexing.set_filename lexbuf file;
  try
    let program = Grammar.library Lexer.token lexbuf in
    close_in chan;
    program
  with
  | Grammar.Error ->
      close_in chan;
      let loc = Syntax.mkloc lexbuf.lex_curr_p in
      let msg = "at token: " ^ Lexing.lexeme lexbuf in
      Common.error ~loc ~kind:"parse error" msg
  | Lexer.SyntaxError msg ->
      close_in chan;
      let loc = Syntax.mkloc lexbuf.lex_curr_p in
      let msg = msg ^ " " ^ Lexing.lexeme lexbuf in
      Common.error ~loc ~kind:"lexer error" msg

(* given a path, parse file, resolve imports, return program ast *)
let load library = parse_file library
