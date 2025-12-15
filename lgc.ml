let usage = "lgc [-debug] [-o <output>] <file1>"
let debug = ref false
let check = ref false
let output_file = ref "game.js"
let files = ref []
let anon_fun f = files := f :: !files

let speclist =
  [
    ("-debug", Arg.Set debug, "Compile in debug mode");
    ("-check", Arg.Set check, "Run only the type checker");
    ("-o", Arg.Set_string output_file, "Set output file name (default: game.js)");
  ]

let parse_file file =
  let chan = open_in file in
  let lexbuf = Lexing.from_channel chan in
  try
    let program = Grammar.library Lexer.token lexbuf in
    close_in chan;
    program
  with
  | Grammar.Error ->
      Printf.eprintf "[line %d] parser error at token: %s\n"
        lexbuf.lex_curr_p.pos_lnum (Lexing.lexeme lexbuf);
      close_in chan;
      exit 1
  | Lexer.SyntaxError msg ->
      Printf.eprintf "[line %d] lex error: %s: %s\n" lexbuf.lex_curr_p.pos_lnum
        msg (Lexing.lexeme lexbuf);
      close_in chan;
      exit 1

let compile file =
  let program = parse_file file in
  if !debug then print_endline (Syntax.show_library program);
  try
    Typecheck.check program;
    if not !check then (
      let game = Compiler.compile program in
      let chan = open_out !output_file in
      Printf.fprintf chan "%s\n" game;
      close_out chan)
  with Typecheck.TypeError (msg, Loc l) ->
    Printf.eprintf "[line %d] type error: %s\n" l msg;
    exit 1

let () =
  Arg.parse speclist anon_fun usage;
  if List.length !files = 1 then compile (List.hd !files)
  else Printf.eprintf "expected only 1 file to compile\n"
