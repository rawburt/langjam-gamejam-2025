let usage = "lgc [-debug] [-o <output>] <file1>"
let debug = ref false
let output_file = ref "game.js"
let files = ref []
let anon_fun f = files := f :: !files

let speclist =
  [
    ("-debug", Arg.Set debug, "Compile in debug mode");
    ("-o", Arg.Set_string output_file, "Set output file name (default: game.js)");
  ]

let parse_file file =
  let chan = open_in file in
  let lexbuf = Lexing.from_channel chan in
  try
    let program = Grammar.program Lexer.token lexbuf in
    close_in chan;
    program
  with
  | Grammar.Error ->
      Printf.eprintf "parser error";
      close_in chan;
      exit 1
  | Lexer.SyntaxError ->
      Printf.eprintf "lexer error";
      close_in chan;
      exit 1

let compile file =
  let program = parse_file file in
  if !debug then print_endline (Syntax.show_program program);
  let game = Compiler.compile program in
  let chan = open_out !output_file in
  Printf.fprintf chan "%s" game;
  close_out chan

let () =
  Arg.parse speclist anon_fun usage;
  if List.length !files = 1 then compile (List.hd !files)
  else Printf.eprintf "expected only 1 file to compile"
