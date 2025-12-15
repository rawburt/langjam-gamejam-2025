let usage = "lgc [options] [-o <output>] <file1>"
let check = ref false
let output_file = ref "game.js"
let files = ref []
let anon_fun f = files := f :: !files

let speclist =
  [
    ("-debug", Arg.Set Common.debug, "Compile in debug mode");
    ("-check", Arg.Set check, "Run only the type checker");
    ("-o", Arg.Set_string output_file, "Set output file name (default: game.js)");
  ]

let compile file =
  let library = Registry.load file in
  if !Common.debug then print_endline (Syntax.show_library library);
  try
    let (Syntax.Library lib) = library in
    let program = Syntax.Program lib.top in
    Typecheck.check program;
    if not !check then (
      let game = Compiler.compile program in
      let chan = open_out !output_file in
      Printf.fprintf chan "%s\n" game;
      close_out chan)
  with Typecheck.TypeError (msg, loc) ->
    Common.error ~loc ~kind:"type error" msg

let () =
  Arg.parse speclist anon_fun usage;
  if List.length !files = 1 then compile (List.hd !files)
  else Common.error "expected only 1 file to compile"
