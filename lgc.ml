let usage = "lgc [options] [-o <output>] <file1>"
let analyze = ref false
let ast = ref false
let output_file = ref "out.js"
let files = ref []
let anon_fun f = files := f :: !files

let speclist =
  [
    ("-debug", Arg.Set Common.debug, "Compile in debug mode");
    ("-analyze", Arg.Set analyze, "Run only the analyzer");
    ("-ast", Arg.Set ast, "Show AST before compiling");
    ("-o", Arg.Set_string output_file, "Set output file name (default: " ^ !output_file ^ ")");
  ]

let compile file =
  let program = Registry.make_program file in
  Common.trace ("compiling: " ^ file);
  try
    if !ast then print_endline ("======= AST:\n" ^ Syntax.show_program program);
    Analyzer.run program;
    if not !analyze then (
      let game = Emit.compile program in
      let chan = open_out !output_file in
      Printf.fprintf chan "%s\n" game;
      close_out chan)
  with
  | Type_check.TypeError (msg, loc) -> Common.error ~loc ~kind:"type error" msg
  | Errors.AnalysisError (msg, loc) ->
      Common.error ~loc ~kind:"analysis error" msg

let () =
  Arg.parse speclist anon_fun usage;
  if List.length !files = 1 then compile (List.hd !files)
  else Common.error "expected only 1 file to compile"
