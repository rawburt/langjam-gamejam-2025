let imported = Hashtbl.create 10
let add_import file = Hashtbl.add imported file ()
let needs_import file = not (Hashtbl.mem imported file)

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

let find_imports file imports =
  let base_dir = Filename.dirname file in
  let lib_dir = !Common.lib_dir in
  let lookups = [ base_dir; lib_dir ] in
  let find_file (import, loc) =
    let basename = import ^ ".lg" in
    let fullpath dir = Filename.concat dir basename in
    let file_to_load =
      List.map fullpath lookups |> List.find_opt Sys.file_exists
    in
    match file_to_load with
    | Some f -> f
    | None -> Common.error ~loc ~kind:"import" ("not found: " ^ import)
  in
  List.map find_file imports

(* given a path, parse file, resolve imports, return program ast *)
let rec load file : Syntax.toplevel list =
  Common.trace ("loading: " ^ file);
  add_import file;
  let library = parse_file file in
  let (Library lib) = library in
  let imports = find_imports file lib.imports |> List.map load in
  imports @ [ lib.top ] |> List.concat

let make_program file = Syntax.Program (load file)
