open Syntax

exception AnalysisError of string * loc

let error loc msg = raise (AnalysisError (msg, loc))

let rec block_always_returns (Block stmts) =
  match stmts with
  | [] -> false
  | stmt :: rest ->
      stmt_always_returns stmt || block_always_returns (Block rest)

and stmt_always_returns = function
  | SRet _ -> true
  | SIfElse (_, b1, Some b2, _) ->
      block_always_returns b1 && block_always_returns b2
  | SIfElse (_, _, None, _) -> false
  | SFor _ -> false
  | _ -> false

let check_all_paths_return def =
  match def.ret with
  | None -> ()
  | Some _ ->
      if not (block_always_returns def.body) then
        error def.loc "not all paths return a value"

let rec return_validation = function
  | [] -> ()
  | TLDef def :: tls ->
      check_all_paths_return def;
      return_validation tls
  | _ :: tls -> return_validation tls

let analyze (Program toplevels) = return_validation toplevels

let run program =
  Typecheck.check program;
  analyze program
