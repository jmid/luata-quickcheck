(** Common operations for lexing and parsing *)

module Lx = Lexing

(*let _ = Parsing.set_trace true*)  (* Uncomment to debug parser *)

exception Error of Lexing.position * string

(*  parse_with_lexbuf : lexbuf -> (string -> string -> pos -> 'a) -> Ast.ast  *)
let parse_with_lexbuf lexbuf error =
  try
    Parser.chunks Scanner.token lexbuf
  with
  | Failure msg  ->
    error "Failure" msg lexbuf.Lexing.lex_curr_p
  | Parsing.Parse_error  -> 
    error "Parse error" "Syntactically incorrect Lua" lexbuf.Lexing.lex_curr_p
  | End_of_file  -> 
    error "Parse error" "Unexpected end of string" lexbuf.Lexing.lex_curr_p

(*  parse_with_filename : string -> Ast.ast  *)
let parse_with_filename fname =
  let in_ch  = open_in fname in
  let lexbuf = Lx.from_channel in_ch in
  let curr_p = lexbuf.Lx.lex_curr_p in
  let ()     = lexbuf.Lx.lex_curr_p <- { curr_p with Lx.pos_fname = fname } in
  let ast    = parse_with_lexbuf lexbuf Error.error in
  let ()     = close_in in_ch in
  ast

(*  parse_string_and_label : string -> Ast.ast * Last.last *)
let parse_string_and_label str =
  let lexbuf = Lx.from_string str in
  let ast    = parse_with_lexbuf lexbuf
    (fun m1 m2 pos ->
      raise (Error (pos, m1 ^ " - " ^ m2))) in
  (ast, Label.label_prog None ast) (* No filename available *)

(* parse_and_label : string -> Ast.ast * Last.last *)
let parse_and_label fname =
  let ()  = Label.reset() in
  let ast = parse_with_filename fname in
  (ast, Label.label_prog (Some fname) ast)
