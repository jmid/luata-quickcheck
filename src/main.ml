(** The main driver for the analysis tool *)

let _ =
  let version   = "v0.012" in
  let filenames = ref [] in
  let pp_ast    = ref false in
  let pp_last   = ref false in
  let pp_heap   = ref true in
  let warnings  = ref true in
  let usagemsg  = "Usage: luata [options] <filename>" in
  let argspec   = 
    Arg.align
      [("-pp-ast",  Arg.Set pp_ast,     " Prettyprint AST");
       ("-pp-last", Arg.Set pp_last,    " Prettyprint labeled AST");
       ("-no-heap", Arg.Clear pp_heap,  " Don't prettyprint the heap");
       ("-no-warn", Arg.Clear warnings, " Don't print analysis-based warnings");  ] in
  begin
    Arg.parse argspec (fun s -> filenames := s::(!filenames)) usagemsg;
    print_endline ("Welcome to LuaTA, " ^ version ^ "\n");
    if !Sys.interactive then () else
      match !filenames with
	| [fname] -> 
	  let ast,last = Frontend.parse_and_label fname in
	  let pnl      = print_newline in
	  let iftrueforce test f = if !test then f() else () in
	  begin
	    Printf.printf " Syntactically correct Lua\n\n";
	    iftrueforce pp_ast  (fun () -> Ast_pprint.pprint_prog Format.std_formatter ast; pnl ());
	    iftrueforce pp_last (fun () -> ignore(Last_pprint.pprint_prog Format.std_formatter last); pnl ());
	    let alat = Transfer.transfer_prog last in
	    iftrueforce pp_heap  (fun () -> Pprint.pprint_prog stdout last alat);
	    iftrueforce warnings (fun () -> ignore(Warnings.warn_prog last alat));
	  end
	| _       -> (Arg.usage argspec usagemsg; exit 1)
  end
