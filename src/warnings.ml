(** Issue warnings based on the resulting analysis fixed point *)

module SL = Statelattice
module AL = Analysislattice

(*  get_heaps : Last.last -> alat -> (label * slat) list  *)
let get_heaps last alat =
  let rec scan_loop j acc = (* inner loop to scan to next present label *)
    if j<0
    then acc 
    else scan_loop (j-1) ((j,AL.lookup alat j)::acc) in
  scan_loop last.Last.ret_label []

(*  get_warnings : Last.last -> (label * alat) list -> (Lexing.position * string) list  *)
let get_warnings last heaps =
  List.fold_left
    (fun acc (lab,slat) ->
      if SL.is_bot slat
      then
	try
	  let stmt = List.assoc lab last.Last.pos_map in
	  let pos  = stmt.Last.stmt_pos.Last.lex_pos in
	  let line = pos.Lexing.pos_lnum in
	  if (pos = Lexing.dummy_pos)   (* weed needless returns *)
	    || List.exists (fun (pos',_) -> pos'.Lexing.pos_lnum = line) acc  (* and filter duplicates *)
	  then acc
	  else (pos,"Unreachable line")::acc
	with Not_found -> acc
      else acc) [] heaps

(*  warn_prog : Last.prog -> alat -> unit *)
let warn_prog p alat =
  let heaps = get_heaps p alat in
  let warns = get_warnings p heaps in
  List.iter (fun (pos,str) ->
               begin
		 Printf.printf "Warning: line %i: %s\n" pos.Lexing.pos_lnum str;
		 (try
		    let line = Error.get_line pos in
		    Printf.printf "%s\n\n" line;
		  with End_of_file ->
		    Printf.printf "Unexpected end of file\n\n")
	       end) (List.rev warns)
