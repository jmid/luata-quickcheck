(** ASCII capable analysis pretty printer *)

module EL = Envlattice
module ST = Storelattice
module SL = Statelattice
module AL = Analysislattice

(*  pprint_prog : out_channel -> Last.prog -> string -> analysislattice -> unit *)
let pprint_prog outch p alat =
  let () = Format.set_formatter_out_channel outch in

  Format.printf "@[<v 0>";
  Format.print_newline ();
  
  (******** First : pretty print the labelled AST ********)

  (*  SL.pprint SL.init; *)
  Format.printf "@[<v>";
  Format.print_flush ();
  let _line_map = Last_pprint.pprint_prog Format.std_formatter p in
  Format.print_flush ();
  Format.printf "@]";
  Format.print_newline (); Format.print_newline ();

  (******** Second: pretty print the abstract heap ********)

  Format.print_newline ();
  let labels = p.Last.ret_label::(List.map fst p.Last.pos_map) in (* add final label *)
  let labels = List.sort compare labels in (* and sort the labels *)
  let _ = List.fold_left
           (fun (i,prefix) lab ->
             let slat = AL.lookup alat lab in
             (****** fold for ascii emitter ******)
	     begin
	       Format.printf "%3i: " lab;
  	       SL.pprint slat;
	       Format.print_space(); Format.print_newline();
       	       (i, ",")
	     end) (0,"") labels in
  Format.print_space();
  Format.printf "@]"
