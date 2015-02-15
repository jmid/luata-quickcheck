(** The JavaScript driver for the analysis tool *)

module SL = Statelattice
module JsUn = Js.Unsafe

(*  heaps_as_array : (label * alat) list -> string js_array  *)
let heaps_as_array heaps =
  Js.array (Array.of_list (List.map (fun (lab,slat) -> Js.string (SL.to_string slat)) heaps))
    
(*  warnings_as_array : (Lexing.position * string) list -> string js_array  *)
let warnings_as_array warnings =
  let warn_list =
    List.map (fun (pos,msg) -> JsUn.obj
                [| ("line", JsUn.inject pos.Lexing.pos_lnum);
		   ("msg",  JsUn.inject (Js.string msg)); |]) warnings in
  Js.array (Array.of_list warn_list)
    
(*  parse_pprint : js_string t -> js_string t  *)
let parse_pprint str =
  try
    let ast,last = Frontend.parse_string_and_label (Js.to_string str) in
    let alat = Transfer.transfer_prog last in
    let buf  = Buffer.create 256 in
    let fmt  = Format.formatter_of_buffer buf in
    let heaps     = Warnings.get_heaps last alat in
    let warnings  = warnings_as_array (Warnings.get_warnings last heaps) in
    let label_map = Last_pprint.pprint_prog fmt last in
    begin
      Format.print_newline ();
      Format.pp_print_flush fmt (); (* remember to flush to get contents *)
      Label.reset ();               (* + to reset gensym *)
      JsUn.obj [| ("last",      JsUn.inject (Js.string (Buffer.contents buf)));
		  ("errors",    JsUn.inject (JsUn.obj [| |]));
		  ("heaps",     JsUn.inject heaps);
		  ("warnings",  JsUn.inject warnings);
		  ("label_map", JsUn.inject (
		    Js.array ( Array.of_list (List.rev (List.map snd label_map) ) )
		   ));
	       |]
    end
  with (Frontend.Error (pos,msg)) ->
    let line,col = Error.calc_pos pos in
    let error    = JsUn.obj [| ("line",   JsUn.inject line);
			       ("column", JsUn.inject col);
			       ("msg",    JsUn.inject (Js.string msg)); |] in
    let obj      = JsUn.obj [| ("last",      JsUn.inject Js.null); (* no last *)
			       ("errors",    JsUn.inject error);
			       ("heaps",     JsUn.inject (Js.array [| |]));
			       ("warnings",  JsUn.inject (Js.array [| |]));
			       ("label_map", JsUn.inject Js.null);
			    |] in
    obj

(* export the parse_pprint and heaps_as_array functions to JS's global scope *)
let () = JsUn.global##parsePPrint <- Js.wrap_callback parse_pprint
let () = JsUn.global##heapsAsStrArray <- Js.wrap_callback heaps_as_array

(*  test_conversion : js_string t -> js_string t  *)
(*
let test_conversion str =
  (*let str' = Js.to_string str in*) (* note: result is identical on float_of_string str' *)
  let res = (try let _ = float_of_string "foo" in "foo-conversion succeeded!"
             with (Failure _) -> "foo-conversion failed!") in
  Js.string res
  
let () = Js.Unsafe.global##convert <- Js.wrap_callback test_conversion
*)
