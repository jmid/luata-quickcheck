(** Operations for error reporting *)

(*  calc_pos : Lexing.position -> int * int  *)
let calc_pos pos =
  let line   = pos.Lexing.pos_lnum in
  let column = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
  (line,column)

(*  pos_to_string : Lexing.position -> string  *)
let pos_to_string pos =
  let fname    = pos.Lexing.pos_fname in
  let line,col = calc_pos pos in
  fname ^ ":" ^ (string_of_int line) ^ ":" ^ (string_of_int col) ^ ":"

(*  print_col_mark : Lexing.position -> string -> unit  *)
let print_col_mark pos line =
  let rec print_col i col =
    if i > col
    then print_string "^"
    else
      begin
	(match line.[i] with
          | '\t' -> print_char '\t'
          | _    -> print_char ' ');
	print_col (i+1) col
      end
  in
  (try print_col 0 (pos.Lexing.pos_cnum - pos.Lexing.pos_bol - 1)
   with Invalid_argument _ ->
     print_newline ();
     print_endline
       "Warning: attempt to print invalid Lexing.position: column index out of bounds")
  
(*  get_line : Lexing.position -> string *)
let rec get_line pos =
  let inch = open_in pos.Lexing.pos_fname in
  let rec read_line i = match i with
    | 1 -> input_line inch; (*  Note: may raise 'End_of_file' *)
    | n -> let _ = input_line inch in
	   read_line (i-1) in
    let line = read_line pos.Lexing.pos_lnum in
    begin
      close_in inch;
      line
    end

(*  error : string -> string -> 'a  *)
let error msg1 msg2 pos =
  if pos = Lexing.dummy_pos
  then
    begin
      List.iter print_string [msg1; ", "; msg2; "\n\n"];
      exit 1
    end
  else
      if pos.Lexing.pos_fname = ""
      then
	begin
	  List.iter print_string [pos_to_string pos; " "; msg1; " - "; msg2; "\n\n"];
	  exit 1
	end
      else
        begin
	  List.iter print_string [pos_to_string pos; " "; msg1; " - "; msg2; "\n\n"];
	  (try
	     let line = get_line pos in
	     print_endline line;
	     print_col_mark pos line;
	     print_newline ();
	     exit 1
	   with
	     | End_of_file -> 
	       print_endline "Unexpected end of file";
	       exit 1)
	end
(*
      let myConsole = Js.Unsafe.variable "console" in
      let () = myConsole##log(Js.string s) in
*)
