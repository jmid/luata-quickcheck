(** Various utility (mostly string-related) functions *)

(*  lua_escape_string : string -> string *)
let lua_escape_string s =
  let cs = ref [] in
  let () = String.iter (fun c -> match c with
                                  | '\n' -> cs := 'n'::'\\'::!cs
                                  | '\\' -> cs := '\\'::'\\'::!cs
                                  | '\"' -> cs := '\"'::'\\'::!cs
                                  | '\'' -> cs := '\''::'\\'::!cs
				  | _    -> cs := c::!cs) s in
  let len = List.length !cs in
  let buf = Buffer.create len in
  let () = List.iter (Buffer.add_char buf) (List.rev !cs) in
  Buffer.contents buf

(*  escape_quotes : string -> string *)
let escape_quotes s =
  let cs = ref [] in
  let () = String.iter (fun c -> match c with
                                  | '\"' -> cs := '\"'::'\\'::!cs
                                  | '\'' -> cs := '\''::'\\'::!cs
				  | _    -> cs := c::!cs) s in
  let len = List.length !cs in
  let buf = Buffer.create len in
  let () = List.iter (Buffer.add_char buf) (List.rev !cs) in
  Buffer.contents buf

(*  html_escape_string : string -> string  *)
let html_escape_string s =
  let cs = ref [] in
  let () = String.iter (fun c -> match c with
                                  | '\n' -> cs := 'n'::'\\'::!cs
                                  | '\\' -> cs := '\\'::'\\'::!cs
                                  | '\"' -> cs := '\"'::'\\'::!cs
                                  | '\'' -> cs := '\''::'\\'::!cs
                                  | '<'  -> cs := ';'::'t'::'l'::'&'::!cs
				  | '>'  -> cs := ';'::'t'::'g'::'&'::!cs
				  | '&'  -> cs := ';'::'p'::'m'::'a'::'&'::!cs
				  | _    -> cs := c::!cs) s in
  let len = List.length !cs in
  let buf = Buffer.create len in
  let () = List.iter (Buffer.add_char buf) (List.rev !cs) in
  Buffer.contents buf
