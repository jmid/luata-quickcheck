(** Abstract value datatype and operations *)

module Num = Numberlattice
module Str = Stringlattice

(** {3 Lattice type and primitives } *)

type tag =
 | Nil
 | Bool
 | Userdata

type proc =
 | Funtag of int
 | Builtin of builtin
and builtin =
 | Error
 | Exit
 | Next
 | INext (* internal to IPairs *)
 | Pairs
 | IPairs
 | Print
 | Write
 | Tonumber
 | Tostring
 | Abs
 | Ceil
 | Floor
 | Mod
 | Random
 | Strlen
 | Strupper
 | Strlower
 | Strchar
 | Strbyte
 | Strsub
 | Sqrt
 | Type
 | Format
 | Tblconcat
 | Getmetatable
 | Setmetatable
 | Rawget
 | Rawset

(* TODO: experiment with Patricia trees  *)
module TagSet   = Set.Make(struct type t = tag 
		 		  let compare = Pervasives.compare end)
module ProcSet  = Set.Make(struct type t = proc
				  let compare = Pervasives.compare end)
module LabelSet = Set.Make(struct type t = int
				  let compare = Pervasives.compare end)

type elem = { tags    : TagSet.t;
	      number  : Num.elem;
	      strings : Str.elem;
	      funs    : ProcSet.t;
	      tables  : LabelSet.t; }

(* let top = *)

let mttagset    = TagSet.empty
let mtprocset   = ProcSet.empty
let mtlabelset  = LabelSet.empty

(*  bot : VL  *)
let bot       = { tags    = mttagset;
		  number  = Num.bot;
		  strings = Str.bot;
 	          funs    = mtprocset;
	          tables  = mtlabelset; }

let mktagelem t = { bot with tags = TagSet.singleton t }

(*  nil : VL  *)
let nil    = mktagelem Nil

(*  bool : VL  *)
let bool   = mktagelem Bool

(*  number : VL  *)
let number = { bot with number  = Num.top }

(*  userdata : VL  *)
let userdata  = mktagelem Userdata

(*  string : string -> VL  *)
let string s  = { bot with strings = Str.const s }

(*  anystring : VL  *)
let anystring = { bot with strings = Str.top }

(*  table : label -> VL  *)
let table t   = { bot with tables  = LabelSet.singleton t }

(*  proc : label -> VL  *)
let proc p    = { bot with funs    = ProcSet.singleton (Funtag p) }

(*  builtin : builtin -> VL  *)
let builtin b = { bot with funs    = ProcSet.singleton (Builtin b) }

(*  eq : VL -> VL -> bool  *)
let eq v1 v2 =
  TagSet.equal v1.tags v2.tags
  && Num.eq v1.number v2.number
  && Str.eq v1.strings v2.strings
  && ProcSet.equal v1.funs v2.funs
  && LabelSet.equal v1.tables v2.tables

(*  leq : VL -> VL -> bool  *)
let leq v1 v2 =
  TagSet.subset v1.tags v2.tags
  && Num.leq v1.number v2.number
  && Str.leq v1.strings v2.strings
  && ProcSet.subset v1.funs v2.funs
  && LabelSet.subset v1.tables v2.tables

(*  join : VL -> VL -> VL  *)
let join v1 v2 =
  { tags    = TagSet.union v1.tags v2.tags;
    number  = Num.join v1.number v2.number;
    strings = Str.join v1.strings v2.strings;
    funs    = ProcSet.union v1.funs v2.funs;
    tables  = LabelSet.union v1.tables v2.tables; }

(*  meet : VL -> VL -> VL  *)
let meet v1 v2 =
  { tags    = TagSet.inter v1.tags v2.tags;
    number  = Num.meet v1.number v2.number;
    strings = Str.meet v1.strings v2.strings;
    funs    = ProcSet.inter v1.funs v2.funs;
    tables  = LabelSet.inter v1.tables v2.tables; }

(*  number_or_nil : VL  *)
let number_or_nil = join nil number


(** {3 Lattice queries } *)

(** Is argument bot in lattice order? *)
(*  is_bot : VL -> bool  *)
let is_bot vlat = leq vlat bot

(** Is argument at most nil in lattice order? *)
(*  is_nil : VL -> bool *)
let is_nil vlat = leq vlat nil

(** Is argument at least nil in lattice order? *)
(*  may_be_nil : VL -> bool  *)
let may_be_nil vlat = leq nil vlat

(** Can argument be different from nil in lattice order? *)
(*  may_be_non_nil : VL -> bool  *)
let may_be_non_nil vlat = 
     vlat.tags <> (TagSet.singleton Nil)
  || vlat.number <> Num.bot
  || vlat.strings <> Str.bot
  || vlat.funs <> mtprocset 
  || vlat.tables <> mtlabelset

(** Is argument at least bool in lattice order? *)
(*  may_be_bool : VL -> bool  *)
let may_be_bool vlat = leq bool vlat

(** Is argument at least userdata in lattice order? *)
(*  may_be_userdata : VL -> bool  *)
let may_be_userdata vlat = leq userdata vlat

(** Is argument at most number in lattice order? *)
(*  is_number : VL -> bool  *)
let is_number vlat = leq vlat number

(** Is argument at least number in lattice order? *)
(*  may_be_number : VL -> bool  *)
let may_be_number vlat = leq number vlat

(** Is argument at most String.top in lattice order? *)
(*  is_strings : VL -> bool  *)
let is_strings vlat = leq vlat anystring

(** Is argument strictly greater than String.bot in lattice order? *)
(*  may_be_strings : VL -> bool  *)
let may_be_strings vlat = vlat.strings <> Str.bot

(** Is argument strictly greater then bot for some non-string component in lattice order? *)
(*  may_be_non_strings : VL -> bool  *)
let may_be_non_strings vlat =
     vlat.number <> Num.bot
  || vlat.tags <> mttagset
  || vlat.funs <> mtprocset 
  || vlat.tables <> mtlabelset

(** Is argument strictly greater than ProcSet.empty in lattice order? *)
(*  may_be_proc : VL -> bool  *)
let may_be_proc vlat = vlat.funs <> mtprocset

(** Is argument strictly greater than LabelSet.empty in lattice order? *)
(*  may_be_table : VL -> bool  *)
let may_be_table vlat = vlat.tables <> mtlabelset


(** {3 Lattice operations } *)

(*  exclude_nil : VL -> VL *)
let exclude_nil lat =
  let tags = TagSet.remove Nil lat.tags in
  { lat with tags = tags }

(*  exclude_strings : VL -> VL *)
let exclude_strings lat =
  { lat with strings = Str.bot }
    
(*  exclude_proc : VL -> VL *)
let exclude_proc lat =
  { lat with funs = mtprocset }

(*  exclude_tables : VL -> VL *)
let exclude_tables lat =
  { lat with tables = mtlabelset }

(*  only_tables : VL -> VL *)
let only_tables lat =
  { bot with tables = lat.tables }

(*  coerce_tonum : VL -> VL *)
let coerce_tonum lat =
  if may_be_number lat
  then number
  else match lat.strings with
    | Str.Top     -> number
    | Str.Const s -> (try let _ = float_of_string (String.trim s) in number
                      with (Failure _) -> bot)
    | Str.Bot     -> bot

(*  coerce_tostring : VL -> VL *)
let coerce_tostring lat =
  if may_be_number lat
  then anystring
  else { bot with strings = lat.strings }

(*  or_join : VL -> VL -> VL *)
let or_join vlat0 vlat1 =
  if is_bot vlat0 || is_bot vlat1 then bot else (*FIXED*)
  join (if may_be_non_nil vlat0 then (exclude_nil vlat0) else bot)
       (if may_be_nil vlat0 then vlat1 else bot)

(*  upper : VL -> VL *)
let upper lat =
  let lat' = coerce_tostring lat in
  { lat' with strings = Str.upper lat'.strings }

(*  lower : VL -> VL *)
let lower lat =
  let lat' = coerce_tostring lat in
  { lat' with strings = Str.lower lat'.strings }

(*  char : VL -> VL *)
let char lat =
  let lat' = coerce_tonum lat in
  if may_be_number lat' (* char of number (or better) is any string *)
  then anystring
  else bot

(*  typep : VL -> VL *)
let typep lat =
  join (if may_be_nil lat        then string "nil" else bot)
   (join (if may_be_bool lat       then string "boolean" else bot)
     (join (if may_be_number lat     then string "number" else bot)
	(join (if may_be_userdata lat  then string "userdata" else bot)
	   (join (if may_be_table lat    then string "table" else bot)
	      (join (if may_be_strings lat then string "string" else bot)
	 	       (if may_be_proc lat   then string "function" else bot))))))

(*  unop : unop -> VL -> VL  *)
let unop op lat = 
  if leq lat bot
  then bot (* unary operation over bot is bot *)
  else
    match op with
    | Ast.Uminus ->
      let lat' = coerce_tonum lat in
      if may_be_number lat' (* unary minus of number (or better) is number *)
      then number
      else bot
    | Ast.Length ->
      if may_be_strings lat || may_be_table lat
      then number
      else bot    (* length of everything but strings and tables is number *)
    | Ast.Not ->
      bool   (* negation of anything is bool *)

(*  binop : binop -> VL -> VL -> VL  *)
let binop op lat0 lat1 = 
  if leq lat0 bot || leq lat1 bot
  then bot (* binary operation over a bot is bot *)
  else
    match op with
    | Ast.Eq
    | Ast.Ne -> bool (* anything can be compared *)
    | Ast.Lt
    | Ast.Gt
    | Ast.Le
    | Ast.Ge -> if (may_be_number lat0 && may_be_number lat1)
		  || (Str.nonempty lat0.strings && Str.nonempty lat1.strings)
                then bool
                else bot
    | Ast.Plus
    | Ast.Minus
    | Ast.Times
    | Ast.Div
    | Ast.Mod
    | Ast.Pow ->
      if may_be_number (coerce_tonum lat0) && may_be_number (coerce_tonum lat1)
      then number
      else bot
    | Ast.Concat ->
      let lat0' = coerce_tostring lat0 in
      let lat1' = coerce_tostring lat1 in
      { bot with strings = Str.concat lat0'.strings lat1'.strings }


(** {3 Pretty printing routines } *)

(*  pprint_tags : tagset -> unit  *)
let pprint_tags ts =
  (*  pprint_tag : tag -> unit  *)
  let pprint_tag t = match t with
    | Nil      -> Format.printf "Nil"
    | Bool     -> Format.printf "Bool"
    | Userdata -> Format.printf "Userdata" in
    begin
      Format.printf "{ @[<h 1>";
      ignore (TagSet.fold (fun t first -> 
	  if first
	  then (pprint_tag t; false)
	  else
	    begin
	      Format.printf ",";
	      Format.print_space ();
	      pprint_tag t;
	      false
	    end) ts true);
      Format.printf "@] }";
    end

(*  pprint_procs : procset -> unit  *)
let pprint_procs ts =
  (*  builtin_name : builtin -> string *)
  let builtin_name b = match b with
    | Error    -> "error"
    | Exit     -> "exit"
    | Next     -> "next"
    | INext    -> "inext"
    | Pairs    -> "pairs"
    | IPairs   -> "ipairs"
    | Print    -> "print"
    | Write    -> "write"
    | Tonumber -> "tonumber"
    | Tostring -> "tostring"
    | Abs      -> "abs"
    | Ceil     -> "ceil"
    | Floor    -> "floor"
    | Mod      -> "mod"
    | Random   -> "random"
    | Strlen   -> "strlen"
    | Strupper -> "strupper"
    | Strlower -> "strlower"
    | Strchar  -> "strchar"
    | Strbyte  -> "strbyte"
    | Strsub   -> "strsub"
    | Sqrt     -> "sqrt"
    | Type     -> "type"
    | Format   -> "format"
    | Tblconcat    -> "tblconcat"
    | Getmetatable -> "getmetatable"
    | Setmetatable -> "setmetatable"
    | Rawget   -> "rawget"
    | Rawset   -> "rawset" in

  (*  pprint_proc : proc -> unit  *)
  let pprint_proc p = match p with
    | Funtag t  -> Format.printf "%i" t
    | Builtin b -> Format.printf "[builtin:%s]" (builtin_name b) in

  begin
    Format.printf "{ @[<h 1>";
    ignore (ProcSet.fold (fun t first -> 
      if first
      then (pprint_proc t; false)
      else
	begin
	  Format.printf ",";
	  Format.print_space ();
	  pprint_proc t;
	  false
	end) ts true);
    Format.printf "@] }";
  end

(*  pprint_labels : labelset -> unit  *)
let pprint_labels ts =
    begin
      Format.printf "{ @[<h 1>";
      ignore (LabelSet.fold (fun t first -> 
	if first
	then (Format.printf "%i" t; false)
	else
	  begin
	    Format.printf ",";
	    Format.print_space ();
	    Format.printf "%i" t;
	    false
	  end) ts true);
      Format.printf "@] }";
    end

(*  pprint : VL -> unit  *)
let pprint v =
  let () = Format.printf "{ @[<v 0>" in
  let funs = 
    [(fun first -> if v.tags = mttagset then first else
	           begin
		     Format.printf "tags:     ";
		     pprint_tags v.tags;
		     false
		   end);
     (fun first -> if v.number = Num.bot then first else
	           begin
		     (if first then () else Format.print_space ());
		     Format.printf "number:   ";
		     Num.pprint v.number;
		     false
		   end);
     (fun first -> if v.strings = Str.bot then first else
	           begin
		     (if first then () else Format.print_space ());
		     Format.printf "strings:  ";
		     Str.pprint v.strings;
		     false
		   end);
     (fun first -> if v.funs = mtprocset then first else
	           begin
		     (if first then () else Format.print_space ());
		     Format.printf "funs:     ";
		     pprint_procs v.funs;
		     false
		   end);
     (fun first -> if v.tables = mtlabelset then first else
	           begin
		     (if first then () else Format.print_space ());
		     Format.printf "tables:   ";
		     pprint_labels v.tables;
		     false
		   end) ] in
  let _ = List.fold_left (fun firstacc f -> f firstacc) true funs in
  Format.printf "@] }"

(*  to_string : elem -> string  *)
let to_string v =
  let buf = Buffer.create 64 in
  let out,flush = Format.get_formatter_output_functions () in (* save previous outputters *)
  begin
    Format.set_formatter_output_functions (Buffer.add_substring buf) (fun () -> ());
    pprint v;
    Format.print_flush ();
    Format.set_formatter_output_functions out flush;          (* restore previous outputters *)
    Buffer.contents buf
  end
