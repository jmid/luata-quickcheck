(** Abstract datatype and operations for property maps (environments) *)

module Abs = Absencelattice
module VL  = Valuelattice

module TableKey = struct
  type t =
    | String of string
    | Metatable
  let compare k k' = match (k,k') with
    | Metatable, Metatable ->  0
    |  String _, Metatable -> -1  (* ordering: forall s. s < metatable = metatable *)
    | Metatable,  String _ ->  1
    |  String s, String s' -> String.compare s s'
end
module TableMap = Map.Make(TableKey)

module ScopeChainSet = 
  Set.Make(struct type t = int list
		  let rec compare is1 is2 = match is1,is2 with
		    | [],[] -> 0
		    | [],_  -> 1
		    | _,[]  -> -1
		    | i1::is1',i2::is2' ->
		      let r = compare is1' is2' in
		      (match r with
		        | 0 -> i1 - i2
		        | _ -> r) end)

(* TODO: add default (integer) index for more precise "array approximation" *)
type elem =
  | Bot
  | Table of table
and table = { table       : (VL.elem * Abs.elem) TableMap.t;
	      default_str : VL.elem;   (* fallback for string entries *)
	      default_key : VL.elem;   (* collective approx. of non-string keys *)
	      default     : VL.elem;   (* collective approx. of non-string entries *)
	      scopechain  : ScopeChainSet.t; }

(*  table : table -> elem  *)
let table { table; default_str; default_key; default; scopechain } =
  Table { table; default_str; default_key; default; scopechain }
  
(*  bot : elem  *)
let bot = Bot

(*  mt : elem  *)
let mt = Table { table       = TableMap.empty;
		 default_str = VL.bot;
		 default_key = VL.bot;
		 default     = VL.bot;
		 scopechain  = ScopeChainSet.empty; }

(*  leq : elem -> elem -> bool  *)
let leq m0 m1 = match m0,m1 with
  | Bot,_ -> true
  | _,Bot -> false
  | Table m0,Table m1 ->
    VL.leq m0.default_str m1.default_str
 && VL.leq m0.default_key m1.default_key
 && VL.leq m0.default m1.default
 && (TableMap.fold
       (fun p (v,a) acc ->
	 acc
	 && (try let v',a' = TableMap.find p m0.table in
	         (VL.leq v' v && Abs.leq a' a)
             with Not_found -> (* not necessarily in first map *)
	       Abs.eq a Abs.maybe_absent)) (* if definite entry in second: not ordered *)
       m1.table true)
  && (TableMap.fold
	(fun p (v,a) acc ->
	  acc
	  &&
	    (try let _ = TableMap.find p m1.table in
		 true (* elements in both have already been checked above *)
             with Not_found -> (* not necessarily in second map *)
	       (match p with
		 | TableKey.String s  -> (VL.leq v m1.default_str)
		 | TableKey.Metatable -> false))) (* metatable in first, but not in second *)
	m0.table true)
    && ScopeChainSet.subset m0.scopechain m1.scopechain

(*  eq : elem -> elem -> bool  *)
let eq m0 m1 = leq m0 m1 && leq m1 m0  (* a pragmatic implementation choice *)
  
(*  join : elem -> elem -> elem  *)
let join m0 m1 = match m0,m1 with
  | Bot,_ -> m1
  | _,Bot -> m0
  | Table m0,Table m1 ->
    let def_str = VL.join m0.default_str m1.default_str in
    let def_key = VL.join m0.default_key m1.default_key in
    let def     = VL.join m0.default m1.default in
    let tab     = TableMap.merge (fun s l0 l1 -> match l0,l1 with
      | None,         None         -> None
      | None,         Some (v1,a1) ->
	(match s with
	  | TableKey.String s  ->
	    Some (VL.join m0.default_str v1, Abs.maybe_absent)
	  | TableKey.Metatable ->
	    Some (v1, Abs.maybe_absent)) (* default does not include metatables *)
      | Some (v0,a0), None         ->
	(match s with
	  | TableKey.String s  ->
	    Some (VL.join v0 m1.default_str, Abs.maybe_absent)  (*fixed was: m1.default *)
	  | TableKey.Metatable ->
	    Some (v0, Abs.maybe_absent))
      | Some (v0,a0), Some (v1,a1) -> Some (VL.join v0 v1, Abs.join a0 a1)) m0.table m1.table in
    let sch     = ScopeChainSet.union m0.scopechain m1.scopechain in
    table { table = tab; default_str = def_str; default_key = def_key; default = def; scopechain = sch }

exception Returnbot

(*  meet : elem -> elem -> elem  *)
let meet m0 m1 = match m0,m1 with
  | Bot,_ -> bot
  | _,Bot -> bot
  | Table m0,Table m1 ->
    try
      let def_str = VL.meet m0.default_str m1.default_str in
      let def_key = VL.meet m0.default_key m1.default_key in
      let def     = VL.meet m0.default m1.default in
      let tab     = TableMap.merge (fun s l0 l1 -> match l0,l1 with
	| None,         None         -> None
	| None,         Some (v1,a1) ->
	  (match s with
	    | TableKey.String s  ->
		let entrycand = VL.meet m0.default_str v1 in
		if VL.is_bot entrycand
		then
		  (if Abs.eq a1 Abs.is_present
		   then raise Returnbot (* certainly in second, but not in meet --> return bot *)
		   else Some (entrycand, a1) (* maybe in second and not in result *)
		  )                          (*   -> absent from meet, represented as uncertain bot entry *)
		else
		  Some (entrycand, a1)
	    | TableKey.Metatable ->
	      (if Abs.eq a1 Abs.is_present
	       then raise Returnbot
	       else None))
	| Some (v0,a0), None     ->
	  (match s with
	    | TableKey.String s  ->
	      let entrycand = VL.meet v0 m1.default_str in
	      if VL.is_bot entrycand
	      then
		(if Abs.eq a0 Abs.is_present
		 then raise Returnbot (* certainly in first, but not in meet --> return bot *)
		 else Some (entrycand, a0) (* maybe in first and not in result *)
		)                          (*   -> absent from meet, represented as uncertain bot entry *)
	      else Some (entrycand, a0)
	    | TableKey.Metatable ->
	      (if Abs.eq a0 Abs.is_present
	       then raise Returnbot
	       else None))
	| Some (v0,a0), Some (v1,a1) ->
	  let abscand = Abs.meet a0 a1 in
	  let entrycand = VL.meet v0 v1 in
	  if VL.is_bot entrycand
	  then
	    (if Abs.eq abscand Abs.is_present
	     then raise Returnbot (* definite, empty entry -> result is bottom *)
	     else (Some (entrycand, abscand)))
	    else
	  Some (entrycand, abscand)) m0.table m1.table in
      let sch     = ScopeChainSet.inter m0.scopechain m1.scopechain in
      table { table = tab; default_str = def_str; default_key = def_key; default = def; scopechain = sch }
    with Returnbot -> Bot

(*  is_bot : map -> bool  *)
let is_bot pl = leq pl bot

(*  mem : str -> map -> bool *)
let mem s map = match map with
  | Bot       -> false
  | Table map -> TableMap.mem (TableKey.String s) map.table

(*  find_exn : str -> map -> VL * Abs *)
let find_exn str map = match map with
  | Bot       -> raise Not_found
  | Table map -> TableMap.find (TableKey.String str) map.table (* used for variable lookup *)

(*  find : str -> map -> VL *)
let find str map = match map with
  | Bot       -> VL.bot
  | Table map ->
    try
      let (vlat,abs) = TableMap.find (TableKey.String str) map.table in
      if abs = Abs.is_present
      then vlat
      else VL.join vlat VL.nil (* not definite read --> include nil *)
    with Not_found ->
      VL.join map.default_str VL.nil (* maybe not covered by default *)

(*  find_nonstr_defaults : map -> VL * VL *)
let find_nonstr_defaults map = match map with
  | Bot       -> (VL.bot,VL.bot)
  | Table map -> (map.default_key, map.default)

(*  find_all_keys : map -> VL *)
let find_all_keys map = match map with
  | Bot       -> VL.bot
  | Table map -> 
    TableMap.fold (fun name (_vlat,_abs) acc -> match name with
                     | TableKey.Metatable    -> acc  (* excluding metatable *)
		     | TableKey.String kname -> VL.join (VL.string kname) acc)
      map.table (VL.join map.default_key
		   (if VL.is_bot map.default_str then VL.bot else VL.anystring))

(*  find_all : map -> VL *)
let find_all map = match map with
  | Bot       -> VL.bot
  | Table map -> 
    TableMap.fold (fun name (vlat,_abs) acc -> match name with
                     | TableKey.Metatable   -> acc  (* excluding metatable *)
		     | TableKey.String name -> VL.join vlat acc) map.table map.default_str

(*  get_metatable : map -> VL *)
let get_metatable map = match map with
  | Bot       -> VL.bot
  | Table map ->
    try
      let (vlat,abs) = TableMap.find TableKey.Metatable map.table in
      if abs = Abs.is_present
      then vlat
      else VL.join vlat VL.nil (* not definite read --> include nil *)
    with Not_found -> VL.nil   (* failure: return nil *)

(*  set_metatable : VL -> map -> map  *)
let set_metatable v map = match map with
  | Bot       -> bot
  | Table map ->
    if v = VL.bot
    then bot
    else table { map with table = TableMap.add TableKey.Metatable (v,Abs.is_present) map.table }

(*  set_metatable_absent : map -> map  *)
let set_metatable_absent map' = match map' with
  | Bot       -> bot
  | Table map ->
    try
      let (vlat,abs) = TableMap.find TableKey.Metatable map.table in
      if abs = Abs.is_present (* already marked absent? *)
      then table { map with table = TableMap.add TableKey.Metatable (vlat,Abs.maybe_absent) map.table }
      else map'
    with Not_found -> map'   (* failure: return nil *)

(*  add : str -> val -> map -> map *)
let add s v map = match map with
  | Bot       -> bot
  | Table map ->
    if VL.leq v VL.bot
    then bot
    else
      table { map with table =
	        let v' = VL.exclude_nil v in
 	        if VL.may_be_nil v || VL.is_bot v'
		then TableMap.add (TableKey.String s) (v',Abs.maybe_absent) map.table
		else TableMap.add (TableKey.String s) (v',Abs.is_present) map.table }

(*  add_local : str -> val -> map -> map *)
let add_local s v map = match map with
  | Bot       -> bot
  | Table map ->
    if VL.leq v VL.bot
    then bot
    else (* adding a local variable with value nil, makes it visible in scope, e.g. for recursion *)
      table { map with table = TableMap.add (TableKey.String s) (v,Abs.is_present) map.table }
	
(*  add_nonstr_default : val -> val -> map -> map *)
let add_nonstr_default k v map = match map with
  | Bot       -> bot
  | Table map ->
    if VL.leq k VL.bot || VL.leq v VL.bot
    then bot
    else
      let k = VL.exclude_nil k in
      if VL.leq k VL.bot
      then table { map with default_key = VL.bot; default = VL.bot } (* reduce domain *)
      else table { map with default_key = k; default = (VL.exclude_nil v) }

(*  add_scopechain : map -> label list -> map *)
let add_scopechain map sc = match map with
  | Bot       -> bot
  | Table map -> table { map with scopechain = ScopeChainSet.add sc map.scopechain }

(*  add_all_str : val -> map -> map *)
let add_all_str v map = match map with
  | Bot       -> bot
  | Table map ->
    if VL.leq v VL.bot
    then bot
    else
      let delete = VL.may_be_nil v in
      table { table       = TableMap.mapi
	                      (fun key (old_vlat,old_abs) -> match key with
				| TableKey.Metatable -> (old_vlat,old_abs)  (* excluding metatable *)
				| TableKey.String _  ->
				  if delete
				  then (VL.join (VL.exclude_nil v) old_vlat,Abs.maybe_absent)
				  else (VL.join (VL.exclude_nil v) old_vlat,old_abs)) map.table;
	      default_str = VL.join v map.default_str;
	      default_key = map.default_key;
	      default     = map.default;
	      scopechain  = map.scopechain; }

(*  add_all_params : str list -> val list -> map -> map *)
let rec add_all_params xs vs map' = match map' with
  | Bot       -> bot
  | Table map -> match (xs,vs) with
      | []   , []    -> map'
      | []   , v::vs -> map'                                    (* drop superflous arguments *)
      | x::xs, []    -> add_all_params xs vs (add x VL.nil (Table map)) (* unsupplied args are nil *)
      | x::xs, v::vs -> add_all_params xs vs (add x v (Table map))


(** {2 Pretty printing} *)

(*  pprint : PL -> unit *)
let pprint plat =
  (*  string_of_tablekey : tablekey -> string *)
  let string_of_tablekey k = match k with
    | TableKey.Metatable -> "metatable"
    | TableKey.String s  -> "\"" ^ s ^ "\""
  in
  (*  pprint_list : int list -> unit *)
  let pprint_list is =
    begin
      Format.printf "[ @[<h 1>";
      ignore (List.fold_left (fun first i ->
	  if first
	  then (Format.print_int i; false)
	  else
	    begin
	      Format.printf ",";
	      Format.print_space ();
	      Format.print_int i;
	      false
	    end) true is);
      Format.printf "@] ]";
    end in
  match plat with
    | Bot        -> Format.printf "bot"
    | Table plat ->
      begin
	Format.printf "{ @[<v 0>";
    (* print table *)
	let first = 
	  TableMap.fold (fun prop (vlat,abs) first -> 
	    if first
	    then
	      begin
		Format.printf "%-12s -> " (string_of_tablekey prop);
		Abs.pprint abs;
		Format.printf " ";
		VL.pprint vlat;
		false
	      end
	    else
	      begin
		Format.print_space ();
		Format.printf "%-12s -> " (string_of_tablekey prop);
		Abs.pprint abs; Format.printf " "; VL.pprint vlat;
		false
	      end) plat.table true in
    (* prettyprint default str *)
	let first =
	  (if VL.is_bot plat.default_str
	   then first
	   else
	      begin
		(if first then () else Format.print_space ());
		Format.printf "%-12s ->   " "default str";
		VL.pprint plat.default_str;
		false
	      end) in
    (* prettyprint default key and default *)
	(if VL.is_bot plat.default_key
	 then ()
	 else
	    begin
	      (if first then () else Format.print_space ());
	      Format.printf "%-12s ->   " "default key";
              VL.pprint plat.default_key;
	      Format.print_space ();
	      Format.printf "%-12s ->   " "default";
	      VL.pprint plat.default
	    end);
    (* prettyprint scopechain *)
	(if plat.scopechain = ScopeChainSet.empty
	 then ()
	 else
	    begin
	      (if first then () else Format.print_space ());
	      Format.printf "%-12s ->   " "scopechain";
	      Format.printf "{ @[<h 1>";
	      ignore (ScopeChainSet.fold (fun sc first -> 
		if first
		then (pprint_list sc; false)
		else
		  begin
		    Format.printf ",";
		    Format.print_space ();
		    pprint_list sc;
		    false
		  end) plat.scopechain true);
	      Format.printf "@] }";
	    end);
	Format.printf "@] }";
      end

(*  to_string : elem -> string  *)
let to_string pl =
  let buf = Buffer.create 128 in
  let out,flush = Format.get_formatter_output_functions () in (* save previous outputters *)
  begin
    Format.set_formatter_output_functions (Buffer.add_substring buf) (fun () -> ());
    pprint pl;
    Format.print_flush ();
    Format.set_formatter_output_functions out flush;          (* restore previous outputters *)
    Buffer.contents buf
  end
