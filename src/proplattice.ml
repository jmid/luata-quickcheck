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
type elem = { table       : (VL.elem * Abs.elem) TableMap.t;
	      default_key : VL.elem;
	      default     : VL.elem;
	      scopechain  : ScopeChainSet.t; }

(*  leq : elem -> elem -> bool  *)
let leq m0 m1 =
  VL.leq m0.default_key m1.default_key
  &&
  VL.leq m0.default m1.default
  &&
  (TableMap.fold (fun p (v,a) acc ->
    let v'',a'' = (match p with
		    | TableKey.String s  ->
		      if VL.leq (VL.string s) m1.default_key
		      then (m1.default,Abs.maybe_absent)
		      else (VL.bot,Abs.maybe_absent)
		    | TableKey.Metatable ->
		      if VL.leq VL.anystring m1.default_key
		      then (m1.default,Abs.maybe_absent)
		      else (VL.bot,Abs.maybe_absent)) in
    let v',a' = try TableMap.find p m1.table
                with Not_found -> (VL.bot,Abs.maybe_absent) (*(m1.default,Abs.maybe_absent)*)
    in
    let v' = VL.join v' v'' in
    let a' = Abs.join a' a''
    in acc && VL.leq v v' && Abs.leq a a') m0.table true)
  &&
  ScopeChainSet.subset m0.scopechain m1.scopechain

(*  eq : elem -> elem -> bool  *)
let eq m0 m1 = leq m0 m1 && leq m1 m0  (* a pragmatic implementation choice *)

(*  join : elem -> elem -> elem  *)
let join m0 m1 = 
  let def_key = VL.join m0.default_key m1.default_key in
  let def     = VL.join m0.default m1.default in
  let tab     = TableMap.merge (fun s l0 l1 -> match l0,l1 with
    | None,         None         -> None
    | None,         Some (v1,a1) ->
      (match s with
	| TableKey.String s  ->
	  if VL.leq (VL.string s) m0.default_key
	  then Some (VL.join m0.default v1, Abs.maybe_absent)
	  else Some (v1, Abs.maybe_absent)
	| TableKey.Metatable ->
	  if VL.leq VL.anystring m0.default_key
	  then Some (VL.join m0.default v1, Abs.maybe_absent)
	  else Some (v1, Abs.maybe_absent))
    | Some (v0,a0), None         ->
      (match s with
	| TableKey.String s  ->
	  if VL.leq (VL.string s) m1.default_key
	  then Some (VL.join v0 m1.default, Abs.maybe_absent)
	  else Some (v0, Abs.maybe_absent)
	| TableKey.Metatable ->
	  if VL.leq VL.anystring m1.default_key
	  then Some (VL.join v0 m1.default, Abs.maybe_absent)
	  else Some (v0, Abs.maybe_absent))
    | Some (v0,a0), Some (v1,a1) -> Some (VL.join v0 v1, Abs.join a0 a1)) m0.table m1.table in
  let sch     = ScopeChainSet.union m0.scopechain m1.scopechain in
  { table = tab; default_key = def_key; default = def; scopechain = sch }

(*  meet : elem -> elem -> elem  *)
let meet m0 m1 = 
  let def_key = VL.meet m0.default_key m1.default_key in
  let def     = VL.meet m0.default m1.default in
  let tab     = TableMap.merge (fun s l0 l1 -> match l0,l1 with
    | None,         None         -> None
    | None,         Some (v1,a1) ->
      (match s with
	| TableKey.String s  ->
	  if VL.leq (VL.string s) m0.default_key
	  then Some (VL.meet m0.default v1, Abs.maybe_absent)
	  else None
	| TableKey.Metatable ->
	  if VL.leq VL.anystring m0.default_key
	  then Some (VL.meet m0.default v1, Abs.maybe_absent)
	  else None)
    | Some (v0,a0), None     ->
      (match s with
	| TableKey.String s  ->
	  if VL.leq (VL.string s) m1.default_key
	  then Some (VL.meet v0 m1.default, Abs.maybe_absent)
	  else None
	| TableKey.Metatable ->
	  if VL.leq VL.anystring m1.default_key
	  then Some (VL.meet v0 m1.default, Abs.maybe_absent)
	  else None)
    | Some (v0,a0), Some (v1,a1) -> Some (VL.meet v0 v1, Abs.meet a0 a1)) m0.table m1.table in
  let sch     = ScopeChainSet.inter m0.scopechain m1.scopechain in
  { table = tab; default_key = def_key; default = def; scopechain = sch }

(*  bot : elem  *)
let bot = { table       = TableMap.empty;
	    default_key = VL.bot;
	    default     = VL.bot;
	    scopechain  = ScopeChainSet.empty; }

(*  mem : str -> map -> bool *)
let mem s map = TableMap.mem (TableKey.String s) map.table

(*  find_exn : str -> map -> VL *)
let find_exn str map =
  let (vlat,_abs) = TableMap.find (TableKey.String str) map.table in (* used for variable lookup *)
  vlat

(*  find : str -> map -> VL *)
let find str map =
  try
    let (vlat,abs) = TableMap.find (TableKey.String str) map.table in
    if abs = Abs.is_present
    then vlat
    else VL.join vlat VL.nil (* not definite read --> include nil *)
  with Not_found -> VL.join map.default VL.nil (* maybe not covered by default *)

(*  find_default : map -> VL * VL *)
let find_default map = (map.default_key, map.default)

(*  find_all_keys : map -> VL *)
let find_all_keys map =
  TableMap.fold (fun name (_vlat,_abs) acc -> match name with
                   | TableKey.Metatable    -> acc  (* excluding metatable *)
		   | TableKey.String kname -> VL.join (VL.string kname) acc) map.table map.default_key

(*  find_all : map -> VL *)
let find_all map = 
  TableMap.fold (fun name (vlat,_abs) acc -> match name with
                   | TableKey.Metatable   -> acc  (* excluding metatable *)
		   | TableKey.String name -> VL.join vlat acc) map.table map.default

(*  get_metatable : map -> VL *)
let get_metatable map =
  try
    let (vlat,abs) = TableMap.find TableKey.Metatable map.table in
    if abs = Abs.is_present
    then vlat
    else VL.join vlat VL.nil (* not definite read --> include nil *)
  with Not_found -> VL.nil   (* failure: return nil *)

(*  set_metatable : VL -> map -> map  *)
let set_metatable v map =
  if v = VL.bot
  then bot
  else { map with table = TableMap.add TableKey.Metatable (v,Abs.is_present) map.table }

(*  set_metatable_absent : map -> map  *)
let set_metatable_absent map =
  try
    let (vlat,abs) = TableMap.find TableKey.Metatable map.table in
    if abs = Abs.is_present (* already marked absent? *)
    then { map with table = TableMap.add TableKey.Metatable (vlat,Abs.maybe_absent) map.table }
    else map
  with Not_found -> map   (* failure: return nil *)

(*  add : str -> val -> map -> map *)
let add s v map =
  { map with table = TableMap.add (TableKey.String s) (v,Abs.is_present) map.table }

(*  add_default : val -> val -> map -> map *)
let add_default k v map =
  let k = VL.exclude_nil k in
  if k = VL.bot || v = VL.bot
  then { map with default_key = VL.bot; default = VL.bot } (* reduce domain *)
  else { map with default_key = k; default = v }

(*  add_scopechain : map -> label list -> map *)
let add_scopechain map sc =
  { map with scopechain = ScopeChainSet.add sc map.scopechain }

(*  add_all : val -> val -> map -> map *)
let add_all k v map = 
  let key = VL.exclude_nil (VL.join k map.default_key) in
  { table       = TableMap.mapi (fun key (old_vlat,old_abs) -> match key with
                                   | TableKey.Metatable -> (old_vlat,old_abs)  (* excluding metatable *)
				   | TableKey.String _  -> (VL.join v old_vlat,old_abs)) map.table;
    default_key = key;
    default     = VL.join v map.default;
    scopechain  = map.scopechain; }

(*  add_all_params : str list -> val list -> map -> map *)
let rec add_all_params xs vs map = match (xs,vs) with
  | []   , []    -> map
  | []   , v::vs -> map                                     (* drop superflous arguments *)
  | x::xs, []    -> add_all_params xs vs (add x VL.nil map) (* unsupplied args are nil *)
  | x::xs, v::vs -> add_all_params xs vs (add x v map)


(** {2 Pretty printing} *)

(*  pprint : PL -> unit *)
let pprint plat =
  (*  string_of_tablekey : tablekey -> string *)
  let string_of_tablekey k = match k with
    | TableKey.Metatable -> "metatable"
    | TableKey.String s  -> s
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
