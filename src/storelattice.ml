(** Abstract store datatype and operations *)

module VL = Valuelattice
module PL = Proplattice
module StoreMap = Map.Make(struct type t = int
				  let compare i i' = i - i' end)
module Str = Stringlattice

type elem = Proplattice.elem StoreMap.t

(*  leq : elem -> elem -> bool  *) 
let leq st0 st1 =
  StoreMap.fold (fun lab obj acc ->
    let obj' = try StoreMap.find lab st1
               with Not_found -> PL.bot
    in acc && PL.leq obj obj') st0 true

(*  eq : elem -> elem -> bool  *) 
let eq = StoreMap.equal Proplattice.eq

(*  join : elem -> elem -> elem  *) 
let join st0 st1 = StoreMap.merge (fun lab obj0 obj1 -> match obj0,obj1 with
  | None,_ -> obj1
  | _,None -> obj0
  | Some obj0,Some obj1 -> Some (PL.join obj0 obj1)) st0 st1

(*  meet : elem -> elem -> elem  *) 
let meet st0 st1 = StoreMap.merge (fun lab obj0 obj1 -> match obj0,obj1 with
  | None,_ -> None
  | _,None -> None
  | Some obj0, Some obj1 -> Some (PL.meet obj0 obj1)) st0 st1

(*  bot : elem  *)
let bot = StoreMap.empty


(** {3 Lattice operations } *)

(*  is_bot : store -> bool  *)
let is_bot st = leq st bot

(*  add_label : store -> label -> PL -> store *)
let add_label st lab prop =
  StoreMap.add lab prop st

(*  find_label_exn : store -> label -> PL *)
let find_label_exn st lab =
  StoreMap.find lab st

(*  find_label : store -> label -> PL *)
let find_label st lab =
  try find_label_exn st lab
  with Not_found -> PL.bot

(*  fold_labels_scopechain : (label list -> 'a -> 'a) -> store -> label -> 'a -> 'a *)
let fold_labels_scopechain f st lab acc =
  if is_bot st
  then acc
  else
    try
      let pl = find_label st lab in
      (match pl with
	| PL.Bot      -> acc
	| PL.Table pl -> PL.ScopeChainSet.fold f pl.PL.scopechain acc)
    with Not_found -> (*failwith ("No scope chain installed for label " ^ (string_of_int lab))*)
      (Printf.printf "No scope chain installed for label %i\n" lab;
       acc)

(*  lookup_str_prop : store -> VL -> str -> VL *)
let lookup_str_prop st vlat prop =
  if is_bot st || VL.is_bot vlat
  then VL.bot
  else
    let tables = vlat.VL.tables in
    VL.LabelSet.fold (fun lab acc -> 
      let plat = find_label st lab in
      VL.join acc (PL.find prop plat) ) tables VL.bot

(*  lookup_all_keys : store -> VL -> VL *)
let lookup_all_keys st vlat =
  let tables = vlat.VL.tables in
  VL.LabelSet.fold (fun lab acc -> 
    let plat = find_label st lab in
    VL.join acc (PL.find_all_keys plat) ) tables VL.bot 

(*  lookup_all_str_props : store -> VL -> VL *)
let lookup_all_str_props st vlat =
  let tables = vlat.VL.tables in
  VL.LabelSet.fold (fun lab acc -> 
    let plat = find_label st lab in
    VL.join acc (PL.find_all plat) ) tables VL.bot 

(*  lookup_all_nonstr_props : store -> VL -> VL *)
let lookup_all_nonstr_props st vlat =
  let tables = vlat.VL.tables in
  VL.LabelSet.fold (fun lab acc -> 
    let plat = find_label st lab in
    VL.join acc (snd (PL.find_nonstr_defaults plat))) tables VL.bot 

(*  lookup_all_props : store -> VL -> VL *)
let lookup_all_props st vlat =
  VL.join (lookup_all_str_props st vlat) (lookup_all_nonstr_props st vlat)

(*  lookup_nonstr_default_prop : store -> VL -> VL -> VL *)
let lookup_nonstr_default_prop st vlat vlat' =
  if is_bot st (*|| VL.is_bot vlat*) || VL.is_bot vlat'
  then VL.bot
  else
    let tables = vlat.VL.tables in
    VL.LabelSet.fold (fun lab acc -> 
      let plat        = find_label st lab in
      let def_key,def = PL.find_nonstr_defaults plat in  (* Note: for non-str entries *)
      let acc'        = VL.join VL.nil acc in
      if VL.is_bot (VL.meet vlat' def_key)
      then acc'
      else VL.join acc' def) tables VL.bot 

(*  lookup_dyn_prop : store -> VL -> VL -> VL *)
let lookup_dyn_prop st vlat vlat' = 
  if is_bot st || VL.is_bot vlat  (* No indexed expr values *)
               || VL.is_bot vlat' (* No indexing expr values *)
  then VL.bot
  else
    let res = if VL.may_be_non_strings vlat'
              then lookup_nonstr_default_prop st vlat vlat' (* all relevant default entries *)
	      else VL.bot in
    let strres = match vlat'.VL.strings with
	          | Str.Top     -> lookup_all_str_props st vlat
		  | Str.Const s -> lookup_str_prop st vlat s
		  | Str.Bot     -> VL.bot in
    VL.join res strres

(*  raw_get : store -> VL -> VL -> VL *)
let raw_get = lookup_dyn_prop

(*  write_str_prop : store -> VL -> str -> VL -> store *)
let write_str_prop st vlat prop vlat' =
  if is_bot st || VL.is_bot vlat || VL.is_bot vlat' then bot else
  let tables = vlat.VL.tables in
  VL.LabelSet.fold
    (fun lab stacc ->
      try
	let plat     = find_label_exn st lab in
	let old_vlat = PL.find prop plat in (* can be improved if not (str <= def_key) *)
	let new_plat = PL.add prop (VL.join vlat' (* join in default + weak update *)
				      (VL.exclude_nil old_vlat)) plat in
	                              (* if prev. entry was nil (not present) filter it and add strongly *)
	join (add_label st lab new_plat) stacc (* improvements: exclude nil from vlat', model removal *)
      with Not_found -> ((*Printf.printf "Unknown allocation site label %i\n" lab;*)
  		        stacc)) tables bot

(*  write_all_str_props : store -> VL -> VL -> VL -> store *)
let write_all_str_props st vlat0 (*vlat1*) vlat =
  if is_bot st || VL.is_bot vlat0 || (*VL.is_bot vlat1 ||*) VL.is_bot vlat
  then bot
  else
    let tables = vlat0.VL.tables in
    VL.LabelSet.fold (fun lab stacc -> 
      try
	let plat     = find_label_exn st lab in
	let new_plat = PL.add_all_str vlat plat in  (* weak updates all str entries -- incl.default_str *)
	add_label stacc lab new_plat
      with Not_found -> ((*Printf.printf "Unknown allocation site label %i\n" lab;*)
			 stacc)) tables st

(*  write_default_nonstr_prop : store -> VL -> VL -> VL -> store *)
let write_default_nonstr_prop st vlat0 vlat1 vlat =
  if is_bot st || VL.is_bot vlat0 || VL.is_bot vlat1 || VL.is_bot vlat
  then bot
  else
    let tables = vlat0.VL.tables in
    VL.LabelSet.fold (fun lab stacc -> 
      let plat = find_label st lab in
      let old_def_key, old_def = PL.find_nonstr_defaults plat in
      let new_plat = PL.add_nonstr_default (VL.join vlat1 old_def_key) (VL.join vlat old_def) plat in (* weak update in default *)
      add_label stacc lab new_plat) tables st

(*  write_dyn_prop : store -> VL -> VL -> VL -> store *)
let write_dyn_prop st vlat0 vlat1 vlat =
  if is_bot st || VL.is_bot vlat0  (* No indexed expr values *)
               || VL.is_bot vlat1  (* No indexing expr values *)
               || VL.is_bot vlat   (* No value to write *)
  then bot
  else
    let res = if VL.may_be_non_strings vlat1
              then write_default_nonstr_prop st vlat0 (VL.exclude_strings vlat1) vlat (* write to all relevant default entries *)
	      else bot in                                 (* don't *)
    let strres = match vlat1.VL.strings with
	          | Str.Top     -> write_all_str_props st vlat0 vlat
		  | Str.Const s -> write_str_prop st vlat0 s vlat
		  | Str.Bot     -> bot in
    if is_bot res
    then strres
    else join res strres

(*  raw_set : store -> VL -> VL -> VL -> store *)
let raw_set = write_dyn_prop

(*  get_metatable : store -> VL -> VL *)
let get_metatable st vlat =   (* fixme: strings have a metatable with a __index metamethod? *)
  if is_bot st || VL.is_bot vlat
  then VL.bot
  else        (* semantics: http://www.lua.org/manual/5.1/manual.html#pdf-getmetatable  *)
    let tables = vlat.VL.tables in
    VL.LabelSet.fold (fun lab acc -> 
      let plat         = find_label st lab in      (* for each table:  *)
      let metatabs     = PL.get_metatable plat in  (*    lookup metatable *)
      let metametatabs = 
	VL.LabelSet.fold (fun metalab acc ->
	  let metatab     = find_label st metalab in         (*    and __metatable in metatable *)
	  let metametatab = PL.find "__metatable" metatab in
	  if VL.may_be_nil metametatab (* second lookup may fail: include metatab *)
	  then VL.join (VL.table metalab) (VL.join (VL.exclude_nil metametatab) acc)
	  else VL.join (VL.exclude_nil metametatab) acc) metatabs.VL.tables VL.bot in
      VL.join acc
	(VL.join (VL.exclude_tables metatabs) metametatabs)) tables VL.bot

(*  set_metatable : store -> VL -> VL -> store *)
let set_metatable st vlat metavlat =
  if is_bot st
  then bot
  else (* semantics: http://www.lua.org/manual/5.1/manual.html#pdf-setmetatable  *)
  let tables = vlat.VL.tables in
  VL.LabelSet.fold (fun lab stacc ->
    try
      let plat         = find_label_exn st lab in
      let old_metavlat = PL.get_metatable plat in
      join stacc
       (join
	 (* no metatable present -> set it *)
	 (if VL.may_be_nil old_metavlat
	  then (* normal operation *)
	     let plat' = PL.set_metatable (VL.exclude_nil metavlat) plat in
	     add_label st lab plat'
	  else bot)

	 (* metatable labels present, potentially with __metatable fields *)
	 (VL.LabelSet.fold (fun metalab acc ->
	   let metatab     = find_label st metalab in
	   let metametatab = PL.find "__metatable" metatab in
	   if VL.may_be_nil metametatab
	   then (* may fail to have __metatable field, i.e., may succeed -> update *)
	     if VL.may_be_nil metavlat && VL.is_nil metavlat (* "rhs" definitely nil *)
	     then
	       let plat' = PL.set_metatable_absent plat in (* weak update: delete+keep = maybe absent *)
	       join acc (add_label st lab plat')
	     else (* weak update *)
	       if VL.may_be_nil metavlat (* maybe delete, maybe write = maybe absent *)
	       then
		 let plat' = PL.set_metatable (VL.join old_metavlat (VL.exclude_nil metavlat)) plat in
		 let plat' = PL.set_metatable_absent plat' in
		 join acc (add_label st lab plat')
	       else (* just write *)
		 let plat' = PL.set_metatable (VL.join old_metavlat (VL.exclude_nil metavlat)) plat in
		 join acc (add_label st lab plat')
	   else
	     acc (* error, so return bot, i.e., no effect on acc *) ) old_metavlat.VL.tables bot))
    with Not_found -> ((*Printf.printf "Unknown allocation site label %i\n" lab;*)
		       stacc)) tables bot

(* The following function helps to implement the metatable(obj)[event] notation *)
(*  modelling an internal metatable lookup *)
(* This operation differs from the semantics of the API getmetatable (it is unprotected)  *)
(*  despite the explanation in http://www.lua.org/manual/5.1/manual.html#2.8 *)
(*  myget_metatable : store -> VL -> VL *)
let myget_metatable st vlat =   (* fixme: strings have a metatable with a __index metamethod? *)
  if is_bot st || VL.is_bot vlat
  then VL.bot
  else
    let tables = vlat.VL.tables in
    VL.LabelSet.fold (fun lab acc -> 
      let plat     = find_label st lab in       (* for each table:  *)
      let metatabs = PL.get_metatable plat in   (*    lookup metatable *)
      VL.join acc metatabs) tables VL.bot

(*  lookup_event : ST -> VL -> string -> VL *)
let lookup_event st vlat strevent =   (* corresponds to metatable(obj)[event] notation in manual *)
  if is_bot st || VL.is_bot vlat
  then VL.bot
  else
    let base = myget_metatable st vlat in  (* http://www.lua.org/manual/5.1/manual.html#2.8 *)
    let res  = raw_get st (VL.exclude_nil base) (VL.string strevent) in
    if VL.may_be_nil base
    then VL.join VL.nil res
    else res

(*  getbinhandler : ST -> VL -> VL -> string -> VL *)
let getbinhandler st vlat0 vlat1 strevent =
  if is_bot st || VL.is_bot vlat0 || VL.is_bot vlat1 (* FIXED *)
  then VL.bot
  else VL.or_join (lookup_event st vlat0 strevent)
                  (lookup_event st vlat1 strevent)


(** {3 Pretty printing} *)

(*  pprint : store -> unit  *)
let pprint st =
  begin
    Format.printf "{ @[<v 0>";
    ignore (StoreMap.fold (fun lab plat first -> 
      if first
      then (Format.printf "%-8i -> " lab; PL.pprint plat; false)
      else
	begin
	  Format.print_space ();
	  Format.printf "%-8i -> " lab;
	  PL.pprint plat;
	  false
	end) st true);
    Format.printf "@]}";
  end

(*  to_string : elem -> string  *)
let to_string st =
  let buf = Buffer.create 128 in
  let out,flush = Format.get_formatter_output_functions () in (* save previous outputters *)
  begin
    Format.set_formatter_output_functions (Buffer.add_substring buf) (fun () -> ());
    pprint st;
    Format.print_flush ();
    Format.set_formatter_output_functions out flush;          (* restore previous outputters *)
    Buffer.contents buf
  end
