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
    in acc && (*Objlattice.leq*) PL.leq obj obj') st0 true

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
  if st = bot
  then acc
  else
    try
      let pl = find_label st lab in
      PL.ScopeChainSet.fold f pl.PL.scopechain acc
    with Not_found -> (*failwith ("No scope chain installed for label " ^ (string_of_int lab))*)
      (Printf.printf "No scope chain installed for label %i\n" lab;
       acc)

(*  lookup_prop : store -> VL -> str -> VL *)
let lookup_prop st vlat prop =
  if st = bot
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

(*  lookup_all_props : store -> VL -> VL *)
let lookup_all_props st vlat =
  let tables = vlat.VL.tables in
  VL.LabelSet.fold (fun lab acc -> 
    let plat = find_label st lab in
    VL.join acc (PL.find_all plat) ) tables VL.bot 

(*  lookup_default_prop : store -> VL -> VL -> VL *)
let lookup_default_prop st vlat vlat' =
  if st = bot (*|| VL.is_bot vlat*) || VL.is_bot vlat'
  then VL.bot
  else
    let tables = vlat.VL.tables in
    VL.LabelSet.fold (fun lab acc -> 
      let plat        = find_label st lab in
      let def_key,def = PL.find_default plat in
      let acc'        = VL.join VL.nil acc in
      if VL.meet vlat' def_key <> VL.bot
      then VL.join acc' def
      else acc') tables VL.bot 

(*  lookup_dyn_prop : store -> VL -> VL -> VL *)
let lookup_dyn_prop st vlat vlat' = 
  if VL.is_bot vlat  (* No indexed expr values *)
  || VL.is_bot vlat' (* No indexing expr values *)
  then VL.bot
  else
    let res = if VL.may_be_non_strings vlat'
              then lookup_default_prop st vlat vlat' (* all relevant default entries *)
	      else VL.bot in
    let strres = match vlat'.VL.strings with
	          | Str.Top     -> lookup_all_props st vlat
		  | Str.Const s -> lookup_prop st vlat s
		  | Str.Bot     -> VL.bot in
    VL.join res strres

(*  raw_get : store -> VL -> VL -> VL *)
let raw_get = lookup_dyn_prop

(*  write_prop : store -> VL -> str -> VL -> store *)
let write_prop st vlat prop vlat' =
  let tables = vlat.VL.tables in
  VL.LabelSet.fold (fun lab stacc ->
    try
      let plat                = find_label_exn st lab in
      let old_vlat            = PL.find prop plat in (* can be improved if not (str <= def_key) *)
      let new_plat = PL.add prop (VL.join vlat' (* join in default + weak update *)
				    (VL.exclude_nil old_vlat)) plat in
      add_label stacc lab new_plat
    with Not_found -> ((*Printf.printf "Unknown allocation site label %i\n" lab;*)
		       stacc)) tables st

(*  write_all_props : store -> VL -> VL -> VL -> store *)
let write_all_props st vlat0 vlat1 vlat =
  if VL.is_bot vlat0
  then bot
  else
    let tables = vlat0.VL.tables in
    VL.LabelSet.fold (fun lab stacc -> 
      try
	let plat     = find_label_exn st lab in
	let new_plat = PL.add_all vlat1 vlat plat in  (* weak updates all entries -- incl.default *)
	add_label stacc lab new_plat
      with Not_found -> ((*Printf.printf "Unknown allocation site label %i\n" lab;*)
			 stacc)) tables st

(*  write_default_prop : store -> VL -> VL -> VL -> store *)
let write_default_prop st vlat0 vlat1 vlat =
  let tables = vlat0.VL.tables in
  VL.LabelSet.fold (fun lab stacc -> 
    let plat = find_label st lab in
    let old_def_key, old_def = PL.find_default plat in
    let new_plat = PL.add_default (VL.join vlat1 old_def_key) (VL.join vlat old_def) plat in (* weak update in default *)
    add_label stacc lab new_plat) tables st

(*  write_dyn_prop : store -> VL -> VL -> VL -> store *)
let write_dyn_prop st vlat0 vlat1 vlat =
  if VL.is_bot vlat0  (* No indexed expr values *)
  || VL.is_bot vlat1  (* No indexing expr values *)
  || VL.is_bot vlat   (* No value to write *)
  then st
  else
    let res = if VL.may_be_non_strings vlat1
              then write_default_prop st vlat0 vlat1 vlat (* write to all relevant default entries *)
	      else bot in                                 (* don't *)
    let strres = match vlat1.VL.strings with
	          | Str.Top     -> write_all_props st vlat0 vlat1 vlat
		  | Str.Const s -> write_prop st vlat0 s vlat
		  | Str.Bot     -> bot in
    if res = bot
    then strres
    else join res strres

(*  raw_set : store -> VL -> VL -> VL -> store *)
let raw_set = write_dyn_prop

(*  get_metatable : store -> VL -> VL *)
let get_metatable st vlat =   (* fixme: strings have a metatable with a __index metamethod? *)
  if st = bot
  then VL.bot
  else        (* semantics: http://www.lua.org/manual/5.1/manual.html#pdf-getmetatable  *)
    let tables = vlat.VL.tables in
    VL.LabelSet.fold (fun lab acc -> 
      let plat         = find_label st lab in                (* for each table:  *)
      let metatabs     = PL.get_metatable plat in            (*    lookup metatable *)
      let metametatabs = 
	VL.LabelSet.fold (fun metalab acc ->
	  let metatab     = find_label st metalab in         (*    and __metatable in metatable *)
	  let metametatab = PL.find "__metatable" metatab in
	  if VL.may_be_nil metametatab (* second lookup may fail: include metatab *)
	  then VL.join (VL.table metalab) (VL.join (VL.only_tables metametatab) acc)
	  else VL.join (VL.only_tables metametatab) acc) metatabs.VL.tables VL.bot in
      VL.join acc
	(VL.join (VL.exclude_tables metatabs) metametatabs)) tables VL.bot

(*  set_metatable : store -> VL -> VL -> store *)
let set_metatable st vlat metavlat =
  if st = bot
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


(*  lookup_event : ST -> VL -> string -> VL *)
let lookup_event st vlat strevent =   (* corresponds to metatable(obj)[event] notation in manual *)
  let base = get_metatable st vlat in  (* http://www.lua.org/manual/5.1/manual.html#2.8 *)
  let res  = raw_get st (VL.exclude_nil base) (VL.string strevent) in
  if VL.may_be_nil base
  then VL.join VL.nil res
  else res

(*  getbinhandler : ST -> VL -> VL -> string -> VL *)
let getbinhandler st vlat0 vlat1 strevent =
  VL.or_join (lookup_event st vlat0 strevent)
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
