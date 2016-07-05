(** Abstract state datatype and operations *)

module Abs = Absencelattice
module VL = Valuelattice
module EL = Envlattice
module PL = Proplattice
module ST = Storelattice

type elem = { store   : ST.elem;
	      env     : EL.elem; }

(*  leq : elem -> elem -> bool  *)
let leq st0 st1 =
  ST.leq st0.store st1.store && EL.leq st0.env st1.env

(*  eq : elem -> elem -> bool  *)
let eq st0 st1 =
  ST.eq st0.store st1.store && EL.eq st0.env st1.env

(*  join : elem -> elem -> bool  *)
let join st0 st1 =
  { store = ST.join st0.store st1.store;
    env   = EL.join st0.env st1.env; }

(*  meet : elem -> elem -> bool  *)
let meet st0 st1 =
  { store = ST.meet st0.store st1.store;
    env   = EL.meet st0.env st1.env; }

(*  bot : elem  *)
let bot = { store = ST.bot;
	    env   = EL.bot; }


(** {3 Lattice operations } *)

(*  is_bot : SL -> bool  *)
let is_bot slat = leq slat bot

(*  build_record : (string * VL) list -> PL *)
let build_record bs =
  let props, vls = List.split bs in
  PL.add_all_params props vls PL.mt

(*  init : elem  *)
let init =
  let localenvlab = Label.make_res_label() in
  let globallabel = Label.make_res_label() in
  let arglabel    = Label.make_res_label() in
  let iolabel     = Label.make_res_label() in
  let mathlabel   = Label.make_res_label() in
  let oslabel     = Label.make_res_label() in
  let stringlabel = Label.make_res_label() in
  let tablelabel  = Label.make_res_label() in
  let store       = List.fold_left
                      (fun store (lab,table) -> ST.add_label store lab table) ST.bot
		        [ (localenvlab, PL.mt);
			  (globallabel, build_record
			                  [ ("error",    VL.builtin VL.Error);
					    ("next",     VL.builtin VL.Next);
					    ("pairs",    VL.builtin VL.Pairs);
					    ("ipairs",   VL.builtin VL.IPairs);
					    ("print",    VL.builtin VL.Print);
					    ("tonumber", VL.builtin VL.Tonumber);
					    ("tostring", VL.builtin VL.Tostring);
			                 (* ("abs",      VL.builtin VL.Abs);   *) (* from Lua 2.5 *)
			                 (* ("write",    VL.builtin VL.Write); *) (* from Lua 2.5 *)
					 (* ("floor",    VL.builtin VL.Floor); *) (* from Lua 2.5 *)
					 (* ("mod",      VL.builtin VL.Mod);   *) (* from Lua 2.5 *)
			                 (* ("strlen",   VL.builtin VL.Strlen);*) (* from Lua 2.5 *)
					 (* ("sqrt",     VL.builtin VL.Sqrt);  *) (* from Lua 2.5 *)
					 (* ("format",   VL.builtin VL.Format);*) (* from Lua 2.5 *)
			                    ("_VERSION", VL.anystring);
					    ("_G",       VL.table globallabel);
					    ("arg",      VL.table arglabel);
					    ("io",       VL.table iolabel);
			                    ("math",     VL.table mathlabel);
					    ("os",       VL.table oslabel);
					    ("string",   VL.table stringlabel);
		                            ("table",    VL.table tablelabel);
		                            ("type",     VL.builtin VL.Type);
					    ("setmetatable", VL.builtin VL.Setmetatable);
					    ("getmetatable", VL.builtin VL.Getmetatable);
					    ("rawget",   VL.builtin VL.Rawget);
					    ("rawset",   VL.builtin VL.Rawset);
					  ]);
		          (arglabel,    PL.add_nonstr_default VL.number VL.anystring PL.mt);
			  (iolabel,     build_record
 			                  [ ("write",  VL.builtin VL.Write); ]);
		          (mathlabel,   build_record
 			                  [ ("abs",    VL.builtin VL.Abs);
					    ("ceil",   VL.builtin VL.Ceil);
					    ("floor",  VL.builtin VL.Floor);
					    ("huge",   VL.number);
					    ("random", VL.builtin VL.Random);
					    ("sqrt",   VL.builtin VL.Sqrt);
					  ]);
		          (oslabel,     build_record
			                  [ ("exit",   VL.builtin VL.Exit); ]);
		          (stringlabel, build_record
 			                  [ ("len",    VL.builtin VL.Strlen);
					    ("upper",  VL.builtin VL.Strupper);
					    ("lower",  VL.builtin VL.Strlower);
					    ("char",   VL.builtin VL.Strchar);
					    ("byte",   VL.builtin VL.Strbyte);
					    ("sub",    VL.builtin VL.Strsub);
					    ("format", VL.builtin VL.Format);
					  ]);
		          (tablelabel,  build_record
			                  [ ("concat",   VL.builtin VL.Tblconcat);
					  ]);
			] in
  let env         = EL.init localenvlab globallabel in
  { store = store;
    env   = env; }


(*  apply_builtin : builtin -> SL -> VL list -> SL * VL list *)
let rec apply_builtin bi slat vlats = 
  if is_bot slat then (bot,[]) else
  match bi with
  | VL.Error    -> (bot,[]) (* unreachable *)
  | VL.Exit     -> (bot,[]) (* unreachable *)
  | VL.Next     ->
    (match vlats with
      | []      -> (bot,[]) (*error*)
      | vlat::_ ->
	if VL.may_be_table vlat	then
	  let keys = VL.join VL.nil (* potentially: end of table *)
		       (ST.lookup_all_keys slat.store vlat) in
	  let values = VL.join VL.nil (* potentially: end of table *)
 	                 (ST.lookup_all_props slat.store vlat) in
	  (* possible strengthening: if 2nd arg is nil and table is definitely non-empty (has definite entries), *)
	  (slat,[keys;values])    (* then result cannot be nil *)
	else (bot,[])) (*error*)
  | VL.INext     ->
    (match vlats with
      | []      -> (bot,[]) (*error*)
      | vlat::_ ->
	if VL.may_be_table vlat
	then
	  let keys = ST.lookup_all_keys slat.store vlat in
	  (if VL.may_be_number keys
	   then
	      let keys   = VL.join VL.nil VL.number in (* potentially: end of table *)
	      let values = VL.join VL.nil              (* potentially: end of table *)
		             (ST.lookup_all_nonstr_props slat.store vlat) in (* array entries are nonstr *)
	      (slat,[keys;values])
	   else
	      let keys,values = VL.nil,VL.nil in       (* end of table *)
	      (slat,[keys;values]))
	else (bot,[])) (*error*)
  | VL.Pairs    ->
    (match vlats with
      | []      -> (bot,[]) (*error*)
      | vlat::_ ->
	let vlat' = VL.only_tables vlat in
	if VL.is_bot vlat'
	then (bot,[]) (*error*)
	else (slat,[VL.builtin VL.Next; vlat'; VL.nil]))
  | VL.IPairs    ->
    (match vlats with
      | []      -> (bot,[]) (*error*)
      | vlat::_ ->
	let vlat' = VL.only_tables vlat in
	if VL.is_bot vlat'
	then (bot,[]) (*error*)
	else (slat,[VL.builtin VL.INext; vlat'; VL.number])) (* return iterator, table, 0 *)
  | VL.Print    -> (slat,[VL.nil]) (* no return value specified - Lua2.5 impl returns nil *)
  | VL.Write    ->
    (match vlats with
      | [] -> (slat, [VL.userdata])
      | vlat::vlats ->
	if VL.may_be_number vlat || VL.may_be_strings vlat (* may succeed *)
	then apply_builtin bi slat vlats
	else (bot, []))
  | VL.Tonumber ->
    (match vlats with
      | []      -> (bot,[]) (*error*)
      | vlat::_ -> 
	if VL.is_nil vlat then (bot,[]) (* unreachable *)
	else
	  let retval =
	         if VL.is_number vlat      then VL.number        (* number arg (at most) *)
	    else if VL.is_strings vlat     then VL.number_or_nil (* string arg (at most) *)
	    else if VL.may_be_number vlat  then VL.number_or_nil (* number or other arg   *)
	    else if VL.may_be_strings vlat then VL.number_or_nil (* string or other arg  *)
	    else VL.nil                                 (* no string or number arguments *)
	  in (slat,[retval]))
  | VL.Tostring ->
    (match vlats with
      | []      -> (bot,[]) (*error*)
      | vlat::_ -> 
	if VL.is_bot vlat then (bot,[]) (* unreachable *)
	else (slat,[VL.anystring]))
  | VL.Abs
  | VL.Ceil
  | VL.Floor    ->
    (match vlats with
      | vlat::_ ->
	if VL.is_bot vlat then (bot, []) (* unreachable *)
	else if VL.may_be_number (VL.coerce_tonum vlat) then (slat, [VL.number])
	else (bot, [])
      | _      -> (bot, []))
  | VL.Mod      ->
    (match vlats with
      | vl0::vl1::_ -> let res = VL.binop Ast.Mod vl0 vl1 in
		       if res = VL.bot then (bot, [VL.bot])
		       else (slat, [res])
      | _           -> (bot, []))
  | VL.Random   ->
    (match vlats with
      | []           -> (slat,[VL.number])
      | vl0::[]      -> if VL.may_be_number (VL.coerce_tonum vl0)
	                then (slat,[VL.number])
	                else (bot, [])
      | vl0::vl1::[] -> if VL.may_be_number (VL.coerce_tonum vl0)
	                   && VL.may_be_number (VL.coerce_tonum vl1)
	                then (slat,[VL.number])
	                else (bot, [])
      | _            -> (bot, []))
  | VL.Strlen   ->
    (match vlats with
      | []      -> (bot,[]) (*error*)
      | vlat::_ -> 
	  if VL.may_be_strings (VL.coerce_tostring vlat)
	  then (slat,[VL.number])
	  else (bot,[]))
  | VL.Strupper ->
    (match vlats with
      | []      -> (bot,[]) (*error*)
      | vlat::_ -> (slat,[VL.upper vlat]))
  | VL.Strlower ->
    (match vlats with
      | []      -> (bot,[]) (*error*)
      | vlat::_ -> (slat,[VL.lower vlat]))
  | VL.Strchar  ->
    (match vlats with
      | []          -> (slat, [VL.string ""])
      | vlat::vlats ->
	let vlat' = VL.char vlat in
	if VL.may_be_strings vlat' (* may succeed *)
	then 
	  let slat',vlats' = apply_builtin bi slat vlats in (* recurse *)
	  (match vlats' with
	    | []        -> (bot, []) (* propagate error *)
            | vlat''::_ -> (slat',[VL.binop Ast.Concat vlat' vlat'']))
        else (bot, []))
  | VL.Strbyte  ->
    (match vlats with
      | []                  -> (bot, [])
      | arg::[]             -> if VL.may_be_strings (VL.coerce_tostring arg)
	                       then (slat,[VL.number_or_nil])
	                       else (bot, [])
      | arg1::arg2::[]      -> if VL.may_be_strings (VL.coerce_tostring arg1)
	                          && VL.may_be_number (VL.coerce_tonum arg2)
	                       then (slat,[VL.number_or_nil])
	                       else (bot, [])
      | arg1::arg2::arg3::_ -> if VL.may_be_strings (VL.coerce_tostring arg1)
	                          && VL.may_be_number (VL.coerce_tonum arg2)
	                          && VL.may_be_number (VL.coerce_tonum arg3)
  	                       then (* almost conservative :-) for static strings, length gives a bound *)
   	                         (slat,[VL.number_or_nil;VL.number_or_nil;VL.number_or_nil;VL.number_or_nil;VL.number_or_nil;
					VL.number_or_nil;VL.number_or_nil;VL.number_or_nil;VL.number_or_nil;VL.number_or_nil ])
	                       else (bot, []))
  | VL.Strsub   ->
    (match vlats with
      | [] | [ _ ]          -> (bot, [])
      | arg1::arg2::[]      -> apply_builtin bi slat ([arg1;VL.number;arg2])
      | arg1::arg2::arg3::_ ->
	let arg1' = VL.coerce_tostring arg1 in
	let arg2' = VL.coerce_tonum arg2 in
	let arg3' = VL.coerce_tonum arg3 in
	if VL.may_be_strings arg1' && VL.may_be_number arg2' && VL.may_be_number arg3'
	then (slat,[VL.anystring])
	else (bot,[]))
  | VL.Sqrt     ->
    (match vlats with
      | []      -> (bot,[]) (*error*)
      | vlat::_ -> 
	  if VL.may_be_number (VL.coerce_tonum vlat)
	  then (slat,[VL.number])
	  else (bot,[]))
  | VL.Type     ->
    (match vlats with
      | []      -> (bot,[]) (*error*)
      | vlat::_ -> (slat,[VL.typep vlat]))
  | VL.Format ->
    (match vlats with
      | []      -> (bot,[]) (*error*)
      | vlat::_ -> 
	     if VL.is_nil vlat         then (bot,[]) (*unreachable*)
	else if VL.may_be_strings vlat then (slat,[VL.anystring])
	else (bot,[]))
  | VL.Tblconcat ->
    (match vlats with
      | []          -> (bot,[]) (*error*)
      | vlat1::vlats1 -> 
	if VL.may_be_table vlat1
	then (match vlats1 with
	  | []            -> (slat,[VL.anystring]) (* one arg *)
	  | vlat2::vlats2 ->
	    if VL.may_be_strings (VL.coerce_tostring vlat2)
	    then (match vlats2 with
	      | []            -> (slat,[VL.anystring]) (* two args *)
	      | vlat3::vlats3 ->
		if VL.may_be_number (VL.coerce_tonum vlat3)
		then (match vlats3 with
		  | []       -> (slat,[VL.anystring]) (* three args *)
		  | vlat4::_ ->
		    if VL.may_be_number (VL.coerce_tonum vlat4)
		    then (slat,[VL.anystring]) (* four args *)
		    else (bot,[]))
		else (bot,[]))
	    else (bot,[]))
	else (bot,[])) (*unreachable*)
  | VL.Getmetatable ->
    (match vlats with
      | []      -> (bot,[]) (*error*)
      | vlat::_ -> (slat,[ST.get_metatable slat.store vlat]))
  | VL.Setmetatable ->
    (match vlats with
      | []             -> (bot,[]) (*error*)
      | [_]            -> (bot,[]) (*error*)
      | vlat::vlat'::_ ->
	let store' = ST.set_metatable slat.store vlat vlat' in
	({slat with store = store'},[vlat]))
  | VL.Rawget ->
    (match vlats with
      | []             -> (bot,[]) (*error*)
      | [_]            -> (bot,[]) (*error*)
      | vlat::vlat'::_ -> (slat,[ST.raw_get slat.store vlat vlat']))
  | VL.Rawset ->
    (match vlats with
      | []                     -> (bot,[]) (*error*)
      | _::[]                  -> (bot,[]) (*error*)
      | _::_::[]               -> (bot,[]) (*error*)
      | vlat0::vlat1::vlat2::_ ->
	let store' = ST.raw_set slat.store vlat0 vlat1 vlat2 in
	({slat with store = store'},[VL.only_tables vlat0]))


(*  add_local : SL -> VL -> str -> SL *)
let add_local slat vlat x =
  if is_bot slat || VL.is_bot vlat
  then bot
  else
    let store' =
      EL.fold (fun (localslab,_) storeacc ->
                            let env  = ST.find_label slat.store localslab in
			    let env' = PL.add_local x vlat env in (* strong update *)
			    ST.join (ST.add_label slat.store localslab env') storeacc)
        slat.env ST.bot in
    { slat with store = store' }

(*  add_local_list : SL -> VL list -> str list -> SL *)
let rec add_local_list slat vlats xs =
  if is_bot slat
  then bot
  else match (xs,vlats) with
    | [],   []          -> slat
    | x::xs,vlat::vlats ->
      let slat' = add_local slat vlat x in
      add_local_list slat' vlats xs
    | x::xs,[]          ->
      let slat' = add_local slat VL.nil x in (* missing value: assign nil *)
      add_local_list slat' [] xs
    | [],   vlat::vlats ->
      add_local_list slat vlats []           (* missing local: drop excess value *)

(*  enter_scope : SL -> label -> SL  *)
let enter_scope slat label =
  if is_bot slat
  then bot
  else
    { store = ST.add_label slat.store label PL.mt; 
      env   = EL.enter_scope slat.env label }

(*  build_prop_chain : EL -> PL  *)
let build_prop_chain scopechain =
  if EL.is_bot scopechain
  then PL.bot
  else
    EL.fold (fun (i,is) scset -> PL.add_scopechain scset (i::is)) scopechain PL.mt

(*  read_name : SL -> string -> VL *)
let read_name slat name = 
  (*      scope_read : lab -> lab list -> VL  *)
  let rec scope_read envlab scopechain =
    if ST.is_bot slat.store
    then VL.bot
    else
      let env = ST.find_label slat.store envlab in
      if PL.is_bot env
      then VL.bot
      else
	try
	  let vl,abs = PL.find_exn name env in (* slight hack: direct lookup *)
	  if Abs.eq abs Abs.maybe_absent
	  then VL.join vl (continue_read scopechain)
	  else vl (* certain entry, don't continue *)
	with Not_found -> continue_read scopechain
  (*  continue_read : lab list -> VL  *)
  and continue_read scopechain = match scopechain with
      | [] -> VL.nil (* not found: return nil *)
      | outerenvlab::scopechain' -> scope_read outerenvlab scopechain'
  in
  EL.fold (fun (envlab,chain) a -> VL.join (scope_read envlab chain) a) slat.env VL.bot

(*  write_name : SL -> string -> VL -> SL *)
let write_name slat name vlat =
  if is_bot slat || VL.is_bot vlat
  then bot
  else
    (*      scope_write : lab -> lab list -> SL  *)
    let rec scope_write envlab scopechain =
      try
	let env = ST.find_label_exn slat.store envlab in
	if PL.is_bot env
	then bot
	else
	  if PL.mem name env (* slight hack: direct lookup *)
	  then
	    let _,abs  = PL.find_exn name env in (* slight hack: direct lookup *)
	    let env'   = PL.add_local name vlat env in (* strong update the local variable *)
	    let store' = ST.add_label slat.store envlab env' in
	    let slat'  = { slat with store = store' } in
	    (if Abs.eq abs Abs.maybe_absent
	     then join slat' (continue_write scopechain)
	     else slat')
	  else continue_write scopechain
      with Not_found -> bot
    (*  continue_write : list -> SL  *)
    and continue_write scopechain = match scopechain with
      | []          -> failwith "Outside any environment\n";
      | [globallab] ->
	(try
	  let globalenv  = ST.find_label_exn slat.store globallab in
	  let globalenv' = PL.add name vlat globalenv in  (* strong update: only *one* global env *)
	  let store'     = ST.add_label slat.store globallab globalenv' in
	  { slat with store = store' }
	 with Not_found -> bot)
      | outerenvlab::scopechain' -> scope_write outerenvlab scopechain'
    in
    EL.fold
      (fun (envlab,chain) slatacc -> 
	(*try *)join (scope_write envlab chain) slatacc
	(*with Not_found -> (*Printf.printf "Unknown environment label\n";*)
	                  slatacc*)) slat.env bot

(*  write_dyn_prop : SL -> VL -> VL -> VL -> SL *)
let write_dyn_prop slat vlat0 vlat1 vlat =
  let store' = ST.write_dyn_prop slat.store vlat0 vlat1 vlat in
  { slat with store = store' }

(*  getbinhandler : SL -> VL -> VL -> string -> VL *)
let getbinhandler slat vlat0 vlat1 strevent =
  ST.getbinhandler slat.store vlat0 vlat1 strevent


(** {3 Pretty printing} *)

(*  pprint : SL -> unit *)
let pprint slat =
    begin
      Format.printf "{ @[<v 0>store:   ";
      ST.pprint slat.store;
      Format.print_space ();
      Format.printf "env:     ";
      EL.pprint slat.env;
      Format.printf "@] }";
    end

(*  to_string : elem -> string  *)
let to_string s =
  let buf = Buffer.create 128 in
  let out,flush = Format.get_formatter_output_functions () in (* save previous outputters *)
  begin
    Format.set_formatter_output_functions (Buffer.add_substring buf) (fun () -> ());
    pprint s;
    Format.print_flush ();
    Format.set_formatter_output_functions out flush;       (* restore previous outputters *)
    Buffer.contents buf
  end
