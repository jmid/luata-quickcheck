open QCheck
open LCheck

module Label =
struct
  type elem = int
  let arb_elem = Arbitrary.int 1000000 (*max_int 4611686018427387903*)
  let to_string = string_of_int
end
module Labellist = MkArbListArg(Label)

(** Generic operations for building arbitrary sets *)

(*  build_set : 'a Set.t -> ('a -> 'a Set.t) -> ('a Set.t -> 'a Set.t -> 'a Set.t)
                         -> 'a list -> 'a Set.t Arbitrary.t *)
let rec build_set mt sglton union ls = match ls with
  | []  -> Arbitrary.return mt
  | [l] -> Arbitrary.return (sglton l)
  | _   -> Arbitrary.(int (1 + List.length ls) >>= fun i -> 
		      let ls,rs = split i ls in
		      lift2 union
			(build_set mt sglton union ls)
			(build_set mt sglton union rs) )

(*  build_map : ('b Map.t) -> ('a -> 'b -> 'b Map.t -> 'b Map.t) -> 'a * 'b list -> 'b Map.t  *)
let build_map mt add ls =
  let rec build ls = match ls with
    | [] ->        Arbitrary.return mt
    | (k,v)::ls -> Arbitrary.(build ls >>= fun tbl -> return (add k v tbl)) in
  build ls

(*  permute : 'a list -> 'a list Arbitrary.t *)
let rec permute es = match es with
  | []  -> Arbitrary.return []
  | [e] -> Arbitrary.return [e]
  | _   ->
    let len = List.length es in
    let front,back = split (len/2) es in
    Arbitrary.(permute front >>= fun front' ->
	       permute back  >>= fun back' ->
	       Arbitrary.bool >>= fun b ->
	       if b then return (front' @ back') else return (back' @ front'))

(*  le_gen : 'a list -> ('a list -> 'b) -> 'b  *)
let le_gen es build =
  let es_gen = permute es in
  Arbitrary.(es_gen >>= fun es ->
	     int (1 + List.length es) >>= fun i -> 
	     let smaller_es,_ = split i es in
	     build smaller_es)

(*  le_entries :  ('b -> 'b Arb.t) -> ('a * 'b) list -> (('a * 'b) list) Arb.t *)
let le_entries arb_elem_le es =
  let rec build es = match es with
    | [] -> Arbitrary.return []
    | (k,v)::es -> Arbitrary.(build es >>= fun es' ->
	  	 	      arb_elem_le v >>= fun v' ->
			      return ((k,v')::es')) in
  build es

(** Absence lattice extended with generators *)
module Abs = struct 
  let name = "absence lattice"
  include Absencelattice
  let arb_elem = Arbitrary.among [bot; top]
  let equiv_pair = Arbitrary.(lift (fun a -> (a,a)) arb_elem)
  let arb_elem_le e = if e = top then arb_elem else Arbitrary.return bot
end

(** Number lattice extended with generators *)
module Num = struct 
  let name = "number lattice"
  include Numberlattice
  let arb_elem = Arbitrary.among [bot; top]
  let equiv_pair = Arbitrary.(lift (fun a -> (a,a)) arb_elem)
  let arb_elem_le e = if e = top then arb_elem else Arbitrary.return bot
end

(** String lattice extended with generators *)
module Str = struct
  let name = "string lattice"
  include Stringlattice
  let arb_elem = Arbitrary.(choose [return bot; lift const string; return top])
  let equiv_pair = Arbitrary.(lift (fun sv -> match sv with
                                               | Const s -> (sv,Const (String.copy s))
					       | _       -> (sv,sv))  arb_elem)
  let arb_elem_le e = match e with
    | Bot     -> Arbitrary.return bot
    | Const s -> Arbitrary.among [bot; Const s]
    | Top     -> arb_elem
end

(** Value lattice extended with generators *)
module VL  = struct
  let name = "value lattice"
  let arb_num         = Num.arb_elem    (* shorthands for the extended Num operations: *)
  let num_equiv_pair  = Num.equiv_pair  (* Moving them below 'include' will shadow Num *)
  let num_arb_elem_le = Num.arb_elem_le  

  let arb_str         = Str.arb_elem    (* shorthands for the extended Str operations: *)
  let str_equiv_pair  = Str.equiv_pair  (* Moving them below 'include' will shadow Str *)
  let str_arb_elem_le = Str.arb_elem_le  

  include Valuelattice
  (* various helper functions *)
  let arb_tag t   = Arbitrary.(among [TagSet.singleton t; TagSet.empty])
  let arb_tags    = Arbitrary.(lift3
				 (fun nl bl ud -> TagSet.union (TagSet.union nl bl) ud)
				 (arb_tag Nil) (arb_tag Bool) (arb_tag Userdata))
  let arb_builtin = Arbitrary.(lift (fun b -> Builtin b)
				 (among [Error;Next;Pairs;Print;Write;Tonumber;Tostring;Abs;Ceil;Floor;
					 Mod;Strlen;Strupper;Strlower;Strchar;Strbyte;Strsub;Sqrt;Type;Format;
					 Getmetatable;Setmetatable]))
  let arb_funtag  = Arbitrary.(lift (fun i -> Funtag i) Label.arb_elem)
  let arb_proc    = Arbitrary.(choose [arb_funtag; arb_builtin])
  let arb_procs   = Arbitrary.(fix ~base:(return ProcSet.empty) (lift2 ProcSet.add arb_proc))
  let arb_labels  = Arbitrary.(fix ~base:(return LabelSet.empty) (lift2 LabelSet.add Label.arb_elem))

  let arb_label_list = Labellist.arb_elem
  let arb_tag_list   = Arbitrary.(list ~len:(int 4) (among [Nil; Bool; Userdata]))
  let arb_proc_list  = Arbitrary.(list ~len:(int 20) arb_proc)

  let build_labelset = build_set LabelSet.empty LabelSet.singleton LabelSet.union
  let build_tagset   = build_set TagSet.empty   TagSet.singleton   TagSet.union  
  let build_procset  = build_set ProcSet.empty  ProcSet.singleton  ProcSet.union 

  (*  lift5 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t *)
  let lift5 f gen_a gen_b gen_c gen_d gen_e =
    Arbitrary.(gen_a >>= fun a ->
	        gen_b >>= fun b ->
	         gen_c >>= fun c ->
		  gen_d >>= fun d ->
		   gen_e >>= fun e -> return (f a b c d e))

  (*  arb_elem : elem Arbitrary.t  *)
  let arb_elem =  (* generate arbitrary tagset, number elem, string elem, fun set, table set *)
    lift5 (fun tags num s procs labels ->
	     { tags    = tags;
	       number  = num;
	       strings = s;
	       funs    = procs;
	       tables  = labels }) arb_tags arb_num arb_str arb_procs arb_labels

  (*  equiv_pair : (elem * elem) Arbitrary.t  *)
  let equiv_pair =
    let equiv_labelsets = Arbitrary.(arb_label_list >>= fun ls -> pair (build_labelset ls) (build_labelset ls)) in
    let equiv_tagsets   = Arbitrary.(arb_tag_list   >>= fun ts -> pair (build_tagset ts) (build_tagset ts)) in
    let equiv_procsets  = Arbitrary.(arb_proc_list  >>= fun ps -> pair (build_procset ps) (build_procset ps)) in
    lift5 (fun (t,t') (n,n') (s,s') (p,p') (l,l') ->
             ({ tags = t;  number = n;  strings = s;  funs = p;  tables = l },
	      { tags = t'; number = n'; strings = s'; funs = p'; tables = l' }))
      equiv_tagsets num_equiv_pair str_equiv_pair equiv_procsets equiv_labelsets

  (*  arb_elem_le : elem -> elem Arbitrary.t  *)
  let arb_elem_le e =
    let le_num_gen   = num_arb_elem_le e.number in
    let le_str_gen   = str_arb_elem_le e.strings in
    let le_tag_gen   = le_gen (TagSet.elements e.tags)     build_tagset in 
    let le_proc_gen  = le_gen (ProcSet.elements e.funs)    build_procset in
    let le_label_gen = le_gen (LabelSet.elements e.tables) build_labelset in
    lift5 (fun t n s p l -> { tags = t; number = n; strings = s; funs = p; tables = l })
      le_tag_gen le_num_gen le_str_gen le_proc_gen le_label_gen
end

(** Environment lattice extended with generators *)
module EL  = struct
  let name = "environment lattice"
  include Envlattice
  (* Helpers *)
  let arb_env_pair = Arbitrary.(pair Label.arb_elem (list ~len:(int_range ~start:1 ~stop:10) Label.arb_elem))
  let arb_env_list = Arbitrary.(list ~len:(int_range ~start:1 ~stop:10) arb_env_pair)
  let build_envset es = build_set PairSet.empty PairSet.singleton PairSet.union es
  (* Actual operations *)
  let arb_elem    = Arbitrary.(arb_env_list >>= build_envset)
  let equiv_pair  = Arbitrary.(arb_env_list >>= fun es -> pair (build_envset es) (build_envset es))
  let arb_elem_le e = le_gen (PairSet.elements e) build_envset
end

(** Property lattice extended with generators *)
module PL = struct
  let name = "property lattice"
  let abs_arb_elem    = Abs.arb_elem
  let abs_arb_elem_le = Abs.arb_elem_le
  let vl_arb_elem     = VL.arb_elem
  let vl_equiv_pair   = VL.equiv_pair
  let vl_arb_elem_le  = VL.arb_elem_le
  include Proplattice
  let build_sc_set = build_set ScopeChainSet.empty ScopeChainSet.singleton ScopeChainSet.union
  let build_tblmap = build_map TableMap.empty TableMap.add
  let arb_sc_list  = Arbitrary.(list ~len:(int_range ~start:1 ~stop:10) (list ~len:(int_range ~start:1 ~stop:10) Label.arb_elem))
  let arb_key = Arbitrary.(small_int >>= fun i ->
                                           if i > 90
                                           then return TableKey.Metatable
					   else lift (fun s -> TableKey.String s) string)
  let arb_entries  = Arbitrary.(list ~len:(int 20) (pair arb_key
							 (pair vl_arb_elem abs_arb_elem)))
  let arb_elem =
    let sc_gen    = Arbitrary.(arb_sc_list >>= build_sc_set) in
    let table_gen = Arbitrary.(arb_entries >>= build_tblmap) in
    Arbitrary.lift4
      (fun t dk df sc -> { table = t; default_key = dk; default = df; scopechain = sc })
      table_gen vl_arb_elem vl_arb_elem sc_gen

  let equiv_pair =
    let equiv_tables      = Arbitrary.(arb_entries >>= build_tblmap >>= fun map ->
 				       permute (TableMap.bindings map) >>= fun perm_entries ->
				       pair (return map) (build_tblmap perm_entries)) in
    let equiv_scopechains = Arbitrary.(arb_sc_list >>= fun sc -> pair (build_sc_set sc)
				                                      (build_sc_set sc)) in
    Arbitrary.lift4
      (fun (t,t') (dk,dk') (df,df') (sc,sc') ->
	({ table = t;  default_key = dk;  default = df;  scopechain = sc },
	 { table = t'; default_key = dk'; default = df'; scopechain = sc' }))
      equiv_tables vl_equiv_pair vl_equiv_pair equiv_scopechains

  let arb_elem_le { table; default_key; default; scopechain } =
    let entry_le (v,a) = Arbitrary.pair (vl_arb_elem_le v) (abs_arb_elem_le a) in
    let table'      = le_gen (TableMap.bindings table)
                             Arbitrary.(fun es -> le_entries entry_le es >>= build_tblmap) in
    let def_key'    = vl_arb_elem_le default_key in
    let default'    = vl_arb_elem_le default in
    let scopechain' = le_gen (ScopeChainSet.elements scopechain) build_sc_set in
    Arbitrary.lift4
      (fun t dk df sc -> {table = t; default_key = dk; default = df; scopechain = sc})
      table' def_key' default' scopechain'
end

(** Store lattice extended with generators *)
module ST = struct
  let name           = "store lattice"
  let pl_arb_elem    = PL.arb_elem
  let pl_arb_elem_le = PL.arb_elem_le
  include Storelattice
  let build_storemap = build_map StoreMap.empty StoreMap.add
  let arb_entries    = Arbitrary.(list ~len:(int 20) (pair Label.arb_elem pl_arb_elem))
  let arb_elem       = Arbitrary.(arb_entries >>= build_storemap)
  let equiv_pair     = Arbitrary.(arb_entries >>= build_storemap >>= fun map ->
 				   permute (StoreMap.bindings map) >>= fun perm_entries ->
				    pair (return map) (build_storemap perm_entries))
  let arb_elem_le st = le_gen (StoreMap.bindings st) 
                              Arbitrary.(fun es -> le_entries pl_arb_elem_le es >>= build_storemap)
end

(** State lattice extended with generators *)
module SL = struct
  let name           = "state lattice"
  let st_arb_elem    = ST.arb_elem
  let st_equiv_pair  = ST.equiv_pair
  let st_arb_elem_le = ST.arb_elem_le
  let el_arb_elem    = EL.arb_elem
  let el_equiv_pair  = EL.equiv_pair
  let el_arb_elem_le = EL.arb_elem_le
  include Statelattice
  let arb_elem = Arbitrary.lift2 (fun st e -> { store = st; env = e }) st_arb_elem el_arb_elem
  let equiv_pair = Arbitrary.lift2
                    (fun (st,st') (e,e') -> ({ store = st;  env = e },
					     { store = st'; env = e' })) st_equiv_pair el_equiv_pair
 let arb_elem_le { store; env } =
   Arbitrary.lift2 (fun st e -> { store = st; env = e }) (st_arb_elem_le store) (el_arb_elem_le env)
end

(** Analysis lattice extended with generators *)
module AL = struct
  let name           = "analysis lattice"
  let sl_arb_elem    = SL.arb_elem
  let sl_arb_elem_le = SL.arb_elem_le
  include Analysislattice
  let build_labelmap = build_map LabelMap.empty LabelMap.add
  let arb_entries    = Arbitrary.(list ~len:(int 20) (pair Label.arb_elem sl_arb_elem))
  let arb_elem = Arbitrary.(arb_entries >>= build_labelmap)
  let equiv_pair     = Arbitrary.(arb_entries >>= build_labelmap >>= fun map ->
 				   permute (LabelMap.bindings map) >>= fun perm_entries ->
				    pair (return map) (build_labelmap perm_entries))
  let arb_elem_le st = le_gen (LabelMap.bindings st) 
                              Arbitrary.(fun es -> le_entries sl_arb_elem_le es >>= build_labelmap)
end

module GenAbsTests  = GenericTests(Abs)
module GenNumTests  = GenericTests(Num)
module GenStrTests  = GenericTests(Str)
module GenValTests  = GenericTests(VL)
module GenEnvTests  = GenericTests(EL)
module GenPropTests = GenericTests(PL)
module GenStoTests  = GenericTests(ST)
module GenStaTests  = GenericTests(SL)
module GenAnaTests  = GenericTests(AL)

module GenAbsTopTests = GenericTopTests(Abs)
module GenNumTopTests = GenericTopTests(Num)
module GenStrTopTests = GenericTopTests(Str)


(** Tests for specific operations *)

module Str_arg =
struct
  type elem = string
  let to_string s = s
  let arb_elem = Arbitrary.string
end
module Strlist = MkArbListArg(Str_arg)

module VLVLpair = MkPairLattice(VL)(VL)
module GenVLVLpairTests = GenericTests(VLVLpair)

module VLlist = MkListLattice(VL)
module GenVLlistTests = GenericTests(VLlist)

module SLVLlistpair = MkPairLattice(SL)(VLlist)
module GenSLVLlistpairTests = GenericTests(SLVLlistpair)


(** String lattice *)

(* Note: the following predicate functions depend on the Boolean lattice: *)
(*       {true,false} under reverse implication ordering,                 *)
(*        bot = true <== true <== false <== false = top                   *)
let empty_tests =
  let str_empty = ("Str.empty",Str.empty) in
  [ testsig (module Str) -$-> (module Bool) =: str_empty;
    testsig (module Str) -<-> (module Bool) =: str_empty;
    testsig (module Str) -~-> (module Bool) =: str_empty; ]

(* Note: the following predicate functions depend on the Boolean lattice: *)
(*       {true,false} under implication ordering,                         *)
(*        bot = false <== false <== true <== true = top                   *)
let nonempty_tests =
  let str_nonempty = ("Str.nonempty",Str.nonempty) in
  [ testsig (module Str) -$-> (module DBool) =: str_nonempty;
    testsig (module Str) -<-> (module DBool) =: str_nonempty;
    testsig (module Str) -~-> (module DBool) =: str_nonempty; ]

let upper_tests =
  let str_upper = ("Str.upper",Str.upper) in
  [ testsig (module Str) -$-> (module Str) =: str_upper;
    testsig (module Str) -<-> (module Str) =: str_upper;
    testsig (module Str) -~-> (module Str) =: str_upper; ]

let lower_tests =
  let str_lower = ("Str.lower",Str.lower) in
  [ testsig (module Str) -$-> (module Str) =: str_lower;
    testsig (module Str) -<-> (module Str) =: str_lower;
    testsig (module Str) -~-> (module Str) =: str_lower; ]

let concat_tests =
  let str_concat = ("Str.concat",Str.concat) in
  [ testsig (module Str) -$-> (module Str) ---> (module Str) =: str_concat;
    testsig (module Str) ---> (module Str) -$-> (module Str) =: str_concat;
    testsig (module Str) -<-> (module Str) ---> (module Str) =: str_concat;
    testsig (module Str) ---> (module Str) -<-> (module Str) =: str_concat;
    testsig (module Str) -~-> (module Str) ---> (module Str) =: str_concat;
    testsig (module Str) ---> (module Str) -~-> (module Str) =: str_concat; ]

let concat_sound = (* forall s,s'. abs(s ^ s') = abs(s) ^ abs(s') *)
  let pp_pair = PP.pair (fun i -> i) (fun i -> i) in
  mk_test ~n:1000 ~pp:pp_pair ~limit:1 ~name:("Str.concat sound") ~size:(fun p -> String.length (pp_pair p))
    Arbitrary.(pair string string)
    (fun (s,s') -> Str.(eq (concat (const s) (const s')) (const (s ^ s')) ))

(* test suite for specific string operations *)
let spec_str_operations =
  flatten
    [ empty_tests;
      nonempty_tests;
      upper_tests;
      lower_tests;
      concat_tests;
      [concat_sound] ]


(** Value lattice *)

(* generic combinators for checking properties of unary queries *)

(* Note: the following predicate functions depend on the Boolean lattice: *)
(*       {true,false} under implication ordering,                         *)
(*        bot = false <== false <== true <== true = top                   *)
let query_monotone_invariant_dbool qname q =
  let vl_pair = (qname,q) in
  [(*testsig (module VL) -$-> (module DBool) =: vl_pair;*)
     testsig (module VL) -<-> (module DBool) =: vl_pair;
     testsig (module VL) -~-> (module DBool) =: vl_pair; ]

(* Note: the following predicate functions depend on the Boolean lattice: *)
(*       {true,false} under reverse implication ordering,                 *)
(*        bot = true <== true <== false <== false = top                   *)
let query_strict_monotone_invariant_bool qname q =
  let vl_pair = (qname,q) in
  [testsig (module VL) -$-> (module Bool) =: vl_pair;
   testsig (module VL) -<-> (module Bool) =: vl_pair;
   testsig (module VL) -~-> (module Bool) =: vl_pair; ]

let unary_is_strict_monotone_invariant name op =
  let vl_pair = (name,op) in
  [ testsig (module VL) -$-> (module VL) =: vl_pair;
    testsig (module VL) -<-> (module VL) =: vl_pair;
    testsig (module VL) -~-> (module VL) =: vl_pair; ]

let binop_is_strict_monotone_invariant name op =
  let vl_pair = (name,op) in
  [ testsig (module VL) -$-> (module VL) ---> (module VL) =: vl_pair;
    testsig (module VL) ---> (module VL) -$-> (module VL) =: vl_pair;
    testsig (module VL) -<-> (module VL) ---> (module VL) =: vl_pair;
    testsig (module VL) ---> (module VL) -<-> (module VL) =: vl_pair;
    testsig (module VL) -~-> (module VL) ---> (module VL) =: vl_pair;
    testsig (module VL) ---> (module VL) -~-> (module VL) =: vl_pair; ]

(* test suite for specific value operations *)
let spec_vl_operations =
  flatten
    [ (* unary op tests *)
      unary_is_strict_monotone_invariant "VL.exclude_nil"    VL.exclude_nil;
      unary_is_strict_monotone_invariant "VL.exclude_proc"   VL.exclude_proc;
      unary_is_strict_monotone_invariant "VL.exclude_tables" VL.exclude_tables;
      unary_is_strict_monotone_invariant "VL.only_tables"  VL.only_tables;
      unary_is_strict_monotone_invariant "VL.coerce_tonum" VL.coerce_tonum;
      unary_is_strict_monotone_invariant "VL.coerce_tostring" VL.coerce_tostring;
      unary_is_strict_monotone_invariant "VL.upper" VL.upper;
      unary_is_strict_monotone_invariant "VL.lower" VL.lower;
      unary_is_strict_monotone_invariant "VL.char"  VL.char;
      unary_is_strict_monotone_invariant "VL.typep" VL.typep;
      unary_is_strict_monotone_invariant "VL.unop Ast.Uminus" (VL.unop Ast.Uminus);
      unary_is_strict_monotone_invariant "VL.unop Ast.Length" (VL.unop Ast.Length);
      unary_is_strict_monotone_invariant "VL.unop Ast.Not"    (VL.unop Ast.Not);
      (* binop tests *)
      binop_is_strict_monotone_invariant "VL.binop Ast.Eq" (VL.binop Ast.Eq);
      binop_is_strict_monotone_invariant "VL.binop Ast.Lt" (VL.binop Ast.Lt);
      binop_is_strict_monotone_invariant "VL.binop Ast.Gt" (VL.binop Ast.Gt);
      binop_is_strict_monotone_invariant "VL.binop Ast.Ne" (VL.binop Ast.Ne);
      binop_is_strict_monotone_invariant "VL.binop Ast.Le" (VL.binop Ast.Le);
      binop_is_strict_monotone_invariant "VL.binop Ast.Ge" (VL.binop Ast.Ge);
      binop_is_strict_monotone_invariant "VL.binop Ast.Plus"  (VL.binop Ast.Plus);
      binop_is_strict_monotone_invariant "VL.binop Ast.Minus" (VL.binop Ast.Minus);
      binop_is_strict_monotone_invariant "VL.binop Ast.Times" (VL.binop Ast.Times);
      binop_is_strict_monotone_invariant "VL.binop Ast.Div" (VL.binop Ast.Div);
      binop_is_strict_monotone_invariant "VL.binop Ast.Mod" (VL.binop Ast.Mod);
      binop_is_strict_monotone_invariant "VL.binop Ast.Pow" (VL.binop Ast.Pow);
      binop_is_strict_monotone_invariant "VL.binop Ast.Concat" (VL.binop Ast.Concat);
      (* query tests (under reverse implication ordering) *)
      query_strict_monotone_invariant_bool "is_bot"     VL.is_bot;
      query_strict_monotone_invariant_bool "is_nil"     VL.is_nil;
      query_strict_monotone_invariant_bool "is_number"  VL.is_number;
      query_strict_monotone_invariant_bool "is_strings" VL.is_strings;
      (* query tests (under implication ordering) *)
      query_monotone_invariant_dbool "may_be_nil" VL.may_be_nil;
      query_monotone_invariant_dbool "may_be_non_nil" VL.may_be_non_nil;
      query_monotone_invariant_dbool "may_be_number" VL.may_be_number;
      query_monotone_invariant_dbool "may_be_non_strings" VL.may_be_non_strings;
      query_monotone_invariant_dbool "may_be_proc" VL.may_be_proc;
      query_monotone_invariant_dbool "may_be_table" VL.may_be_proc; ]
    (* The queries 'is_nil/is_number/is_strings' don't seem to exhibit lattice-based properties *)
    (* e.g., is_nil vlat = VL.leq vlat VL.nil 
       is true at bot, and VL.nil but becomes false as the arg increases *)


(** Environment lattice *)

(* Note: the following predicate depends on the Boolean lattice  *)
(*       {true,false} under reverse implication ordering.        *)
let is_bot_tests =
  let el_is_bot = ("EL.is_bot",EL.is_bot) in
  [ testsig (module EL) -$-> (module Bool) =: el_is_bot;
    testsig (module EL) -<-> (module Bool) =: el_is_bot;
    testsig (module EL) -~-> (module Bool) =: el_is_bot; ]

let enter_scope_tests = (*  enter_scope : EL -> label -> EL *)
  let el_enter_scope = ("EL.enter_scope",EL.enter_scope) in
  [ pw_right (module Label) op_strict    (module EL) (module EL) =:: el_enter_scope;
    pw_right (module Label) op_monotone  (module EL) (module EL) =:: el_enter_scope;
    pw_right (module Label) op_invariant (module EL) (module EL) =:: el_enter_scope; ]

(* test suite for specific environment operations *)
let spec_env_operations =
  flatten [
    is_bot_tests;
    enter_scope_tests;
  ]


(** Property lattice *)

let find_exn_tests = (* find_exn : string -> PL -> VL + Not_found *)
  let find_exn' str map = try PL.find_exn str map with Not_found -> VL.bot in (* wrap 'find_exn' (which may raise exception) *)
  let pl_find_exn = ("PL.find_exn",find_exn') in
  [ pw_left (module Str_arg) op_strict    (module PL) (module VL) =:: pl_find_exn;
    pw_left (module Str_arg) op_monotone  (module PL) (module VL) =:: pl_find_exn;
    pw_left (module Str_arg) op_invariant (module PL) (module VL) =:: pl_find_exn; ]

let find_fail = (* forall s. find s bot = VL.nil *)
  mk_test ~n:1000 ~pp:PP.string ~limit:1 ~name:("find fail in " ^ PL.name) ~size:(fun s -> String.length (PP.string s))
    Arbitrary.string (fun s -> VL.eq (PL.find s PL.bot) VL.nil)

let find_tests = (* find : string -> PL -> VL *)
  let pl_find = ("PL.find",PL.find) in
  [ find_fail;
  (*pw_left (module Str_arg) op_strict    (module PL) (module VL) =:: pl_find;*) (* not strict b/c default *)
  (*pw_left (module Str_arg) op_monotone  (module PL) (module VL) =:: pl_find;*) (* not monotone b/c default *)
    pw_left (module Str_arg) op_invariant (module PL) (module VL) =:: pl_find; ]

let find_default_tests =
  let pl_find_default = ("PL.find_default",PL.find_default) in
  [ testsig (module PL) -$-> (module VLVLpair) =: pl_find_default;
    testsig (module PL) -<-> (module VLVLpair) =: pl_find_default;
    testsig (module PL) -~-> (module VLVLpair) =: pl_find_default; ]

let find_all_keys_tests =
  let pl_find_all_keys = ("PL.find_all_keys",PL.find_all_keys) in
  [ testsig (module PL) -$-> (module VL) =: pl_find_all_keys;
    testsig (module PL) -<-> (module VL) =: pl_find_all_keys;
    testsig (module PL) -~-> (module VL) =: pl_find_all_keys; ]

let find_all_tests =
  let pl_find_all = ("PL.find_all",PL.find_all) in
  [ testsig (module PL) -$-> (module VL) =: pl_find_all;
    testsig (module PL) -<-> (module VL) =: pl_find_all;
    testsig (module PL) -~-> (module VL) =: pl_find_all; ]

let get_metatable_tests =
  let pl_get_metatable = ("PL.get_metatable",PL.get_metatable) in
  [ (*testsig (module PL) -$-> (module VL) =: pl_get_metatable;*) (* by adding the field definitely result will not *)
    (*testsig (module PL) -<-> (module VL) =: pl_get_metatable;*) (* include nil, hence not strict and monotone *)
    testsig (module PL) -~-> (module VL) =: pl_get_metatable; ]

let set_metatable_tests =
  let pl_set_metatable = ("PL.set_metatable",PL.set_metatable) in
  [ testsig (module VL) -$-> (module PL) ---> (module PL) =: pl_set_metatable;
    testsig (module VL) -<-> (module PL) ---> (module PL) =: pl_set_metatable;
    testsig (module VL) -~-> (module PL) ---> (module PL) =: pl_set_metatable;
    (*testsig (module VL) ---> (module PL) -$-> (module PL) =: pl_set_metatable;*) (* adding field to bot is not bot *)
    testsig (module VL) ---> (module PL) -<-> (module PL) =: pl_set_metatable;
    testsig (module VL) ---> (module PL) -~-> (module PL) =: pl_set_metatable; ]

let set_metatable_absent_tests =
  let pl_set_metatable_absent = ("PL.set_metatable_absent",PL.set_metatable_absent) in
  [ testsig (module PL) -$-> (module PL) =: pl_set_metatable_absent;
    testsig (module PL) -<-> (module PL) =: pl_set_metatable_absent;
    testsig (module PL) -~-> (module PL) =: pl_set_metatable_absent; ]

let get_metatable_set_metatable_extensive = (* forall v,p. v <= get_metatable (set_metatable v p) *)
  let pp_pair = PP.pair VL.to_string PL.to_string in
  mk_test ~n:1000 ~pp:pp_pair ~limit:1 ~name:("PL.get_meta_table-PL.set_meta_table extensive")
          ~size:(fun p -> String.length (pp_pair p))
    Arbitrary.(pair VL.arb_elem PL.arb_elem)
    (fun (v,p) -> VL.leq v (PL.get_metatable (PL.set_metatable v p)))

let add_tests = (* add : string -> VL -> PL -> PL *)
  let pl_add = ("PL.add",PL.add) in (* current 'add' not strict in val arg; adding field to bot is not bot  *)
  [(*pw_left (module Str_arg) (pw_right (module PL) op_strict)    (module VL) (module PL) =:: pl_add;*) 
     pw_left (module Str_arg) (pw_right (module PL) op_monotone)  (module VL) (module PL) =:: pl_add;
     pw_left (module Str_arg) (pw_right (module PL) op_invariant) (module VL) (module PL) =:: pl_add;
   (*pw_left (module Str_arg) (pw_left (module VL) op_strict)    (module PL) (module PL) =:: pl_add;*)
     pw_left (module Str_arg) (pw_left (module VL) op_monotone)  (module PL) (module PL) =:: pl_add;
     pw_left (module Str_arg) (pw_left (module VL) op_invariant) (module PL) (module PL) =:: pl_add; ]

let add_default_tests =
  let pl_add_default = ("PL.add_default",PL.add_default) in
  [ testsig (module VL) -<-> (module VL) ---> (module PL) ---> (module PL) =: pl_add_default;
    testsig (module VL) -~-> (module VL) ---> (module PL) ---> (module PL) =: pl_add_default;
    testsig (module VL) ---> (module VL) -<-> (module PL) ---> (module PL) =: pl_add_default;
    testsig (module VL) ---> (module VL) -~-> (module PL) ---> (module PL) =: pl_add_default;
    testsig (module VL) ---> (module VL) ---> (module PL) -<-> (module PL) =: pl_add_default;
    testsig (module VL) ---> (module VL) ---> (module PL) -~-> (module PL) =: pl_add_default; ]
(* Current 'add_default' not strict in first/second args *)

let add_scopechain_tests = (* add_scopechain : PL -> label list -> PL *)
  let pl_add_scopechain = ("PL.add_scopechain",PL.add_scopechain) in
  [(*pw_right (module Labellist) op_strict    (module PL) (module PL) =:: pl_add_scopechain;*)
     pw_right (module Labellist) op_monotone  (module PL) (module PL) =:: pl_add_scopechain;
     pw_right (module Labellist) op_invariant (module PL) (module PL) =:: pl_add_scopechain; ]
(* Current 'add_scopechain' not strict in first arg *)

let add_all_tests =
  let pl_add_all = ("PL.add_all", PL.add_all) in
  [ testsig (module VL) -<-> (module VL) ---> (module PL) ---> (module PL) =: pl_add_all;
    testsig (module VL) -~-> (module VL) ---> (module PL) ---> (module PL) =: pl_add_all;
    testsig (module VL) ---> (module VL) -<-> (module PL) ---> (module PL) =: pl_add_all;
    testsig (module VL) ---> (module VL) -~-> (module PL) ---> (module PL) =: pl_add_all;
    testsig (module VL) ---> (module VL) ---> (module PL) -<-> (module PL) =: pl_add_all;
    testsig (module VL) ---> (module VL) ---> (module PL) -~-> (module PL) =: pl_add_all; ]
(* Current 'add_all' not strict in first/second args *)

let add_all_params_tests = (* add_all_params : string list -> VL list -> PL -> PL *)
  let pl_add_all_params = ("PL.add_all_params",PL.add_all_params) in
  [ pw_left (module Strlist) (pw_left (module VLlist) op_monotone)  (module PL) (module PL) =:: pl_add_all_params;
    pw_left (module Strlist) (pw_left (module VLlist) op_invariant) (module PL) (module PL) =:: pl_add_all_params; ]
      
let find_add_extensive = (* forall s,v,p. v <= find s (add s v p) *)
  let pp_triple = PP.triple PP.string VL.to_string PL.to_string in
  mk_test ~n:1000 ~pp:pp_triple ~limit:1 ~name:("find-add extensive in " ^ PL.name)
          ~size:(fun t -> String.length (pp_triple t))
    Arbitrary.(triple string VL.arb_elem PL.arb_elem)
    (fun (s,v,p) -> VL.leq v (PL.find s (PL.add s v p)))

let find_add_monotone = (* forall s,v,v',p. v <= v'  ==>  find s (add s v p) <= find s (add s v' p)*)
  let pp_triple = PP.triple PP.string GenValTests.pp_pair PL.to_string in
  mk_test ~n:1000 ~pp:pp_triple ~limit:1 ~name:("find-add monotone in " ^ PL.name)
          ~size:(fun t -> String.length (pp_triple t))
    Arbitrary.(triple string GenValTests.ord_pair PL.arb_elem)
    (fun (s,(v,v'),p) -> Prop.assume (VL.leq v v');
		         VL.leq (PL.find s (PL.add s v p)) (PL.find s (PL.add s v' p)))

let find_exn_add_extensive = (* forall s,v,p. v <= find_exn s (add s v p) *)
  let pp_triple = PP.triple PP.string VL.to_string PL.to_string in
  mk_test ~n:1000 ~pp:pp_triple ~limit:1 ~name:("find_exn-add extensive in " ^ PL.name)
          ~size:(fun t -> String.length (pp_triple t))
    Arbitrary.(triple string VL.arb_elem PL.arb_elem)
    (fun (s,v,p) -> VL.leq v (PL.find_exn s (PL.add s v p)))

let find_exn_add_monotone = (* forall s,v,v',p. v <= v'
                                            ==> find_exn s (add s v p) <= find_exn s (add s v' p) *)
  let pp_triple = PP.triple PP.string GenValTests.pp_pair PL.to_string in
  mk_test ~n:1000 ~pp:pp_triple ~limit:1 ~name:("find_exn-add monotone in " ^ PL.name)
          ~size:(fun t -> String.length (pp_triple t))
    Arbitrary.(triple string GenValTests.ord_pair PL.arb_elem)
    (fun (s,(v,v'),p) -> Prop.assume (VL.leq v v');
		         VL.leq (PL.find_exn s (PL.add s v p)) (PL.find_exn s (PL.add s v' p)))

(* test suite for specific property operations *)
let spec_prop_operations =
  flatten [
    find_exn_tests;
    find_tests;
    find_default_tests;
    find_all_keys_tests;
    find_all_tests;
    get_metatable_tests;
    set_metatable_tests;
    [get_metatable_set_metatable_extensive];
    set_metatable_absent_tests;
    add_tests;
    add_default_tests;
    add_scopechain_tests;
    add_all_tests;
    add_all_params_tests;
    [ find_add_extensive; find_add_monotone;
      find_exn_add_extensive; find_exn_add_monotone; ] ]


(** Store lattice *)

(* Note: the following predicate depends on the Boolean lattice  *)
(*       {true,false} under reverse implication ordering.        *)
let is_bot_tests =
  let st_is_bot = ("ST.is_bot",ST.is_bot) in
  [ testsig (module ST) -$-> (module Bool) =: st_is_bot;
    testsig (module ST) -<-> (module Bool) =: st_is_bot;
    testsig (module ST) -~-> (module Bool) =: st_is_bot; ]

let add_label_tests = (* add_label : ST -> label -> PL -> ST *)
  let st_add_label = ("ST.add_label",ST.add_label) in (* adding entry to bot, or empty table should not give bot *)
  [(*pw_right (module Label) (pw_right (module PL) op_strict)    (module ST) (module ST) =:: st_add_label;*)
     pw_right (module Label) (pw_right (module PL) op_monotone)  (module ST) (module ST) =:: st_add_label;
     pw_right (module Label) (pw_right (module PL) op_invariant) (module ST) (module ST) =:: st_add_label;
   (*pw_left (module ST) (pw_left (module Label) op_strict)    (module PL) (module ST) =:: st_add_label;*)
     pw_left (module ST) (pw_left (module Label) op_monotone)  (module PL) (module ST) =:: st_add_label;
     pw_left (module ST) (pw_left (module Label) op_invariant) (module PL) (module ST) =:: st_add_label; ]
      
let find_label_tests = (* find_label : ST -> label -> PL *)
  let st_find_label = ("ST.find_label",ST.find_label) in
  [ pw_right (module Label) op_strict    (module ST) (module PL) =:: st_find_label;
    pw_right (module Label) op_monotone  (module ST) (module PL) =:: st_find_label;
    pw_right (module Label) op_invariant (module ST) (module PL) =:: st_find_label; ]

let find_label_add_label_id = (* forall s,l,p. find_label (add_label s l p) l = p *)
  let pp_triple = PP.triple ST.to_string PP.int PL.to_string in
  mk_test ~n:1000 ~pp:pp_triple ~limit:1 ~name:("find_label-add_label identity in " ^ ST.name)
          ~size:(fun t -> String.length (pp_triple t))
    Arbitrary.(triple ST.arb_elem Label.arb_elem PL.arb_elem)
    (fun (s,l,p) -> PL.eq (ST.find_label (ST.add_label s l p) l) p)

let lookup_prop_tests = (* lookup_prop : ST -> VL -> string -> VL *)
  let st_lookup_prop = ("ST.lookup_prop",ST.lookup_prop) in
  [ pw_right (module VL) (pw_right (module Str_arg) op_strict)    (module ST) (module VL) =:: st_lookup_prop;
    pw_right (module VL) (pw_right (module Str_arg) op_monotone)  (module ST) (module VL) =:: st_lookup_prop;
    pw_right (module VL) (pw_right (module Str_arg) op_invariant) (module ST) (module VL) =:: st_lookup_prop;
    pw_left  (module ST) (pw_right (module Str_arg) op_strict)    (module VL) (module VL) =:: st_lookup_prop;
    pw_left  (module ST) (pw_right (module Str_arg) op_monotone)  (module VL) (module VL) =:: st_lookup_prop;
    pw_left  (module ST) (pw_right (module Str_arg) op_invariant) (module VL) (module VL) =:: st_lookup_prop; ]

let lookup_all_keys_tests =
  let st_lookup_all_keys = ("ST.lookup_all_keys",ST.lookup_all_keys) in
  [ testsig (module ST) -$-> (module VL) ---> (module VL) =: st_lookup_all_keys;
    testsig (module ST) -<-> (module VL) ---> (module VL) =: st_lookup_all_keys;
    testsig (module ST) -~-> (module VL) ---> (module VL) =: st_lookup_all_keys;
    testsig (module ST) ---> (module VL) -$-> (module VL) =: st_lookup_all_keys;
    testsig (module ST) ---> (module VL) -<-> (module VL) =: st_lookup_all_keys;
    testsig (module ST) ---> (module VL) -~-> (module VL) =: st_lookup_all_keys; ]

let lookup_all_props_tests =
  let st_lookup_all_props = ("ST.lookup_all_props",ST.lookup_all_props) in
  [ testsig (module ST) -$-> (module VL) ---> (module VL) =: st_lookup_all_props;
    testsig (module ST) -<-> (module VL) ---> (module VL) =: st_lookup_all_props;
    testsig (module ST) -~-> (module VL) ---> (module VL) =: st_lookup_all_props;
    testsig (module ST) ---> (module VL) -$-> (module VL) =: st_lookup_all_props;
    testsig (module ST) ---> (module VL) -<-> (module VL) =: st_lookup_all_props;
    testsig (module ST) ---> (module VL) -~-> (module VL) =: st_lookup_all_props; ]

let lookup_default_prop_tests =
  let st_lookup_default_prop = ("ST.lookup_default_prop",ST.lookup_default_prop) in
  [ testsig (module ST) -$-> (module VL) ---> (module VL) ---> (module VL) =: st_lookup_default_prop;
    testsig (module ST) -<-> (module VL) ---> (module VL) ---> (module VL) =: st_lookup_default_prop;
    testsig (module ST) -~-> (module VL) ---> (module VL) ---> (module VL) =: st_lookup_default_prop;
    testsig (module ST) ---> (module VL) -$-> (module VL) ---> (module VL) =: st_lookup_default_prop;
    testsig (module ST) ---> (module VL) -<-> (module VL) ---> (module VL) =: st_lookup_default_prop;
    testsig (module ST) ---> (module VL) -~-> (module VL) ---> (module VL) =: st_lookup_default_prop;
    testsig (module ST) ---> (module VL) ---> (module VL) -$-> (module VL) =: st_lookup_default_prop;
    testsig (module ST) ---> (module VL) ---> (module VL) -<-> (module VL) =: st_lookup_default_prop;
    testsig (module ST) ---> (module VL) ---> (module VL) -~-> (module VL) =: st_lookup_default_prop; ]

let lookup_dyn_prop_tests =
  let st_lookup_dyn_prop = ("ST.lookup_dyn_prop",ST.lookup_dyn_prop) in
  [ testsig (module ST) -$-> (module VL) ---> (module VL) ---> (module VL) =: st_lookup_dyn_prop;
    testsig (module ST) -<-> (module VL) ---> (module VL) ---> (module VL) =: st_lookup_dyn_prop;
    testsig (module ST) -~-> (module VL) ---> (module VL) ---> (module VL) =: st_lookup_dyn_prop;
    testsig (module ST) ---> (module VL) -$-> (module VL) ---> (module VL) =: st_lookup_dyn_prop;
    testsig (module ST) ---> (module VL) -<-> (module VL) ---> (module VL) =: st_lookup_dyn_prop;
    testsig (module ST) ---> (module VL) -~-> (module VL) ---> (module VL) =: st_lookup_dyn_prop;
    testsig (module ST) ---> (module VL) ---> (module VL) -$-> (module VL) =: st_lookup_dyn_prop;
    testsig (module ST) ---> (module VL) ---> (module VL) -<-> (module VL) =: st_lookup_dyn_prop;
    testsig (module ST) ---> (module VL) ---> (module VL) -~-> (module VL) =: st_lookup_dyn_prop; ]

let write_prop_tests = (* write_prop : ST -> VL -> string -> VL -> ST *)
  let wp = ("ST.write_prop",ST.write_prop) in (* result of writing to empty table should be non bot *)
  [ pw_right (module VL) (pw_right (module Str_arg) (pw_right (module VL) op_strict))    (module ST) (module ST) =:: wp;
    pw_right (module VL) (pw_right (module Str_arg) (pw_right (module VL) op_monotone))  (module ST) (module ST) =:: wp;
    pw_right (module VL) (pw_right (module Str_arg) (pw_right (module VL) op_invariant)) (module ST) (module ST) =:: wp;
 (* pw_left (module ST) (pw_right (module Str_arg) (pw_right (module VL) op_strict))    (module VL) (module ST) =:: wp;*)
    pw_left (module ST) (pw_right (module Str_arg) (pw_right (module VL) op_monotone))  (module VL) (module ST) =:: wp;
    pw_left (module ST) (pw_right (module Str_arg) (pw_right (module VL) op_invariant)) (module VL) (module ST) =:: wp;
 (* pw_left (module ST) (pw_left (module VL) (pw_left (module Str_arg) op_strict))    (module VL) (module ST) =:: wp;*)
    pw_left (module ST) (pw_left (module VL) (pw_left (module Str_arg) op_monotone))  (module VL) (module ST) =:: wp;
    pw_left (module ST) (pw_left (module VL) (pw_left (module Str_arg) op_invariant)) (module VL) (module ST) =:: wp; ]
 (* Current 'write_prop' not strict in 2nd and 4th arg *)
    
let write_all_props_tests = (* result of writing to no table should be bot *)
  let st_write_all_props = ("ST.write_all_props",ST.write_all_props) in
  [(*testsig (module ST) -$-> (module VL) ---> (module VL) ---> (module VL) ---> (module ST) =: st_write_all_props;*)
     testsig (module ST) -<-> (module VL) ---> (module VL) ---> (module VL) ---> (module ST) =: st_write_all_props;
     testsig (module ST) -~-> (module VL) ---> (module VL) ---> (module VL) ---> (module ST) =: st_write_all_props;
     testsig (module ST) ---> (module VL) -$-> (module VL) ---> (module VL) ---> (module ST) =: st_write_all_props;
     testsig (module ST) ---> (module VL) -<-> (module VL) ---> (module VL) ---> (module ST) =: st_write_all_props;
     testsig (module ST) ---> (module VL) -~-> (module VL) ---> (module VL) ---> (module ST) =: st_write_all_props;
   (*testsig (module ST) ---> (module VL) ---> (module VL) -$-> (module VL) ---> (module ST) =: st_write_all_props;*)
     testsig (module ST) ---> (module VL) ---> (module VL) -<-> (module VL) ---> (module ST) =: st_write_all_props;
     testsig (module ST) ---> (module VL) ---> (module VL) -~-> (module VL) ---> (module ST) =: st_write_all_props;
   (*testsig (module ST) ---> (module VL) ---> (module VL) ---> (module VL) -$-> (module ST) =: st_write_all_props;*)
     testsig (module ST) ---> (module VL) ---> (module VL) ---> (module VL) -<-> (module ST) =: st_write_all_props;
     testsig (module ST) ---> (module VL) ---> (module VL) ---> (module VL) -~-> (module ST) =: st_write_all_props; ]
 (* Current 'write_all_props' not strict in 1st, 3rd and 4th arg *)

let write_default_prop_tests = (* result of writing to empty table should not be bot *)
  let st_write_default_prop = ("ST.write_default_prop",ST.write_default_prop) in
  [(*testsig (module ST) -$-> (module VL) ---> (module VL) ---> (module VL) ---> (module ST) =: st_write_default_prop;*)
     testsig (module ST) -<-> (module VL) ---> (module VL) ---> (module VL) ---> (module ST) =: st_write_default_prop;
     testsig (module ST) -~-> (module VL) ---> (module VL) ---> (module VL) ---> (module ST) =: st_write_default_prop;
   (*testsig (module ST) ---> (module VL) -$-> (module VL) ---> (module VL) ---> (module ST) =: st_write_default_prop;*)
     testsig (module ST) ---> (module VL) -<-> (module VL) ---> (module VL) ---> (module ST) =: st_write_default_prop;
     testsig (module ST) ---> (module VL) -~-> (module VL) ---> (module VL) ---> (module ST) =: st_write_default_prop;
   (*testsig (module ST) ---> (module VL) ---> (module VL) -$-> (module VL) ---> (module ST) =: st_write_default_prop;*)
     testsig (module ST) ---> (module VL) ---> (module VL) -<-> (module VL) ---> (module ST) =: st_write_default_prop;
     testsig (module ST) ---> (module VL) ---> (module VL) -~-> (module VL) ---> (module ST) =: st_write_default_prop;
   (*testsig (module ST) ---> (module VL) ---> (module VL) ---> (module VL) -$-> (module ST) =: st_write_default_prop;*)
     testsig (module ST) ---> (module VL) ---> (module VL) ---> (module VL) -<-> (module ST) =: st_write_default_prop;
     testsig (module ST) ---> (module VL) ---> (module VL) ---> (module VL) -~-> (module ST) =: st_write_default_prop; ]
 (* Current 'write_default_prop' not strict in 1st, 3rd and 4th args *)

let write_dyn_prop_tests = (* result of writing to empty table should not be bot *)
  let st_write_dyn_prop = ("ST.write_dyn_prop",ST.write_dyn_prop) in
  [(*testsig (module ST) -$-> (module VL) ---> (module VL) ---> (module VL) ---> (module ST) =: st_write_dyn_prop;*)
     testsig (module ST) -<-> (module VL) ---> (module VL) ---> (module VL) ---> (module ST) =: st_write_dyn_prop;
     testsig (module ST) -~-> (module VL) ---> (module VL) ---> (module VL) ---> (module ST) =: st_write_dyn_prop;
   (*testsig (module ST) ---> (module VL) -$-> (module VL) ---> (module VL) ---> (module ST) =: st_write_dyn_prop;*)
     testsig (module ST) ---> (module VL) -<-> (module VL) ---> (module VL) ---> (module ST) =: st_write_dyn_prop;
     testsig (module ST) ---> (module VL) -~-> (module VL) ---> (module VL) ---> (module ST) =: st_write_dyn_prop;
   (*testsig (module ST) ---> (module VL) ---> (module VL) -$-> (module VL) ---> (module ST) =: st_write_dyn_prop;*)
     testsig (module ST) ---> (module VL) ---> (module VL) -<-> (module VL) ---> (module ST) =: st_write_dyn_prop;
     testsig (module ST) ---> (module VL) ---> (module VL) -~-> (module VL) ---> (module ST) =: st_write_dyn_prop;
   (*testsig (module ST) ---> (module VL) ---> (module VL) ---> (module VL) -$-> (module ST) =: st_write_dyn_prop;*)
     testsig (module ST) ---> (module VL) ---> (module VL) ---> (module VL) -<-> (module ST) =: st_write_dyn_prop;
     testsig (module ST) ---> (module VL) ---> (module VL) ---> (module VL) -~-> (module ST) =: st_write_dyn_prop; ]
 (* Current 'write_dyn_prop' not strict in 1st, 3rd and 4th args *)

let get_metatable_tests = (* get_metatable : ST -> VL -> VL *)
  let st_get_metatable = ("ST.get_metatable",ST.get_metatable) in
  [ testsig (module ST) -$-> (module VL) ---> (module VL) =: st_get_metatable;
    testsig (module ST) -<-> (module VL) ---> (module VL) =: st_get_metatable;
    testsig (module ST) -~-> (module VL) ---> (module VL) =: st_get_metatable;
    testsig (module ST) ---> (module VL) -$-> (module VL) =: st_get_metatable;
    testsig (module ST) ---> (module VL) -<-> (module VL) =: st_get_metatable;
    testsig (module ST) ---> (module VL) -~-> (module VL) =: st_get_metatable; ]

let set_metatable_tests = (* set_metatable : ST -> VL -> VL -> ST *)
  let st_set_metatable = ("ST.set_metatable",ST.set_metatable) in (* result of writing to table-less heap should be bot *)
  [ testsig (module ST) -$-> (module VL) ---> (module VL) ---> (module ST) =: st_set_metatable;
    testsig (module ST) -<-> (module VL) ---> (module VL) ---> (module ST) =: st_set_metatable;
    testsig (module ST) -~-> (module VL) ---> (module VL) ---> (module ST) =: st_set_metatable;
  (*testsig (module ST) ---> (module VL) -$-> (module VL) ---> (module ST) =: st_set_metatable;*)
    testsig (module ST) ---> (module VL) -<-> (module VL) ---> (module ST) =: st_set_metatable;
    testsig (module ST) ---> (module VL) -~-> (module VL) ---> (module ST) =: st_set_metatable;
  (*testsig (module ST) ---> (module VL) ---> (module VL) -$-> (module ST) =: st_set_metatable;*)
    testsig (module ST) ---> (module VL) ---> (module VL) -<-> (module ST) =: st_set_metatable;
    testsig (module ST) ---> (module VL) ---> (module VL) -~-> (module ST) =: st_set_metatable; ]
 (* Current 'set_metatable' not strict in 2nd and 3rd arg *)

(* not extensive: for empty store, set_metatable will return the empty store
                  and get_metatable's output will be incomparable to v' *)
(*let get_metatable_set_metatable_extensive = (* forall v,v',st. v' <= get_metatable (set_metatable st v v') v *)
    let pp = PP.triple VL.to_string VL.to_string ST.to_string in
    mk_test ~n:1000 ~pp:pp ~limit:1 ~size:(fun a -> String.length (pp a))
                    ~name:("ST.get_meta_table-ST.set_meta_table extensive")
      Arbitrary.(triple VL.arb_elem VL.arb_elem ST.arb_elem)
      (fun (v,v',st) -> VL.leq (VL.only_tables v') (ST.get_metatable (ST.set_metatable st v (VL.only_tables v')) v)) *)

let lookup_event_tests = (* lookup_event : ST -> VL -> string -> VL *)
  let st_lookup_event = ("ST.lookup_event",ST.lookup_event) in
  [ pw_right (module VL) (pw_right (module Str_arg) op_strict)    (module ST) (module VL) =:: st_lookup_event;
    pw_right (module VL) (pw_right (module Str_arg) op_monotone)  (module ST) (module VL) =:: st_lookup_event;
    pw_right (module VL) (pw_right (module Str_arg) op_invariant) (module ST) (module VL) =:: st_lookup_event;
    pw_left  (module ST) (pw_right (module Str_arg) op_strict)    (module VL) (module VL) =:: st_lookup_event;
    pw_left  (module ST) (pw_right (module Str_arg) op_monotone)  (module VL) (module VL) =:: st_lookup_event;
    pw_left  (module ST) (pw_right (module Str_arg) op_invariant) (module VL) (module VL) =:: st_lookup_event; ]

let getbinhandler_tests = (* getbinhandler : ST -> VL -> VL -> string -> VL *)
  let st_getbinhandler = ("ST.getbinhandler",ST.getbinhandler) in
  [ pw_right (module VL) (pw_right (module VL) (pw_right (module Str_arg) op_strict))    (module ST) (module VL) =:: st_getbinhandler;
    pw_right (module VL) (pw_right (module VL) (pw_right (module Str_arg) op_monotone))  (module ST) (module VL) =:: st_getbinhandler;
    pw_right (module VL) (pw_right (module VL) (pw_right (module Str_arg) op_invariant)) (module ST) (module VL) =:: st_getbinhandler;
    pw_left  (module ST) (pw_right (module VL) (pw_right (module Str_arg) op_strict))    (module VL) (module VL) =:: st_getbinhandler;
    pw_left  (module ST) (pw_right (module VL) (pw_right (module Str_arg) op_monotone))  (module VL) (module VL) =:: st_getbinhandler;
    pw_left  (module ST) (pw_right (module VL) (pw_right (module Str_arg) op_invariant)) (module VL) (module VL) =:: st_getbinhandler;
    pw_left  (module ST) (pw_left  (module VL) (pw_right (module Str_arg) op_strict))    (module VL) (module VL) =:: st_getbinhandler;
    pw_left  (module ST) (pw_left  (module VL) (pw_right (module Str_arg) op_monotone))  (module VL) (module VL) =:: st_getbinhandler;
    pw_left  (module ST) (pw_left  (module VL) (pw_right (module Str_arg) op_invariant)) (module VL) (module VL) =:: st_getbinhandler; ]

(* test suite for specific store operations *)
let spec_store_operations =
  flatten [
    is_bot_tests;
    add_label_tests;
    find_label_tests;
    [ find_label_add_label_id; ];
    lookup_prop_tests;
    lookup_all_keys_tests;
    lookup_all_props_tests;
    lookup_default_prop_tests;
    lookup_dyn_prop_tests;
    write_prop_tests;
    write_all_props_tests;
    write_default_prop_tests;
    write_dyn_prop_tests;
    get_metatable_tests;
    set_metatable_tests;
    (*[get_metatable_set_metatable_extensive];*)
    lookup_event_tests;
    getbinhandler_tests;
  ]


(** State lattice *)

(* Note: the following predicate depends on the Boolean lattice  *)
(*       {true,false} under reverse implication ordering.        *)
let is_bot_tests =
  let sl_is_bot = ("SL.is_bot",SL.is_bot) in
  [ testsig (module SL) -$-> (module Bool) =: sl_is_bot;
    testsig (module SL) -<-> (module Bool) =: sl_is_bot;
    testsig (module SL) -~-> (module Bool) =: sl_is_bot; ]

(* test suite for specific state operations *)
let app_bi_pair (biname,bi) = ("SL.apply_builtin " ^ biname, SL.apply_builtin bi)

let apply_builtin_test_fst p =
  let app_bi_pair = app_bi_pair p in
  [ testsig (module SL) -$-> (module VLlist) ---> (module SLVLlistpair) =: app_bi_pair;
    testsig (module SL) -<-> (module VLlist) ---> (module SLVLlistpair) =: app_bi_pair;
    testsig (module SL) -~-> (module VLlist) ---> (module SLVLlistpair) =: app_bi_pair; ]

let apply_builtin_test p =
  let app_bi_pair = app_bi_pair p in
  flatten
    [ apply_builtin_test_fst p;
      [ testsig (module SL) ---> (module VLlist) -$-> (module SLVLlistpair) =: app_bi_pair;
	testsig (module SL) ---> (module VLlist) -<-> (module SLVLlistpair) =: app_bi_pair;
	testsig (module SL) ---> (module VLlist) -~-> (module SLVLlistpair) =: app_bi_pair; ]]

let apply_builtin_tests =
  flatten
    [ apply_builtin_test ("VL.Error",    VL.Error);
      apply_builtin_test ("VL.Exit",     VL.Exit);
      apply_builtin_test ("VL.Next",     VL.Next);
      apply_builtin_test ("VL.INext",    VL.INext);
      apply_builtin_test ("VL.Pairs",    VL.Pairs);
      apply_builtin_test ("VL.IPairs",   VL.IPairs);
      apply_builtin_test_fst ("VL.Print",    VL.Print); (* note: print not strict in second arg: ok with no args *)
      apply_builtin_test_fst ("VL.Write",    VL.Write); (* note: write not strict in second arg: ok with no args *)
      [ testsig (module SL) ---> (module VLlist) -~-> (module SLVLlistpair) =: (app_bi_pair ("VL.Print",VL.Print));
	testsig (module SL) ---> (module VLlist) -<-> (module SLVLlistpair) =: (app_bi_pair ("VL.Print",VL.Print));
        testsig (module SL) ---> (module VLlist) -~-> (module SLVLlistpair) =: (app_bi_pair ("VL.Write",VL.Write)); ];
      apply_builtin_test ("VL.Tonumber", VL.Tonumber);  (* note: write not monotone under above list ordering *)
      apply_builtin_test ("VL.Tostring", VL.Tostring);
      apply_builtin_test ("VL.Abs",      VL.Abs);
      apply_builtin_test ("VL.Ceil",     VL.Ceil);
      apply_builtin_test ("VL.Floor",    VL.Floor);
      apply_builtin_test ("VL.Mod",      VL.Mod);
      (*apply_builtin_test ("VL.Random",      VL.Random);*)(* random not strict in second arg: ok with no args *)
      apply_builtin_test_fst ("VL.Random",  VL.Random);    (* random not monotone under above list ordering (exactly 0,1,2 args) *)
      [testsig (module SL) ---> (module VLlist) -~-> (module SLVLlistpair) =: (app_bi_pair ("VL.Random",VL.Random));];
      apply_builtin_test ("VL.Strlen",   VL.Strlen);
      apply_builtin_test ("VL.Strupper", VL.Strupper);
      apply_builtin_test ("VL.Strlower", VL.Strlower);
      (*apply_builtin_test ("VL.Strchar",  VL.Strchar);*) (* char not strict in second arg: ok with no args *)
      apply_builtin_test_fst ("VL.Strchar", VL.Strchar);  (* char not monotone under above list ordering *)
      [testsig (module SL) ---> (module VLlist) -~-> (module SLVLlistpair) =: (app_bi_pair ("VL.Strchar",VL.Strchar))];
      (*apply_builtin_test ("VL.Strbyte",  VL.Strbyte);*)
      apply_builtin_test_fst ("VL.Strbyte", VL.Strbyte);  (* byte not monotone under above list ordering: add non-num args *)
      [testsig (module SL) ---> (module VLlist) -$-> (module SLVLlistpair) =: (app_bi_pair ("VL.Strbyte",VL.Strbyte));
       testsig (module SL) ---> (module VLlist) -~-> (module SLVLlistpair) =: (app_bi_pair ("VL.Strbyte",VL.Strbyte))];
      (*apply_builtin_test ("VL.Strsub",   VL.Strsub);*)
      apply_builtin_test_fst ("VL.Strsub",  VL.Strsub);   (* sub not monotone under above list ordering! (exactly 0,1,2 args) *)
      [testsig (module SL) ---> (module VLlist) -$-> (module SLVLlistpair) =: (app_bi_pair ("VL.Strsub",VL.Strsub));
       testsig (module SL) ---> (module VLlist) -~-> (module SLVLlistpair) =: (app_bi_pair ("VL.Strsub",VL.Strsub));];
      apply_builtin_test ("VL.Sqrt",     VL.Sqrt);
      apply_builtin_test ("VL.Type",     VL.Type);
      apply_builtin_test ("VL.Format",   VL.Format);
      (*apply_builtin_test ("VL.Tblconcat",    VL.Tblconcat); *)
      apply_builtin_test_fst ("VL.Tblconcat", VL.Tblconcat);  (* tblconcat not monotone in 2nd arg under above list ordering: add non-num args *)
      [testsig (module SL) ---> (module VLlist) -$-> (module SLVLlistpair) =: (app_bi_pair ("VL.Tblconcat",VL.Tblconcat));
       testsig (module SL) ---> (module VLlist) -~-> (module SLVLlistpair) =: (app_bi_pair ("VL.Tblconcat",VL.Tblconcat))];
      apply_builtin_test ("VL.Getmetatable", VL.Getmetatable); 
      apply_builtin_test ("VL.Setmetatable", VL.Setmetatable); 
      apply_builtin_test ("VL.Rawget", VL.Rawget); 
      apply_builtin_test ("VL.Rawset", VL.Rawset);
 ]

let add_local_tests = (* add_local : SL -> VL -> string -> SL *)
  let sl_add_local = ("SL.add_local",SL.add_local) in
  [ pw_right (module VL) (pw_right (module Str_arg) op_strict)    (module SL) (module SL) =:: sl_add_local;
    pw_right (module VL) (pw_right (module Str_arg) op_monotone)  (module SL) (module SL) =:: sl_add_local;
    pw_right (module VL) (pw_right (module Str_arg) op_invariant) (module SL) (module SL) =:: sl_add_local;
    pw_left  (module SL) (pw_right (module Str_arg) op_strict)    (module VL) (module SL) =:: sl_add_local;
    pw_left  (module SL) (pw_right (module Str_arg) op_monotone)  (module VL) (module SL) =:: sl_add_local;
    pw_left  (module SL) (pw_right (module Str_arg) op_invariant) (module VL) (module SL) =:: sl_add_local; ]
       
let add_local_list_tests = (* add_local_list : SL -> VL list -> string list -> SL *)
  let sl_add_local_list = ("SL.add_local_list",SL.add_local_list) in
  [ pw_right (module VLlist) (pw_right (module Strlist) op_strict)    (module SL) (module SL) =:: sl_add_local_list;
    pw_right (module VLlist) (pw_right (module Strlist) op_monotone)  (module SL) (module SL) =:: sl_add_local_list;
    pw_right (module VLlist) (pw_right (module Strlist) op_invariant) (module SL) (module SL) =:: sl_add_local_list;
 (* pw_left  (module SL) (pw_right (module Strlist) op_strict)    (module VLlist) (module SL) =:: sl_add_local_list;
    pw_left  (module SL) (pw_right (module Strlist) op_monotone)  (module VLlist) (module SL) =:: sl_add_local_list; *)
    pw_left  (module SL) (pw_right (module Strlist) op_invariant) (module VLlist) (module SL) =:: sl_add_local_list; ]
(* Current 'add_local_list' not strict in list arg. Not monotone as adding 'no value' will default to nil *)

let enter_scope_tests = (* enter_scope : SL -> label -> SL *)
  let sl_enter_scope = ("SL.enter_scope",SL.enter_scope) in
  [ pw_right (module Label) op_strict    (module SL) (module SL) =:: sl_enter_scope;
    pw_right (module Label) op_monotone  (module SL) (module SL) =:: sl_enter_scope;
    pw_right (module Label) op_invariant (module SL) (module SL) =:: sl_enter_scope; ]

let build_prop_chain_tests =
  let sl_build_prop_chain = ("SL.build_prop_chain",SL.build_prop_chain) in
  [ testsig (module EL) -$-> (module PL) =: sl_build_prop_chain;
    testsig (module EL) -<-> (module PL) =: sl_build_prop_chain;
    testsig (module EL) -~-> (module PL) =: sl_build_prop_chain; ]

let read_name_tests = (* read_name : SL -> string -> VL *)
  let sl_read_name = ("SL.read_name",SL.read_name) in
  [ pw_right (module Str_arg) op_strict    (module SL) (module VL) =:: sl_read_name;
    pw_right (module Str_arg) op_monotone  (module SL) (module VL) =:: sl_read_name;
    pw_right (module Str_arg) op_invariant (module SL) (module VL) =:: sl_read_name; ]

let write_name_tests = (* write_name : SL -> string -> VL -> SL *)
  let sl_write_name = ("SL.write_name",SL.write_name) in
  [(*pw_right (module Str_arg) (pw_right (module VL) op_strict)    (module SL) (module SL) =:: sl_write_name;*)
     pw_right (module Str_arg) (pw_right (module VL) op_monotone)  (module SL) (module SL) =:: sl_write_name;
     pw_right (module Str_arg) (pw_right (module VL) op_invariant) (module SL) (module SL) =:: sl_write_name;
   (*pw_left (module SL) (pw_left (module Str_arg) op_strict)    (module VL) (module SL) =:: sl_write_name;*)
     pw_left (module SL) (pw_left (module Str_arg) op_monotone)  (module VL) (module SL) =:: sl_write_name;
     pw_left (module SL) (pw_left (module Str_arg) op_invariant) (module VL) (module SL) =:: sl_write_name;  ]
  (* Current 'write_name' not strict in 1st and 3rd args *)
    
let write_dyn_prop_tests =
  let sl_write_dyn_prop = ("SL.write_dyn_prop",SL.write_dyn_prop) in
  [  (* write_dyn_prop not strict in first arg *)
     testsig (module SL) -<-> (module VL) ---> (module VL) ---> (module VL) ---> (module SL) =: sl_write_dyn_prop;
     testsig (module SL) -~-> (module VL) ---> (module VL) ---> (module VL) ---> (module SL) =: sl_write_dyn_prop;
     testsig (module SL) ---> (module VL) -<-> (module VL) ---> (module VL) ---> (module SL) =: sl_write_dyn_prop;
     testsig (module SL) ---> (module VL) -~-> (module VL) ---> (module VL) ---> (module SL) =: sl_write_dyn_prop;
     testsig (module SL) ---> (module VL) ---> (module VL) -<-> (module VL) ---> (module SL) =: sl_write_dyn_prop;
     testsig (module SL) ---> (module VL) ---> (module VL) -~-> (module VL) ---> (module SL) =: sl_write_dyn_prop;
     testsig (module SL) ---> (module VL) ---> (module VL) ---> (module VL) -<-> (module SL) =: sl_write_dyn_prop;
     testsig (module SL) ---> (module VL) ---> (module VL) ---> (module VL) -~-> (module SL) =: sl_write_dyn_prop; ]

let getbinhandler_tests = (* getbinhandler : SL -> VL -> VL -> string -> VL *)
  let sl_getbinhandler = ("SL.getbinhandler",SL.getbinhandler) in
  [ pw_right (module VL) (pw_right (module VL) (pw_right (module Str_arg) op_strict))    (module SL) (module VL) =:: sl_getbinhandler;
    pw_right (module VL) (pw_right (module VL) (pw_right (module Str_arg) op_monotone))  (module SL) (module VL) =:: sl_getbinhandler;
    pw_right (module VL) (pw_right (module VL) (pw_right (module Str_arg) op_invariant)) (module SL) (module VL) =:: sl_getbinhandler;
    pw_left  (module SL) (pw_right (module VL) (pw_right (module Str_arg) op_strict))    (module VL) (module VL) =:: sl_getbinhandler;
    pw_left  (module SL) (pw_right (module VL) (pw_right (module Str_arg) op_monotone))  (module VL) (module VL) =:: sl_getbinhandler;
    pw_left  (module SL) (pw_right (module VL) (pw_right (module Str_arg) op_invariant)) (module VL) (module VL) =:: sl_getbinhandler;
    pw_left  (module SL) (pw_left  (module VL) (pw_right (module Str_arg) op_strict))    (module VL) (module VL) =:: sl_getbinhandler;
    pw_left  (module SL) (pw_left  (module VL) (pw_right (module Str_arg) op_monotone))  (module VL) (module VL) =:: sl_getbinhandler;
    pw_left  (module SL) (pw_left  (module VL) (pw_right (module Str_arg) op_invariant)) (module VL) (module VL) =:: sl_getbinhandler; ]

let spec_state_operations =
  flatten
    [ is_bot_tests;
      apply_builtin_tests;
      add_local_tests;
      add_local_list_tests;
      enter_scope_tests;
      build_prop_chain_tests;
      read_name_tests;
      write_name_tests;
      write_dyn_prop_tests;
      getbinhandler_tests; ]


(** Analysis lattice *)

(* test suite for specific analysis operations *)
let lookup_tests = (* lookup : AL -> label -> SL *)
  let al_lookup = ("AL.lookup",AL.lookup) in
  [ pw_right (module Label) op_strict    (module AL) (module SL) =:: al_lookup;
    pw_right (module Label) op_monotone  (module AL) (module SL) =:: al_lookup;
    pw_right (module Label) op_invariant (module AL) (module SL) =:: al_lookup; ]

let add_tests = (* add : AL -> label -> SL -> AL *)
  let al_add = ("AL.add",AL.add) in
    [ (* add not strict in either arg *)
      pw_right (module Label) (pw_right (module SL) op_monotone)  (module AL) (module AL) =:: al_add;
      pw_right (module Label) (pw_right (module SL) op_invariant) (module AL) (module AL) =:: al_add;
      pw_left (module AL) (pw_left (module Label) op_monotone)  (module SL) (module AL) =:: al_add;
      pw_left (module AL) (pw_left (module Label) op_invariant) (module SL) (module AL) =:: al_add;  ]

let spec_analysis_operations =
  flatten [
    lookup_tests;
    add_tests; ]


(** Test suite code *)

let _ =
  run_tests
    (flatten
       [(* generic lattice tests *)
	GenAbsTests.suite;
	GenNumTests.suite;
	GenStrTests.suite;
	GenValTests.suite;
	GenEnvTests.suite;
	GenPropTests.suite;
	GenStoTests.suite;
	GenStaTests.suite;
	GenAnaTests.suite;
	(* generic lattice top tests *)
	GenAbsTopTests.suite;
	GenNumTopTests.suite;
	GenStrTopTests.suite;
	(* tests of helper lattices *)
	GenBoolTests.suite;
	GenBoolTopTests.suite;
	GenDBoolTests.suite;
	GenDBoolTopTests.suite;
	GenVLVLpairTests.suite;
	GenVLlistTests.suite;
	GenSLVLlistpairTests.suite;
	(* specific lattice operation tests *)
	spec_str_operations; 
	spec_vl_operations;
	spec_env_operations;
	spec_prop_operations;
	spec_store_operations;
	spec_state_operations;
	spec_analysis_operations; ])
