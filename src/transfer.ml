(** Transfer functions over the analysis lattice *)

(** Following classical compositional abstract interpretation,
    the analysis is implemented as a structural decent over the AST.

    This avoids any risk of divergence, as a monotone analyse will take
    at most |lattice height| traversals over the AST before converging.
 *)

module L   = Last
module Str = Stringlattice
module VL  = Valuelattice
module EL  = Envlattice
module PL  = Proplattice
module ST  = Storelattice
module SL  = Statelattice
module AL  = Analysislattice

(* a record type of commonly passed arguments *)
type info = { fun_map     : L.label -> L.lit;
	      break_label : L.label;
	      ret_label   : L.label }


(** {3 A helper function} *)

(*  join_rlat_lists : VL list -> VL list -> VL list *)
let rec join_rlat_lists vs ws = match vs,ws with
  | [], []      -> []
  | [], ws      -> ws
  | vs, []      -> vs
  | v::vs,w::ws -> (VL.join v w)::(join_rlat_lists vs ws)


(** {2 Monad } *)

(** The following 'return' and 'bind' implements a (syntactic) monad
    which is handy to avoid a lot of let bindings. *)

let return vlat = fun (alat,slat) -> (alat,slat,vlat)
let red_return vlat = 
  fun (alat,slat) -> if VL.is_bot vlat || SL.is_bot slat ||
                        ST.is_bot slat.SL.store || EL.is_bot slat.SL.env
                     then (alat,SL.bot,VL.bot) else (alat,slat,vlat)
let list_return vlat = fun (alat,slat) -> (alat,slat,[vlat])
let red_list_return vlats = 
  fun (alat,slat) -> if vlats = [] || vlats = [VL.bot] ||
		       SL.is_bot slat || ST.is_bot slat.SL.store || EL.is_bot slat.SL.env
                     then (alat,SL.bot,[VL.bot]) else (alat,slat,vlats)

let bind v f = fun (alat,slat) -> match v (alat,slat) with
  | (alat',slat',vlat) -> f vlat (alat',slat')
let ( >>= ) = bind
(*let ( >> ) m f = bind m (fun _ -> f ())*)


(** {3 Monadic operations } *)

let adjust_to_single vlats = match vlats with
                               | []      -> return VL.nil
			       | vlat::_ -> return vlat

let merror     = fun (alat,slat) -> (alat,SL.bot,VL.bot)
(*let list_error = fun (alat,slat) -> (alat,SL.bot,[VL.bot])*)

let curr_scope_chain        = fun (alat,slat) -> (alat,slat,slat.SL.env)
let restore_scope_chain env = fun (alat,slat) -> (alat,{slat with SL.env = env},())

let get_curr_state     = fun (alat,slat) -> (alat,slat,slat)
let restore_state slat = fun (alat,_)    -> (alat,slat,())

let lookup_state label = fun (alat,slat) -> (alat,slat,AL.lookup alat label)
let set_state label    = (lookup_state label >>= fun slat -> restore_state slat)
let record_state label =
  fun (alat,slat) -> (AL.add alat label (SL.join slat (AL.lookup alat label)), slat, ())

let mfailwith msg   = fun (_alat,_slat) -> failwith msg

let enter_scope label   = fun (alat,slat) -> (alat, SL.enter_scope slat label, ()) 
let add_locals xs vlats = fun (alat,slat) -> (alat, SL.add_local_list slat vlats xs, ()) 


(** {4 lvalue read/write operations } *)

let read_name x           = get_curr_state >>= fun slat -> red_return (SL.read_name slat x)
(*let lookup_prop v x       = get_curr_state >>= fun slat -> red_return (ST.lookup_prop slat.SL.store v x)*)
(*let lookup_dyn_prop v0 v1 = get_curr_state >>= fun slat -> red_return (ST.lookup_dyn_prop slat.SL.store v0 v1)*)

let lookup_event tabv strev = get_curr_state >>= fun slat -> return (ST.lookup_event slat.SL.store tabv strev)
let raw_get tabv idxv       = get_curr_state >>= fun slat -> return (ST.raw_get slat.SL.store tabv idxv)

let write_name n vlat = get_curr_state >>= fun slat ->
                        let slat' = SL.write_name slat n vlat in
 		        restore_state slat'
(*let write_prop vlat x vlat' = get_curr_state >>= fun slat ->
                              let slat' = SL.write_prop slat vlat x vlat' in
			       restore_state slat'*)
let write_dyn_prop vlat0 vlat1 vlat = get_curr_state >>= fun slat ->
                                      let slat' = SL.write_dyn_prop slat vlat0 vlat1 vlat in
				       restore_state slat'

let alloc_closure l prop_chain =
  fun (alat,slat) -> (alat,{slat with SL.store = ST.add_label slat.SL.store l prop_chain},())

let alloc_resstore ret_label vlats = (* Hack: write 'ret_label.ret_label.res1' in heap *)
  fun (alat,slat) ->
    if SL.is_bot slat then (alat,SL.bot,ST.bot)
    else
      let vlats = match vlats with
	           | [] -> [VL.nil] (* if return is empty, return nil value *)
		   | _  -> vlats in
      let _,res_table = List.fold_left
	(fun (i,tbl) vlat -> let entry = "res" ^ (string_of_int i) in
			     (i+1,PL.add entry vlat tbl)) (1,PL.bot) vlats in
      (alat,slat,ST.add_label slat.SL.store ret_label res_table)

let getbinhandler op1 op2 event =
  fun (alat,slat) -> (alat,slat,SL.getbinhandler slat op1 op2 event)

(** {4 control flow operations } *)

let and_join slat0 vlat0 slat1 vlat1 =
  (fun (alat,slat) -> (* potentially false if overapprox. has nil *)
     let slat,vlat   = if VL.may_be_nil vlat0 then (slat0,vlat0) else (SL.bot,VL.bot) in
     let slat',vlat' = if VL.may_be_non_nil vlat0 then (slat1,vlat1) else (SL.bot,VL.bot) in
     (alat, SL.join slat slat', VL.join vlat vlat')) (* potentially true if overapprox. has non nil *)
     (* potentially reduce *)

let or_join slat0 vlat0 slat1 vlat1 =
  (fun (alat,slat) -> (* potentially true if overapprox. has non nil *)
     let slat,vlat   = if VL.may_be_non_nil vlat0 then (slat0,VL.exclude_nil vlat0) else (SL.bot,VL.bot) in
     let slat',vlat' = if VL.may_be_nil vlat0 then (slat1,vlat1) else (SL.bot,VL.bot) in
     (alat, SL.join slat slat', VL.join vlat vlat')) (* potentially false if overapprox. has nil *)
     (* potentially reduce *)

let mvl_join branch1 branch2 = (fun (alat,slat) ->
  let (alat1,slat1,vlat1) = branch1 (alat,slat) in
  let (alat2,slat2,vlat2) = branch2 (alat,slat) in
  (AL.join alat1 alat2, SL.join slat1 slat2, VL.join vlat1 vlat2))

let branch_and_join env thenbr elsebr =
  (fun lat -> let (alat1,slat1,()),(alat2,slat2,()) = thenbr lat, elsebr lat in
	      let joined_store                      = ST.join slat1.SL.store slat2.SL.store in
	      (AL.join alat1 alat2, (if ST.leq joined_store ST.bot then SL.bot
	                             else {SL.env = env; SL.store = joined_store}), ()))

let rec loop env test body =
  (fun (alat_prev,slat_prev) ->
    let (alat',slat',_vlat') = test (alat_prev,slat_prev) in
    let (alat'',slat'',())   = body (alat',slat') in
    if AL.leq alat'' alat' && ST.leq slat''.SL.store slat'.SL.store  (* Fixpoint upto scoping of local variables *)
    then (alat',slat',())
    else loop (* Otherwise:iterate *)
      env test body ((*AL.join alat'*) alat'', {SL.env = env; SL.store = ST.join slat'.SL.store slat''.SL.store}))


(** {3 Transfer functions } *)

(*  transfer_call : label -> label -> VL list -> info -> (AL * SL) -> (AL * SL * VL list) *)
let transfer_call clab tgtlabel vlats info (alat,slat) =
  let f = info.fun_map tgtlabel in
  match f with
    | L.Fun (label',xs,bl,ret_label) ->
      if (tgtlabel != label')
      then failwith ("Mismatch in fun_map between labels "
	               ^ (string_of_int tgtlabel) ^ " and " ^ (string_of_int label'))
      else (match bl with
	| None    -> failwith "encountered bodyless function"
	| Some bl ->
	  let body_label   = bl.L.label in
	  let entry_state  = AL.lookup alat body_label in
(*
 Caller invariant:    - pass arguments and clabel
                      - receive results through ret_label entry

 Receiver invariant:  - pair formals and actuals and store in clabel,
                      - add an extended scope chain,
                      - store result(s) in AL[ret_label][ret_label][res1]

 Needs two allocation sites
  - caller-entry - for scope-chain allocation
  - caller-exit  - for ?

 propagate call:    - vlats into localenv of function label
                    - slat.store into store of function label
 propagate return:  - vlat results from return label to caller
                    - slat.store from return label into store of caller
 *)
(* caller: clab    callee: tgtlabel body_label ret_label *)

	  (* extend stored env with new level *)
	  let ext_env      = ST.fold_labels_scopechain
	                       (fun is accpairset -> EL.PairSet.add (clab,is) accpairset)
  			         slat.SL.store tgtlabel EL.PairSet.empty in
	  if EL.is_bot ext_env (* scopechain potentially deleted because of domain reduction *)
	  then (alat,slat,[VL.bot])
	  else
	  (* add bindings from formals to actuals *)
	  (* join store and environment into receiver's state *)
	  let entry_state' = SL.join entry_state
			       { SL.store   =
				   ST.add_label slat.SL.store clab (PL.add_all_params xs vlats PL.bot);
				 SL.env     = ext_env } in
	  let alat'        = AL.add alat body_label entry_state' in

	  (* lookup exit state and results *)
	  let exit_state   = AL.lookup alat' ret_label in
	  (* if body reachable but exit is bot,
               either func completes abnormally
	       or: flow has not propagated through function yet *)
	  if not (SL.is_bot entry_state) && SL.is_bot exit_state 
	  then (alat',SL.bot,[VL.bot]) (* unreachable: no return state injected *)
	  else
	  let exit_prop    = ST.find_label exit_state.SL.store ret_label in

	  let rec build_res_lat i = (try
				       let entry = "res" ^ (string_of_int i) in
				       let vlat  = PL.find_exn entry exit_prop in
				       let vlats = build_res_lat (i+1) in
				       vlat::vlats
	                             with Not_found ->
				       if i=1
				       then [VL.bot] (* no result yet: head is bot *)
				       else [] ) in  (* Hack: read 'result' local *)

	  let res_lats      = build_res_lat 1 in
	  (* Note: L.Return is responsible for propagating to ret_label
	           Here we only read off the propagated input  *)

	  (* return store/heap effects to caller's state (joined by transfer_calls) *)
	  (* and restore environment*)
	  let slat'        = (*SL.join slat*)
			     { SL.store   = exit_state.SL.store;
			       SL.env     = slat.SL.env (* EL.bot;*) } in
	  (alat',slat',res_lats)
      )
    | _ ->
      failwith "Got non-function literal from function map"

(*  transfer_calls : label -> VL -> VL list -> info -> (AL * SL) -> (AL * SL * VL list) *)
let transfer_calls clab vlat vlats info =
  if not (VL.may_be_proc vlat)  (* type error: receiver is definitely not a function *)
  then restore_state SL.bot >>= fun () -> list_return VL.bot
  else
    (fun (alat,slat) ->
      let funs = vlat.VL.funs in
	VL.ProcSet.fold 
	  (fun proc (alatacc,slatacc,rlatacc) -> match proc with
	    | VL.Funtag f   -> 
	      let (alatacc',slat',rlats) = transfer_call clab f vlats info (alatacc,slat) in
	      (alatacc',SL.join slatacc slat',join_rlat_lists rlatacc rlats)
	    | VL.Builtin bi -> 
	      let slat',rlats = SL.apply_builtin bi slat vlats in
	      (alatacc,SL.join slatacc slat',join_rlat_lists rlatacc rlats))
	  funs (alat, { SL.store = ST.bot;
			SL.env   = EL.bot } ,[]))
     >>= red_list_return (* slight hack to reduce to bot on callee error *)

(*  transfer_str : str -> info -> (AL * SL) -> (AL * SL * VL) *)
let rec transfer_str s info =
  match s with
    | Ast.Normal ns -> red_return (VL.string ns)
    | Ast.Char cs   -> red_return (VL.string cs)
    | Ast.Long ls   -> red_return (VL.string ls)

(*  transfer_lit : lit -> info -> (AL * SL) -> (AL * SL * VL) *)
let rec transfer_lit l info =
  match l with
  | L.Nil              -> red_return VL.nil
  | L.Bool _           -> red_return VL.bool
  | L.String s         -> transfer_str s info
  | L.Number _         -> red_return VL.number
  | L.Table (l,uns,ns) -> 
    (* add unamed entries *)
    transfer_exp_list uns info >>= fun unvlats ->
     let joined_unsvlats = List.fold_left (fun acc vlat -> VL.join acc vlat) VL.bot unvlats in
     let plat            = PL.add_default VL.number joined_unsvlats PL.bot in(*unnamed/numeric all go in default*)
     (* add named entries *)
     let (props, exps)   = List.split ns in
      transfer_exp_list exps info >>= fun vlats ->
       let plat'  = List.fold_left2 (fun plat prop vlat -> PL.add prop vlat plat) plat props vlats in
        get_curr_state >>= fun slat'' ->
         restore_state { slat'' with SL.store = ST.add_label slat''.SL.store l plat' } >>= fun () ->
          return (VL.table l)
  | L.Fun (l,_,body,ret_label) -> 
    (match body with
      | None      -> mfailwith "encountered bodyless function"
      | Some body ->
	get_curr_state >>= fun slat ->
	 curr_scope_chain >>= fun scopechain ->
          let prop_chain = SL.build_prop_chain scopechain in
	  set_state body.L.label >>= fun () ->
 	   let fun_info   = { info with ret_label = ret_label } in
	   transfer_stmts body.L.stmts fun_info >>= fun () ->
	    restore_state slat >>= fun () ->
   	     (* allocate abstract closure for l: record scope chain *)
	     alloc_closure l prop_chain >>= fun () ->
	      return (VL.proc l))
    (* invariant: any proc l, has had its scopechain allocated in store *)

(*  transfer_gettable_event : label -> info -> VL -> VL -> (AL * SL) -> AL * SL * VL *)
and transfer_gettable_event clab info tabvlat idxvlat = 
  (*  inner_gettable_event : VL -> VL -> (AL * SL) -> AL * SL * VL *)
  let rec inner_gettable_event tabvlat acc =
    if VL.is_bot tabvlat (* strict *)
    then return VL.bot
    else
      get_curr_state >>= fun slat ->
       raw_get tabvlat idxvlat >>= fun v ->
        lookup_event tabvlat "__index" >>= fun h ->
         mvl_join
	   (if VL.may_be_table tabvlat
	    then return 
	       (VL.join
		  (VL.exclude_nil v)
		  (if VL.may_be_nil v && VL.may_be_nil h then VL.nil else VL.bot))
	    else merror)
	   (mvl_join
	      (if VL.may_be_proc h
	       then (transfer_calls clab h [tabvlat;idxvlat] info >>= (*-- call the handler *)
		       adjust_to_single)
	       else merror)
	      (let h' = VL.exclude_proc h in
	       if VL.leq h' acc
	       then merror (* already explored base values *)
	       else inner_gettable_event h' (VL.join h' acc)))        (*-- or repeat operation on it *)
  in
  inner_gettable_event tabvlat VL.bot (* empty accumulator of explored 'base' values *)

(*  transfer_lvalue_read : lvalue -> info -> (AL * SL) -> (AL * SL * VL) *)
and transfer_lvalue_read lv info =
  match lv with
  | L.Name x ->
    read_name x
(*  | L.Index (e,x) ->
    transfer_exp e info >>= adjust_to_single >>= fun vlat ->
     lookup_prop vlat x *)
  | L.DynIndex (clab,e0,e1) ->
    transfer_exp e0 info >>= adjust_to_single >>= fun vlat0 ->
     transfer_exp e1 info >>= adjust_to_single >>= fun vlat1 ->
      transfer_gettable_event clab info vlat0 vlat1
      (*lookup_dyn_prop vlat0 vlat1*)

(*  transfer_lvalue_write_list : lvalue list -> exp list -> info -> (AL * SL) -> (AL * SL * unit) *)
and transfer_lvalue_write_list lvals exps info =
  (* Three passes: 1. evaluate lvalues down through recursion
                   2. at end, evaluate rhs, left-to-right
                   3. perform assignment back through the recursion *)

  (*  adjust_length : int -> VL list -> VL list *)
  let rec adjust_length lvallen vlats = match (lvallen,vlats) with
    | (0,[])    -> []
    | (0, _)    -> []     (* too many values: drop the rest *)
    | (n,[])    -> VL.nil::(adjust_length (n-1) []) (* too few values: add nils *)
    | (n,vlat::vlats) -> vlat::(adjust_length (n-1) vlats)
  in
  (*  inner_write_list : lvalue list -> exp list -> info -> (AL * SL) -> (AL * SL * VL list) *)
  let rec inner_write_list lvs exps info = match lvs with
    | [] ->
      transfer_exp_list exps info >>= fun vlats ->  (* step 2: evaluate rhs *)
       let lvallen = List.length lvals in
       (* too few values: add nils, too many values: drop some *)
       return (List.rev (adjust_length lvallen vlats))
    | lv::lvs ->
      (match lv with
	| Last.Name n ->
	  inner_write_list lvs exps info >>= fun vlats ->
 	   (match vlats with
	     | [] ->	      mfailwith ("missing rhs in name assignment: " ^ n)
	     | vlat::vlats -> write_name n vlat >>= fun () -> return vlats)
(*	| Last.Index (e,x) ->
	  transfer_exp e info >>= adjust_to_single >>= fun vlat ->
 	   inner_write_list lvs exps info >>= fun vlats ->
	    (match vlats with
	      | [] ->		mfailwith "missing rhs in indexed assignment"
	      | vlat'::vlats -> write_prop vlat x vlat' >>= fun () -> return vlats) *)
	| Last.DynIndex (clab,e0,e1) ->
	  transfer_exp e0 info >>= adjust_to_single >>= fun vlat0 ->
	   red_return (VL.only_tables vlat0) >>= fun vlat0 -> (* slight hack to reduce to bot on index error *)
  	    transfer_exp e1 info >>= adjust_to_single >>= fun vlat1 ->
	     inner_write_list lvs exps info >>= fun vlats ->
	      (match vlats with
		| [] ->		mfailwith "missing rhs in dyn. indexed assignment"
		| vlat::vlats -> write_dyn_prop vlat0 vlat1 vlat >>= fun () -> return vlats))
  in
  inner_write_list lvals exps info >>= fun vlats -> (* step 1: evaluate lvalues *)
   match vlats with
     | []   -> return ()
     | _::_ -> mfailwith "additional rhs in assignment"

(*  transfer_arith_event : label -> info -> Ast.binop -> string -> VL -> VL -> (AL * SL) -> AL * SL * VL *)
and transfer_arith_event clab info op event op1 op2 =
  let o1,o2 = VL.coerce_tonum op1, VL.coerce_tonum op2 in
  mvl_join
    (if VL.may_be_number (VL.meet o1 o2)  (* -- both operands are numeric? *)
     then red_return (VL.binop op o1 o2)  (* -- '+' here is the primitive 'add' *)
     else merror)
    (getbinhandler op1 op2 event >>= fun h ->
     if VL.may_be_proc h
     then (* -- call the handler with both operands *)
       transfer_calls clab h [op1;op2] info >>= adjust_to_single
     else (* -- no handler available: default behavior *)
       merror)

(*  transfer_binop : op -> label -> info -> VL -> VL -> (AL * SL) -> (AL * SL * VL) *)
and transfer_binop op clab info vlat0 vlat1 = match op with
  | Ast.Plus  -> transfer_arith_event clab info op "__add" vlat0 vlat1
  | Ast.Minus -> transfer_arith_event clab info op "__sub" vlat0 vlat1
  | Ast.Times -> transfer_arith_event clab info op "__mul" vlat0 vlat1
  | Ast.Div   -> transfer_arith_event clab info op "__div" vlat0 vlat1
  | Ast.Mod   -> transfer_arith_event clab info op "__mod" vlat0 vlat1
  | Ast.Pow   -> transfer_arith_event clab info op "__pow" vlat0 vlat1
  | _        -> red_return (VL.binop op vlat0 vlat1) (* potentially reduce statelattice to bot *)

(*  transfer_exp : exp -> info -> (AL * SL) -> (AL * SL * VL list) *)
and transfer_exp e info = match e with
  | L.Lit l ->
    transfer_lit l info >>=
     list_return
  | L.Lvalue lv ->
    transfer_lvalue_read lv info >>=
     list_return
  | L.Unop (op,e) ->
    transfer_exp e info >>= adjust_to_single >>= fun vlat ->
     red_return (VL.unop op vlat) >>=         (* potentially reduce statelattice to bot *)
      list_return
  | L.Binop (clab,e0,op,e1) ->
    transfer_exp e0 info >>= adjust_to_single >>= fun vlat0 ->  (* left-to-right eval *)
     transfer_exp e1 info >>= adjust_to_single >>= fun vlat1 ->
      transfer_binop op clab info vlat0 vlat1 >>=
       list_return                            (* potentially reduce statelattice to bot *)
  | L.And (e0,e1) ->
    transfer_exp e0 info >>= adjust_to_single >>= fun vlat0 ->  (* left-to-right eval *)
     get_curr_state >>= fun slat0 ->
      transfer_exp e1 info >>= adjust_to_single >>= fun vlat1 ->
       get_curr_state >>= fun slat1 ->
        and_join slat0 vlat0 slat1 vlat1 >>=
	 list_return
  | L.Or (e0,e1) ->
    transfer_exp e0 info >>= adjust_to_single >>= fun vlat0 ->  (* left-to-right eval *)
     get_curr_state >>= fun slat0 ->
      transfer_exp e1 info >>= adjust_to_single >>= fun vlat1 ->
       get_curr_state >>= fun slat1 ->
        or_join slat0 vlat0 slat1 vlat1 >>=
	 list_return
  | L.Call (clab,e,es) -> 
    transfer_exp e info >>= adjust_to_single >>= fun vlat -> (* left-to-right eval *)
     transfer_exp_list es info >>= fun vlats ->
      transfer_calls clab vlat vlats info
  | L.Methcall (clab,ilab,e,mname,es) -> 
    transfer_exp e info >>= adjust_to_single >>= fun vlat -> (* left-to-right eval *)
     transfer_gettable_event ilab info vlat (VL.string mname) >>= fun recv ->
      transfer_exp_list es info >>= fun vlats ->
       transfer_calls clab recv (vlat::vlats) info (* pass self (vlat) as first arg *)
  | L.Paren e -> 
    transfer_exp e info >>= adjust_to_single >>= list_return

(*  transfer_exp_list : exp list -> info -> (AL * SL) -> (AL * SL * VL list) *)
and transfer_exp_list es info =
  match es with
  | []    -> return []
  | [e]   -> transfer_exp e info
  | e::es ->
    transfer_exp e info >>= adjust_to_single >>= fun vlat ->
    transfer_exp_list es info >>= fun vlats ->
    return (vlat::vlats)

(*  transfer_stmts : stmts -> info -> (AL * SL) -> (AL * SL * unit) *)
and transfer_stmts ss info =
  match ss with
  | [] ->
    return () (* empty sequence does not necessarily mean 'Return'
		       -- only when falling through a procedure
		       patch: add an explicit 'Return nil' to all procedures in parser (when absent) *)
  | s::ss ->
    let lab = s.L.stmt_pos.L.line_label in
    record_state lab >>= fun () -> (* join into prev.elm. *)
    set_state lab >>= fun () ->    (* and read off joined result *)
    (match s.L.stmt with
    | L.Break ->
      record_state info.break_label >>= fun () -> (*join state into outer break label *)
       restore_state SL.bot >>= fun () ->  (* i.e., unreachable *)
        return ()
    | L.If (e,bl1,bl2) ->
      curr_scope_chain >>= fun env ->
       transfer_exp e info >>= adjust_to_single >>= fun _vlat ->
        let thenbranch = transfer_block bl1 info in
        let elsebranch = transfer_block bl2 info in
	branch_and_join env thenbranch elsebranch >>= fun () ->
 	 transfer_stmts ss info
    | L.WhileDo (e,bl,endlab) ->
      let loop_info = { info with break_label = endlab } in
      curr_scope_chain >>= fun env ->
       let test = transfer_exp e info >>= adjust_to_single in
       let body = transfer_block bl loop_info in
       loop env test body >>= fun () ->
        restore_scope_chain env >>= fun () ->
         (* joined breaked state with post condition *)
         record_state endlab >>= fun () ->
          set_state endlab >>= fun () ->
           transfer_stmts ss info
    | L.Doend bl ->
      curr_scope_chain >>= fun env ->
       transfer_block bl info >>= fun () ->
        restore_scope_chain env >>= fun () ->
         transfer_stmts ss info
    | L.Assign (lvs,es) ->
      transfer_lvalue_write_list lvs es info >>= fun () ->
       transfer_stmts ss info
    | L.Local (xs,es) ->
      transfer_exp_list es info >>= fun vlats -> (* left-to-right eval *)
       enter_scope lab >>= fun () ->
        add_locals xs vlats >>= fun () ->
         transfer_stmts ss info
    | L.Callstmt (e,es) ->
      transfer_exp e info >>= adjust_to_single >>= fun vlat -> (* left-to-right eval *)
       transfer_exp_list es info >>= fun vlats ->
        transfer_calls lab vlat vlats info >>= fun _vlats -> (* stmt call, so ignore results *)
         transfer_stmts ss info
    | L.Methcallstmt (ilab,e,mname,es) ->
      transfer_exp e info >>= adjust_to_single >>= fun vlat -> (* left-to-right eval *)
       transfer_gettable_event ilab info vlat (VL.string mname) >>= fun recv ->
        transfer_exp_list es info >>= fun vlats ->
         transfer_calls lab recv (vlat::vlats) info >>= fun _vlats -> (* pass self (vlat) as first arg *)
          transfer_stmts ss info
    | L.Return es ->
      let ret_label  = info.ret_label in
      transfer_exp_list es info >>= fun vlats ->
       alloc_resstore ret_label vlats >>= fun ret_store ->
        restore_state { SL.bot with SL.store = ret_store } >>= fun () ->
 	 record_state ret_label >>= fun () ->
	  restore_state SL.bot (* i.e., unreachable *)
    )
(* Since return is a control-delimiter,
   neither a semantics nor an analysis of a statement list
   should be a simple fold *)

(*  transfer_block : block -> info -> (AL * SL) -> (AL * SL * unit)  *)
and transfer_block bl info = match bl with
  | None    -> return ()
  | Some bl -> transfer_stmts bl.L.stmts info

(*  transfer_prog : Last.prog -> AL  *)
let transfer_prog p =
  let init = (AL.init, SL.init) in
  let info = { fun_map     = p.L.fun_map;
	       break_label = p.L.ret_label;
	       ret_label   = p.L.ret_label } in
  let stmts = match p.L.last with
    | None    -> []
    | Some bl -> bl.L.stmts in
  let rec loop prev =
    let (alat,slat,()) = transfer_stmts stmts info prev in
    if AL.leq alat (fst prev)
    then (alat,slat)
    else loop (alat, SL.init)
  in
  let (alat,slat) = loop init in
  let alat = AL.add alat p.L.ret_label slat in (* add extra end-of-program label to lattice *)
  alat
