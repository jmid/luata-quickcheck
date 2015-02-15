(** A translation from unlabeled AST to labeled AST.  *)

(* Requirements:

   Lexing.position/ASTnode --> line_label  (for pprinting analysis result)

   fun_label   --> Fun literal          (for CFA)
   table_label --> Table literal        (for heap analysis)

   + enclosing function label (option)
*)

module A = Ast
module L = Last

let reset,make_label,make_res_label =
  let count     = ref 1 in
  let res_count = ref 0 in
  (fun () -> count := 1),
  (fun () -> let label = !count in
	     begin 
	       incr count;
	       label
	     end),
  (fun () -> let label = !res_count in
	     begin 
	       decr res_count;
	       label
	     end)

type info = 
  { mutable enclosing_fun : L.label option;
    mutable inside_loop   : bool;
    mutable pos_map       : (L.label * L.stmt) list; (* stmt or stmt list? *)
    mutable fun_map       : (L.label * L.lit) list;
    mutable table_map     : (L.label * L.lit) list; }

let rec label_lit info l = match l with
  | A.Nil      -> L.Nil
  | A.Bool b   -> L.Bool b
  | A.Number n -> L.Number n
  | A.String s -> L.String s
  | A.Table (unnamed,named) -> 
    let lab      = make_label () in
    let unnamed' = List.map (fun e -> label_exp info e) unnamed in
    let named'   = List.map (fun (s,e) -> (s,label_exp info e)) named in
    let tab      = L.Table (lab,unnamed',named') in
    info.table_map <- (lab,tab)::info.table_map; (*add table*)
    tab
    
  | A.Fun (xs,bl) ->
    let old_lab  = info.enclosing_fun in
    let old_flag = info.inside_loop in
    let lab      = make_label () in
    let ()       = info.enclosing_fun <- Some lab in (*new enclosing*)
    let ()       = info.inside_loop   <- false in    (*inside function, not loop*)
    let bl'      = label_block info bl in
    let ()       = info.enclosing_fun <- old_lab in  (*reset label*)
    let ()       = info.inside_loop   <- old_flag in (*reset flag*)
    let endlab   = make_label () in
    let f        = L.Fun (lab,xs,bl',endlab) in
    info.fun_map <- (lab,f)::info.fun_map; (*add function*)
    f

and label_lvalue info lv = match lv with
  | A.Name n -> L.Name n
  | A.Index (e,f) -> 
    let clab = make_label () in
    let e' = label_exp info e in
    (* L.Index (e',f) *)
    let e'' = label_exp info (A.Lit (A.String (A.Normal f))) in
    L.DynIndex(clab,e',e'')
  | A.DynIndex (e0,e1) ->
    let clab = make_label () in
    let e0' = label_exp info e0 in
    let e1' = label_exp info e1 in
    L.DynIndex (clab,e0',e1')

and label_exp info e = match e with
  | A.Lit l     -> L.Lit (label_lit info l)
  | A.Lvalue lv -> L.Lvalue (label_lvalue info lv)
  | A.Unop (uo,e) ->
    let e' = label_exp info e in
    L.Unop (uo,e')
  | A.Binop (e0,bo,e1) ->
    let clab = make_label () in
    let e0'  = label_exp info e0 in
    let e1'  = label_exp info e1 in
    L.Binop (clab,e0',bo,e1')
  | A.And (e0,e1) ->
    let e0' = label_exp info e0 in
    let e1' = label_exp info e1 in
    L.And (e0',e1')
  | A.Or (e0,e1) ->
    let e0' = label_exp info e0 in
    let e1' = label_exp info e1 in
    L.Or (e0',e1')
  | A.Call (e,es) ->
    let clab = make_label () in
    let e'   = label_exp info e in
    let es'  = List.map (label_exp info) es in
    L.Call (clab,e',es')
  | A.Methcall (e,mname,es) ->
    let clab = make_label () in
    let ilab = make_label () in
    let e'   = label_exp info e in
    let es'  = List.map (label_exp info) es in
    L.Methcall (clab,ilab,e',mname,es')
  | A.Paren e0 ->
    let e0' = label_exp info e0 in
    L.Paren e0'

and label_stmt info s =
  let line_lab = make_label () in
  let pos      = { L.lex_pos = s.A.stmt_pos;
		   L.func = info.enclosing_fun;
		   L.line_label = line_lab; } in
  let mkstmt s =
    let stm = { L.stmt_pos = pos; L.stmt = s } in
    info.pos_map <- (line_lab,stm)::info.pos_map; (* add stm *)
    stm in
  match s.A.stmt with
  | A.Break ->
    if info.inside_loop
    then mkstmt L.Break
    else Error.error "Structural error" "break not inside loop" s.A.stmt_pos
  | A.If (e,bl1,bl2) ->
    let e'   = label_exp info e in
    let bl1' = label_block info bl1 in
    let bl2' = label_block info bl2 in
    mkstmt (L.If (e',bl1',bl2'))
  | A.WhileDo (e,bl) ->
    let elab = make_label () in
    let flag = info.inside_loop in
    let e'   = label_exp info e in
    info.inside_loop <- true; (* entering loop *)
    let bl'  = label_block info bl in
    let ()   = info.inside_loop <- flag in (*;*) (* reset loop flag *)
    let stm = mkstmt (L.WhileDo (e',bl',elab)) in
    let () = info.pos_map <- (elab,stm)::info.pos_map in(*;*) (* add end-label to while-stm binding *)
    stm
(*  | A.For (x,e0,e1,bl) ->
    let e0'  = label_exp info e0 in
    let e1'  = label_exp info e1 in
    let bl'  = label_block info bl in
    mkstmt (L.For (x,e0',e1',bl')) *)
  | A.Doend bl ->
    let bl'  = label_block info bl in
    mkstmt (L.Doend bl')
  | A.Assign (lvs,es) ->
    let lvs' = List.map (label_lvalue info) lvs in
    let es'  = List.map (label_exp info) es in
    mkstmt (L.Assign (lvs',es'))
  | A.Local (xs,es) ->
    let es'  = List.map (label_exp info) es in
    mkstmt (L.Local (xs,es'))
  | A.Callstmt (e,es) ->
    let e'   = label_exp info e in
    let es'  = List.map (label_exp info) es in
    mkstmt (L.Callstmt (e',es'))
  | A.Methcallstmt (e,mname,es) ->
    let ilab = make_label () in
    let e'   = label_exp info e in
    let es'  = List.map (label_exp info) es in
    mkstmt (L.Methcallstmt (ilab,e',mname,es'))
  | A.Return es ->
    let es'  = List.map (label_exp info) es in
    mkstmt (L.Return es')

and label_block info bl = 
  let stmts = List.map (label_stmt info) bl in
  match stmts with
    | []   -> None
    | s::_ -> 
      let lab = s.L.stmt_pos.L.line_label in
      Some { L.label = lab;
	     L.stmts = stmts; }

(*  label_prog : string option -> Ast.block -> Last.last  *)
let label_prog fname_opt p =
  let info = { enclosing_fun = None;
	       inside_loop = false;
	       pos_map = [];
	       fun_map = [];
	       table_map = []; } in
  let p' = label_block info p in
  let retlab = make_label () in
  let pos_map (*l*) = (*List.assoc l*) info.pos_map in
  let fun_map l = List.assoc l info.fun_map in
  let table_map l = List.assoc l info.table_map in
  { L.name      = fname_opt;
    L.last      = p';
    L.ret_label = retlab;
    L.pos_map   = pos_map;
    L.fun_map   = fun_map;
    L.table_map = table_map }
