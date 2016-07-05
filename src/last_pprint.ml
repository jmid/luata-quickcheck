(** Pretty printer for labelled ASTs *)

open Format
module L = Last

type info = { mutable curr_line  : int;
	      mutable curr_label : int;
	      mutable line_map   : (int * int) list;
	      fmt : formatter;
	    }

let update_line_map info =
  begin
    info.line_map <- (info.curr_line,info.curr_label)::info.line_map;
    info.curr_line <- info.curr_line + 1;
  end

let pprint_str info s = match s with
  | Ast.Normal ns -> fprintf info.fmt "\"%s\"" (Util.escape_quotes ns)
  | Ast.Char cs   -> fprintf info.fmt "'%s'" (Util.escape_quotes cs)
  | Ast.Long ls   ->
    begin
      String.iter (fun c -> match c with
        | '\n' -> update_line_map info
	| _    -> ()) ls;
      fprintf info.fmt "[[%s]]" ls
    end

let rec pprint_lit info l = match l with
  | L.Nil        -> fprintf info.fmt "nil"
  | L.Bool true  -> fprintf info.fmt "true"
  | L.Bool false -> fprintf info.fmt "false"
  | L.Number n   -> Ast_pprint.pprint_number info.fmt n
  | L.String s   -> pprint_str info s 
  | L.Table (l,un,n)    ->
    let outer_lab = info.curr_label in
    begin
      fprintf info.fmt "%i:{ @[<v 0>" l;
      fprintf info.fmt "@[<h 0>";
      let first =
	List.fold_left
	  (fun first e -> 
	    if first
	    then (pprint_exp info e; false)
	    else
	      begin
		fprintf info.fmt "; ";
		pp_print_space info.fmt (); (*update_line_map info;*)
		pprint_exp info e;
		false
	      end) true un
      in
      fprintf info.fmt "@]";
      ignore(List.fold_left
	       (fun first (s,e) -> 
		 if first
		 then
		   begin
		     fprintf info.fmt "%s = " s;
		     pprint_exp info e;
		     false
		   end
		 else
		   begin
		     fprintf info.fmt ";";
		     pp_print_space info.fmt (); update_line_map info;
		     fprintf info.fmt "%s = " s;
		     pprint_exp info e;
		     false
		   end) first n);
      fprintf info.fmt "@] }";
      info.curr_label <- outer_lab; (* restore outer label *)
    end
  | L.Fun (l,formals,body,el) ->
(*    let outer_lab = info.curr_label in *)
    begin
      fprintf info.fmt "@[<v 0>%i:function (" l;
      pprint_var_list info formals;
      fprintf info.fmt ")";
      pp_print_space info.fmt (); update_line_map info;
      fprintf info.fmt " ";
      pprint_block info body;
      pp_print_space info.fmt (); update_line_map info;
      fprintf info.fmt "%i:end" el; info.curr_label <- el; (* enter new label *)
      fprintf info.fmt "@]";
      (*info.curr_label <- outer_lab; (* restore outer label *) *)
    end

and pprint_var_list info xs =
  begin
    fprintf info.fmt "@[<h 1>";
    ignore(List.fold_left
	     (fun first var -> 
 	        if first
		then (pp_print_string info.fmt var; false)
		else
		  begin
		    fprintf info.fmt ",";
		    pp_print_space info.fmt ();
		    pp_print_string info.fmt var;
		    false
		  end) true xs);
    fprintf info.fmt "@]"
  end

and pprint_lvalue info lv = match lv with
  | L.Name n -> fprintf info.fmt "%s" n
  | L.DynIndex (clabel,e,e') -> 
    begin
      fprintf info.fmt "%i:" clabel;
      pprint_exp info e;
      fprintf info.fmt "[";
      pprint_exp info e';
      fprintf info.fmt "]";
    end

and pprint_lvalue_list info lvs =
  begin
    fprintf info.fmt "@[<h 1>";
    ignore(List.fold_left
	     (fun first lv -> 
	       if first
	       then (pprint_lvalue info lv; false)
	       else
		 begin
	           fprintf info.fmt ",";
		   pp_print_space info.fmt ();
		   pprint_lvalue info lv;
		   false
		 end) true lvs);
    fprintf info.fmt "@]";
  end

and pprint_exp info e = match e with
  | L.Lit l     -> pprint_lit info l
  | L.Lvalue lv -> pprint_lvalue info lv
  | L.Unop (uo,e) ->
    begin
      Ast_pprint.pprint_unop info.fmt uo;
      fprintf info.fmt " ";
      pprint_exp info e;
    end
  | L.Binop (clabel,e,bo,e') ->
    begin
      fprintf info.fmt "%i:(" clabel;
      pprint_exp info e;
      fprintf info.fmt " ";
      Ast_pprint.pprint_binop info.fmt bo;
      fprintf info.fmt " ";
      pprint_exp info e';
      fprintf info.fmt ")";
    end
  | L.And (e,e') ->
    begin
      pprint_exp info e;
      fprintf info.fmt " and ";
      pprint_exp info e';
    end
  | L.Or (e,e') ->
    begin
      pprint_exp info e;
      fprintf info.fmt " or ";
      pprint_exp info e';
    end
  | L.Call (clabel,e,args) ->
    begin
      fprintf info.fmt "%i:" clabel;
      pprint_exp info e;
      fprintf info.fmt "(";
      pprint_exp_list info args;
      fprintf info.fmt ")";
    end
  | L.Methcall (clabel,ilabel,e,mname,args) ->
    begin
      fprintf info.fmt "%i:(%i:" clabel ilabel;
      pprint_exp info e;
      fprintf info.fmt ":%s)(" mname;
      pprint_exp_list info args;
      fprintf info.fmt ")";
    end
  | L.Paren e ->
    begin
      fprintf info.fmt "(";
      pprint_exp info e;
      fprintf info.fmt ")";
    end

and pprint_exp_list info es =
  begin
(*    fprintf info.fmt "@[<h 1>"; *) (* may trigger newline, which screws up javascript line->label map *)
    ignore(List.fold_left
	     (fun first e -> 
	       if first
	       then (pprint_exp info e; false)
	       else
		 begin
	           fprintf info.fmt ", ";
		   pprint_exp info e;
		   false	
		 end) true es);
  end

and pprint_stmt info s = 
  let label = s.L.stmt_pos.L.line_label in
  fprintf info.fmt "%3i: " label;
  info.curr_label <- label; (* enter new label *)
  match s.L.stmt with
  | L.Break ->
      fprintf info.fmt "break";
  | L.If (e,bl,bl') ->
      fprintf info.fmt "@[<v 0>if ";
      pprint_exp info e;
      pp_print_space info.fmt (); update_line_map info;
      fprintf info.fmt "@[<v 1>then";
      info.curr_label <- label; (* restore outer 'if'-label *)
      pp_print_space info.fmt (); update_line_map info;
      pprint_block info bl;
      fprintf info.fmt "@]";
      pp_print_space info.fmt (); update_line_map info;
      fprintf info.fmt "@[<v 1>else";
      info.curr_label <- label; (* restore outer 'if'-label *)
      pp_print_space info.fmt (); update_line_map info;
      pprint_block info bl';
      fprintf info.fmt "@]";
      pp_print_space info.fmt (); update_line_map info;
      fprintf info.fmt "end@]";
      info.curr_label <- label; (* restore outer 'if'-label *)
  | L.WhileDo (e,bl,endlab) ->
      fprintf info.fmt "@[<v 0>while ";
      pprint_exp info e;
      fprintf info.fmt " do";
      pp_print_space info.fmt (); update_line_map info;
      fprintf info.fmt " ";
      pprint_block info bl;
      fprintf info.fmt "@]";
      pp_print_space info.fmt (); update_line_map info;
      fprintf info.fmt "%3i: end" endlab;
      info.curr_label <- endlab; (* set end 'while'-label *)
(*  | L.For (x,e,e',bl) ->
      fprintf info.fmt "@[<v 0>for %s = " x;
      pprint_exp info e;
      fprintf info.fmt ", ";
      pprint_exp info e;
      fprintf info.fmt " do";
      pp_print_space info.fmt (); update_line_map info;
      fprintf info.fmt "   ";
      pprint_block info bl;
      pp_print_space info.fmt (); update_line_map info;
      fprintf info.fmt "end@]";*)
  | L.Doend bl ->
      fprintf info.fmt "@[<v 0>do";
      pp_print_space info.fmt (); update_line_map info;
      fprintf info.fmt " ";
      pprint_block info bl;
      pp_print_space info.fmt (); update_line_map info;
      fprintf info.fmt "end@]";
      info.curr_label <- label; (* restore outer 'do-end'-label *)
  | L.Assign (lvs,es) ->
    begin
      pprint_lvalue_list info lvs;
      fprintf info.fmt " = ";
      pprint_exp_list info es
    end
  | L.Local (xs,es) ->
    begin
      fprintf info.fmt "local ";
      pprint_var_list info xs;
      match es with
	| [] -> ()
	| _  ->
	  begin
	    fprintf info.fmt " = ";
	    pprint_exp_list info es
	  end
    end
  | L.Callstmt (e,args) ->
    begin
      pprint_exp info e;
      fprintf info.fmt "(";
      pprint_exp_list info args;
      fprintf info.fmt ")"
    end
  | L.Methcallstmt (ilabel,e,mname,args) ->
    begin
      fprintf info.fmt "%i:" ilabel;
      pprint_exp info e;
      fprintf info.fmt ":%s(" mname;
      pprint_exp_list info args;
      fprintf info.fmt ")"
    end
  | L.Return es ->
    begin
      fprintf info.fmt "return ";
      pprint_exp_list info es;
    end

(* Prints stmts, with a call to pp_print_space info.fmt in between *)
and pprint_block info bl = match bl with
  | None -> ()
  | Some bl ->
    begin
      fprintf info.fmt "@[<v 0>";
      ignore (List.fold_left (fun first s -> 
	if first
	then (pprint_stmt info s; false)
	else
	  begin 
	    pp_print_space info.fmt (); update_line_map info;
	    pprint_stmt info s; 
	    false
	  end) true bl.L.stmts);
      fprintf info.fmt "@]"
    end

(*  pprint_prog : formatter -> Last.prog -> (int * label) list  *)
let pprint_prog fmt p =
  let info     = { curr_line  = 0; (* invariant: always update when we pp_print_space info.fmt in vertical mode <v *)
		   curr_label = 1;
		   line_map   = [];
		   fmt        = fmt; } in 
  pprint_block info p.L.last;
  pp_print_space info.fmt();
  update_line_map info; (* to add entry for last line *)
  Format.fprintf info.fmt "%3i: " p.L.ret_label;
  info.curr_label <- p.L.ret_label; (* set return label at end *)
  update_line_map info;             (* and add entry for return label *)
  info.line_map
