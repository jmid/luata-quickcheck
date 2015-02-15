(** AST pretty printer *)

open Format
module A = Ast

let pprint_unop fmt uo = match uo with
  | A.Not    -> fprintf fmt "not"
  | A.Length -> fprintf fmt "#"
  | A.Uminus -> fprintf fmt "-"

let pprint_binop fmt bo = match bo with
  | A.Eq     -> fprintf fmt "=="
  | A.Lt     -> fprintf fmt "<"
  | A.Gt     -> fprintf fmt ">"
  | A.Ne     -> fprintf fmt "~="
  | A.Le     -> fprintf fmt "<="
  | A.Ge     -> fprintf fmt ">="
  | A.Plus   -> fprintf fmt "+"
  | A.Minus  -> fprintf fmt "-"
  | A.Times  -> fprintf fmt "*"
  | A.Div    -> fprintf fmt "/"
  | A.Mod    -> fprintf fmt "%%"
  | A.Pow    -> fprintf fmt "^"
  | A.Concat -> fprintf fmt ".."

let pprint_str fmt s = match s with
  | A.Normal ns -> fprintf fmt "\"%s\"" ns
  | A.Char cs   -> fprintf fmt "'%s'" cs
  | A.Long ls   -> fprintf fmt "[[%s]]" ls (*FIXME: level*)

let pprint_number fmt n = match n with
  | A.Int i   -> fprintf fmt "%i" i
  | A.Hex i   -> fprintf fmt "0x%x" i
  | A.Float f -> fprintf fmt "%s" (string_of_float f)

let rec pprint_lit fmt l = match l with
  | A.Nil        -> fprintf fmt "nil"
  | A.Bool true  -> fprintf fmt "true"
  | A.Bool false -> fprintf fmt "false"
  | A.String s   -> pprint_str fmt s
  | A.Number n   -> pprint_number fmt n
  | A.Table (un,n) ->
    begin
      fprintf fmt "{ @[<v 0>";
      let first =
	List.fold_left
	  (fun first e -> 
	    if first
	    then (pprint_exp fmt e; false)
	    else
	      begin
		fprintf fmt "; ";
		(*print_space (); *)
		pprint_exp fmt e;
		false
	      end) true un
      in
      ignore(List.fold_left
	       (fun first (s,e) -> 
		 if first
		 then
		   begin
		     fprintf fmt "%s = " s;
		     pprint_exp fmt e;
		     false
		   end
		 else
		   begin
		     fprintf fmt ";";
		     pp_print_space fmt ();
		     fprintf fmt "%s = " s;
		     pprint_exp fmt e;
		     false
		   end) first n);
      fprintf fmt "@] }";
    end
  | A.Fun (formals,body) ->
    begin
      fprintf fmt "@[<v 0>function (";
      pprint_var_list fmt formals;
      fprintf fmt ")";
      pp_print_space fmt ();
      fprintf fmt "   ";
      pprint_block fmt body;
      pp_print_space fmt ();
      fprintf fmt "end";
      fprintf fmt "@]"
    end

and pprint_var_list fmt xs =
  begin
    fprintf fmt "@[<h 1>";
    ignore(List.fold_left
	     (fun first var -> 
 	        if first
		then (pp_print_string fmt var; false)
		else
		  begin
		    fprintf fmt ",";
		    pp_print_space fmt ();
		    pp_print_string fmt var;
		    false
		  end) true xs);
    fprintf fmt "@]"
  end

and pprint_lvalue fmt lv = match lv with
  | A.Name n -> fprintf fmt "%s" n
  | A.Index (e,f) -> 
    begin
      pprint_exp fmt e;
      fprintf fmt ".%s" f;
    end
  | A.DynIndex (e,e') -> 
    begin
      pprint_exp fmt e;
      fprintf fmt "[";
      pprint_exp fmt e';
      fprintf fmt "]";
    end

and pprint_lvalue_list fmt lvs =
  begin
    fprintf fmt "@[<h 1>";
    ignore(List.fold_left
	     (fun first lv -> 
	       if first
	       then (pprint_lvalue fmt lv; false)
	       else
		 begin
	           fprintf fmt ",";
		   pp_print_space fmt ();
		   pprint_lvalue fmt lv;
		   false	
		 end) true lvs);
    fprintf fmt "@]";
  end

and pprint_exp fmt e = match e with
  | A.Lit l     -> pprint_lit fmt l
  | A.Lvalue lv -> pprint_lvalue fmt lv
  | A.Unop (uo,e) ->
    begin
      pprint_unop fmt uo;
      fprintf fmt " ";
      pprint_exp fmt e;
    end
  | A.Binop (e,bo,e') ->
    begin
      pprint_exp fmt e;
      fprintf fmt " ";
      pprint_binop fmt bo;
      fprintf fmt " ";
      pprint_exp fmt e';
    end
  | A.And (e,e') ->
    begin
      pprint_exp fmt e;
      fprintf fmt " and ";
      pprint_exp fmt e';
    end
  | A.Or (e,e') ->
    begin
      pprint_exp fmt e;
      fprintf fmt " or ";
      pprint_exp fmt e';
    end
  | A.Call (e,args) ->
    begin
      pprint_exp fmt e;
      fprintf fmt "(";
      pprint_exp_list fmt args;
      fprintf fmt ")";
    end
  | A.Methcall (e,mname,args) ->
    begin
      pprint_exp fmt e;
      fprintf fmt ":%s(" mname;
      pprint_exp_list fmt args;
      fprintf fmt ")";
    end
  | A.Paren e ->
    begin
      fprintf fmt "(";
      pprint_exp fmt e;
      fprintf fmt ")";
    end

and pprint_exp_list fmt es =
  begin
    fprintf fmt "@[<h 1>";
    ignore(List.fold_left
	     (fun first e -> 
	       if first
	       then (pprint_exp fmt e; false)
	       else
		 begin
	           fprintf fmt ",";
		   pp_print_space fmt ();
		   pprint_exp fmt e;
		   false	
		 end) true es);
    fprintf fmt "@]";
  end

and pprint_stmt fmt s = match s.A.stmt with
  | A.Break ->
      fprintf fmt "break";
  | A.If (e,bl,bl') ->
      fprintf fmt "@[<v 0>if ";
      pprint_exp fmt e;
      pp_print_space fmt ();
      fprintf fmt "@[<v 3>then";
      pp_print_space fmt ();
      pprint_block fmt bl;
      fprintf fmt "@]";
      pp_print_space fmt ();
      fprintf fmt "@[<v 3>else";
      pp_print_space fmt ();
      pprint_block fmt bl';
      fprintf fmt "@]";
      pp_print_space fmt ();
      fprintf fmt "end@]";
  | A.WhileDo (e,bl) ->
      fprintf fmt "@[<v 0>while ";
      pprint_exp fmt e;
      fprintf fmt " do";
      pp_print_space fmt ();
      fprintf fmt "   ";
      pprint_block fmt bl;
      pp_print_space fmt ();
      fprintf fmt "end@]";
(*  | A.For (x,e,e',bl) ->
      fprintf fmt "@[<v 0>for %s = " x;
      pprint_exp fmt e;
      fprintf fmt ", ";
      pprint_exp fmt e;
      fprintf fmt " do";
      pp_print_space fmt ();
      fprintf fmt "   ";
      pprint_block fmt bl;
      pp_print_space fmt ();
      fprintf fmt "end@]";*)
  | A.Doend bl ->
      fprintf fmt "@[<v 0>do";
      pp_print_space fmt ();
      fprintf fmt "   ";
      pprint_block fmt bl;
      pp_print_space fmt ();
      fprintf fmt "end@]";
  | A.Assign (lvs,es) ->
    begin
      pprint_lvalue_list fmt lvs;
      fprintf fmt " = ";
      pprint_exp_list fmt es
    end
  | A.Local (xs,es) ->
    begin
      fprintf fmt "local ";
      pprint_var_list fmt xs;
      match es with
	| [] -> ()
	| _  ->
	  begin
	    fprintf fmt " = ";
	    pprint_exp_list fmt es
	  end
    end
  | A.Callstmt (e,args) ->
    begin
      pprint_exp fmt e;
      fprintf fmt "(";
      pprint_exp_list fmt args;
      fprintf fmt ")"
    end
  | A.Methcallstmt (e,mname,args) ->
    begin
      pprint_exp fmt e;
      fprintf fmt ":%s(" mname;
      pprint_exp_list fmt args;
      fprintf fmt ")"
    end
  | A.Return es ->
    begin
      fprintf fmt "return ";
      pprint_exp_list fmt es;
    end

(* Prints stmts, with a call to print_space in between *)
and pprint_block fmt bl =
  begin
    fprintf fmt "@[<v 0>";
    ignore (List.fold_left (fun first s -> 
      if first
      then (pprint_stmt fmt s; false)
      else
	begin 
	  pp_print_space fmt ();
	  pprint_stmt fmt s; 
	  false
	end) true bl);
    fprintf fmt "@]"
  end

(*  pprint_prog : formatter -> Ast.chunk -> unit  *)
let pprint_prog fmt p = (*pprint_chunks p*)
    fprintf fmt " @[<v>";
    pprint_block fmt p;
    fprintf fmt "@]";
    pp_print_newline fmt ()
