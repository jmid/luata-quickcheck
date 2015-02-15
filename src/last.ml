(** Labelled AST for Lua *)

type pos = {
  lex_pos    : Lexing.position;
  func       : int option;
  line_label : int;
}

type label = int

type lit =
 | Nil
 | Bool   of bool
 | String of Ast.str
 | Number of Ast.number (*float*)
 | Table  of label(*NEW*) * exp list * (string * exp) list
 | Fun    of label(*NEW*) * string list * block * label(*NEW*) (*fixme: pos*)

and lvalue =
 | Name of string
(* | Index of exp * string *)
 | DynIndex of label(*NEW*) * exp * exp

and exp =
 | Lit of lit
 | Lvalue of lvalue
 | Unop of Ast.unop * exp
 | Binop of label(*NEW*) * exp * Ast.binop * exp
 | And of exp * exp
 | Or of exp * exp
 | Call of label(*NEW*) * exp * exp list
 | Methcall of label(*NEW*) * label(*NEW*) * exp * string * exp list (* second label for exp:string lvalue *)
 | Paren of exp

and stmt = { stmt_pos : pos; 
	     stmt     : stmt_desc }
and stmt_desc =
 | Break
 | If of exp * block * block
 | WhileDo of exp * block * label
(* | For of string * exp * exp * block *)
 | Doend of block
 | Assign of lvalue list * exp list
 | Local of string list * exp list
 | Callstmt of exp * exp list
 | Methcallstmt of label(*NEW*) * exp * string * exp list
 | Return of exp list

and block = block_desc option
and block_desc = { label : label;
		   stmts : stmt list }

type last = {
  name      : string option;
  last      : block;
  ret_label : label;
  pos_map   : (label * stmt) list;
  fun_map   : label -> lit;
  table_map : label -> lit;
}
